$!============================================================================
$!   MON_PML.COM
$!
$!   SMSC ENTITY CLASS MONITORING  --- run from SMSC account
$!
$! [optional]
$!  p1: verbose [0,1]         [default: logs are created (1)]
$!  p2: configuration file    [default: monitor_config.cnf]
$!  p3: output folder         [default: current / CONFIG FILE VALUE]
$!  p4: autosubmit [0,1]      [default: no autosubmit (0) / CONFIG FILE VALUE]
$!
$!============================================================================
$!
$! Version 1501.03
$!
$!
$!
$ set noverify
$ set noon
$ DCL_DATA :== $SMSC$ROOT:[BIN]DCL_DATA.EXE
$ scriptname = F$ENVIRONMENT("PROCEDURE")
$ working_dir = f$parse(scriptname, , , "DEVICE") + -
                f$parse(scriptname, , , "DIRECTORY")
$ scriptfile = scriptname - f$Parse(scriptname, , , "VERSION")
$ scriptname = f$Parse(scriptname, , , "NAME")
$ say := "if enable_log then write sys$output "
$ pml := $smsc$root:[bin]pml
$ mondir = F$ENVIRONMENT("DEFAULT")
$ enable_log = 1
$! Default for PML_SAMPLE_PERIOD (in minutes) will be overridden by [COMMON]
$! configuration settings read by DCL if present
$ PML_SAMPLE_PERIOD = 5
$!
$ IF F$TrnLnm("python_root", "LNM$SYSTEM", , , , ) .EQS. ""
$  THEN
$   pyth = f$search("SYS$COMMON:[PYTHON*.VMS]STARTUP.COM", 1)
$   IF pyth .NES. "" then @'pyth
$   IF .NOT. $STATUS THEN goto NOT_PYTHON
$ ENDIF
$ python_launch :== $python_root:[vms.bin]PYTHON.EXE
$!
$ IF F$TYPE(PYTHON) .EQS. ""
$  THEN
$   pyth = f$search("SYS$COMMON:[PYTHON*.VMS]SETUP.COM", 1)
$   IF pyth .NES. "" then @'pyth
$   IF .NOT. $STATUS THEN goto NOT_PYTHON
$ ENDIF
$!
$ if p2 .EQS. ""
$  then
$   config_file = f$parse(mondir, , , "device") + -
                  f$parse(mondir, , , "directory") + "MONITOR_CONFIG.CNF"
$  else
$   config_file = f$parse(p2, , , , "syntax_only")
$   config_file = f$extract(0, f$length(config_file) - 1, config_file)
$ endif
$!
$ if f$search(config_file) .EQS. "" then GOTO BAD_CONFIG
$!
$ DCL_DATA /FILE='config_file' "COMMON"
$!
$ IF p3 .EQS. ""
$  THEN
$   if F$TYPE (OUTPUT_FOLDER) .NES. ""
$    then
$     if f$length(OUTPUT_FOLDER) .EQ. 0 THEN OUTPUT_FOLDER = mondir
$    else
$     OUTPUT_FOLDER = mondir
$   endif
$   outdir = f$parse(OUTPUT_FOLDER, , , "device") + -
             f$parse(OUTPUT_FOLDER, , , "directory")
$  ELSE
$   outdir = f$parse(p3, , , "device") + f$parse(p3, , , "directory")
$ ENDIF
$!
$ if (p1 .EQS. "0") .or. (f$edit(p1, "UPCASE,COLLAPSE") .EQS. "N") -
   then enable_log = 0
$!
$!
$!
$ tmpfname   = outdir + "monitor_hcx.pml"
$ index_cfg = 0
$ entcfg_suffixes = "0,1,2,3,4,5,6,7,8,9"
$! entcfg_suffixes are the valid sufixes for multiple lines in
$! TODO: ==> MOVE TO CONFIG FILE AND DCL!
$!
$ DCL_DATA /FILE='config_file' "SMSC_ENTITIES"
$!
$ IF (F$TYPE (ENTITY_LIST) .EQS. "") .OR. (F$TYPE (PREFIX) .EQS. "")
$ THEN
$    goto BAD_CONFIG
$ ELSE
$    IF (F$LENGTH (ENTITY_LIST) .EQ. 0) .OR. (F$LENGTH (PREFIX) .EQS. "")
$    THEN
$       WRITE SYS$OUTPUT "Missing value for mandatory field(s)."
$       GOTO BAD_CONFIG
$    ENDIF
$ ENDIF
$!
$!
$!
$ NODE_NAME = F$GETSYI ("NODENAME")
$ NODE_NAME = F$EXTRACT(0, F$LENGTH(NODE_NAME) - 1, NODE_NAME)
$ IF F$GETSYI("CLUSTER_NODES") .EQ. 0
$  THEN
$    NODE_NR = 1
$  ELSE
$    NODE_NR = F$GETSYI("CLUSTER_NODES")
$ ENDIF
$ TIMESTAMP = F$CVTIME(F$TIME(), "ABSOLUTE")
$ NEXTIME = F$CVTIME("''TIMESTAMP'+00:''PML_SAMPLE_PERIOD':00", "ABSOLUTE")
$ DAYTAG = F$FAO("!2ZL!AS!AS", F$INT(F$CVTIME(TIMESTAMP, , "DAY")), -
  F$CVTIME(TIMESTAMP, "ABSOLUTE", "MONTH"), F$CVTIME(TIMESTAMP, , "YEAR"))
$!
$!
$ SAY "Looking for previous instances..."
$!
$ TEMP = F$GETQUI("")
$ Queue_loop:
$       Qname = F$GetQui("Display_Queue", "QUEUE_NAME", "*")
$       If Qname .Eqs. "" Then Goto Out_Loop
$!       SAY "Queue : ''Qname'"
$       If .Not. F$GetQui("Display_Queue", -
                          "QUEUE_BATCH", -
                          "*", -
                          "WILDCARD, FREEZE_CONTEXT") Then Goto Queue_Loop
$! SAY "Have found a suitable batch queue... now looking for suitable jobs..."
$!
$ Job_Loop:
$       NoAccess = F$GetQui("Display_Job", "JOB_INACCESSIBLE", , "ALL_JOBS")
$       If NoAccess .Eqs. "TRUE" Then Goto Job_Loop
$       IF NoAccess .Eqs. ""     Then Goto Queue_Loop
$       Jname = F$GetQui("DISPLAY_JOB", "JOB_NAME", , "FREEZE_CONTEXT")
$       If F$Extract(0, F$LENGTH(scriptname), Jname) .NES. scriptname -
         Then Goto Job_Loop
$       found_entry = F$GetQui("DISPLAY_JOB", "ENTRY_NUMBER", , -
                               "FREEZE_CONTEXT")
$       if f$environment("INTERACTIVE") then goto ALREADY_RUNNING
$       my_job = f$getqui("display_job", "entry_number", , "this_job")
$       if my_job .ne. found_entry
$        then
$         SAY "Found job with ID: ''found_entry', waiting for it to finish..."
$         synchronize /ENTRY='found_entry'
$        endif
$!
$ Out_Loop:
$!
$! Resubmit, if told to do so
$!
$ resubmit_myself = -
       "SUBMIT /NOPRINT/QUEUE=SYS$BATCH/after=""''NEXTIME'"" " + -
       "''scriptfile' /param=(''enable_log',''config_file',''OUTDIR',1)"
$!
$ if enable_log then resubmit_myself = resubmit_myself + -
  "/log=SYS$SYSDEVICE:[SMSC.LOG]''PREFIX'PML_''NODE_NAME'.LOG"
$!
$!
$ If p4 .NES. ""
$  Then
$    if (p4 .EQS. 1) .or. (f$edit(p4, "COLLAPSE, UPCASE")) .EQS. "Y"
$     Then
$      resubmission = 1
$     Else
$       resubmission = 0
$     Endif
$  Else
$    resubmission = 0
$    if F$TYPE (MON_AUTOSUBMIT) .NES. ""
$    then
$     if (MON_AUTOSUBMIT .EQS. "1") .or. -
      (f$edit(MON_AUTOSUBMIT, "UPCASE, COLLAPSE") .EQS. "Y") -
      THEN resubmission = 1
$    endif
$ Endif
$!
$ if resubmission
$  then
$   SAY "Resubmitting to run in ''PML_SAMPLE_PERIOD' minutes:"
$   resubmit_myself
$ endif
$!
$ num_suffixes = 0
$ count_suffix:
$ IF F$ELEMENT(num_suffixes, ",", entcfg_suffixes) .NES. ","
$  THEN
$   num_suffixes = num_suffixes + 1
$   GOTO count_suffix
$ ENDIF
$!
$!
$! index_cfg goes through ENTITY_LIST string in config file
$!
$! Calls to PML, generates the OUTPUT, and goes through the loop of entities
$! configured in ENTITY_LIST
$ ENTITY_LOOP:
$!
$  ENTITY = F$EDIT(F$ELEMENT(index_cfg, ",", ENTITY_LIST), "COLLAPSE, UPCASE")
$  if (ENTITY .EQS. ",") .OR. (ENTITY .EQS. "") THEN goto END_ENTITY_LOOP_1
$!
$!
$ entname = ENTITY
$ ELEMCLASS = ""
$  pml_result = "sys$scratch:''ENTITY'_out.tmp"
$  say ">>> Gathering PML for entity ''entname'"
$!
$! Check if output and temporary files are already open or exist
$  if f$trnlnm("pml_out") .nes. "" then close pml_out
$  if f$search(tmpfname) .NES. "" THEN DELETE 'tmpfname';*
$!
$  OPEN /write/error=bad_end pml_out 'tmpfname'
$   WRITE pml_out "CONNECT"
$   WRITE pml_out "CANCEL CLASS MD /ALARM=ALL"
$   WRITE pml_out "ENABLE OUTPUT /FILE=''pml_result'"
$!  WRITE pml_out "TIMESTAMP"
$!
$! Going through the ENTITY_X entries, if any. Order with index_suffix
$ index_suffix = 0
$ cum_string = ""
$ SUFFIX_LOOP:
$  index_ent = 0
$! index_ent goes through the list of configured entities of a class
$! (ELEMCLASS element list)
$ if index_suffix .GE. 1 -
   then ENTITY = entname + "_" + f$element(index_suffix-1, ",", -
                                           entcfg_suffixes)
$ say "''index_suffix'/''num_suffixes': Processing entry for ''ENTITY'"
$!
$! 'ENTITY' is actually the attributes of the
$! PML> show class 'entname' /attributes command
$!
$   statistical = 0
$   cumulative = 0
$   IF F$TYPE('ENTITY') .EQS. ""
$    THEN
$    say "Nothing was found for ''ENTITY'"
$    goto next_suffix
$   ENDIF
$   pml_attrib = f$element(1, "|", 'ENTITY')
$   num_attrib = 0
$ countloop:
$   if f$element(num_attrib + 1, "/", pml_attrib) .NES. "/"
$    then
$     num_attrib = num_attrib + 1
$     goto countloop
$   endif
$   counter_type = f$edit(f$element(0, "|", 'ENTITY'), "COLLAPSE, UPCASE")
$   if counter_type .EQS. "STA" then statistical = 1
$   if counter_type .EQS. "CUM" then cumulative = 1
$   if counter_type .EQS. "TAB" then cumulative = 2
$   say "Found ''num_attrib' parameters for ''ENTITY': ""''pml_attrib'"""
$! We get all the configured entity names of class "entname" --> "ELEMCLASS"
$   gosub ENTITY_NAME_LOOKUP
$   num_entities = f$element(0, "|", ELEMCLASS)
$   cum_string = cum_string + f$fao("!''num_attrib'*''cumulative'")
$ ENTCLASSLOOP:
$   pml_sentence = "SHOW CLASS ''entname' ENTITY " + -
                   f$element(index_ent, ",", f$element(1, "|", ELEMCLASS)) + -
                   " ''pml_attrib'"
$   if statistical -
     then pml_sentence = pml_sentence + " /CONTENT=''PML_SAMPLE_PERIOD'"
$   if counter_type .EQS. "INT" -
     then pml_sentence = pml_sentence + " /INTERVAL=''PML_SAMPLE_PERIOD'"
$   WRITE /SYMBOL pml_out pml_sentence
$   index_ent = index_ent + 1
$   if index_ent .LT. num_entities then goto ENTCLASSLOOP
$ next_suffix:
$  index_suffix = index_suffix + 1
$  if index_suffix .LE. num_suffixes THEN goto SUFFIX_LOOP
$   WRITE pml_out "DISCONNECT"
$   WRITE pml_out "EXIT"
$  CLOSE pml_out
$!
$!
$ if enable_log then type 'tmpfname'
$ if ELEMCLASS .EQS. "" then goto PURGE_AND_NEXT
$!
$! Running PML to actually get the values
$! Opclass=2 means read-only
$!
$ define/user sys$output nla0:
$ pml /prompt="MON> " /input_file='tmpfname' /queue=monitor_hcx -
      /response_time=10 /opclass=2
$!
$!
$! Modifying ELEMCLASS to parse into python script
$ ELEMCLASS = NODE_NAME + "," + f$element(1, "|", ELEMCLASS)
$!
$! Converting PML output into CSV;
$! output_shc is used to avoid max command length errors
$ output_csv = "''outdir'''entname'_''NODE_NAME'_''DAYTAG'.csv"
$ output_tmp = "''outdir'''entname'_''NODE_NAME'_''DAYTAG'.tmp"
$!If first run of the day, check if yesterday's latest is available
$!and copy to 00:00:00.00
$ last_outmp = f$search(output_tmp)
$ if last_outmp .EQS. "" then GOSUB first_of_day_check
$ output_shc :== "''output_csv'"
$ say /symbol "PML2CSV ""''pml_result'"" ""''entname'"""
$ say /symbol "Elemclass=''ELEMCLASS'"
$ say /symbol "At ''timestamp' , OUTPUT=''output_csv'"
$ say "Nature of counters: --''cum_string'--"
$ python_launch 'working_dir'pml2csv.py "''pml_result'" "''entname'" -
                "''ELEMCLASS'" "''timestamp'" 'output_shc "''cum_string'"
$ delete 'pml_result';*
$ adjusted_time = f$cvtime("''timestamp'-00:''PML_SAMPLE_PERIOD':00", -
                           "ABSOLUTE")
$ adjusted_time = f$cvtime("''adjusted_time'-00:''PML_SAMPLE_PERIOD':00", -
                           "ABSOLUTE")
$ Say "Deleting old temporary files"
$ delete /log 'outdir''entname'_'NODE_NAME'_*.tmp;* /before="''adjusted_time'"
$! Check version and reset if higher than 30000
$ if (f$parse(last_outmp, , , "version") - ";" .gt. 30000) -
   then @smsc$root:[scripts]reset_version 'outout_tmp "/log"
$!
$ PURGE_AND_NEXT:
$ delete 'tmpfname';*
$ index_cfg = index_cfg + 1
$ goto ENTITY_LOOP
$!
$ END_ENTITY_LOOP_1:
$ if resubmission .AND. enable_log
$  then
$   say "Deleting old log files..."
$   logfile_reg = "SYS$SYSDEVICE:[SMSC.LOG]''PREFIX'PML_''NODE_NAME'.LOG"
$   purge 'logfile_reg /keep=10
$!  Check version and reset if higher than 30000
$   say "''logfile_reg': " + -
        f$parse(f$search(logfile_reg), , , "version") - ";"
$   if (f$parse(f$search(logfile_reg), , , "version") - ";" .gt. 30000) -
      then @smsc$root:[scripts]reset_version 'logfile_reg "/log /keep=10"
$ endif
$!
$ EXTRASCRIPT_LOOP:
$ Say "Looking for extra scripts..."
$! All MON_PML_EXTRA*.COM scripts must accept the same type of optional input
$! arguments:
$!  p1: verbose [0,1]         [default: logs are created (1)]
$!  p2: output folder         [default: running directory]
$!  p3: timestamp             [default: current system time]
$!
$ extra_com = f$search("''working_dir'MON_PML_EXTRA*.COM", 123)
$ if extra_com .NES. ""
$  then
$   Say "Calling ''extra_com'..."
$   @'extra_com' "''enable_log'" "''outdir'" "''timestamp'"
$   Say "Extra script finished!  (''extra_com')"
$   goto EXTRASCRIPT_LOOP
$ endif
$!
$! Move all files older than KEEPDAYS (under COMMON) to the [.old] subfolder
$!
$ olddir = f$extract(0, f$length(outdir) - 1, outdir) + ".old]"
$ if f$search("''outdir'old.dir") .EQS. "" then create /dir 'olddir
$ IF F$TYPE(KEEP_DAYS) .NES. ""
$  THEN IF F$LENGTH(KEEP_DAYS) .EQ. 0 THEN KEEP_DAYS = 3
$! Otherwise it will keep the configured value
$  ELSE KEEP_DAYS = 3
$ ENDIF
$!
$ rename 'outdir'*.csv 'olddir' /before="-''KEEP_DAYS'-" /LOG
$ Say "Finished!"
$ exit %X00030001
$!
$!
$!
$!
$!
$!
$!
$!
$!============================================================================
$!
$! SUBROUTINES
$!
$!============================================================================
$!
$! LOOK FOR THE LIST OF ENTITIES OF A CLASS, returns contents in ELEMCLASS;
$! source is "entname"
$! Format of ELEMCLASS is "Nr_of_entities|ENTname_1,ENTname_2,...,ENTname_N"
$!
$ ENTITY_NAME_LOOKUP:
$ ENTITIES = ""
$ CINDEX = 0
$ NODE_IX = 0
$!
$!
$ say "LOOKING FOR CONFIGURED ENTITIES OF CLASS ''entname' ON " + -
      "CLUSTER ''NODE_NAME'..."
$!
$ FILE = "SYS$SYSDEVICE:[SMSC]SMH.PML"
$!
$ NODE_LOOP:
$ say "Attempt to read PML file: ''FILE'"
$ IF F$SEARCH(FILE) .NES. "" THEN gosub READ_FILE
$ NODE_IX = NODE_IX + 1
$ FILE = "SYS$SYSDEVICE:[SMSC]''NODE_NAME'''NODE_IX'.PML"
$ IF NODE_IX .LE. NODE_NR THEN GOTO NODE_LOOP
$!
$ say "Counting elements..."
$ CINDEX = 0
$ COUNTLIST:
$ IF F$ELEMENT(CINDEX, ",", ELEMCLASS) .NES. ","
$  THEN
$   CINDEX = CINDEX + 1
$   GOTO COUNTLIST
$  ENDIF
$ ELEMCLASS = "''CINDEX'|" + ELEMCLASS
$!
$ say "Finished! Result=""''ELEMCLASS'"""
$ RETURN %X00030001
$!
$!
$!
$ READ_FILE:
$!
$ OPEN /READ /ERROR=NOPMLFILE PMLFILE 'FILE'
$!
$ READ_FILE_LOOP:
$  READ /END=END_READ PMLFILE LINE
$  LINE = F$EDIT(LINE, "TRIM,COMPRESS")
$! If an entry in the PML file is commented out,
$! the entity is not created=> SKIP
$  IF F$EXTRACT(0, 1, LINE) .EQS. "!" THEN GOTO READ_FILE_LOOP
$    IF F$LOCATE(entname, LINE) .NES. F$LENGTH(LINE)
$     THEN
$      CANDIDATE = F$ELEMENT(2, " ", LINE)
$      VALID_CANDIDATE = 0
$      GOSUB NEW_ENTITY
$      IF VALID_CANDIDATE .EQ. 1
$       THEN
$        ELEMCLASS = F$ELEMENT(4, " ", LINE)
$       ELSE
$        LOOKUP = LOOKUP + 1
$        ELEMCLASS = ELEMCLASS + "," + F$ELEMENT(4, " ", LINE)
$      ENDIF
$     ENDIF
$  GOTO READ_FILE_LOOP
$ END_READ:
$ CLOSE PMLFILE
$ RETURN
$!
$!
$!
$ NEW_ENTITY:
$!
$ LOOKUP = 0
$ CHECK_LOOP:
$  IF F$ELEMENT(LOOKUP, " ", ENTITIES) .EQS. CANDIDATE THEN RETURN
$  LOOKUP = LOOKUP + 1
$  IF LOOKUP .LT. CINDEX THEN GOTO CHECK_LOOP
$  VALID_CANDIDATE = 1
$  ENTITIES = F$EDIT(ENTITIES + " " + CANDIDATE, "TRIM")
$  CINDEX = CINDEX + 1
$ RETURN
$!
$!
$!
$ first_of_day_check:
$ YESTERDAYTAG = F$CVTIME("''TIMESTAMP'-1-", "ABSOLUTE", "DATE") - "-" - "-"
$ tmp_yst = "''outdir'''entname'_''NODE_NAME'_''YESTERDAYTAG'.tmp"
$ say "Looking for last result."
$ if f$search(tmp_yst) .NES. ""
$  then
$   say ">>> Copying last entry from yesterday and deleting any others"
$   copy 'tmp_yst 'output_tmp /log
$   delete 'tmp_yst';* /log
$ endif
$ RETURN
$!
$!
$!
$!============================================================================
$!
$! FAILURE EXITPOINTS
$!
$!============================================================================
$!
$!
$ BAD_END:
$  WRITE SYS$OUTPUT -
   "Error saving temp. file to ''OUTDIR'. Write permission or disk full?"
$  THE_END:
$  exit
$!
$!
$! BAD_T4LOGICAL:
$!  WRITE SYS$OUTPUT "No logical(s) defined: T4$SYS || T4$DATA"
$!  WRITE SYS$OUTPUT "Please do:"
$!  WRITE SYS$OUTPUT "define /system /exec t4$data <output_folder_for_T4>"
$!  WRITE SYS$OUTPUT "define /system /exec t4$sys  <T4 tool directory>"
$!  WRITE SYS$OUTPUT ""
$!  exit
$!
$ NOT_PYTHON:
$  WRITE SYS$OUTPUT -
    "Python not found, check its startup and setup scripts are present " + -
    "in SYSTARTUP_VMS and SYLOGIN respectively..."
$  WRITE SYS$OUTPUT ""
$  exit
$!
$ ALREADY_RUNNING:
$  WRITE SYS$OUTPUT "There is already an instance running for " + -
                    "''scriptname' with ID ''found_entry' on queue ''Qname':"
$  WRITE SYS$OUTPUT ""
$  show entry 'found_entry'
$  WRITE SYS$OUTPUT "Aborting..."
$  WRITE SYS$OUTPUT ""
$  exit
$!
$ BAD_CONFIG:
$  WRITE SYS$OUTPUT "Bad config file ''config_file' or file missing, " + -
                    "aborting."
$  exit
$!
$ NOPMLFILE:
$ WRITE SYS$OUTPUT "Not possible to open ''FILE'!!!"
$ WRITE SYS$OUTPUT "Does file exist?"
$ CLOSE OUTFILE
$ exit

