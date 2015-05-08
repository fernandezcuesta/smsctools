$! ####################################################################
$!# OpenVMS backup script                                              #
$!# J.M. Fernandez (fernandez.cuesta@gmail.com)                        #
$! ####################################################################
$!
$! =====================================================================
$! SYS_BACKUP.COM   (configuration file: BACKUP_CONFIG.DAT)
$!
$!  p1: Autosubmit every Ndays (Yes/[No]; opt: "NOW")
$!  p2: Incremental (Yes/[No])
$!  p3: s/FTP file push (On/[Off]/Pull=none)
$!  p4: Backup directory ([bck_dsk:[backup]])
$! =====================================================================
$! Preference: Input parameters ELSE backup_config.dat ELSE coded values
$! A "Default" value ignores the parameter
$! Incremental = yes => Incremental AND Full backup during Backup_wday
$! ---------------------------------------------------------------------
$!______________________________________________________________________
$! Redistribution and use of this software, with or without modification
$! are permitted provided the following conditions are met:
$! - Redistribution of source code must retain the above copyright
$!   notice, this list of conditions and this disclaimer.
$! - This software is distributed on an "AS IS" BASIS, WITHOUT ANY
$!   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
$!______________________________________________________________________
$!
$!
$!============    Default coded values (last preference)    ============
$!
$ DEFAULT_Backup_location = "bck_dsk:[backup]"
$ DEFAULT_Backup_wday = "Monday"
$ DEFAULT_Backup_time = "02:00"
$ DEFAULT_INCREMENTAL = "No"
$ DEFAULT_RESCHEDULE = "No"
$ DEFAULT_SDB_KEEPDAYS = 30                 ! #days to store SDB backups
$ DEFAULT_BACKUP_LOG_DIR = "sys$scratch"
$ DEFAULT_BACKUP_EXCLUDELIST = ""
$ DEFAULT_BACKUP_REMOTE_DIRECTORY = "."
$ DEFAULT_BACKUP_DAYS_KEEP_LOCAL_ARCHIVE = "30"
$ DEFAULT_BACKUP_REM_NODE = "ftp_san"
$ DEFAULT_BACKUP_REM_USER = "ANONYMOUS"
$ DEFAULT_FILE_TRANSFER = "PULL"
$ DEFAULT_BACKUP_REMOTE_SYSTEM_TYPE = "UNIX"
$ DEFAULT_FILE_COUNT_FOR_TRANSFER = 3       ! #attempts for transference
$ DEFAULT_ZIP_ARCHIVE = "SKIP"                    ! #skips ZIP archiving
$ DEFAULT_SDB_FORCE = "NO"
$ DEFAULT_REMOTE_SFTP_COMMAND = ""
$ DEFAULT_SDB_NAME = "db_sdb"
$!
$!======================================================================
$ set noverify
$ set noon
$ say :== "write sys$output "
$ line = f$fao("!80*-")
$ TRAP := REQUEST /TO=OPER1
$ BELL[0,7] = 7                                          ! Terminal bell
$ ESC[0,7]  = 27                                      ! Escape character
$ BOLD = ESC+"[1;7m"                                         ! Bold type
$ NORM = ESC+"[0m"                                         ! Normal type
$ DEFINE /sys/exec BACKUP SYS$COMMON:[SYSEXE]BACKUP.EXE
$ zip := $cmg$tools:zip

$ CONFIG_PARSER$ENVIRONMENT("PROCEDURE")
$ mydir = f$parse(myscript,,,"DEVICE") + f$parse(myscript,,,"DIRECTORY")
$ myscript = myscript - f$Parse(myscript,,,"VERSION")
$ myname = f$parse(myscript,,,"NAME")
$ freeblocks = f$getdvi("dsa0:","FREEBLOCKS")
$ maxblocks = f$getdvi("dsa0:","MAXBLOCK")
$ max_backup_size = (maxblocks - freeblocks) * 7/10    !assume 0.7 ratio
$!
$ python 'mydir'CONFIG_PARSER.PY --file 'mydir'BACKUP_CONFIG.DAT --section SYS_BACKUP_CFG
$ check_param = "BACKUP_LOCATION"
$ gosub check_value
$ check_param = "BACKUP_WDAY"
$ gosub check_value
$ check_param = "BACKUP_TIME"
$ gosub check_value
$ check_param = "INCREMENTAL"
$ gosub check_value
$ check_param = "RESCHEDULE"
$ gosub check_value
$ check_param = "SDB_KEEPDAYS"
$ gosub check_value
$ check_param = "FILE_COUNT_FOR_TRANSFER"
$ gosub check_value
$ check_param = "BACKUP_LOG_DIR"
$ gosub check_value
$ check_param = "BACKUP_EXCLUDELIST"
$ gosub check_value
$ check_param = "BACKUP_REMOTE_DIRECTORY"
$ gosub check_value
$ check_param = "BACKUP_DAYS_KEEP_LOCAL_ARCHIVE"
$ gosub check_value
$ check_param = "BACKUP_REM_NODE"
$ gosub check_value
$ check_param = "BACKUP_REM_USER"
$ gosub check_value
$ check_param = "FILE_TRANSFER"
$ gosub check_value
$ check_param = "BACKUP_REMOTE_SYSTEM_TYPE"
$ gosub check_value
$ check_param = "ZIP_ARCHIVE"
$ gosub check_value
$ check_param = "SDB_FORCE"
$ gosub check_value
$ check_param = "REMOTE_SFTP_COMMAND"
$ gosub check_value
$ check_param = "SDB_NAME"
$ gosub check_value
$ BACKUP_REMOTE_SYSTEM_TYPE = -
  f$edit(BACKUP_REMOTE_SYSTEM_TYPE,"UPCASE,COLLAPSE")
$ ZIP_ARCHIVE = f$edit(ZIP_ARCHIVE,"UPCASE,COLLAPSE")
$!
$! Check if BACKUP_LOG_DIR exists, else create it, else set default one
$ BACKUP_LOG_DIR = f$parse(BACKUP_LOG_DIR,,,"DEVICE","SYNTAX_ONLY") + -
  f$parse(BACKUP_LOG_DIR,,,"DIRECTORY","SYNTAX_ONLY")
$ dir_deep = 0
$ dirpath = f$parse(BACKUP_LOG_DIR,,,"DIRECTORY","SYNTAX_ONLY")-"["-"]"
$ s2loop:
$  if f$element(dir_deep,".",dirpath) .NES. "."
$   THEN
$    dir_deep = dir_deep + 1
$    goto s2loop
$  endif
$!
$ if dir_deep .EQ. 1
$  then
$   bck_logdir2 = f$parse(BACKUP_LOG_DIR,,,"DEVICE","SYNTAX_ONLY") + -
                  "[000000]''dirpath'.dir"
$  else
$   lastdir = f$element(dir_deep-1,".",dirpath)
$   bck_logdir2 = f$parse(BACKUP_LOG_DIR,,,"DEVICE","SYNTAX_ONLY") + -
                  "[" + (dirpath - ".''lastdir'") + "]''lastdir'.DIR"
$ endif
$!
$ IF F$SEARCH(bck_logdir2) .EQS. "" THEN CREATE /DIR 'BACKUP_LOG_DIR'
$ IF F$PARSE(BACKUP_LOG_DIR) .EQS. "" -
   THEN BACKUP_LOG_DIR = DEFAULT_BACKUP_LOG_DIR
$!
$! Checking that backup is not done in SYS$SYSDEVICE
$ if f$parse(Backup_location,,,"device","NO_CONCEAL") .EQS. -
     f$parse("sys$sysdevice:",,,"device","NO_CONCEAL") -
     then goto DEST_IS_DSA0
$!
$ bck_dev = f$parse(Backup_location,,,"device","SYNTAX_ONLY")
$ bck_dir = f$parse(Backup_location,,,"directory","SYNTAX_ONLY")
$ Backup_location = bck_dev + bck_dir
$!
$! Check that local backup location exists, else create it
$ param4 = F$EDIT(p4, "UPCASE,COLLAPSE")
$ IF (param4 .NES. "") .AND. (param4 .NES. "DEFAULT") -
  THEN Backup_location = param4
$ IF F$PARSE(Backup_location) .EQS. "" -
  THEN create /dir 'Backup_location /log
$ If F$PARSE(Backup_location) .EQS. "" then goto FULLBERROR
$!
$! Look for transfer method (FTP, SFTP or PULL=none)
$ param3 = F$EDIT(p3, "UPCASE,COLLAPSE")
$ IF (param3 .EQS. "SFTP") .OR. (param3 .EQS. "FTP") .OR. -
     (param3 .EQS. "PULL") .OR. (param3 .EQS. "NONE")
$    THEN FILE_TRANSFER = param3
$    ELSE FILE_TRANSFER = f$edit(FILE_TRANSFER,"UPCASE,COLLAPSE")
$ ENDIF
$!
$ param2 = F$EDIT(p2, "UPCASE,COLLAPSE")
$ if param2 .EQS. "" then param2 = f$edit(INCREMENTAL,"UPCASE,COLLAPSE")
$ if param2 .EQS. "YES"
$    then delta = 1
$    else delta = 0
$ endif
$!
$ param1 = F$EDIT(p1, "UPCASE,COLLAPSE")
$ if (param1 .EQS. "") .OR. (param1 .EQS. "NOW") -
   then param1 = f$edit(RESCHEDULE,"UPCASE,COLLAPSE")
$ if param1 .EQS. "YES"
$  then autosubmit = 1
$  else autosubmit = 0
$ endif
$!
$ mimer_db = f$trnlnm("MIMER_DATABASE")
$ IF mimer_db .NES. ""
$  THEN                                                        !Mimer DB
$   sdb_location = ""
$   @sbr$root:[scripts]LIB_GET_MIMER_DB_DIR "''mimer_db'" "sdb_location"
$  ELSE                                                      !Oracle RDB
$   sdb_location = "SMSC_RDB"
$ ENDIF
$!
$ SDB_DISK = 1
$ if (F$PARSE(sdb_location,,,"DEVICE","NO_CONCEAL") .EQS. -
     F$PARSE("SYS$SYSDEVICE",,,"DEVICE","NO_CONCEAL")) .AND. -
     (F$EDIT(SDB_FORCE,"upcase,collapse") .NES. "YES") then SDB_DISK = 0
$!
$ say line
$ say "Backup location = <''Backup_location'>"
$ say "Backup time = <''Backup_time'>"
$ say "Backup weekday = <''Backup_wday'>"
$ say "Incremental? (0|1) = <''delta'>"
$ say "Reschedule? (0|1) = <''autosubmit'>"
$ IF SDB_DISK then say "SDB backup files deleted " + -
                       "after=<''sdb_keepdays' days>"
$ say "Backup keep days = <''backup_days_keep_local_archive'>"
$ say "Script log location = <''backup_log_dir'>"
$ say "Exclude list = <''backup_excludelist'>"
$ say line
$!
$ If f$environment("INTERACTIVE")
$  THEN
$    tock = F$CVTIME("+00:02","ABSOLUTE") !Putting a 2 min delta to give
$!                                    some time for the script execution
$    Current_wday = F$EDIT(F$CVTIME(tock,,"WEEKDAY"),"UPCASE,COLLAPSE")
$    Current_time = F$CVTIME(tock,,"TIME")
$    Current_date = F$CVTIME(tock,"ABSOLUTE","DATE")
$    Backup_wday = F$EDIT(Backup_wday,"UPCASE,COLLAPSE")
$    Backup_date = Current_date
$    FullBck_date= Current_date
$!
$! Additional option for starting the backup (almost) immediately
$    if f$edit(p1,"UPCASE,COLLAPSE") .EQS. "NOW"
$     then
$      Backup_wday = Current_wday
$      Backup_time = Current_time
$    endif
$!
$    If (Current_time .GTS. Backup_time) .OR. -
        (Current_wday .NES. Backup_wday)
$     Then
$      DayLoop: FullBck_date = -
       F$CVTIME(FullBck_date+"+1-","ABSOLUTE","DATE")
$      IF Backup_wday .NES. -
          F$EDIT(F$CVTIME(FullBck_date,,"WEEKDAY"),"UPCASE,COLLAPSE") -
       THEN GOTO DayLoop
$    EndIf
$!
$    IF Current_time .GTS. Backup_time THEN -
       Backup_date = F$CVTIME(Backup_date+"+1-","ABSOLUTE","DATE")
$  ELSE
$    Backup_date = F$CVTIME("+1-","ABSOLUTE","DATE")
$    FullBck_date = F$CVTIME("+7-","ABSOLUTE","DATE")
$  ENDIF
$!
$ Bck_realday=f$edit(F$CVTIME(Backup_date,,"WEEKDAY"),"UPCASE,COLLAPSE")
$! If incremental backup overlaps full backup day --> move 1 day ahead
$ IF delta .AND. (Backup_wday .EQS. Bck_realday)
$  THEN
$   Backup_date = F$CVTIME(Backup_date+"+1-","ABSOLUTE","DATE")
$   Bck_realday = -
              f$edit(F$CVTIME(Backup_date,,"WEEKDAY"),"UPCASE,COLLAPSE")
$ ENDIF
$!
$! At this point we have real:
$! Backup_date, Backup_wday, Backup_time, Bck_realday, FullBck_date
$!
$ if delta
$   then
$     SBDATE = F$CVTIME(Backup_date,,"DATE") - "-" - "-"
$     SBDATE = F$EXTRACT(2,F$LENGTH(SBDATE)-2,SBDATE)
$     BACKUP_TYPE_TAG = "DELTA"
$   else
$     SBDATE = F$CVTIME(Fullbck_date,,"DATE") - "-" - "-"
$ 	  SBDATE = F$EXTRACT(2,F$LENGTH(SBDATE)-2,SBDATE)
$     BACKUP_TYPE_TAG = "FULL"
$ endif
$ SET PROCESS /NAME="''BACKUP_TYPE_TAG'BCK_''SBDATE'"
$!
$ found_type = -1
$ TEMP = F$GETQUI("")
$ Queue_loop:
$       Qname = F$GetQui("Display_Queue", "QUEUE_NAME", "*")
$       If Qname .Eqs. "" Then Goto Out_Loop
$       If .Not. -
 F$GetQui("Display_Queue","QUEUE_BATCH","*","WILDCARD,FREEZE_CONTEXT") -
         Then Goto Queue_Loop
$ Job_Loop:
$       NoAccess=F$GetQui("Display_Job","JOB_INACCESSIBLE",,"ALL_JOBS")
$       If NoAccess .Eqs. "TRUE" Then Goto Job_Loop
$       If NoAccess .Eqs. ""     Then Goto Queue_Loop
$       Jname = F$GetQui("DISPLAY_JOB","JOB_NAME",,"FREEZE_CONTEXT")
$       If F$Extract(0,F$LENGTH(myname),Jname) .NES. myname -
         Then Goto Job_Loop
$       found_entry = -
                F$GetQui("DISPLAY_JOB","ENTRY_NUMBER",,"FREEZE_CONTEXT")
$       say "Found another instance already queued! ''Jname' " + -
            "(''found_entry')"
$       found_p1=F$GetQui("DISPLAY_JOB","PARAMETER_1",,"FREEZE_CONTEXT")
$       found_p2=F$GetQui("DISPLAY_JOB","PARAMETER_2",,"FREEZE_CONTEXT")
$       my_job = f$getqui("display_job","entry_number",,"this_job")
$       if my_job .ne. found_entry
$        then
$           if f$edit(found_p1,"COLLAPSE,UPCASE") .EQS. "YES"
$            then found_auto = 1
$            else found_auto = 0
$           endif
$           if f$edit(found_p2,"COLLAPSE,UPCASE") .EQS. "YES"
$            then found_type = 1
$            else found_type = 0
$           endif
$           IF f$environment("INTERACTIVE")
$            THEN
$! When interactive,only 2 situations where it can coexist
$! (negative=error)
$! 1) There is a full backup queued, today isn't Backup_wday and I'm
$!    incremental w/o autosubmission
$! 2) There is an incremental queued w/o autosubmission, I'm full
$!
$             if (found_type .OR. (Backup_wday .EQS. Bck_realday) .OR. -
                (.NOT. delta) .OR. autosubmit) .AND. -
                ((.NOT. found_type) .OR. found_auto .OR. delta) -
              then goto ERROR_ALREADY_RUNNING
$            ELSE        ! When batch, only 1 exception:
$             if (.NOT. found_type) .OR. delta .OR. found_auto -
                 then goto Submitted
$           ENDIF
$        else
$         say "Own was found, ignoring..."
$         goto Job_Loop
$       endif
$!
$ Out_Loop:
$!
$! First submit (if told to do so), then run if in batch mode
$!
$ If f$environment("INTERACTIVE") then autosubmit=1
$ IF delta .AND. autosubmit
$  THEN
$  SUBMIT /noprint/que=SYS$BATCH/AFTER="''Backup_date'+''Backup_time'" -
          /name="''myname'_INC" /log='BACKUP_LOG_DIR''myname'.log -
          /param=('param1',"YES",'Backup_location') /priorit=3 'myscript
$  IF f$type(found_type) .NES. "" then -
       if .NOT. found_type then goto Submitted
$ ENDIF
$!
$ If autosubmit then SUBMIT /noprint/queue=SYS$BATCH/priority=3 -
                     /AFTER="''FullBck_date'+''Backup_time'" -
                     /name="''myname'_FULL" -
                     /log='BACKUP_LOG_DIR''myname'.log -
                     'myscript /param=('param1',"NO",'Backup_location')
$ Submitted:
$ if f$environment("INTERACTIVE") then EXIT %X00030001
$ say line
$ say "Executing phase at " + f$time()
$!
$ if delta
$  then say "Launching incremental backup..."
$  else say "Launching full backup..."
$ endif
$!
$! Running (always in batch mode!)
$ set rms/block=127/buff=8/exten=65535      !Optimize Backup performance
$ db_bck_location = "dsa0:[backup]"
$ NR_Clunodes = f$getsyi("CLUSTER_NODES")
$ node = f$getsyi("nodename")
$ node = F$EXTRACT(0,F$LENGTH(node)-1,node)
$ Full_Version = F$GetSyi("Version")
$ Version_For_Compare = 99999
$ TODATE = F$CVTIME(,,"DATE") - "-" - "-"
$ TODATE = F$EXTRACT(2,F$LENGTH(TODATE)-2,TODATE)
$ If (F$Extract(0, 1, Full_Version) .Eqs. "V")
$  Then
$   If F$Locate ("-",Full_Version) .Eq. F$Length(Full_Version) -
     Then Full_Version = Full_Version + "-0"
$   Major_Version = F$Element(0, ".", Full_Version)
$   Minor_Version = F$Element(0, "-", Full_Version)-Major_Version - "."
$   Edit_Level    = F$Extract(0, 1, F$Element(1,"-", Full_Version))
$   Major_Version = F$Extract(1, 1, Major_Version)
$   Version_For_Compare = 100*Major_Version+10*Minor_Version+Edit_Level
$  EndIf
$!
$ if .NOT. SDB_DISK then goto POST_SDB_BACKUP
$ ON ERROR THEN SAY BELL + BOLD + -
  "Error creating backup on ''db_bck_location'. " + -
  "No SDB backup is possible!" + NORM
$!
$ say line
$ say "  > Entering SDB backup phase"
$ IF F$PARSE(db_bck_location) .EQS. "" THEN create /dir 'db_bck_location /log
$!
$! Create SDB online backup and zip it with "-V"
$ IF mimer_db .NES. ""
$  THEN                                                        !Mimer DB
$     sdbBackup = "''db_bck_location'''node'_sdb_''TODATE'.bck"
$     @sbr$root:[scripts]bck_mimer_rdb.com 'sdbBackup'
$  ELSE                                                      !Oracle RDB
$     sdbBackup = "''db_bck_location'''node'_sdb_''TODATE'.rbf"
$     rmu /backup/online 'SDB_NAME' 'sdbBackup'
$ ENDIF
$ zip -9jm "-V" 'db_bck_location'sdb_backup_'TODATE' 'sdbBackup'
$ delete /log 'db_bck_location'sdb_backup_*.zip.* -
         /before="-''SDB_KEEPDAYS'-"
$ say "  > SDB backup phase finished!"
$!
$ POST_SDB_BACKUP:
$ ON ERROR THEN CONTINUE
$ ON SEVERE_ERROR THEN GOTO FULLBERROR
$!
$ bck_cmd = "backup /record/log/ignore=interlock"
$ if BACKUP_EXCLUDELIST .NES. ""
$  THEN
$   BACKUP_EXCLUDELIST = BACKUP_EXCLUDELIST + ",*.TPU$JOURNAL,GPT.SYS"
$   bck_cmd = bck_cmd + "/exclude=(''BACKUP_EXCLUDELIST') "
$ endif
$!
$ if (Version_For_Compare .GT. 830) .AND. -
     ((BACKUP_REMOTE_SYSTEM_TYPE .EQS. "VMS") .OR. -
                         (ZIP_ARCHIVE .EQS. "SKIP"))
$  then
$    bck_cmd = bck_cmd + "/data_format=compress"
$    zip_level = 0         !Indicate that archive was already compressed
$  else
$    zip_level = 2
$    max_backup_size = maxblocks - freeblocks     !overrides for old VMS
$ endif
$!
$ backup_file="''Backup_location'''node'_sysdisk_''BACKUP_TYPE_TAG'" + -
              "_''TODATE'.bck"
$ if delta
$  then
$   bck_cmd= "''bck_cmd'/fast/since=backup/label=""INC@''TODATE'"" " + -
    "dsa0:[000000...]*.*;* ''backup_file' /save_set"
$  else
$   IF BACKUP_EXCLUDELIST .EQS. ""
$    THEN
$      bck_cmd = "''bck_cmd' /image /label=""IMG@''TODATE'"" " + -
       "dsa0: ''backup_file' /save_set"
$    ELSE
$      bck_cmd = "''bck_cmd' /label=""Full@''TODATE'"" " + -
       "dsa0:[000000...]*.*;* ''backup_file' /save_set"
$   ENDIF
$ endif
$!
$ mynr = f$getsyi("nodename") - node
$ REPLY/ALL/BELL -
      "System Backup about to begin -- Open files will NOT be backed up"
$ say line
$ say "Backup on node ''node'''mynr' started on " + f$time()
$ say "''BACKUP_TYPE_TAG' backup command is:"
$ say bck_cmd
$!
$ Free_at_destination = f$getdvi(bck_dev,"FREEBLOCKS")
$ say "Free at destination = ''f$cunits(Free_at_destination)'"
$!
$ old_seek = "''Backup_location'''node'_SYSDISK_''BACKUP_TYPE_TAG'_*.*"
$ say "Looking for old backup copies: ''old_seek'"
$ oldest_b_file = F$SEARCH(old_seek,555)
$ if (BACKUP_REMOTE_SYSTEM_TYPE.NES."VMS").and.(ZIP_ARCHIVE.NES."SKIP")
$  then ovK = 22      !oversize factor due to ZIP compression in non VMS
$  else ovK = 10      !x10 due to integer-only operations in DCL
$ endif
$ if oldest_b_file .NES. ""
$  then
$   oldest_b_size = F$FILE_ATTRIBUTES(oldest_b_file,"ALQ")
$   new_bk_size = oldest_b_size*12/10*ovK/10
$   say "Previous backup was found with size: " + -
        "''f$cunits(oldest_b_size)'; assuming that " + -
        "''f$cunits(new_bk_size)' are needed."
$  else
$   new_bk_size = max_backup_size*ovK/10
$   say "Assuming ''f$cunits(new_bk_size)' are needed " + -
        "since no previous backup was found and DSA0: current " + -
        "usage is ''f$cunits(maxblocks - freeblocks)'."
$ endif
$ if new_bk_size .GE. Free_at_destination
$  then
$   say "Oldest copy of ''BACKUP_TYPE_TAG' backup being purged due " + -
$       "to disk space limitations: ''oldest_b_file'"
$   delete 'oldest_b_file
$   local_was_purged = 1
$  else local_was_purged = 0
$ endif
$! Check again once oldest backup has been purged
$ if new_bk_size .GE. Free_at_destination then goto sizerror
$!
$ bck_cmd
$!
$! Backup archive is done, now zip and push it or leave for being pulled
$ say line
$!
$! Verify that the file is not locked
$ OPEN/READ BCK_FILE 'Backup_file' /ERROR=FILERROR
$ CLOSE BCK_FILE
$ Backup_filename = f$parse(Backup_file,,,"NAME","SYNTAX_ONLY")
$!
$ IF (BACKUP_REMOTE_SYSTEM_TYPE .EQS. "UNIX").AND.(F$EXTRACT(F$LENGTH -
  (BACKUP_REMOTE_DIRECTORY)-1,1,BACKUP_REMOTE_DIRECTORY) .NES. "/") -
   THEN BACKUP_REMOTE_DIRECTORY = BACKUP_REMOTE_DIRECTORY + "/"
$!
$ REM_NODE = F$EDIT(BACKUP_REM_NODE,"COLLAPSE")
$ BACKUP_REM_USER = F$EDIT(BACKUP_REM_USER,"COMPRESS,TRIM")
$ REM_USER = F$ELEMENT(0," ",BACKUP_REM_USER)
$ REM_P = F$ELEMENT(1," ",BACKUP_REM_USER)
$!
$ ANALYZE /RMS/FDL 'Backup_file' -
  /OUT='Backup_location''node'_FDLfile_'BACKUP_TYPE_TAG'_'TODATE'.FDL
$ IF ZIP_ARCHIVE .NES. "SKIP"
$  THEN
$   if (zip_level .EQ. 0) .AND. (FILE_TRANSFER .EQS. "FTP") .AND. -
       (ZIP_ARCHIVE .NES. ALWAYS)
$   then
$!   For FTP we dont zip it unless necessary, RMS info is kept with /FDL
$     bck_archive = Backup_file
$     goto XFER_PHASE
$   else bck_archive = Backup_location + Backup_filename + ".ZIP"
$   endif
$!
$   ON ERROR THEN GOTO ZIPERROR
$!  Adding comment (-z) to zip, ending with a line only with '.'
$   OPEN /WRITE ZIPCMD sys$scratch:TMP_ZIP.COM
$   WRITE ZIPCMD "$ zip -''zip_level'jmqz ""-V"" ''bck_archive' " + -
    "''Backup_file' ''Backup_location'''Backup_filename'.FDL"
$   WRITE ZIPCMD bck_cmd
$   WRITE ZIPCMD "."
$   CLOSE ZIPCMD
$   SPAWN /WAIT /PROCESS=ZIP_BACKUP /INPUT=sys$scratch:TMP_ZIP.COM
$   if $STATUS .ne. %x17A38001 then GOTO ZIPERROR     !%x17A38001=Normal
$   DELETE sys$scratch:TMP_ZIP.COM;*
$   ON ERROR THEN CONTINUE
$  ELSE
$   bck_archive = Backup_file
$ ENDIF
$!
$ IF FILE_TRANSFER .EQS. "PULL"
$  THEN GOTO XFER_DONE
$  ELSE say "Backup file push via ''FILE_TRANSFER' " + -
            "started at ''f$time()'."
$ ENDIF
$!
$ IF FILE_TRANSFER .EQS. "SFTP"
$ THEN
$! Putting the command in a batch file to be able to take the output to
$! a file using spawn /intput=<batchfile> /output=<logfile>
$  OPEN /WRITE SFTPCOM sys$scratch:spn_sftp.com
$  WRITE SFTPCOM "$ SET NOON"
$  WRITE SFTPCOM "$ OPEN /WRITE SFTPCMD sys$scratch:bck.sftp"
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""open ''REM_USER'@''REM_NODE'"""
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""binary"""
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""lcd ''Backup_location'"""
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""cd ''BACKUP_REMOTE_DIRECTORY'"""
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""put ''backup_filename'.*"""
$  if REMOTE_SFTP_COMMAND .NES. "" -
    then WRITE SFTPCOM "$ WRITE SFTPCMD ""!''REMOTE_SFTP_COMMAND'"""
$  WRITE SFTPCOM "$ WRITE SFTPCMD ""quit"""
$  WRITE SFTPCOM "$ CLOSE SFTPCMD"
$!  Convert SFTP commandfile to unix format
$  WRITE SFTPCOM "$ CONVERT /FDL=SYS$INPUT sys$scratch:bck.sftp " + -
                 "sys$scratch:bck.sftp"
$  WRITE SFTPCOM "		RECORD"
$  WRITE SFTPCOM "			FORMAT STREAM_LF"
$  WRITE SFTPCOM "$ sftp ""-B"" ""/sys$scratch/bck.sftp"""
$  WRITE SFTPCOM "$ DELETE sys$scratch:bck.sftp;*"
$  CLOSE SFTPCOM
$ ENDIF
$!
$ XFER_PHASE:
$! Ready for the actual transfer. If it fails it will be retried.
$!
$ XFER_LOG = BACKUP_LOG_DIR + NODE + "_''FILE_TRANSFER'_BACKUP_PUSH.LOG"
$!
$ ATTEMPT=0
$ XFER_LOOP:
$  ATTEMPT = ATTEMPT + 1
$  IF ATTEMPT .GT. FILE_COUNT_FOR_TRANSFER THEN GOTO XFER_FAILURE
$ say "Spawning transfer attempt ''attempt'/" + -
     "''FILE_COUNT_FOR_TRANSFER' via ''FILE_TRANSFER' on ''f$time()'..."
$  IF FILE_TRANSFER .EQS. "SFTP"
$   THEN
$    SPAWN /WAIT /PROCESS="BCK_Push_sftp" -
      /INPUT=sys$scratch:spn_sftp.com /OUTPUT='XFER_LOG
$!   Transfer finished, look for status on XFER_LOG
$     OPEN /READ IN_FILE 'XFER_LOG
$     READ_NEXT:
$      READ /END_OF_FILE=UNSUCCESSFUL_SFTP IN_FILE LINE
$      LINE_EDIT = F$EDIT (LINE,"COMPRESS")
$      IF F$LOCATE("100%", LINE_EDIT) .EQ. F$LENGTH(LINE_EDIT)
$       THEN GOTO READ_NEXT
$       ELSE GOTO SUCCESSFUL_SFTP
$      ENDIF
$    UNSUCCESSFUL_SFTP:
$     CLOSE IN_FILE
$     PURGE 'XFER_LOG' /LOG /KEEP='f$integer(file_count_for_transfer*5)
$     GOTO XFER_LOOP              !Keeping in worst case 5 TX iterations
$    SUCCESSFUL_SFTP:
$     CLOSE IN_FILE
$     DELETE sys$scratch:spn_sftp.com;*
$     PURGE 'XFER_LOG' /KEEP=5 /LOG
$  ELSE                                                !FTP transference
$    IF F$EDIT(REM_USER,"UPCASE") .EQS. "ANONYMOUS"
$     THEN
$      COPY /FTP /BIN /ANONYMOUS /FDL /LOG 'bck_archive' -
       'BACKUP_REM_NODE'::"''BACKUP_REMOTE_DIRECTORY'"
$     ELSE
$      COPY /FTP /BIN /LOG /FDL 'bck_archive'-
   'BACKUP_REM_NODE'"''REM_USER' ''REM_P'"::"''BACKUP_REMOTE_DIRECTORY'"
$    ENDIF
$    XFER_STATUS = $STATUS              !Transfer finished, check status
$    say "FINISHED WITH STATUS ''XFER_STATUS'"
$    IF XFER_STATUS .EQS. "X17649ABA"                !Connection timeout
$     THEN ATTEMPT = ATTEMPT + 1
$     GOTO XFER_LOOP
$    ENDIF
$    IF XFER_STATUS .NES. "%X17649B11"              !%TCPIP-S-FTP_NORMAL
$     THEN
$      CMD = "HELP /MESSAGE/STATUS=''XFER_STATUS'"
$      CMD
$      GOTO XFER_FAILURE
$    ENDIF
$ ENDIF
$!
$ OK_STR="%BACKUP-I-SUCCESS, Transfer backup complete successfull, " + -
  "of Node:''NODE' Date:''DATE' has been transferred."
$ SAY OK_STR
$ TRAP "''OK_STR'"
$!
$ XFER_DONE:
$ GOSUB COPY_OTHER_NODES
$!FINISHED:
$! Save local backup for maximum defined
$ DELETE /LOG 'Backup_location''node'_sysdisk_*.*;* -
  /BEFORE="-''BACKUP_DAYS_KEEP_LOCAL_ARCHIVE'-"
$! Clear all symbols defined
$ python 'mydir'CONFIG_PARSER.PY --file 'mydir'BACKUP_CONFIG.DAT --section SYS_BACKUP_CFG --delete
$ say line
$ say "Finished at " + f$time()
$ EXIT %X00030001
$!
$!
$!================      FUNCTIONS AND SUBROUTINES      =================
$ create_othernode_dir:
$  OPEN/WRITE tmp_file sys$scratch:cdir_tmp.com /error=notmpfile
$  WRITE tmp_file -
       "do create /dir ''othernode_dir' /log"
$  CLOSE tmp_file
$  mc sysman set environment /node='node''index'
   @sys$scratch:cdir_tmp.com
   exit
$  delete sys$scratch:cdir_tmp.com;* /LOG
$ return
$! ---------------------------------------------------------------------
$!
$ check_value:
$ IF F$TYPE('check_param) .NES. "" THEN -
   IF F$LENGTH('check_param) .NES. 0 then return
$! Otherwise we put the default
$ 'check_param = DEFAULT_'check_param
$ return
$! ---------------------------------------------------------------------
$!
$ check_islocal:
$!Check if destation disk is shared or local. If shared, do not copy.
$ islocal = 1
$ show device 'bck_dev' /full /output=sys$scratch:bck_dev.txt
$ open /read dev_info sys$scratch:bck_dev.txt
$ read_devinfo:
$  read /end_of_file=islocal_checked dev_info cline
$  cline = f$edit(cline,"UPCASE")
$  IF F$LOCATE("ALSO MOUNTED",cline) .NE. f$length(cline)
$   THEN
$    islocal=0
$    goto islocal_checked
$   ENDIF
$ goto read_devinfo
$!
$ islocal_checked:
$ close dev_info
$ delete sys$scratch:bck_dev.txt;*
$ return
$! ---------------------------------------------------------------------
$!
$ COPY_OTHER_NODES:
$ say "Backup finished, now copying to other nodes (if needed)."
$! Check if destination directory resides on a share disk or not (local)
$ gosub check_islocal     !return islocal=(0|1)
$!
$! Check if destination directories exist
$ dir_deep = 0
$ dirpath = bck_dir - "[" - "]"
$ s2loop:
$  if f$element(dir_deep,".",dirpath) .NES. "."
$   THEN
$    dir_deep = dir_deep + 1
$    goto s2loop
$  endif
$!
$ index = 0
$ node_loop:
$ index = index + 1
$ if index .gt. NR_Clunodes then RETURN
$ if islocal .AND. (index .NE. mynr)
$  then othernode_dir = node+"''index'::''bck_dev'"
$  else othernode_dir = bck_dev
$ endif
$!
$ if dir_deep .EQ. 1
$  then
$   othernode_dir2 = "''othernode_dir'[000000]''dirpath'.dir"
$  else
$   lastdir = f$element(dir_deep-1,".",dirpath)
$   othernode_dir2 = "''othernode_dir'[" + (dirpath - ".''lastdir'") + -
                     "]''lastdir'.DIR"
$ endif
$ ON ERROR THEN CONTINUE
$ othernode_dir = othernode_dir + bck_dir
$!
$ od2_exists = F$search(othernode_dir2)
$ IF islocal .AND. (od2_exists .EQS. "") THEN GOSUB create_othernode_dir
$!
$ IF islocal
$  THEN
$  IF index .NE. mynr
$   THEN
$    othernode_bck="''node'''index'::''Backup_location'''node'_sysdi*.*"
$    say "Deleting old copies on ''othernode_dir'..."
$    if local_was_purged
$     then
$        oldest_at_remote = f$search(othernode_bck)
$        say "Oldest copy of ''BACKUP_TYPE_TAG' backup being purged" + -
$       "due to disk space limitations: ''oldest_at_remote'"
$        delete /log 'oldest_at_remote
$    endif
$    delete /log 'othernode_bck';* -
            /before="-''BACKUP_DAYS_KEEP_LOCAL_ARCHIVE'-"
$    say "Copying ''bck_archive' to ''othernode_dir'..."
$    copy /log 'bck_archive' 'othernode_dir'
$  ENDIF
$  goto node_loop
$ ENDIF
$ RETURN
$!
$!==============          FAILURE EXITPOINTS          ==================
$!
$ NOTMPFILE:
$ SAY BOLD+LINE
$ SAY "Error creating temporary file on SYS$SCRATCH." + -
  "Backup was not copied to other nodes!" + NORM
$ EXIT
$!
$ FULLBERROR:
$ SAY BOLD+LINE
$ SAY "Error creating backup on ''Backup_location'. " + -
  "No full backup was possible!" + NORM
$ EXIT
$!
$ ERROR_ALREADY_RUNNING:
$ SAY BELL+BOLD+LINE
$ SAY "Error, another instance was found!"
$ SAY "Check queue for entry ''found_entry' named ''Jname'!!!" + NORM
$ EXIT
$!
$ DEST_IS_DSA0:
$ SAY BOLD+LINE
$ SAY "Destination for system volume backup cannot reside in itself!"
$ SAY "Please choose another destination rather than ''Backup_location'"
$ EXIT
$!
$ SIZERROR:
$ SAY BOLD+LINE
$ SAY "Destination filesystem has ''f$cunits(Free_at_destination)' free"
$ SAY "Please purge old files!!!"
$ EXIT
$!
$ FILERROR:
$ TXTE = "Specified backup file not found or locked: ''Backup_location'"
$ GOTO ETXT
$!
$ XFERROR:
$ TXTE = "Backup file ''Backup_file' could not be transferred to " + -
         "''BACKUP_REM_NODE'."
$ GOTO ETXT
$!
$ ZIPERROR:
$ TXTE = "No free space on ''Backup_location' (" + -
  F$PARSE(Backup_location,,,"DEVICE","SYNTAX_ONLY") + ") for ZIP. " + -
  "Backup file could not be compressed."
$ GOSUB COPY_OTHER_NODES
$ GOTO ETXT
$!
$ XFER_FAILURE:
$ TXTE = "Transfer backup failed, of node:''NODE' date: ''TODATE' " + -
         "could not been transferred to ''BACKUP_REM_NODE'."
$ IF FILE_TRANSFER .EQS. "SFTP" THEN DEL sys$scratch:spn_sftp.com;* /LOG
$ GOSUB COPY_OTHER_NODES
$ GOTO ETXT
$!
$ ETXT:
$ SAY BOLD + TXTE + NORM
$ TRAP "%BACKUP-F-FAILED, ''TXTE'"
$ EXIT
