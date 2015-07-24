$!==============================================================================
$!   MON_PML_EXTRA_PMS.COM
$!
$!   Take some values from PMS output
$!
$![optional]
$!  p1: verbose [0,1]               [default: no logs created (0)]
$!  p2: output folder               [Default: running directory]
$!  p3: timestamp                   [default: none]
$!  p4: allday [0,1]                [default: 0]
$! Taking timestamp from main script allows CSV merge
$!==============================================================================
$!
$! Version 1501.01
$! Taking the last file in the last 10 minutes
$!
$!
$!
$! CONFIGURATION SETTINGS: ----------------------------------------
$ entity_class = "GIW"
$ search_string = "inated SM:"
$! --- END OF CONFIGURATION SETTINGS ------------------------------
$!
$!
$!
$ set noverify
$ set noon
$!
$!
$ scriptname = F$ENVIRONMENT("PROCEDURE")
$ scriptname = scriptname - f$Parse(scriptname,,,"VERSION")
$ working_dir = f$parse(scriptname,,,"DEVICE") + -
                f$parse(scriptname,,,"DIRECTORY")
$ enable_log = 0
$ NODE_NAME = F$GETSYI ("NODENAME")
$ NODE_NAME = F$EXTRACT(0,F$LENGTH(NODE_NAME)-1,NODE_NAME)
$ temp_file = "sys$scratch:tmp.pms"
$!
$!
$ if (p1 .EQS. "1") .or. (f$edit(p1,"UPCASE,COLLAPSE") .EQS. "Y") -
   then enable_log = 1
$!
$ if p2 .EQS. ""
$  then
$   OUTPUT_FOLDER = working_dir
$  else
$   OUTPUT_FOLDER = p2
$ endif
$ OUTPUT_FOLDER = f$parse(OUTPUT_FOLDER,,,"device") + -
                  f$parse(OUTPUT_FOLDER,,,"directory")
$!
$ if p3 .NES. ""
$  then
$   TIMESTAMP = p3
$  else
$   TIMESTAMP = F$TIME()
$ endif
$!
$ DAYTAG = F$CVTIME(TIMESTAMP,"COMPARISON","DATE")
$ DAYTAG = DAYTAG - "20" - "-" - "-"
$ TIMETAG = F$CVTIME(TIMESTAMP,,"TIME") - ":"
$ TIMETAG = F$EXTRACT(0,3,TIMETAG)
$ if p4 .NES. ""
$  then
$   if p4 .EQS. "1"
$     then
$      TIMETAG="*"
$      allday = 1
$     else
$      allday = 0
$      TIMETAG = TIMETAG + "%"
$   endif
$  else
$   allday = 0
$   TIMETAG = TIMETAG + "%"
$ endif
$!
$ wilcard_pms = "SMSC$ROOT:[PMS]COUNTERS_SMSC.''DAYTAG'''TIMETAG'"
$ DAYTAG = F$CVTIME(TIMESTAMP,"ABSOLUTE","DATE") - "-" - "-"
$ IF F$LENGTH(DAYTAG) .LT. 9 THEN DAYTAG = "0''DAYTAG'"
$!
$ output_csv = "''OUTPUT_FOLDER'pmsout_''NODE_NAME'_''DAYTAG'.csv"
$!
$ say := "if enable_log then write sys$output "
$!
$!
$!
$ IF F$TrnLnm("python_root", "LNM$SYSTEM", , , , ) .EQS. ""
$  THEN
$   pyth = f$search("SYS$COMMON:[PYTHON*.VMS]STARTUP.COM",1)
$   IF pyth .NES. "" then @'pyth
$   IF .NOT. $STATUS THEN goto NOT_PYTHON
$ ENDIF
$ python_launch :== $python_root:[vms.bin]PYTHON.EXE
$!
$ IF F$TYPE(PYTHON) .EQS. ""
$  THEN
$   pyth = f$search("SYS$COMMON:[PYTHON*.VMS]SETUP.COM",1)
$   IF pyth .NES. "" then @'pyth
$   IF .NOT. $STATUS THEN goto NOT_PYTHON
$ ENDIF
$!
$!
$ fichloop:
$ say "Looking for ''wilcard_pms'"
$ ficher_ = f$search("''wilcard_pms'",676)
$ if allday .EQ. 0
$   then
$   if ficher_ .NES. ""
$    then
$     fichero = ficher_
$     goto fichloop
$   endif
$!
$   if f$type(fichero) .EQS. "" then goto BAD_END
$   SAY "Processing ''fichero' with wildcard ""''search_string'""..."
$   search 'fichero' "''search_string'" /win=(2,1) /output='temp_file'
$   shortcut :== "''output_csv'"
$   say /symbol "python_launch ''working_dir'pms2csv.py ""''temp_file'"" ""''entity_class'"" ""''NODE_NAME'"" ""''output_csv'"" ""''timestamp'"""
$   python_launch 'working_dir'pms2csv.py "''temp_file'" "''entity_class'" "''NODE_NAME'" 'shortcut "''timestamp'" 
$!
$   delete 'temp_file';*
$   EXIT %X00030001
$!
$ else
$! Looping for all files of that day
$   say "Allday (''allday')> Looking for ''wilcard_pms'"
$   ficher_ = f$search("''wilcard_pms'",767)
$   if ficher_ .NES. ""
$    then
$     fichero = ficher_
$     found_timestamp = f$element(1, ".", fichero)
$     f_hour = f$extract(6,2,found_timestamp)
$     f_min = f$extract(8,2,found_timestamp)
$     new_timestamp = timestamp + " ''f_hour':''f_min':00.00"
$       if f$type(fichero) .EQS. "" then goto BAD_END
$       SAY "Processing ''fichero' with wildcard ""''search_string'""..."
$       search 'fichero' "''search_string'" /win=(2,1) /output='temp_file'
$       shortcut :== "''output_csv'"
$       say /symbol "python_launch ''working_dir'pms2csv.py ""''temp_file'"" ""''entity_class'"" ""''NODE_NAME'"" ""''new_timestamp'"" ""''output_csv'"""
$     python_launch 'working_dir'pms2csv.py "''temp_file'" "''entity_class'" "''NODE_NAME'" "''new_timestamp'" 'shortcut
$     goto fichloop
$     endif
$   delete 'temp_file';*
$   EXIT %X00030001
$ endif
$!
$!
$!
$!
$!
$ BAD_END:
$  write sys$output "Couldn't find any PMS file with wildcard ''wilcard_pms'."
$ exit
$!
$!
$!
