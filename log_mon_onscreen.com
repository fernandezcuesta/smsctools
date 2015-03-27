$!==============================================================================
$! LOG_MON_ONSCREEN.COM
$!
$! Settings:
$!
$! - A logical name must be defined for the output to be saved
$!               define /system mon$dir <output_folder>
$!
$! - Two element lists can be customized: "files" and "greps"
$!
$! - files:   Element list separated with '\'
$!
$! - greps:   What to look for on each element (same order), separated with '\'
$!            For different searches in the same element, separate with '|'.
$!
$! - windows: Search window for each element.
$!            Can be a single number or before,after mode
$!
$![optional]
$!  p1: number of versions to keep  [default: 5]
$!  p2: configuration file          [default: monitor_config.cnf]
$!==============================================================================
$!
$! Version 1420.02
$!
$!
$!
$ set noon
$ set noverify
$ scriptname = F$ENVIRONMENT("PROCEDURE")
$ mydir = f$parse(scriptname, , , "DEVICE") + -
          f$parse(scriptname, , , "DIRECTORY")
$ outdir = mydir
$ nomnode = F$GETSYI ("NODENAME")
$!
$ if p1 .EQS. ""
$  then
$   num_versions = 5
$  else
$   num_versions = p2
$ endif
$!
$ if num_versions .le. 0 then num_versions = 1
$!
$ if p2 .EQS. ""
$  then
$   config_file = mydir + "MONITOR_CONFIG.CNF"
$ else
$   config_file = p2
$ endif
$!
$!
$!
$ line = "====================================================================="
$ DCL_DATA :== $SMSC$ROOT:[BIN]DCL_DATA.EXE
$!
$ if f$search(config_file) .EQS. "" then GOTO BAD_CONFIG
$ DCL_DATA /FILE='config_file' "LOGMONITOR"
$!
$ IF (F$TYPE (FILES) .EQS. "") .OR. (F$TYPE (GREPS) .EQS. "") -
     .OR. (F$TYPE (WINDOWS) .EQS. "")
$ THEN
$    goto BAD_CONFIG
$ ELSE 
$    IF (F$LENGTH (FILES) .EQ. 0) .OR. (F$LENGTH (GREPS) .EQ. 0) -
        .OR. (F$LENGTH (WINDOWS) .EQ. 0)
$    THEN
$       write sys$output -
              "Missing value for mandatory field(s) in section [LOGMONITOR]" 
$       GOTO BAD_CONFIG
$    ENDIF
$ ENDIF
$!
$
$ write sys$output "Checking logs in node ''nomnode' at ''F$TIME()'"
$ write sys$output line
$ write sys$output " "
$!
$ index_1 = 0
$!
$ FILE_LOOP:
$ index_2 = 0
$ current_file = f$element(index_1,"\",files)
$ if current_file .EQS. "\" then goto END_FILE_LOOP
$!
$ GREP_LOOP:
$ g = f$element(index_2,"|",f$element(index_1,"\",greps))
$ if g .EQS. "|" then goto DO_LOOKUP
$!
$!
$ if index_2 .eq. 0
$  then
$   current_grep = g
$  else
$   current_grep = current_grep + """,""" + g
$ endif
$!
$ index_2 = index_2 + 1
$ goto GREP_LOOP
$!
$ DO_LOOKUP:
$ index_1 = index_1 + 1
$ if f$search("''current_file'") .EQS. "" then goto BAD
$!
$! Check if grep has tilde sign to be translated: ` into "
$ adjusted_grep = ""
$ POSQUOTE1:
$  posquote = f$locate("`",current_grep)
$  if posquote .nes. f$length(current_grep)
$   then
$    adjusted_grep = adjusted_grep + f$extract(0,posquote,current_grep) + """"
$    current_grep = f$extract(1+posquote,f$length(current_grep),current_grep)
$    goto POSQUOTE1
$  endif
$ adjusted_grep = adjusted_grep + current_grep
$!
$ current_grep = f$edit(adjusted_grep,"UPCASE,COLLAPSE") 
$ if f$extract(0,5,current_grep) + -
     f$extract(f$length(current_grep)-1,1,current_grep) .EQS. "EVAL()"
$  THEN
$    adjusted_grep = f$extract(5,f$length(adjusted_grep)-6,adjusted_grep)
$    commd = "search ''current_file' ""''adjusted_grep'"" /win=(" + -
             f$element(index_1-1,"\",windows) + ")"
$  ELSE
$    commd = "search ''current_file' """ + adjusted_grep + -
             """ /win=(" + f$element(index_1-1,"\",windows) + ")"
$ endif
$!
$ write sys$output "Current search command:"
$ write sys$output commd
$ write sys$output line
$ commd
$ 
$ GOTO FILE_LOOP
$!
$!
$ END_FILE_LOOP:
$ write sys$output line
$!
$ CMD_LINE = 0
$ CMDLINELOOP:
$ index_2 = 0
$ CMD_LINE = CMD_LINE + 1
$ IF F$TYPE (OTHERCMDS_'CMD_LINE') .EQS. "" THEN GOTO FIN
$ IF F$LENGTH (OTHERCMDS_'CMD_LINE') .EQ. 0 THEN GOTO FIN
$ OTHER_COMMANDS:
$  commd = f$element(index_2,"\",OTHERCMDS_'CMD_LINE')
$  if commd .EQS. "\" then goto CMDLINELOOP
$! Check if grep has tilde sign to be translated: ` into "
$  dmmoc = commd
$  commd = ""
$ POSQUOTE:
$  posquote = f$locate("`",dmmoc)
$  if posquote .nes. f$length(dmmoc)
$   then
$    commd = commd + f$extract(0,posquote,dmmoc) + """"
$    dmmoc = f$extract(1+posquote,f$length(dmmoc),dmmoc)
$    goto POSQUOTE
$  endif
$ commd = commd + dmmoc
$  write sys$output line
$  write sys$output "Running command: " + commd + "..."
$  spawn /process=mon_commd commd
$  index_2 = index_2 + 1
$  goto OTHER_COMMANDS
$!
$!
$ FIN:
$ write sys$output line
$ write sys$output "FINISHED at ''F$TIME()' on ''nomnode'"
$ exit
$!
$!
$ BAD:
$ write sys$output "Cannot open ''current_file'"
$ goto FILE_LOOP
$!
$!
$ BAD_CONFIG:
$  write sys$output "Bad config file ''config_file' or file missing, aborting."
$  exit
