$!============================================================================
$!DISK_MON.COM
$!
$! Monitors disk space statistics, based on disk.com script
$! --------------------------------------------------------
$!
$![optional]
$!  p1: verbose
$!  p2: output folder               [Default: running directory]
$!  p3: timestamp
$!  p4: t4mode [YES/NO]             [Default: "YES"]
$!                                  saves T4 compliant CSV
$!============================================================================
$! Version 1401.01
$!
$!
$!
$ set noon
$ verif = F$VERIFY (0)
$ on control_Y then goto CLEANUP
$ On warning then goto CLEANUP
$!
$ node = F$GETSYI ("NODENAME",,F$CSID(null))
$ scriptname = F$ENVIRONMENT("PROCEDURE")
$ scriptname = scriptname - f$Parse(scriptname,,,"VERSION")
$ mydir = f$parse(scriptname,,,"DEVICE") + f$parse(scriptname,,,"DIRECTORY")
$! mydir = F$ENVIRONMENT("DEFAULT")
$!
$ NODE_NAME = F$EXTRACT(0, F$LENGTH(NODE_NAME)-1, NODE_NAME)
$!
$ if (p1 .EQS. "1") .or. (f$edit(p1, "UPCASE, COLLAPSE") .EQS. "Y") -
  then write sys$output "Checking disk space..."
$!
$ if p2 .EQS. ""
$  then
$   outdir = mydir
$  else
$   outdir = p2
$ endif
$ outdir = F$parse(outdir, , , "Device") + F$parse(outdir, , , "DIRECTORY")
$!
$ if p3 .EQS. ""
$  then
$   timestamp = f$time()
$  else
$   timestamp = p3
$ endif
$!
$ if p4 .EQS. ""
$  then
$   t4mode = "YES"
$  else
$   t4mode = f$edit(p4, "UPCASE")
$ endif
$!
$ if t4mode .eqs. "YES"
$  then line = timestamp
$  else line = ""
$ endif
$!
$ DAYTAG = F$CVTIME(timestamp, , "DAY") + -
           F$CVTIME(timestamp, "ABSOLUTE", "MONTH") + -
           F$CVTIME(timestamp, , "YEAR")
$ output_file = "''outdir'disks_" + F$EXTRACT(0, F$LENGTH(node)-1, node) + -
                "_''DAYTAG'.csv"
$ tmpfile = "''outdir'diskspace.tmp"
$!
$ if f$search(output_file) .EQS. ""
$  then
$   out_new = 1
$   OPEN /write /error=BAD_OUTFILE out 'output_file'
$   if t4mode .eqs. "YES"
$    then
$     write out node + ","
$     write out "$$$ START COLUMN HEADERS $$$"
$     header = "Sample Time"
$    else write out -
    "Sample Time, Device Name, Volume Label, Used Blocks, %Free, Total Blocks"
$   endif
$  else
$   out_new = 0
$   OPEN /append /error=BAD_OUTFILE out 'output_file'
$!
$ endif
$!
$!
$ define/user SYS$OUTPUT 'tmpfile'
$ show device d /mounted
$!
$ WRITE_IT:
$ open/read file 'tmpfile'
$ read file null
$ read file null
$ !
$ READ_REC:
$  QUTA = ""
$  read/end=CLEANUP file rec
$  IF F$LOCATE (":", rec) .eqs. F$LENGTH (rec) THEN GOTO READ_REC
$  disk = f$edit(F$EXTRACT (0, F$LOCATE (":", rec)+1, rec),"collapse")
$  cyl = F$GETDVI ("''disk'", "DEVCLASS")
$  If cyl .ne. 1 then goto READ_REC
$  for = F$GETDVI ("''disk'", "FOR")
$  If for then goto READ_REC
$  label = F$GETDVI ("''disk'", "VOLNAM")
$  GB_FACTOR = 1
$!  IF P1 .EQS. "GB" then GB_FACTOR = 2 * 1024 * 1024
$  free = F$GETDVI ("''disk'", "FREEBLOCKS")  / 'GB_FACTOR'
$  total = F$GETDVI ("''disk'", "MAXBLOCK")   / 'GB_FACTOR'
$  used = total - free
$  factor = 1
$  if total .le. 100 then factor = 100
$  total1 = (total*'factor')/100
$  peruse = (used*'factor')/total1
$  perfre = 100 - peruse
$!
$ if t4mode .eqs. "YES"
$  then
$   line = line + f$edit(F$FAO ( ",!10UL,!3UL,!10UL", used, peruse, total), -
                         "COLLAPSE")
$   if out_new then header = header + -
    f$edit(F$FAO(",[!12AS]Used Blocks,[!12AS]%Used,[!12AS]Total Blocks", -
    label, label, label), "COLLAPSE")
$  else
$   line = timestamp + f$edit(F$FAO ( ",!12AS,!12AS,!10UL,!3UL,!10UL",  -
               disk, label, used, perfre, total), "COLLAPSE")
$   write out line
$ endif
$ goto READ_REC
$ !
$ !
$ CLEANUP:
$  if t4mode .eqs. "YES"
$   then
$    if out_new
$     then
$      write/symbol out header
$      write out "$$$ END COLUMN HEADERS $$$"
$    endif
$    write/symbol out line
$  endif
$  set noon
$  close/error=no file
$  close out
$ !
$ NO:
$  open/error=not_open file 'tmpfile'
$  close file
$  delete/nolog/noconfirm 'tmpfile';*
$ !
$ NOT_OPEN:
$  verif = F$VERIFY (verif)
$  exit
$!
$ BAD_OUTFILE:
$  write sys$output "Cannot write to output file: ''output_file'"
$  write sys$output "Bad permissions?"
$  open/error=not_open out 'output_file'
$  close out
$  delete/nolog/noconfirm 'output_file';*
$  exit
$!
