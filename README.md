# smsctools
OpenVMS DCL and Python scripts for Acision v5 SMSC monitoring

##sys_backup.com
SMSC system disk and database backup

##mon_pml.com
DCL script to periodically gather v5 statistics

  ```console
  > @mon_pml [verbose] [configuration_file] [output_folder] [autosubmit]
  
    p1: verbose [0,1]         [default: logs are created (1)]
    p2: configuration file    [default: monitor_config.cnf]
    p3: output folder         [default: current / CONFIG FILE VALUE]
    p4: autosubmit [0,1]      [default: no autosubmit (0) / CONFIG FILE VALUE]
  ```
- Optional Cisco ITP monitor connects via telnet
- Allows extra monitors automatically called after each run (every DCL script named `mon_pml_extra*.com`)
- Places logs in `smsc$root:[log]` (automatically cleaning old log files and resetting the version numbers)


##log_mon_onscreen.com
Makes a search on defined log files and prints the output

###pml2csv.py
Internal function called from `mon_pml.com` which converts tidy PML output into T4-Format2 compliant CSV file.

###pms2csv.py
Internal function called from `mon_pml_extra_PMS.com` which converts a PMS statistic file into T4-Format2 compliant CSV file.
