#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
 OpenVMS settings file parser - py2.5
 ------------------------------------------------------------------------------
 2008 (c) J.M. Fernández - fernandez.cuesta@gmail.com

"""

import sys
import getopt
import ConfigParser
import logging
from vms.rtl.lib import set_symbol, delete_symbol
from vms.libclidef import LIB_K_CLI_LOCAL_SYM

__version__ = "0.5"

class ConfigReadError(Exception):
    """Exception raised while reading configuration file"""
    pass

HELP = """
config_parser.py -f <inputfile> -s <sectionname> -d

-f, --file:    file containing the configuration passed to configparser
-s, --section: section name, defaults to all sections found in input file
-d, --delete:  delete symbols defined in inputfile
-a, --append:  append section name to each symbol as prefix
"""


class SettingsParser(object):
    """
    Settings file parser, simply a handler of ConfigParser for OpenVMS
    """

    def __init__(self, delete_after=True,
                 append_section_name=False,
                 **kwargs):
        """
        Initialize configuration parser
        - ifile: file containing the configuration passed to configparser
        - section: section name, defaults to all sections found in input file
        - delete_after:  whether or not delete the symbols on exit
        - append_section_name: whether or not append the section name to each
                               symbol name while defining
        """
        self.sections = []
        self.delete_after = delete_after
        self.append_section_name = append_section_name
        self.config = ConfigParser.SafeConfigParser()
        self.kwargs = kwargs
        self.init_logger()

    def _parse(self, ifile=None, section=None):
        try:
            section = section.upper()
            settings = self.config.read(ifile)
            if not settings:
                raise ConfigReadError
            if section:
                assert section in self.config.sections()
                self.sections = [section]
            else:
                self.sections = self.config.sections()

        except ConfigReadError:
            self.logger.error('Could not read configuration file %s', ifile)
        except ConfigParser.MissingSectionHeaderError:
            self.logger.error('File %s contains no section headers', ifile)
        except AssertionError:
            self.logger.error('Section %s not found in configuration file %s',
                              section,
                              ifile)

    def __enter__(self):
        """
        Invoked when running the configparser in a with statement
        """
        self.set_symbols(**self.kwargs)

    def __exit__(self, etype, *args):
        if self.delete_after:
            self.clear_symbols()

    def set_vms_symbol(self, name, value):
        """
        Delete symbol `name` from the local table (LIB_K_CLI_LOCAL_SYM)
        """
        self.logger.info("Setting symbol: %s=%s", name, value)
        return set_symbol(name, value, LIB_K_CLI_LOCAL_SYM)

    def delete_vms_symbol(self, name):
        """
        Delete symbol `name` from the local table (LIB_K_CLI_LOCAL_SYM)
        """
        self.logger.info('Removing symbol for %s', name)
        return delete_symbol(name, LIB_K_CLI_LOCAL_SYM)

    def set_symbols(self, **kwargs):
        """ Go through all settings defined in file and assign each
            (key, value) pair to OpenVMS local symbol table """
        self.logger.info('Assigning symbols to local table')
        self._parse(**kwargs)
        for section in self.sections:
            for setting in self.config.items(section):
                self.set_vms_symbol('%s_%s' % (section, setting[0])
                                    if self.append_section_name else setting[0],
                                    setting[1])

    def clear_symbols(self):
        """ Clear VMS symbols defined in config file from local table """
        self.logger.info('Clearing symbols')
        for section in self.sections:
            for setting in self.config.items(section):
                self.delete_vms_symbol('%s_%s' % (section, setting[0])
                                       if self.append_section_name
                                       else setting[0])

    def cli_mode(self, **kwargs):
        """ Checks whether we want to set or delete the symbols while invoked
            from the CLI or not from a with statement
        """
        if self.delete_after:  # running in CLI: just delete
            self.clear_symbols()
        else:
            self.set_symbols(**kwargs)

    def init_logger(self, name=__name__):
        """ Initialize a console logger """
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s | %(levelname)-8s | %(message)s'
        )
        self.logger = logging.getLogger(name)
        self.logger.info('Initialized logger')


def main(argv):
    """
    Read input arguments
    """
    append_section_name = delete_after = False
    ifile = None
    section = None

    try:
        opts, args = getopt.getopt(argv,
                                   "hadf:s:",
                                   ["help", "delete", "append",
                                    "file=", "section="])
    except getopt.GetoptError:
        print('Invalid argument')
        print(HELP)
        sys.exit(2)

    for opt, arg in opts:
        if opt in ('-h', '--help'):
            print(HELP)
            sys.exit()
        elif opt in ("-f", "--file"):
            ifile = arg
        elif opt in ("-s", "--section"):
            section = arg
        elif opt in ("-d", "--delete"):
            delete_after = True
        elif opt in ("-a", "--append"):
            append_section_name = True
    return (ifile, section, delete_after, append_section_name)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print(HELP)
    else:
        (IFILE,
         SECTION,
         DELETE_AFTER,
         APPEND_SECTION_NAME) = main(sys.argv[1:])

        parser = SettingsParser(delete_after=DELETE_AFTER,
                                append_section_name=APPEND_SECTION_NAME)
        logger = logging.getLogger(name='CONFIG_PARSER')

        parser.cli_mode(ifile=IFILE, section=SECTION)
