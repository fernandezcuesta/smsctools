import string
import random
import functools
import cStringIO

import fabric
from fabric.api import env, put
from fabric.operations import _execute as operations_execute
from fabric.network import to_dict

__all__ = ('run',
           'exists',
           'pml_run_cmd',
           'pml_run_file')


def _execute_openvms(f):
    """
    Execute a command on a OpenVMS host and set the status according to
    the value of $SEVERITY.
        - If $SEVERITY is odd, everything went fine --> return status=0
        - If $SEVERITY is even, there was a failure --> return status=1

    https://groups.google.com/forum/#!topic/comp.os.vms/dSeJtsqWXM4
    """
    @functools.wraps(f)
    def _wrapper(*args, **kwargs):
        _original_ret_codes = env.ok_ret_codes
        env.ok_ret_codes = [-1]
        wrapped_kwargs = kwargs.copy()
        wrapped_kwargs['command'] = 'pipe %s ; write sys$output $SEVERITY' % (
            kwargs['command'],
        )
        stdout, result_stderr, _ = f(*args, **wrapped_kwargs)
        stdout = stdout.split('\n')
        # restore original ok_ret_codes
        if env.ok_ret_codes != _original_ret_codes:
            env.ok_ret_codes = _original_ret_codes
        return ('\n'.join(stdout[:-1]), result_stderr, 1 - int(stdout[-1]) % 2)
    return _wrapper


@_execute_openvms
def _execute(*args, **kwargs):
    return operations_execute(*args, **kwargs)


def run(*args, **kwargs):
    """ wrapper overriding fabric.operations.run """
    with fabric.api.hide('everything'):
        kwargs['shell'] = None
        stdout = fabric.operations.run(*args, **kwargs)
    return '\n'.join(['[%s] out: %s' % (env.host_string, line)
                      for line in stdout.split('\n')]) if stdout else ''


def exists(remote_file):
    while not env.get('host_string', False):
        fabric.utils.handle_prompt_abort("the target host connection string")
        host_string = raw_input("No hosts found. Please specify (single)"
                                " host string for connection: ")
        env.update(to_dict(host_string))
    sftp_session = fabric.sftp.SFTP(env.host_string)
    return sftp_session.exists(remote_file)


def pml_run_cmd(cmd_list):
    """
        Run a list of commands in SMSC v5's PML interface
    """
    assert isinstance(cmd_list, list)
    # Create a temporary file with the command followed by 'disconnect, exit'
    cmd_file = cStringIO.StringIO()
    cmd_file.write('CONNECT\n')
    cmd_file.write('DISABLE OPCOM\n')
    # The output will be sent to a temporary file with a random name
    out_file = 'sys$scratch:%s.dat' % (
        ''.join(random.SystemRandom().choice(string.ascii_uppercase +
                                             string.digits)
                for _ in range(8))
    )
    cmd_file.write('ENABLE OUTPUT /FILE=%s\n' % (out_file, ))
    cmd_file.write('CANCEL CLASS MD /ALARM=ALL\n')  # Discard log messages
    for cmd in cmd_list:
        cmd_file.write('%s\n' % (cmd, ))
    cmd_file.write('DISCONNECT\n')
    cmd_file.write('EXIT\n')  # Gracefully close the PML session
    # Runs the temporary file
    pml_run_file(cmd_file, out_file)
    # Close the  object
    cmd_file.close()


def pml_run_file(pml_file, out_file=None):
    """ Run PML script and returns on screen output """
    # pml_file may be a filename, a  or a file-like object

    # first we need to upload the PML file to sys$scratch
    pml_filename = \
        pml_file if isinstance(pml_file, str) else 'fabric_temp.pml'
    put(pml_file, pml_filename)
    # then we run pml /input=pml_file
    run('pml /opclass=2 /queue=fabric /response_time=10 '
        '/input=%s' % (pml_filename, ))
    # Type the contents of the output file
    run('type %s' % (out_file, ))
    # Remove the temporary pml_file
    run('delete /nolog %s;*' % (pml_filename, ))
    # If any, delete the temporary output file
    if out_file:
        run('delete /nolog %s;*' % (out_file, ))


fabric.operations._execute = _execute
