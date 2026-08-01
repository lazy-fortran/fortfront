program test_shell_commands_windows_quotes
    implicit none

    call verify_shell_helpers(.true.)

contains

    include '../common/shell_commands.inc'

end program test_shell_commands_windows_quotes

