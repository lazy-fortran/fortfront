program test_shell_commands_windows_quotes
    use test_shell_commands, only: verify_shell_helpers
    implicit none

    call verify_shell_helpers(.true.)
end program test_shell_commands_windows_quotes

