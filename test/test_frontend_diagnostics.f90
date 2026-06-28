program test_frontend_diagnostics
    use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
        DIAG_BINARY_DATA, DIAG_NO_PROGRAM_UNIT, &
        DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO
    use fortfront_types, only: diagnostic_t, source_range_t
    implicit none

    call test_basic_diagnostic_creation()
    call test_diagnostic_formatting_error()
    call test_diagnostic_formatting_warning()
    call test_diagnostic_with_location()
    call test_binary_data_diagnostic()

    print *, "All frontend_diagnostics tests passed"

contains

    subroutine test_basic_diagnostic_creation()
        type(diagnostic_t) :: diag

        diag = make_diagnostic("F001", DIAGNOSTIC_ERROR, "Test error message")

        if (diag%code /= "F001") then
            error stop "Expected code F001"
        end if

        if (diag%severity /= DIAGNOSTIC_ERROR) then
            error stop "Expected severity DIAGNOSTIC_ERROR"
        end if

        if (diag%message /= "Test error message") then
            error stop "Expected message to match"
        end if
    end subroutine test_basic_diagnostic_creation

    subroutine test_diagnostic_formatting_error()
        type(diagnostic_t) :: diag
        character(len=:), allocatable :: formatted

        diag = make_diagnostic("F002", DIAGNOSTIC_ERROR, "Binary data detected")
        formatted = format_diagnostic(diag)

        if (index(formatted, "[F002]") == 0) then
            error stop "Expected [F002] in formatted output"
        end if

        if (index(formatted, "ERROR") == 0) then
            error stop "Expected ERROR in formatted output"
        end if

        if (index(formatted, "Binary data detected") == 0) then
            error stop "Expected message in formatted output"
        end if
    end subroutine test_diagnostic_formatting_error

    subroutine test_diagnostic_formatting_warning()
        type(diagnostic_t) :: diag
        character(len=:), allocatable :: formatted

        diag = make_diagnostic("F010", DIAGNOSTIC_WARNING, "Deprecated syntax")
        formatted = format_diagnostic(diag)

        if (index(formatted, "[F010]") == 0) then
            error stop "Expected [F010] in formatted output"
        end if

        if (index(formatted, "WARNING") == 0) then
            error stop "Expected WARNING in formatted output"
        end if
    end subroutine test_diagnostic_formatting_warning

    subroutine test_diagnostic_with_location()
        type(diagnostic_t) :: diag
        type(source_range_t) :: loc
        character(len=:), allocatable :: formatted

        loc%start%line = 42
        loc%start%column = 15
        loc%end%line = 42
        loc%end%column = 20

        diag = make_diagnostic("F003", DIAGNOSTIC_ERROR, "Syntax error", loc)
        formatted = format_diagnostic(diag)

        if (index(formatted, "line 42") == 0) then
            error stop "Expected line 42 in formatted output"
        end if

        if (index(formatted, ":15:") == 0) then
            error stop "Expected column 15 in formatted output"
        end if
    end subroutine test_diagnostic_with_location

    subroutine test_binary_data_diagnostic()
        type(diagnostic_t) :: diag
        character(len=:), allocatable :: formatted

        diag = make_diagnostic(DIAG_BINARY_DATA, DIAGNOSTIC_ERROR, &
            "Input appears to be binary data")
        formatted = format_diagnostic(diag)

        if (index(formatted, "[F002]") == 0) then
            error stop "Expected F002 code for binary data"
        end if

        if (index(formatted, "ERROR") == 0) then
            error stop "Expected ERROR severity"
        end if
    end subroutine test_binary_data_diagnostic

end program test_frontend_diagnostics
