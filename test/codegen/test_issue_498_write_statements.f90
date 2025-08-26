program test_issue_498_write_statements
    ! Test case for Issue #498: Write statements not recognized as valid Fortran
    ! Tests specifically the input validation layer that was causing UNRECOGNIZED_INPUT
    use lexer_core
    use input_validation, only: validate_basic_syntax
    use frontend, only: transform_lazy_fortran_string
    implicit none

    call test_write_statement_validation()
    call test_write_with_string_literal()
    call test_write_with_variables()
    call test_write_with_format()
    call test_write_integration()

contains

    subroutine test_write_statement_validation()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(tokenize_result_t) :: lex_result

        print *, "Test: write statement input validation"

        ! Test the exact case from Issue #498
        source = "write(*,*) 'Hello'"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Validate - this should NOT generate UNRECOGNIZED_INPUT error
        call validate_basic_syntax(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Input validation failed: ", error_msg
            stop 1
        end if

        print *, "PASS: write statement input validation"
    end subroutine test_write_statement_validation

    subroutine test_write_with_string_literal()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(tokenize_result_t) :: lex_result

        print *, "Test: write with string literal validation"

        source = "write(*,*) 'Hello World'"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Validate
        call validate_basic_syntax(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Input validation failed: ", error_msg
            stop 1
        end if

        print *, "PASS: write with string literal validation"
    end subroutine test_write_with_string_literal

    subroutine test_write_with_variables()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(tokenize_result_t) :: lex_result

        print *, "Test: write with variables validation"

        source = "write(*,*) x, y, z"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Validate
        call validate_basic_syntax(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Input validation failed: ", error_msg
            stop 1
        end if

        print *, "PASS: write with variables validation"
    end subroutine test_write_with_variables

    subroutine test_write_with_format()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(tokenize_result_t) :: lex_result

        print *, "Test: write with format validation"

        source = "write(*, '(A)') 'formatted'"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Validate
        call validate_basic_syntax(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Input validation failed: ", error_msg
            stop 1
        end if

        print *, "PASS: write with format validation"
    end subroutine test_write_with_format

    subroutine test_write_integration()
        character(len=:), allocatable :: source, generated, error_msg

        print *, "Test: write statement full integration"

        ! Test the exact failure case from Issue #498 through full pipeline
        source = "write(*,*) 'Hello'"
        
        ! Transform through full frontend (same as CLI)
        call transform_lazy_fortran_string(source, generated, error_msg)
        
        ! Check compilation succeeded
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: Compilation failed: ", trim(error_msg)
            stop 1
        end if

        ! Check proper program structure generated
        if (index(generated, "program main") == 0) then
            print *, "FAIL: No program structure generated"
            print *, "Generated: ", generated
            stop 1
        end if

        ! Check write statement preserved
        if (index(generated, "write") == 0) then
            print *, "FAIL: Write statement not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        ! Check string literal preserved
        if (index(generated, "Hello") == 0) then
            print *, "FAIL: String literal not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: write statement full integration"
    end subroutine test_write_integration

end program test_issue_498_write_statements