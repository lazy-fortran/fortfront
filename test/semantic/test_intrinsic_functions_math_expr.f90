program test_intrinsic_functions_math_expr
    use lexer_api, only: lex_source, lex_file
    use parser_api, only: parse_tokens, parse_tokens_safe
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, compile_source
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg

    print *, "Testing intrinsic functions in mathematical expressions..."

    ! Test the exact code from issue #92
    call read_example('examples/f90/intrinsic_functions_math_expr_complex.f90', &
                      test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Complex mathematical expression with sqrt compiled successfully"
        print *, "✓ No type mismatch errors occurred"
    else
        print *, "ERROR: Failed to analyze mathematical expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    ! Test simpler sqrt case
    call read_example('examples/f90/intrinsic_functions_math_expr_simple.f90', &
                      test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Simple sqrt expression compiled successfully"
    else
        print *, "ERROR: Failed to analyze simple sqrt expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    ! Test nested sqrt expressions
    call read_example('examples/f90/intrinsic_functions_math_expr_nested.f90', &
                      test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Nested sqrt expressions compiled successfully"
    else
        print *, "ERROR: Failed to analyze nested sqrt expressions"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    print *, "All intrinsic function tests passed!"

contains

    include '../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_intrinsic_functions_math_expr
