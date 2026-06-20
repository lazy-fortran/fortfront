program test_issue_2816_walrus_operator
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered

    call test_walrus_declaration_inference()
    call test_walrus_redeclaration_error()

    print *, 'PASS: walrus operator declaration-with-inference'

contains

    subroutine test_walrus_declaration_inference()
        call read_example('examples/lf/issue_2816_walrus_declaration.lf', input_code)
        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: valid walrus reported an error'
            write (error_unit, '(A)') trim(error_msg)
            error stop 1
        end if

        lowered = to_lower(output_code)

        call require(index(lowered, 'integer :: x') > 0, &
                     'integer x not inferred from x := 42', output_code)
        call require(index(lowered, 'double precision :: y') > 0, &
                     'double precision y not inferred from y := 3.14d0', output_code)
        call require(index(lowered, 'logical :: flag') > 0, &
                     'logical flag not inferred from flag := .true.', output_code)
        call require(index(lowered, 'character(len=5) :: s') > 0, &
                     'character s not inferred from s := "hello"', output_code)
        call require(index(lowered, 'integer') > 0 .and. &
                     index(lowered, 'nums') > 0, &
                     'array nums not declared from nums := [1, 2, 3]', output_code)
        call require(index(lowered, 'x = 42') > 0, &
                     'walrus did not emit assignment x = 42', output_code)
    end subroutine test_walrus_declaration_inference

    subroutine test_walrus_redeclaration_error()
        call read_example('examples/lf/issue_2816_walrus_redeclaration.lf', &
                          input_code)
        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) == 0) then
            write (error_unit, '(A)') &
                'FAIL: walrus same-scope redeclaration was not rejected'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(to_lower(error_msg), 'redeclares') == 0) then
            write (error_unit, '(A)') &
                'FAIL: redeclaration error message missing "redeclares"'
            write (error_unit, '(A)') trim(error_msg)
            error stop 1
        end if
    end subroutine test_walrus_redeclaration_error

    subroutine require(condition, message, produced)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: produced

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//message
            write (error_unit, '(A)') trim(produced)
            error stop 1
        end if
    end subroutine require

    include 'common/read_example.inc'
end program test_issue_2816_walrus_operator
