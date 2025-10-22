program test_issue_1707_derived_type_end
    use frontend, only: transform_lazy_fortran_string
    implicit none

    call test_module_header_without_double_colon()
    call test_program_header_without_double_colon()
    print *, ""
    print *, "All tests passed for issue 1707."

contains

    subroutine test_module_header_without_double_colon()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_header
        integer :: idx_footer
        integer :: idx_wrong_footer

        input_code = "module types_mod" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    type point_t" // new_line('A') // &
                     "        real :: x" // new_line('A') // &
                     "        real :: y" // new_line('A') // &
                     "    end type point_t" // new_line('A') // &
                     "end module types_mod"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: module test returned error:", trim(error_msg)
            error stop 1
        end if

        idx_header = index(output_code, "type :: point_t")
        if (idx_header <= 0) then
            print *, "FAIL: module test missing standardized type header"
            error stop 1
        end if

        idx_footer = index(output_code, "end type point_t")
        if (idx_footer <= 0) then
            print *, "FAIL: module test missing matching end type name"
            error stop 1
        end if

        idx_wrong_footer = index(output_code, "end type x")
        if (idx_wrong_footer > 0) then
            print *, "FAIL: module test emitted component name in end type"
            error stop 1
        end if

        print *, "PASS: module header without double colon preserves type name"
    end subroutine test_module_header_without_double_colon

    subroutine test_program_header_without_double_colon()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_header
        integer :: idx_footer

        input_code = "program header_test" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    type point_t" // new_line('A') // &
                     "        real :: value" // new_line('A') // &
                     "    end type point_t" // new_line('A') // &
                     "    type(point_t) :: p" // new_line('A') // &
                     "    p%value = 1.0" // new_line('A') // &
                     "    print *, p%value" // new_line('A') // &
                     "end program header_test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: program test returned error:", trim(error_msg)
            error stop 1
        end if

        idx_header = index(output_code, "type :: point_t")
        if (idx_header <= 0) then
            print *, "FAIL: program test missing standardized type header"
            error stop 1
        end if

        idx_footer = index(output_code, "end type point_t")
        if (idx_footer <= 0) then
            print *, "FAIL: program test missing matching end type name"
            error stop 1
        end if

        print *, "PASS: program header without double colon preserves type name"
    end subroutine test_program_header_without_double_colon

end program test_issue_1707_derived_type_end
