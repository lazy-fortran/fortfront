program test_issue_1353_program_derived_type
    use frontend, only: transform_lazy_fortran_string
    implicit none

    call test_type_definition_inside_program()
    call test_type_definition_with_attributes()
    print *, ""
    print *, "All tests passed for issue 1353."

contains

    subroutine test_type_definition_inside_program()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_type, idx_end, idx_decl

        input_code = "program test_derived_type" // new_line('A') // &
                     "    implicit none" // new_line('A') // new_line('A') // &
                     "    type :: point_t"//new_line('A')// &
                     "        real :: x"//new_line('A')// &
                     "        real :: y"//new_line('A')// &
                     "    end type point_t"//new_line('A')//new_line('A')// &
                     "    type(point_t) :: p1, p2"//new_line('A')//new_line('A')// &
                     "    p1%x = 1.0"//new_line('A')// &
                     "    p1%y = 2.0"//new_line('A')//new_line('A')// &
                     "    p2 = p1"//new_line('A')// &
                     "    p2%x = 3.0"//new_line('A')//new_line('A')// &
                     "    print *, ""p1:"", p1%x, p1%y" // new_line('A') // &
                     "    print *, ""p2:"", p2%x, p2%y" // new_line('A') // &
                     "end program test_derived_type"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: derived type definition missing"
            error stop 1
        end if

        idx_end = index(output_code, "end program test_derived_type")
        if (idx_end <= 0) then
            print *, "FAIL: end program statement missing"
            error stop 1
        end if

        if (idx_type > idx_end) then
            print *, "FAIL: type definition emitted after program end"
            error stop 1
        end if

        idx_decl = index(output_code, "type(point_t) :: p1, p2")
        if (idx_decl <= 0) then
            print *, "FAIL: derived type variable declaration missing"
            error stop 1
        end if

        if (index(output_code, "p1%x = 1.0") <= 0) then
            if (index(output_code, "p1%x = 1.0d0") <= 0) then
                print *, "FAIL: assignment to p1%x missing"
                error stop 1
            end if
        end if

        if (index(output_code, "print *, ""p2:""") <= 0) then
            print *, "FAIL: print statement for p2 missing"
            error stop 1
        end if

        print *, "PASS: Derived type stays inside program with correct declarations"
    end subroutine test_type_definition_inside_program

    subroutine test_type_definition_with_attributes()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: type_segment
        integer :: idx_type, idx_end, idx_decl
        integer :: header_len
        integer :: idx_duplicate

        input_code = "program attr_type_test" // new_line('A') // &
                     "    implicit none" // new_line('A') // new_line('A') // &
                     "    type, public :: point_t" // new_line('A') // &
                     "        real :: x" // new_line('A') // &
                     "        real :: y" // new_line('A') // &
                     "    end type point_t" // new_line('A') // new_line('A') // &
                     "    type(point_t) :: p" // new_line('A') // new_line('A') // &
                     "    p%x = 1.0" // new_line('A') // &
                     "    p%y = 2.0" // new_line('A') // new_line('A') // &
                     "    print *, p%x, p%y" // new_line('A') // &
                     "end program attr_type_test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error for attribute test:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: attribute test missing derived type definition"
            error stop 1
        end if

        idx_end = index(output_code, "end type point_t")
        if (idx_end <= 0) then
            print *, "FAIL: attribute test missing end type statement"
            error stop 1
        end if

        if (idx_end <= idx_type) then
            print *, "FAIL: attribute test emitted end type before type header"
            error stop 1
        end if

        header_len = len("type :: point_t")
        if (idx_type + header_len <= len(output_code)) then
            idx_duplicate = index(output_code(idx_type + header_len:), &
                                  "type :: point_t")
            if (idx_duplicate > 0) then
                print *, "FAIL: duplicate derived type header detected"
                error stop 1
            end if
        end if

        type_segment = output_code(idx_type:idx_end)
        if (index(type_segment, "type(point_t) ::") > 0) then
            print *, "FAIL: derived type variable declared inside type definition"
            error stop 1
        end if

        idx_decl = index(output_code, "type(point_t) :: p")
        if (idx_decl <= 0) then
            print *, "FAIL: attribute test missing type variable declaration"
            error stop 1
        end if

        if (idx_decl < idx_end) then
            print *, "FAIL: attribute test left declaration inside type"
            error stop 1
        end if

        print *, "PASS: Derived type attributes keep declarations outside type"
    end subroutine test_type_definition_with_attributes

end program test_issue_1353_program_derived_type
