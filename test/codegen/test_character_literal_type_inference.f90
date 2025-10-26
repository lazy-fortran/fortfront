program test_character_literal_type_inference
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.
    if (.not. test_single_character_assignment()) all_passed = .false.
    if (.not. test_select_case_character_assignments()) all_passed = .false.

    if (all_passed) then
        print *, 'Character literal inference tests passed'
        stop 0
    else
        print *, 'Character literal inference tests failed'
        stop 1
    end if

contains

    function test_single_character_assignment() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors

        call transform_lazy_fortran_string('value = "Hello"', output, errors)

        passed = .false.
        if (allocated(errors)) then
            if (len_trim(errors) > 0) then
                print *, 'FAIL: unexpected error for single literal case'
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, 'FAIL: missing output for single literal case'
            return
        end if

        if (index(output, 'character(len=5) :: value') == 0) then
            print *, 'FAIL: missing character(len=5) declaration'
            return
        end if

        if (index(output, 'value = "Hello"') == 0) then
            print *, 'FAIL: missing literal assignment in output'
            return
        end if

        passed = .true.
    end function test_single_character_assignment

    function test_select_case_character_assignments() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors

        source = 'grade = 85' // new_line('a') // new_line('a') // &
                 'select case (grade)' // new_line('a') // &
                 '    case (90:)' // new_line('a') // &
                 '        result = "A"' // new_line('a') // &
                 '    case (80:89)' // new_line('a') // &
                 '        result = "B"' // new_line('a') // &
                 '    case (70:79)' // new_line('a') // &
                 '        result = "C"' // new_line('a') // &
                 '    case default' // new_line('a') // &
                 '        result = "F"' // new_line('a') // &
                 'end select'

        call transform_lazy_fortran_string(source, output, errors)

        passed = .false.
        if (allocated(errors)) then
            if (len_trim(errors) > 0) then
                print *, 'FAIL: unexpected error for select case literal case'
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, 'FAIL: missing output for select case literal case'
            return
        end if

        if (index(output, 'character(len=1) :: result') == 0) then
            print *, 'FAIL: missing character(len=1) declaration'
            return
        end if

        passed = .true.
    end function test_select_case_character_assignments

end program test_character_literal_type_inference
