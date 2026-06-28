program test_character_literal_type_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
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

    include '../common/read_example.inc'

    function test_single_character_assignment() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: source

        call read_example('examples/lf/character_literal_assignment.lf', source)
        call transform_lazy_fortran_string(source, output, errors)

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

        call read_example('examples/lf/select_case_character_results.lf', source)
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
