program test_issue_1614_character_assumed_length
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1614: Character length parameter support ==='
    print *

    if (.not. test_case_1_parameter_length()) all_passed = .false.
    if (.not. test_case_2_deferred_length()) all_passed = .false.
    if (.not. test_case_3_function_result()) all_passed = .false.
    if (.not. test_case_4_module_variable()) all_passed = .false.
    if (.not. test_case_5_mixed_attributes()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1614 verified: All character length cases work correctly'
    else
        print *, 'Issue #1614: Some test cases failed'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'

    logical function test_case_1_parameter_length()
        character(len=:), allocatable :: source, output, error_msg

        test_case_1_parameter_length = .true.
        print *, 'Test Case 1: character(n) with parameter'

        call read_example( &
            'examples/f90/issue_1614_character_parameter_length.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: Transformation error -', trim(error_msg)
            test_case_1_parameter_length = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_case_1_parameter_length = .false.
            return
        end if

        if (index(output, 'character(len=max_len)') == 0 .and. &
            index(output, 'character(len=32)') == 0 .and. &
            index(output, 'character(max_len)') == 0) then
            print *, '  FAIL: character(n) parameter not preserved'
            test_case_1_parameter_length = .false.
            return
        end if

        print *, '  PASS: character(n) with parameter works'
    end function test_case_1_parameter_length

    logical function test_case_2_deferred_length()
        character(len=:), allocatable :: source, output, error_msg

        test_case_2_deferred_length = .true.
        print *, 'Test Case 2: character(len=:), allocatable'

        call read_example( &
            'examples/f90/issue_1614_character_deferred_length.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: Transformation error -', trim(error_msg)
            test_case_2_deferred_length = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_case_2_deferred_length = .false.
            return
        end if

        if (index(output, 'character(len=:)') == 0) then
            print *, '  FAIL: character(len=:) not preserved'
            test_case_2_deferred_length = .false.
            return
        end if

        if (index(output, 'allocatable') == 0) then
            print *, '  FAIL: allocatable attribute lost'
            test_case_2_deferred_length = .false.
            return
        end if

        print *, '  PASS: character(len=:), allocatable works'
    end function test_case_2_deferred_length

    logical function test_case_3_function_result()
        character(len=:), allocatable :: source, output, error_msg

        test_case_3_function_result = .true.
        print *, 'Test Case 3: Dynamic character function result'

        call read_example( &
            'examples/f90/issue_1614_character_function_result.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: Transformation error -', trim(error_msg)
            test_case_3_function_result = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_case_3_function_result = .false.
            return
        end if

        if (index(output, 'character(len=n)') == 0 .and. &
            index(output, 'character(n)') == 0) then
            print *, '  FAIL: Dynamic character function result not preserved'
            test_case_3_function_result = .false.
            return
        end if

        print *, '  PASS: Dynamic character function result works'
    end function test_case_3_function_result

    logical function test_case_4_module_variable()
        character(len=:), allocatable :: source, output, error_msg

        test_case_4_module_variable = .true.
        print *, 'Test Case 4: Module-level character variable'

        call read_example( &
            'examples/f90/issue_1614_character_module_variable.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: Transformation error -', trim(error_msg)
            test_case_4_module_variable = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_case_4_module_variable = .false.
            return
        end if

        if (index(output, 'character(len=buf_size)') == 0 .and. &
            index(output, 'character(len=256)') == 0 .and. &
            index(output, 'character(buf_size)') == 0) then
            print *, '  FAIL: Module-level character variable not preserved'
            test_case_4_module_variable = .false.
            return
        end if

        print *, '  PASS: Module-level character variable works'
    end function test_case_4_module_variable

    logical function test_case_5_mixed_attributes()
        character(len=:), allocatable :: source, output, error_msg
        test_case_5_mixed_attributes = .true.
        print *, 'Test Case 5: Mixed character attributes'
        call read_example( &
            'examples/f90/issue_1614_character_mixed_attributes.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: Transformation error -', trim(error_msg)
            test_case_5_mixed_attributes = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_case_5_mixed_attributes = .false.
            return
        end if

        if (index(output, 'character(len=:)') == 0) then
            print *, '  FAIL: Deferred-length character not preserved'
            test_case_5_mixed_attributes = .false.
            return
        end if

        if (index(output, 'character(len=n)') == 0 .and. &
            index(output, 'character(len=10)') == 0) then
            print *, '  FAIL: Array character length not preserved'
            test_case_5_mixed_attributes = .false.
            return
        end if

        if (index(output, 'character(len=*)') == 0) then
            print *, '  FAIL: Assumed-length parameter not preserved'
            test_case_5_mixed_attributes = .false.
            return
        end if

        print *, '  PASS: Mixed character attributes work'
    end function test_case_5_mixed_attributes


end program test_issue_1614_character_assumed_length
