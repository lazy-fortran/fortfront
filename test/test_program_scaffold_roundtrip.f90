program test_program_scaffold_roundtrip
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed

    test_passed = .true.

    call verify_program_preservation( &
        'examples/f90/program_scaffold_preserve.f90', 'scaffold_test')
    call verify_program_preservation( &
        'examples/f90/program_scaffold_special_name.f90', 'custom_program_name')

    if (test_passed) then
        print *, "test_program_scaffold_roundtrip PASSED"
    else
        print *, "test_program_scaffold_roundtrip FAILED"
        error stop 1
    end if

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine verify_program_preservation(filepath, expected_name)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: expected_name

        call read_example(filepath, input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed for", filepath, ":", &
                trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'program ' // trim(expected_name)) == 0) then
            print *, "FAIL: program declaration missing for", expected_name
            test_passed = .false.
        end if

        if (index(output, 'implicit none') == 0) then
            print *, "FAIL: implicit none was dropped for", expected_name
            test_passed = .false.
        end if

        if (index(output, 'end program ' // trim(expected_name)) == 0) then
            print *, "FAIL: end program missing or renamed for", expected_name
            test_passed = .false.
        end if
    end subroutine verify_program_preservation


end program test_program_scaffold_roundtrip
