program test_issue_2227_achar_kind_bounds
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    character(len=*), parameter :: example_path = &
        'examples/f90/issue_2227_achar_kind_crash.f90'

    test_passed = .true.

    ! Test that achar with explicit kind parameter doesn't cause bounds error
    call test_achar_kind_no_crash()

    if (test_passed) then
        print *, "test_issue_2227_achar_kind_bounds PASSED"
    else
        print *, "test_issue_2227_achar_kind_bounds FAILED"
        error stop 1
    end if

contains

    subroutine test_achar_kind_no_crash()
        call read_example(example_path, input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        ! Verify the output contains the achar call with kind parameter
        if (index(output, 'achar') == 0) then
            print *, "ERROR: Lost achar intrinsic call"
            test_passed = .false.
            return
        end if

        ! Verify the kind parameter is preserved
        if (index(output, 'kind') == 0) then
            print *, "ERROR: Lost kind parameter"
            test_passed = .false.
            return
        end if

        print *, "  - achar with kind parameter processed without crash"
    end subroutine test_achar_kind_no_crash

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, stat, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) then
            print *, "ERROR: Cannot open file:", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (character(len=file_size) :: content)
        allocate (buffer(file_size))

        read (unit, iostat=stat) buffer
        close (unit)

        if (stat /= 0) then
            print *, "ERROR: Cannot read file:", trim(filepath)
            error stop 1
        end if

        content = transfer(buffer, content)
        deallocate (buffer)
    end subroutine read_example

end program test_issue_2227_achar_kind_bounds
