program test_memory_corruption_fix
    ! Test for issue #71: Memory corruption in semantic analyzer
    ! This test reproduces the conditions that cause double-free errors
    use fortfront
    implicit none

    character(len=:), allocatable :: test_code, output_code, error_msg

    call read_example('examples/f90/memory_corruption_test.f90', test_code)
    print *, "Testing memory corruption fix..."

    ! Test multiple rounds of semantic analysis to trigger potential double-free
    block
        integer :: round
        do round = 1, 5
            print *, "Round ", round

            ! Use the high-level API to test the full pipeline
            call transform_lazy_fortran_string(test_code, output_code, error_msg)

            ! Check for errors
            if (allocated(error_msg) .and. len(error_msg) > 0) then
                print *, "ERROR in round ", round, ": ", error_msg
                stop 1
            end if

            ! Check basic functionality
            if (len(output_code) == 0) then
                print *, "ERROR: No output generated in round ", round
                stop 1
            end if

            print *, "Round ", round, " completed successfully"
        end do
    end block

    print *, "Memory corruption fix test PASSED"
    print *, "All 5 rounds of semantic analysis completed without crashes"

contains
    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_memory_corruption_fix
