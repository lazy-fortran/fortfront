program test_issue_2013_nested_implied_do
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
                                  INPUT_MODE_STANDARD
    implicit none

    type(transform_context_t) :: context
    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_2013_nested_implied_do_duplicate_var.f90', &
                      source)

    context%input_mode = INPUT_MODE_STANDARD
    call transform_with_context(source, transformed, error_msg, context)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // trim(error_msg)
            stop 1
        end if
    end if

    ! Primary fix: array size should be 12 (3*4), not 3
    if (index(transformed, 'matrix_flat(12)') == 0) then
        write (error_unit, '(a)') 'FAIL: array size not correctly calculated as 12'
        write (error_unit, '(a)') 'Output: ' // trim(transformed)
        stop 1
    end if

    ! Verify nested implied-do structure is preserved
    if (index(transformed, 'j=1, 4') == 0 .or. &
        index(transformed, 'i=1, 3') == 0) then
        write (error_unit, '(a)') 'FAIL: nested implied-do structure not preserved'
        stop 1
    end if

    write (*, '(a)') 'PASS: nested implied-do array size correctly calculated'

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: file_unit, file_size, ios_read
        character(len=1), allocatable :: buffer(:)

        open (newunit=file_unit, file=filepath, status='old', &
              action='read', access='stream', iostat=ios_read)
        if (ios_read /= 0) then
            print *, "ERROR: cannot open file: ", filepath
            error stop 1
        end if

        inquire (unit=file_unit, size=file_size)
        allocate (buffer(file_size))
        read (file_unit, iostat=ios_read) buffer
        close (file_unit)

        if (ios_read /= 0) then
            print *, "ERROR: cannot read file: ", filepath
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_2013_nested_implied_do
