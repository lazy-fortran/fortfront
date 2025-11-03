program test_issue_1355_lazy_matrix
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    integer :: idx_inner_do, idx_assignment, idx_inner_end

    print *, '=== Issue #1355: multidimensional array inference ==='

    call read_example('examples/lf/issue_1355_lazy_matrix.lf', input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    call require(.not. allocated(error_msg) .or. len_trim(error_msg) == 0, &
                'Unexpected compiler error: '//merge(error_msg, '', allocated(error_msg)))
    call require(allocated(output_code), 'No code generated')

    call require(index(output_code, 'integer :: matrix(3,3)') > 0, &
                 'Matrix declaration not inferred as integer :: matrix(3,3)')
    call require(index(output_code, 'allocatable :: matrix') == 0, &
                 'Matrix declaration still allocatable')

    idx_inner_do = index(output_code, 'do j = 1, cols')
    call require(idx_inner_do > 0, 'Inner loop not found in generated code')
    idx_assignment = index(output_code, 'matrix(i, j) = i*10 + j')
    call require(idx_assignment > idx_inner_do, 'Assignment not inside inner do loop')
    idx_inner_end = index(output_code(idx_inner_do:), 'end do')
    call require(idx_inner_end > 0, 'Inner loop end not found')
    idx_inner_end = idx_inner_do + idx_inner_end - 1
call require(idx_assignment < idx_inner_end, 'Assignment emitted outside inner loop body')

contains

    subroutine require(cond, message)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: message
        if (.not. cond) then
            if (len_trim(message) > 0) then
                write (error_unit, '(a)') 'ERROR: ' // trim(message)
            end if
            stop 1
        end if
    end subroutine require

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
        if (stat /= 0) error stop 'Failed to read example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_1355_lazy_matrix
