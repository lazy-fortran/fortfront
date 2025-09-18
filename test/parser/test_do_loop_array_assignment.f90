program test_do_loop_array_assignment
    ! Regression test for Issue #1271: ensure do loop bodies handle array element assignments
    use frontend, only: compile_source, compilation_options_t
    use iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: input_path, output_path
    character(len=256) :: error_msg, line
    type(compilation_options_t) :: options
    integer :: unit, iostat
    logical :: found_assignment, found_unparsed

    print *, "=== Testing array element assignments in do loop bodies (Issue #1271) ==="

    input_path = 'test_do_array_assignment_input.f90'
    output_path = 'test_do_array_assignment_output.f90'

    open(newunit=unit, file=input_path, status='replace', action='write', iostat=iostat)
    if (iostat /= 0) then
        write(error_unit, '(a)') 'ERROR: cannot create input file'
        stop 1
    end if
    write(unit, '(a)') 'program array_update'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i'
    write(unit, '(a)') '  integer :: arr(5)'
    write(unit, '(a)') '  arr = [1, 2, 3, 4, 5]'
    write(unit, '(a)') '  do i = 1, 5'
    write(unit, '(a)') '    arr(i) = arr(i) + 1'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program array_update'
    close(unit)

    options%output_file = output_path
    call compile_source(input_path, options, error_msg)
    if (len_trim(error_msg) > 0) then
        write(error_unit, '(a)') 'ERROR: '//trim(error_msg)
        stop 1
    end if

    open(newunit=unit, file=output_path, status='old', action='read', iostat=iostat)
    if (iostat /= 0) then
        write(error_unit, '(a)') 'ERROR: cannot open output file'
        stop 1
    end if

    found_assignment = .false.
    found_unparsed = .false.
    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (index(line, '! Unparsed') > 0) found_unparsed = .true.
        if (index(adjustl(line), 'arr(i) = arr(i) + 1') > 0) found_assignment = .true.
    end do
    close(unit)

    if (found_unparsed) then
        write(error_unit, '(a)') 'ERROR: unexpected ! Unparsed placeholder emitted'
        stop 1
    end if
    if (.not. found_assignment) then
        write(error_unit, '(a)') 'ERROR: array assignment missing from output'
        stop 1
    end if

    print *, 'PASS: parser keeps array element assignments intact inside do loops'
    stop 0
end program test_do_loop_array_assignment
