program test_if_inside_do_loop
    ! Regression test for Issue #1324: ensure IF statements inside DO loops parse
    use frontend, only: compile_source, compilation_options_t
    use iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: input_path, output_path
    character(len=256) :: error_msg, line
    type(compilation_options_t) :: options
    integer :: unit, iostat
    integer :: inline_if_count
    logical :: found_do_loop, found_unparsed, found_nested_if

    print *, "=== Testing IF statements inside DO loops (Issue #1324) ==="

    input_path = 'test_inline_if_in_do_input.f90'
    output_path = 'test_inline_if_in_do_output.f90'

    open(newunit=unit, file=input_path, status='replace', action='write', iostat=iostat)
    if (iostat /= 0) then
        write(error_unit, '(a)') 'ERROR: cannot create input file'
        stop 1
    end if
    write(unit, '(a)') 'program inline_if_fixture'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  real :: x'
    write(unit, '(a)') '  integer :: i, n'
    write(unit, '(a)') '  n = 3'
    write(unit, '(a)') '  call random_number(x)'
    write(unit, '(a)') '  do i = 1, n'
    write(unit, '(a)') '    call random_number(x)'
    write(unit, '(a)') '    print*, "x =", x'
    write(unit, '(a)') '    if (x > 0.3) print*, "x larger than 0.3"'
    write(unit, '(a)') '    if (x > 0.2) then'
    write(unit, '(a)') '      print*, "x larger than 0.2"'
    write(unit, '(a)') '    end if'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program inline_if_fixture'
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

    inline_if_count = 0
    found_do_loop = .false.
    found_unparsed = .false.
    found_nested_if = .false.

    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (index(line, '! Unparsed') > 0) found_unparsed = .true.
        if (index(adjustl(line), 'do i = 1, n') > 0) found_do_loop = .true.
        if (index(line, 'if (x > 0.3d0)') > 0) inline_if_count = inline_if_count + 1
        if (index(line, 'if (x > 0.2d0) then') > 0) found_nested_if = .true.
    end do
    close(unit)

    if (found_unparsed) then
        write(error_unit, '(a)') 'ERROR: unexpected ! Unparsed placeholder emitted'
        stop 1
    end if
    if (.not. found_do_loop) then
        write(error_unit, '(a)') 'ERROR: DO loop missing from output'
        stop 1
    end if
    if (inline_if_count < 1) then
        write(error_unit, '(a)') 'ERROR: missing IF (x > 0.3d0) inside loop'
        stop 1
    end if
    if (.not. found_nested_if) then
        write(error_unit, '(a)') 'ERROR: nested IF (x > 0.2d0) block missing'
        stop 1
    end if

    print *, 'PASS: parser retains IF statements inside DO loops without placeholders'
    stop 0
end program test_if_inside_do_loop
