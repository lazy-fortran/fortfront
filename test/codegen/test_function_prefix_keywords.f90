program test_function_prefix_keywords
    use frontend, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit
    integer :: io_status
    logical :: found_elemental, found_pure
    character(len=256) :: line

    found_elemental = .false.
    found_pure = .false.

    input_file = 'test_prefix_keywords.lf'
    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'elemental function square(x)'
    write (unit, '(a)') '    result = x * x'
    write (unit, '(a)') 'end function'
    write (unit, '(a)') ''
    write (unit, '(a)') 'pure function compute(a, b)'
    write (unit, '(a)') '    result = a + b'
    write (unit, '(a)') 'end function'
    close (unit)

    output_file = 'test_prefix_keywords_out.f90'
    options%output_file = output_file

    call compile_source(input_file, options, error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, 'Compiler reported error: ', trim(error_msg)
        stop 1
    end if

    open (newunit=unit, file=output_file, status='old', action='read')
    do
        read (unit, '(a)', iostat=io_status) line
        if (io_status /= 0) exit
        if (.not. found_elemental) then
           if (index(line, 'elemental') > 0 .and. index(line, 'function square') > 0) then
                found_elemental = .true.
            end if
        end if
        if (.not. found_pure) then
            if (index(line, 'pure') > 0 .and. index(line, 'function compute') > 0) then
                found_pure = .true.
            end if
        end if
        if (found_elemental .and. found_pure) exit
    end do
    close (unit)

    if (.not. found_elemental) then
        print *, 'Did not find ELEMENTAL prefix in output'
        stop 1
    end if

    if (.not. found_pure) then
        print *, 'Did not find PURE prefix in output'
        stop 1
    end if

    stop 0
end program test_function_prefix_keywords
