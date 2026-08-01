program test_function_prefix_keywords
    use transformation_api, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit
    integer :: io_status
    logical :: found_elemental, found_pure
    logical :: found_elemental_intent
    logical :: found_pure_intent_a, found_pure_intent_b
    character(len=256) :: line
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    integer :: exit_code

    found_elemental = .false.
    found_pure = .false.
    found_elemental_intent = .false.
    found_pure_intent_a = .false.
    found_pure_intent_b = .false.

    exit_code = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    input_file = join_path(temp_dir, 'test_prefix_keywords.lf', sep)
    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'elemental function square(x)'
    write (unit, '(a)') '    result = x * x'
    write (unit, '(a)') 'end function'
    write (unit, '(a)') ''
    write (unit, '(a)') 'pure function compute(a, b)'
    write (unit, '(a)') '    result = a + b'
    write (unit, '(a)') 'end function'
    close (unit)

    output_file = join_path(temp_dir, 'test_prefix_keywords_out.f90', sep)
    options%output_file = output_file

    call compile_source(input_file, options, error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, 'Compiler reported error: ', trim(error_msg)
        exit_code = 1
        goto 999
    end if

    open (newunit=unit, file=output_file, status='old', action='read')
    do
        read (unit, '(a)', iostat=io_status) line
        if (io_status /= 0) exit
        if (.not. found_elemental) then
            if (index(line, 'elemental') > 0 .and. &
                index(line, 'function square') > 0) then
                found_elemental = .true.
            end if
        end if
        if (.not. found_pure) then
            if (index(line, 'pure') > 0 .and. &
                index(line, 'function compute') > 0) then
                found_pure = .true.
            end if
        end if
        if (.not. found_elemental_intent) then
            if (index(line, 'intent(in)') > 0 .and. index(line, ':: x') > 0) then
                found_elemental_intent = .true.
            end if
        end if
        if (.not. found_pure_intent_a) then
            if (index(line, 'intent(in)') > 0 .and. index(line, ':: a') > 0) then
                found_pure_intent_a = .true.
            end if
        end if
        if (.not. found_pure_intent_b) then
            if (index(line, 'intent(in)') > 0 .and. index(line, ':: b') > 0) then
                found_pure_intent_b = .true.
            end if
        end if
        if (found_elemental .and. found_pure .and. &
            found_elemental_intent .and. found_pure_intent_a .and. &
            found_pure_intent_b) exit
    end do
    close (unit)

    if (.not. found_elemental) then
        print *, 'Did not find ELEMENTAL prefix in output'
        exit_code = 1
        goto 999
    end if

    if (.not. found_pure) then
        print *, 'Did not find PURE prefix in output'
        exit_code = 1
        goto 999
    end if

    if (.not. found_elemental_intent) then
        print *, 'Did not find intent(in) declaration for elemental function parameter'
        exit_code = 1
        goto 999
    end if

    if (.not. found_pure_intent_a) then
        print *, 'Did not find intent(in) declaration for pure function argument a'
        exit_code = 1
        goto 999
    end if

    if (.not. found_pure_intent_b) then
        print *, 'Did not find intent(in) declaration for pure function argument b'
        exit_code = 1
        goto 999
    end if

    999 continue
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code

contains

    include '../common/filesystem_helpers.inc'

end program test_function_prefix_keywords
