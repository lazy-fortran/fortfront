program frontend_probe
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use frontend_compiler_api, only: compiler_frontend_options_t, &
                                     compiler_frontend_result_t, &
                                     compile_frontend_from_file
    implicit none

    character(len=:), allocatable :: source_path
    character(len=:), allocatable :: source_file
    character(len=:), allocatable :: diagnostic_text
    character(len=:), allocatable :: json_line
    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options
    integer :: argc
    integer :: arg_len
    integer :: alloc_stat

    argc = command_argument_count()
    if (argc /= 1) then
        call report_usage()
        stop 1
    end if

    call get_command_argument(1, length=arg_len)
    allocate (character(len=arg_len) :: source_file, stat=alloc_stat)
    if (alloc_stat /= 0) then
        write (error_unit, '(A)') 'frontend_probe: unable to allocate argument'
        stop 1
    end if
    call get_command_argument(1, value=source_file)

    options%run_semantics = .true.
    call compile_frontend_from_file(trim(source_file), result, options)

    if (allocated(result%source_path)) then
        source_path = result%source_path
    else
        source_path = trim(source_file)
    end if

    if (allocated(result%diagnostic_text)) then
        diagnostic_text = result%diagnostic_text
    else
        diagnostic_text = ''
    end if

    json_line = '{"source_path":"' // escape_json(source_path) // &
                '","parse_ok":' // logical_json(result%parse_ok) // &
                ',"semantic_ok":' // logical_json(result%semantic_ok) // &
                ',"diagnostic_text":"' // escape_json(diagnostic_text) // '"}'
    write (output_unit, '(A)') trim(json_line)

contains

    subroutine report_usage()
        write (error_unit, '(A)') 'Usage: frontend_probe <source-file>'
    end subroutine report_usage

    pure function logical_json(value) result(text)
        logical, intent(in) :: value
        character(len=5) :: text

        if (value) then
            text = 'true '
        else
            text = 'false'
        end if
    end function logical_json

    pure function escape_json(text) result(escaped)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: buffer
        integer :: len_text
        integer :: i
        integer :: pos
        integer :: code
        character :: ch
        character :: backslash

        len_text = len_trim(text)
        if (len_text == 0) then
            escaped = ''
            return
        end if

        allocate (character(len=len_text) :: trimmed)
        trimmed = text(1:len_text)

        allocate (character(len=len_text * 6) :: buffer)
        backslash = achar(92)
        pos = 1

        do i = 1, len_text
            ch = trimmed(i:i)
            select case (ch)
            case ('"')
                buffer(pos:pos) = backslash
                pos = pos + 1
                buffer(pos:pos) = '"'
                pos = pos + 1
            case (achar(92))
                buffer(pos:pos) = backslash
                pos = pos + 1
                buffer(pos:pos) = backslash
                pos = pos + 1
            case (char(9))
                buffer(pos:pos) = backslash
                pos = pos + 1
                buffer(pos:pos) = 't'
                pos = pos + 1
            case (char(10))
                buffer(pos:pos) = backslash
                pos = pos + 1
                buffer(pos:pos) = 'n'
                pos = pos + 1
            case (char(13))
                buffer(pos:pos) = backslash
                pos = pos + 1
                buffer(pos:pos) = 'r'
                pos = pos + 1
            case default
                code = iachar(ch)
                if (code < 32) then
                    buffer(pos:pos) = backslash
                    pos = pos + 1
                    buffer(pos:pos) = 'u'
                    pos = pos + 1
                    buffer(pos:pos + 3) = hex4(code)
                    pos = pos + 4
                else
                    buffer(pos:pos) = ch
                    pos = pos + 1
                end if
            end select
        end do

        pos = pos - 1
        if (pos < 1) then
            escaped = ''
        else
            allocate (character(len=pos) :: escaped)
            escaped = buffer(1:pos)
        end if
    end function escape_json

    pure function hex4(value) result(chars)
        integer, intent(in) :: value
        character(len=4) :: chars
        integer :: idx
        integer :: temp
        integer :: digit

        temp = value
        do idx = 4, 1, -1
            digit = mod(temp, 16)
            select case (digit)
            case (0:9)
                chars(idx:idx) = achar(iachar('0') + digit)
            case default
                chars(idx:idx) = achar(iachar('a') + digit - 10)
            end select
            temp = temp / 16
        end do
    end function hex4

end program frontend_probe
