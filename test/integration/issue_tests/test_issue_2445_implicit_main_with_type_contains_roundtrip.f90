program test_issue_2445_implicit_main_with_type_contains_roundtrip
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use frontend_core, only: emit_fortran, lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t, to_lower
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered
    integer :: contains_pos
    integer :: helper_pos
    integer :: end_program_pos
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index

    call read_example('examples/f90/issue_2445_implicit_main_with_type_contains.f90', &
        source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: lexing failed: ' // trim(error_msg)
            error stop 1
        end if
    end if

    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: parsing failed: ' // trim(error_msg)
            error stop 1
        end if
    end if

    call emit_fortran(arena, prog_index, output)
    lowered = to_lower(output)

    contains_pos = index(lowered, new_line('a')//'contains'//new_line('a'))
    helper_pos = index(lowered, 'subroutine helper')
    end_program_pos = index(lowered, new_line('a')//'end program')

    if (contains_pos == 0) then
        write (error_unit, '(A)') 'FAIL: contains marker missing after roundtrip'
        error stop 1
    end if

    if (helper_pos == 0) then
        write (error_unit, '(A)') 'FAIL: helper subroutine missing after roundtrip'
        error stop 1
    end if

    if (end_program_pos == 0) then
        write (error_unit, '(A)') 'FAIL: end program marker missing after roundtrip'
        error stop 1
    end if

    if (.not. (contains_pos < helper_pos .and. helper_pos < end_program_pos)) then
        write (error_unit, '(A)') 'FAIL: internal procedure misplaced around contains'
        error stop 1
    end if

    call assert_compiles(output, 'issue_2445_roundtrip_generated')

    write (*, '(A)') 'PASS: implicit main contains preserves internal procedure'

contains

    include '../../common/filesystem_helpers.inc'
    include '../../common/shell_commands.inc'

    include '../../common/read_example.inc'

    subroutine assert_compiles(text, basename)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: basename
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: cmd
        logical :: is_windows
        character(len=:), allocatable :: temp_dir
        character(len=1) :: sep
        integer :: unit, ios, exit_code

        is_windows = check_if_windows()
        call create_temp_directory(temp_dir, is_windows)
        if (len_trim(temp_dir) == 0) error stop &
            'FAIL: could not create temporary directory'

        sep = path_separator_for(temp_dir)
        filename = join_path(temp_dir, trim(basename)//'.f90', sep)

        open (newunit=unit, file=filename, status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(A)') 'FAIL: could not create ' // trim(filename)
            error stop 1
        end if

        write (unit, '(A)') text
        close (unit)

        cmd = build_compile_command(filename, '', temp_dir, is_windows)
        call execute_command_line(cmd, exitstat=exit_code)
        call cleanup_temp_directory(temp_dir, is_windows)
        if (exit_code /= 0) then
            write (error_unit, '(A)') 'FAIL: generated code does not compile'
            error stop 1
        end if
    end subroutine assert_compiles

end program test_issue_2445_implicit_main_with_type_contains_roundtrip
