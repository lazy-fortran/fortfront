program test_issue_2388_contains_preserved
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use frontend_core, only: emit_fortran, lex_source
    use frontend_parsing, only: parse_tokens
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: token_t, to_lower
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered
    integer :: type_pos, binding_pos, type_contains_pos
    integer :: module_anchor, module_contains_pos
    integer :: run_pos, bump_pos, internal_contains_pos
    integer :: decl_pos, program_contains_pos, bump_counter_pos
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index

    call read_example('examples/f90/issue_2388_uppercase_contains.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: diagnostics produced: ' // &
                trim(error_msg)
            error stop 1
        end if
    end if

    lowered = to_lower(output)

    type_pos = index(lowered, 'type :: worker_t')
    binding_pos = index(lowered, 'procedure :: do_work')
    if (type_pos == 0 .or. binding_pos == 0) then
        write (error_unit, '(A)') 'FAIL: type or binding lost during roundtrip'
        error stop 1
    end if

    type_contains_pos = index(lowered(type_pos:), 'contains')
    if (type_contains_pos == 0) then
        write (error_unit, '(A)') 'FAIL: type-bound contains removed'
        error stop 1
    end if
    type_contains_pos = type_contains_pos + type_pos - 1
    if (.not. (type_pos < type_contains_pos .and. type_contains_pos < &
        binding_pos)) then
        write (error_unit, '(A)') 'FAIL: type contains misplaced'
        error stop 1
    end if

    module_anchor = index(lowered, 'end type worker_t')
    if (module_anchor == 0) then
        write (error_unit, '(A)') 'FAIL: end type marker missing'
        error stop 1
    end if

    module_contains_pos = index(lowered(module_anchor:), &
        new_line('a')//'contains'//new_line('a')// &
        '    subroutine run')
    if (module_contains_pos == 0) then
        write (error_unit, '(A)') 'FAIL: module-level contains stripped'
        error stop 1
    end if
    module_contains_pos = module_contains_pos + module_anchor - 1

    run_pos = index(lowered, 'subroutine run')
    if (run_pos == 0) then
        write (error_unit, '(A)') 'FAIL: subroutine run missing after roundtrip'
        error stop 1
    end if
    if (module_contains_pos >= run_pos) then
        write (error_unit, '(A)') 'FAIL: module contains not emitted before run'
        error stop 1
    end if

    bump_pos = index(lowered, 'function bump')
    if (bump_pos == 0) then
        write (error_unit, '(A)') 'FAIL: internal function bump missing'
        error stop 1
    end if

    internal_contains_pos = index(lowered(run_pos:), new_line('a')//'contains')
    if (internal_contains_pos == 0) then
        write (error_unit, '(A)') 'FAIL: internal contains removed'
        error stop 1
    end if
    internal_contains_pos = internal_contains_pos + run_pos - 1
    if (.not. (run_pos < internal_contains_pos .and. internal_contains_pos < &
        bump_pos)) then
        write (error_unit, '(A)') 'FAIL: internal contains misplaced'
        error stop 1
    end if

    call assert_compiles(output, 'issue_2388_uppercase_contains_generated')

    call read_example('examples/f90/issue_2388_program_contains.f90', source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: lexing failed for program example: ' &
                // trim(error_msg)
            error stop 1
        end if
    end if

    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: parsing failed for program example: ' &
                // trim(error_msg)
            error stop 1
        end if
    end if

    call emit_fortran(arena, prog_index, output)
    lowered = to_lower(output)

    decl_pos = index(lowered, 'integer :: counter')
    bump_counter_pos = index(lowered, 'subroutine bump_counter')
    program_contains_pos = index(lowered, new_line('a')//'contains'// &
        new_line('a'))

    if (decl_pos == 0 .or. bump_counter_pos == 0) then
        write (error_unit, '(A)') 'FAIL: program elements missing after roundtrip'
        error stop 1
    end if

    if (program_contains_pos == 0) then
        write (error_unit, '(A)') 'FAIL: program-level contains removed'
        error stop 1
    end if

    if (.not. (decl_pos < program_contains_pos .and. &
        program_contains_pos < bump_counter_pos)) then
        write (error_unit, '(A)') 'FAIL: program contains misplaced'
        error stop 1
    end if

    call assert_compiles(output, 'issue_2388_program_contains_generated')

    write (*, '(A)') &
        'PASS: uppercase CONTAINS markers preserved through roundtrip'
    write (*, '(A)') 'PASS: program-level CONTAINS preserved through roundtrip'

contains

    include '../../common/read_example.inc'


    subroutine assert_compiles(text, basename)
        use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
            cleanup_temp_directory, join_path, &
            path_separator_for
        use test_shell_commands, only: build_compile_command
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

end program test_issue_2388_contains_preserved
