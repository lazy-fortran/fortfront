module test_roundtrip_core
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none
    private

    public :: roundtrip_result_t
    public :: run_roundtrip_test
    public :: compile_fortran_source
    public :: is_complete_compilation_unit

    type :: roundtrip_result_t
        logical :: success = .false.
        logical :: lex_error = .false.
        logical :: parse_error = .false.
        logical :: relex_error = .false.
        logical :: reparse_error = .false.
        logical :: output_differs = .false.
        logical :: compile_error = .false.
        character(len=512) :: error_message = ''
        character(len=:), allocatable :: first_output
        character(len=:), allocatable :: second_output
    end type roundtrip_result_t

contains

    include '../common/filesystem_helpers.inc'
    include '../common/shell_commands.inc'

    subroutine run_roundtrip_test(source, result, skip_compile, is_windows)
        character(len=*), intent(in) :: source
        type(roundtrip_result_t), intent(out) :: result
        logical, intent(in), optional :: skip_compile
        logical, intent(in), optional :: is_windows

        type(ast_arena_t) :: arena1, arena2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        integer :: root1, root2
        character(len=:), allocatable :: error_msg
        logical :: do_compile, is_win

        do_compile = .true.
        if (present(skip_compile)) do_compile = .not. skip_compile

        is_win = .false.
        if (present(is_windows)) is_win = is_windows

        result%success = .false.

        arena1 = create_ast_arena()
        call lex_source(source, tokens1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            result%lex_error = .true.
            result%error_message = 'lex error: '//trim(error_msg)
            return
        end if

        call parse_tokens(tokens1, arena1, root1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            result%parse_error = .true.
            result%error_message = 'parse error: '//trim(error_msg)
            return
        end if

        call emit_fortran(arena1, root1, result%first_output)

        arena2 = create_ast_arena()
        call lex_source(result%first_output, tokens2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            result%relex_error = .true.
            result%error_message = 'relex error: '//trim(error_msg)
            return
        end if

        call parse_tokens(tokens2, arena2, root2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            result%reparse_error = .true.
            result%error_message = 'reparse error: '//trim(error_msg)
            return
        end if

        call emit_fortran(arena2, root2, result%second_output)

        if (result%first_output /= result%second_output) then
            result%output_differs = .true.
            result%error_message = 'roundtrip output differs'
            return
        end if

        if (do_compile) then
            if (.not. compile_fortran_source(result%first_output, is_win)) then
                result%compile_error = .true.
                result%error_message = 'gfortran compilation failed'
                return
            end if
        end if

        result%success = .true.
    end subroutine run_roundtrip_test

    logical function compile_fortran_source(source, is_windows)
        character(len=*), intent(in) :: source
        logical, intent(in) :: is_windows

        character(len=:), allocatable :: temp_dir, temp_file, command
        integer :: unit_num, ios, exit_code
        character(len=256) :: temp_filename

        compile_fortran_source = .false.

        call create_temp_directory(temp_dir, is_windows)
        if (len_trim(temp_dir) == 0) return

        if (is_windows) then
            temp_filename = trim(temp_dir)//'\roundtrip_test.f90'
        else
            temp_filename = trim(temp_dir)//'/roundtrip_test.f90'
        end if

        open (newunit=unit_num, file=trim(temp_filename), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            call cleanup_temp_directory(temp_dir, is_windows)
            return
        end if

        write (unit_num, '(A)', iostat=ios) source
        close (unit_num)

        if (ios /= 0) then
            call cleanup_temp_directory(temp_dir, is_windows)
            return
        end if

        command = build_compile_command(trim(temp_filename), '', temp_dir, &
            is_windows)
        if (len_trim(command) == 0) then
            call cleanup_temp_directory(temp_dir, is_windows)
            return
        end if

        call execute_command_line(trim(command), exitstat=exit_code)
        compile_fortran_source = (exit_code == 0)

        call cleanup_temp_directory(temp_dir, is_windows)
    end function compile_fortran_source

    pure logical function is_complete_compilation_unit(source)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: lower_source
        integer :: i

        is_complete_compilation_unit = .false.
        if (len_trim(source) == 0) return

        allocate (character(len=len(source)) :: lower_source)
        do i = 1, len(source)
            if (source(i:i) >= 'A' .and. source(i:i) <= 'Z') then
                lower_source(i:i) = achar(iachar(source(i:i)) + 32)
            else
                lower_source(i:i) = source(i:i)
            end if
        end do

        if (index(lower_source, 'program ') > 0) then
            is_complete_compilation_unit = .true.
        else if (index(lower_source, 'submodule ') > 0) then
            is_complete_compilation_unit = .true.
        else if (index(lower_source, 'module ') > 0) then
            is_complete_compilation_unit = .true.
        else if (index(lower_source, 'block data') > 0) then
            is_complete_compilation_unit = .true.
        end if

        if (index(lower_source, 'subroutine ') > 0 .or. &
            index(lower_source, 'function ') > 0) then
            if (index(lower_source, 'end subroutine') > 0 .or. &
                index(lower_source, 'end function') > 0) then
                is_complete_compilation_unit = .true.
            end if
        end if
    end function is_complete_compilation_unit

end module test_roundtrip_core
