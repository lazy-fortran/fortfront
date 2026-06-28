module frontend_tooling_api
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_constants, only: MAX_PARSE_ERROR_LEN
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_arena_source_text, only: set_source_text
    use frontend_core, only: lex_source, analyze_semantics
    use frontend_parsing, only: parse_tokens
    implicit none
    private

    type :: tooling_parse_options_t
        logical :: run_semantics = .false.
        logical :: reuse_arena = .false.
    end type tooling_parse_options_t

    public :: tooling_parse_options_t
    public :: tooling_load_ast_from_string
    public :: tooling_load_ast_from_file
    public :: read_file_contents
    public :: message_has_error

contains

    subroutine tooling_load_ast_from_string(source_code, arena, root_index, &
            error_msg, options, tokens)
        character(len=*), intent(in) :: source_code
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: root_index
        character(len=:), allocatable, intent(out) :: error_msg
        type(tooling_parse_options_t), intent(in), optional :: options
        type(token_t), allocatable, intent(out), optional :: tokens(:)
        type(tooling_parse_options_t) :: opts
        type(token_t), allocatable :: local_tokens(:)
        character(len=:), allocatable :: lex_error
        character(len=MAX_PARSE_ERROR_LEN) :: parse_error

        opts = tooling_parse_options_t()
        if (present(options)) opts = options

        call lex_source(source_code, local_tokens, lex_error)
        if (message_has_error(lex_error)) then
            call move_alloc(lex_error, error_msg)
            root_index = 0
            if (allocated(local_tokens)) deallocate (local_tokens)
            return
        end if

        call initialize_arena(arena, opts%reuse_arena)
        call set_source_text(arena, source_code)
        root_index = 0

        parse_error = ''
        call parse_tokens(local_tokens, arena, root_index, parse_error)
        if (len_trim(parse_error) > 0) then
            call set_message_from_char(parse_error, error_msg)
            if (allocated(local_tokens)) deallocate (local_tokens)
            return
        end if

        if (opts%run_semantics) then
            call analyze_semantics(arena, root_index)
        end if

        call ensure_empty_message(error_msg)

        if (present(tokens)) then
            call move_alloc(local_tokens, tokens)
        else if (allocated(local_tokens)) then
            deallocate (local_tokens)
        end if
    end subroutine tooling_load_ast_from_string

    subroutine tooling_load_ast_from_file(path, arena, root_index, error_msg, &
            options, tokens)
        character(len=*), intent(in) :: path
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: root_index
        character(len=:), allocatable, intent(out) :: error_msg
        type(tooling_parse_options_t), intent(in), optional :: options
        type(token_t), allocatable, intent(out), optional :: tokens(:)
        character(len=:), allocatable :: source
        character(len=:), allocatable :: io_error

        call read_file_contents(path, source, io_error)
        if (message_has_error(io_error)) then
            call move_alloc(io_error, error_msg)
            root_index = 0
            return
        end if

        if (present(tokens)) then
            call tooling_load_ast_from_string(source, arena, root_index, &
                error_msg, options, tokens)
        else
            call tooling_load_ast_from_string(source, arena, root_index, &
                error_msg, options)
        end if
    end subroutine tooling_load_ast_from_file

    subroutine initialize_arena(arena, reuse_existing)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: reuse_existing

        if (reuse_existing) then
            if (allocated(arena%entries)) then
                call arena%clear()
            else
                arena = create_ast_arena()
            end if
        else
            arena = create_ast_arena()
        end if
    end subroutine initialize_arena

    subroutine read_file_contents(path, contents, error_msg)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: contents
        character(len=:), allocatable, intent(out) :: error_msg
        integer(int64) :: file_size
        integer(int64) :: max_default
        integer :: char_len
        integer :: unit
        integer :: stat
        logical :: exists

        max_default = int(huge(0), int64)
        call ensure_empty_message(error_msg)

        inquire (file=path, exist=exists, size=file_size)
        if (.not. exists) then
            call set_message_from_char('File not found: '//trim(path), &
                error_msg)
            call allocate_empty_string(contents)
            return
        end if

        if (file_size < 0_int64) then
            call set_message_from_char('Unable to determine file size: '// &
                trim(path), error_msg)
            call allocate_empty_string(contents)
            return
        end if

        if (file_size > max_default) then
            call set_message_from_char('File too large to load: '// &
                trim(path), error_msg)
            call allocate_empty_string(contents)
            return
        end if

        char_len = int(file_size)
        if (char_len <= 0) then
            call allocate_empty_string(contents)
        else
            allocate (character(len=char_len) :: contents)
        end if

        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=stat)
        if (stat /= 0) then
            call set_message_from_char('Failed to open file: '//trim(path), &
                error_msg)
            if (allocated(contents)) deallocate (contents)
            call allocate_empty_string(contents)
            return
        end if

        if (len(contents) > 0) then
            read (unit, pos=1, iostat=stat) contents
            if (stat /= 0) then
                call set_message_from_char('Failed to read file: '// &
                    trim(path), error_msg)
                close (unit)
                deallocate (contents)
                call allocate_empty_string(contents)
                return
            end if
        end if

        close (unit)
        call ensure_empty_message(error_msg)
    end subroutine read_file_contents

    logical function message_has_error(message)
        character(len=:), allocatable, intent(in) :: message

        if (.not. allocated(message)) then
            message_has_error = .false.
        else
            message_has_error = len_trim(message) > 0
        end if
    end function message_has_error

    subroutine set_message_from_char(text, message)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: message
        integer :: length_trimmed

        length_trimmed = len_trim(text)
        if (length_trimmed <= 0) then
            allocate (character(len=0) :: message)
        else
            allocate (character(len=length_trimmed) :: message)
            message = text(1:length_trimmed)
        end if
    end subroutine set_message_from_char

    subroutine ensure_empty_message(message)
        character(len=:), allocatable, intent(inout) :: message

        if (allocated(message)) then
            if (len(message) == 0) return
            deallocate (message)
        end if
        allocate (character(len=0) :: message)
    end subroutine ensure_empty_message

    subroutine allocate_empty_string(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine allocate_empty_string

end module frontend_tooling_api
