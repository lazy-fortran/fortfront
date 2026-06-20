module frontend_compiler_api
    use fortfront_constants, only: MAX_PARSE_ERROR_LEN
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_arena_source_text, only: set_source_text
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use frontend_tooling_api, only: read_file_contents, message_has_error
    use frontend_transformation_semantics, only: get_detailed_semantic_errors
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program, has_semantic_errors
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use semantic_operating_mode, only: OPERATING_MODE_INFER
    use standardizer, only: standardize_ast, standardize_multi_unit_children, &
                            set_standardizer_input_mode
    implicit none
    private

    type :: compiler_frontend_options_t
        logical :: run_semantics = .true.
        integer :: input_mode = INPUT_MODE_LAZY
        integer :: operating_mode = OPERATING_MODE_INFER
        ! Synthesize inferred declarations and wrap fragments in a program.
        ! Backends that lower from declared AST set this; off preserves the
        ! raw analyzed arena for tooling that walks inferred-type nodes.
        logical :: standardize = .false.
    end type compiler_frontend_options_t

    type :: compiler_frontend_result_t
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        integer :: root_index = 0
        logical :: parse_ok = .false.
        logical :: semantic_ok = .false.
        character(len=:), allocatable :: source_path
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: diagnostic_text
        type(token_t), allocatable :: tokens(:)
    contains
        procedure :: success => compiler_frontend_success
    end type compiler_frontend_result_t

    public :: compiler_frontend_options_t
    public :: compiler_frontend_result_t
    public :: compile_frontend_from_string
    public :: compile_frontend_from_file

contains

    subroutine compile_frontend_from_string(source_code, result, options)
        character(len=*), intent(in) :: source_code
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t), intent(in), optional :: options
        type(compiler_frontend_options_t) :: opts
        type(token_t), allocatable :: local_tokens(:)
        character(len=:), allocatable :: lex_error
        character(len=MAX_PARSE_ERROR_LEN) :: parse_error

        opts = compiler_frontend_options_t()
        if (present(options)) opts = options

        call initialize_result(result)

        call lex_source(source_code, local_tokens, lex_error)
        if (message_has_error(lex_error)) then
            call set_result_error(result, lex_error)
            if (allocated(local_tokens)) deallocate (local_tokens)
            return
        end if

        result%arena = create_ast_arena()
        call set_source_text(result%arena, source_code)

        parse_error = ''
        call parse_tokens(local_tokens, result%arena, result%root_index, &
                          parse_error)
        if (len_trim(parse_error) > 0) then
            call set_result_error(result, parse_error)
            if (allocated(local_tokens)) deallocate (local_tokens)
            return
        end if

        result%parse_ok = result%root_index > 0
        call move_alloc(local_tokens, result%tokens)

        call create_semantic_context(result%semantic_ctx)
        result%semantic_ctx%input_mode = opts%input_mode
        result%semantic_ctx%operating_mode = opts%operating_mode

        if (opts%run_semantics) then
            call analyze_program(result%semantic_ctx, result%arena, &
                                 result%root_index)
            result%semantic_ok = .not. has_semantic_errors(result%semantic_ctx)
            if (.not. result%semantic_ok) then
                result%diagnostic_text = &
                    get_detailed_semantic_errors(result%semantic_ctx)
                result%error_msg = result%diagnostic_text
            end if
        else
            result%semantic_ok = .true.
        end if

        if (opts%standardize .and. result%semantic_ok) then
            call set_standardizer_input_mode(opts%input_mode)
            call standardize_multi_unit_children(result%arena, result%root_index)
            call standardize_ast(result%arena, result%root_index)
        end if
    end subroutine compile_frontend_from_string

    subroutine compile_frontend_from_file(path, result, options)
        character(len=*), intent(in) :: path
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t), intent(in), optional :: options
        character(len=:), allocatable :: source
        character(len=:), allocatable :: io_error

        call read_file_contents(path, source, io_error)
        if (message_has_error(io_error)) then
            call initialize_result(result)
            call set_result_error(result, io_error)
            result%source_path = trim(path)
            return
        end if

        call compile_frontend_from_string(source, result, options)
        result%source_path = trim(path)
    end subroutine compile_frontend_from_file

    logical function compiler_frontend_success(this)
        class(compiler_frontend_result_t), intent(in) :: this

        compiler_frontend_success = this%parse_ok .and. this%semantic_ok
    end function compiler_frontend_success

    subroutine initialize_result(result)
        type(compiler_frontend_result_t), intent(out) :: result

        result%arena = create_ast_arena()
        call create_semantic_context(result%semantic_ctx)
        result%root_index = 0
        result%parse_ok = .false.
        result%semantic_ok = .false.
        call allocate_empty(result%source_path)
        call allocate_empty(result%error_msg)
        call allocate_empty(result%diagnostic_text)
    end subroutine initialize_result

    subroutine set_result_error(result, message)
        type(compiler_frontend_result_t), intent(inout) :: result
        character(len=*), intent(in) :: message
        integer :: message_length

        message_length = len_trim(message)
        if (allocated(result%error_msg)) deallocate (result%error_msg)
        if (allocated(result%diagnostic_text)) deallocate (result%diagnostic_text)

        if (message_length <= 0) then
            call allocate_empty(result%error_msg)
            call allocate_empty(result%diagnostic_text)
        else
            allocate (character(len=message_length) :: result%error_msg)
            result%error_msg = message(1:message_length)
            allocate (character(len=message_length) :: result%diagnostic_text)
            result%diagnostic_text = message(1:message_length)
        end if
    end subroutine set_result_error

    subroutine allocate_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine allocate_empty

end module frontend_compiler_api
