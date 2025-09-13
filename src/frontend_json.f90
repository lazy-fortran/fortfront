module frontend_json
    ! JSON-based compile entry points split from frontend_core to avoid
    ! pulling json-fortran into builds that do not need it (improves
    ! Windows stability by reducing stack usage from dependency code).

    use lexer_core, only: token_t
    use frontend_core, only: compilation_options_t, parse_tokens, &
                             standardize_ast, generate_fortran_code, &
                             write_compiled_output
    use compiler_arena, only: compiler_arena_t, create_compiler_arena, destroy_compiler_arena
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program, has_semantic_errors
    use path_validation, only: validate_input_path, path_validation_result_t
    use json_reader, only: json_read_tokens_from_file, json_read_ast_from_file, &
                           json_read_semantic_from_file
    implicit none
    private

    public :: compile_from_tokens_json, compile_from_ast_json, &
              compile_from_semantic_json

contains

    ! Local helper: minimal semantic analysis wrapper
    subroutine run_semantic(arena, prog_index, error_msg)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(out) :: error_msg

        block
            type(semantic_context_t) :: ctx
            ctx = create_semantic_context()
            ctx%strict_mode = .false.
            call analyze_program(ctx, arena, prog_index)
            if (has_semantic_errors(ctx)) then
                error_msg = "Semantic analysis failed"
                return
            end if
        end block
        error_msg = ""
    end subroutine run_semantic

    ! Compile from tokens JSON (skip phase 1)
    subroutine compile_from_tokens_json(tokens_json_file, options, error_msg)
        character(len=*), intent(in) :: tokens_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(token_t), allocatable :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result

        error_msg = ""

        validation_result = validate_input_path(tokens_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Tokens JSON path validation failed: " // validation_result%get_message()
            return
        end if

        compiler_arena = create_compiler_arena()

        tokens = json_read_tokens_from_file(tokens_json_file)

        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call run_semantic(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call standardize_ast(compiler_arena%ast, prog_index)
        call generate_fortran_code(compiler_arena%ast, prog_index, code)
        call write_compiled_output(options, code, error_msg)
        call destroy_compiler_arena(compiler_arena)
    end subroutine compile_from_tokens_json

    ! Compile from AST JSON (skip phases 1-2)
    subroutine compile_from_ast_json(ast_json_file, options, error_msg)
        character(len=*), intent(in) :: ast_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result

        error_msg = ""

        validation_result = validate_input_path(ast_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "AST JSON path validation failed: " // validation_result%get_message()
            return
        end if

        compiler_arena = create_compiler_arena()

        if (index(ast_json_file, '.json') > 0) then
            prog_index = json_read_ast_from_file(ast_json_file, compiler_arena%ast)
            if (prog_index == 0) then
                error_msg = "Failed to load AST from JSON file"
                call destroy_compiler_arena(compiler_arena)
                return
            end if
        else
            error_msg = "AST file must be a JSON file for from_json compilation"
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call run_semantic(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call standardize_ast(compiler_arena%ast, prog_index)
        call generate_fortran_code(compiler_arena%ast, prog_index, code)
        call write_compiled_output(options, code, error_msg)
        call destroy_compiler_arena(compiler_arena)
    end subroutine compile_from_ast_json

    ! Compile from semantic JSON (annotated AST to codegen)
    subroutine compile_from_semantic_json(semantic_json_file, options, error_msg)
        character(len=*), intent(in) :: semantic_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result
        type(semantic_context_t) :: sem_ctx

        error_msg = ""

        validation_result = validate_input_path(semantic_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Semantic JSON path validation failed: " // validation_result%get_message()
            return
        end if

        compiler_arena = create_compiler_arena()

        if (index(semantic_json_file, '.json') > 0) then
            call json_read_semantic_from_file(semantic_json_file, compiler_arena%ast, prog_index, sem_ctx)
            if (prog_index == 0) then
                error_msg = "Failed to load semantic AST from JSON file"
                call destroy_compiler_arena(compiler_arena)
                return
            end if
        else
            error_msg = "Semantic file must be a JSON file for from_semantic_json compilation"
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call generate_fortran_code(compiler_arena%ast, prog_index, code)
        call write_compiled_output(options, code, error_msg)
        call destroy_compiler_arena(compiler_arena)
    end subroutine compile_from_semantic_json

end module frontend_json
