module parser_procedure_definitions_module
    use parser_state_module, only: parser_state_t
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_function_def, push_subroutine_def
    use parser_procedure_signatures_module, only: &
        parse_function_prefix_keywords, parse_function_signature, &
        parse_function_result_clause, parse_parameter_list, &
        merge_parameter_attributes_if_needed, ensure_recursive_prefix, &
        parse_subroutine_header, parse_bind_c_clause
    use parser_procedure_definition_bodies_module, only: parse_procedure_body, &
                                                         parse_interface_body
    use parser_interface_blocks_module, only: parse_interface_block, &
                                              set_interface_procedure_parser
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_NEWLINE, TK_WHITESPACE
    implicit none
    private

    public :: parse_function_definition
    public :: parse_subroutine_definition
    public :: parse_interface_block
    public :: init_interface_procedure_parser

    logical :: parser_initialized = .false.

contains

    subroutine init_interface_procedure_parser()
        if (.not. parser_initialized) then
            call set_interface_procedure_parser(parse_interface_procedure_impl)
            parser_initialized = .true.
        end if
    end subroutine init_interface_procedure_parser

    function parse_interface_procedure_impl(parser, arena, prefix_buffer) &
        result(proc_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: proc_index

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        proc_index = 0
        token = parser%peek()

        if (token%kind == TK_KEYWORD) then
            lowered_text = to_lower(token%text)
            if (trim(lowered_text) == "subroutine") then
                proc_index = parse_interface_subroutine(parser, arena, prefix_buffer)
            else if (trim(lowered_text) == "function") then
                proc_index = parse_interface_function(parser, arena, prefix_buffer)
            end if
        end if
    end function parse_interface_procedure_impl

    function parse_interface_subroutine(parser, arena, prefix_buffer) &
        result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: sub_index

        character(len=:), allocatable :: subroutine_name, bind_c_clause
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        logical :: has_recursive_keyword

        has_recursive_keyword = .false.

        call parse_function_prefix_keywords(parser, prefix_buffer, &
                                            prefix_keywords=prefix_keywords, &
                                            has_recursive_keyword= &
                                            has_recursive_keyword)
        call parse_subroutine_header(parser, subroutine_name, line, column)
        call parse_parameter_list(parser, arena, param_indices)
        call parse_bind_c_clause(parser, bind_c_clause)
        call parse_interface_body(parser, arena, subroutine_name, "subroutine", &
                                  body_indices)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)

        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, &
                                        body_indices, line, column, &
                                        is_recursive=has_recursive_keyword, &
                                        prefix_keywords=prefix_keywords, &
                                        bind_c_clause=bind_c_clause)
    end function parse_interface_subroutine

    function parse_interface_function(parser, arena, prefix_buffer) &
        result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: func_index

        character(len=:), allocatable :: function_name, return_type_str, &
                                         result_variable_name, &
                                         return_type_from_prefix, bind_c_clause
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        logical :: has_recursive_keyword, is_valid
        character(len=16), allocatable :: prefix_keywords(:)

        call parse_function_prefix_keywords(parser, prefix_buffer, &
                                            prefix_keywords=prefix_keywords, &
                                            has_recursive_keyword= &
                                            has_recursive_keyword, &
                                            return_type_from_prefix= &
                                            return_type_from_prefix)

        call parse_function_signature(parser, return_type_str, function_name, &
                                      line, column, is_valid, &
                                      return_type_from_prefix=return_type_from_prefix)
        if (.not. is_valid) then
            func_index = 0
            return
        end if

        call parse_parameter_list(parser, arena, param_indices)
        call parse_function_result_clause(parser, result_variable_name)
        call parse_bind_c_clause(parser, bind_c_clause)
        if (len_trim(result_variable_name) == 0) then
            call parse_function_result_clause(parser, result_variable_name)
        end if
        call parse_interface_body(parser, arena, function_name, "function", &
                                  body_indices)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)

        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, &
                                       result_variable=result_variable_name, &
                                       is_recursive=has_recursive_keyword, &
                                       prefix_keywords=prefix_keywords, &
                                       bind_c_clause=bind_c_clause)
    end function parse_interface_function

    function parse_function_definition(parser, arena, prefix_buffer, prefix_list) &
        result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        integer :: func_index

        character(len=:), allocatable :: function_name, return_type_str, &
                                         result_variable_name, &
                                         return_type_from_prefix, &
                                         bind_c_clause
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        logical :: has_recursive_keyword, is_valid
        logical :: infer_recursive_from_body
        character(len=16), allocatable :: prefix_keywords(:)

        infer_recursive_from_body = .false.

        call parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
                                            prefix_keywords, has_recursive_keyword, &
                                            return_type_from_prefix)

        call parse_function_signature(parser, return_type_str, function_name, &
                                      line, column, is_valid, &
                                      return_type_from_prefix)
        if (.not. is_valid) then
            func_index = 0
            return
        end if

        call parse_parameter_list(parser, arena, param_indices)
        call parse_function_result_clause(parser, result_variable_name)
        call parse_bind_c_clause(parser, bind_c_clause)
        if (len_trim(result_variable_name) == 0) then
            call parse_function_result_clause(parser, result_variable_name)
        end if
        call parse_procedure_body(parser, arena, function_name, "function", &
                                  body_indices, infer_recursive_from_body, &
                                  parse_function_proc=parse_function_definition, &
                                  parse_subroutine_proc=parse_subroutine_definition)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)
        call ensure_recursive_prefix(has_recursive_keyword, &
                                     infer_recursive_from_body, prefix_keywords)

        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, &
                                       result_variable=result_variable_name, &
                                       is_recursive=has_recursive_keyword, &
                                       prefix_keywords=prefix_keywords, &
                                       bind_c_clause=bind_c_clause)
    end function parse_function_definition

    function parse_subroutine_definition(parser, arena, prefix_buffer, prefix_list) &
        result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        integer :: sub_index

        character(len=:), allocatable :: subroutine_name, bind_c_clause
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        logical :: has_recursive_keyword
        logical :: infer_recursive_from_body

        has_recursive_keyword = .false.
        infer_recursive_from_body = .false.

        call parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
                                            prefix_keywords=prefix_keywords, &
                                            has_recursive_keyword=has_recursive_keyword)
        call parse_subroutine_header(parser, subroutine_name, line, column)
        call parse_parameter_list(parser, arena, param_indices)
        call parse_bind_c_clause(parser, bind_c_clause)
        call parse_procedure_body(parser, arena, subroutine_name, "subroutine", &
                                  body_indices, infer_recursive_from_body, &
                                  parse_function_proc=parse_function_definition, &
                                  parse_subroutine_proc=parse_subroutine_definition)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)
        call ensure_recursive_prefix(has_recursive_keyword, &
                                     infer_recursive_from_body, prefix_keywords)

        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, &
                                        body_indices, line, column, &
                                        is_recursive=has_recursive_keyword, &
                                        prefix_keywords=prefix_keywords, &
                                        bind_c_clause=bind_c_clause)
    end function parse_subroutine_definition

end module parser_procedure_definitions_module
