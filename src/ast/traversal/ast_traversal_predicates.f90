module ast_traversal_predicates
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_core, only: assignment_node, binary_op_node, &
                              call_or_subscript_node, identifier_node, &
                              literal_node, program_node
    use ast_nodes_data, only: declaration_node, derived_type_node, module_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_misc, only: interface_block_node, use_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    implicit none
    private

    public :: is_program_node, is_assignment_node, is_binary_op_node
    public :: is_function_def_node, is_subroutine_def_node
    public :: is_identifier_node, is_literal_node, is_declaration_node
    public :: is_if_node, is_do_loop_node, is_do_while_node
    public :: is_call_or_subscript_node, is_subroutine_call_node
    public :: is_print_statement_node, is_use_statement_node
    public :: is_select_case_node, is_derived_type_node
    public :: is_module_node, is_interface_block_node

contains

    function is_program_node(arena, index) result(is_program)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_program

        is_program = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (program_node)
            is_program = .true.
        end select
    end function is_program_node

    function is_assignment_node(arena, index) result(is_assignment)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_assignment

        is_assignment = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (assignment_node)
            is_assignment = .true.
        end select
    end function is_assignment_node

    function is_binary_op_node(arena, index) result(is_binary_op)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_binary_op

        is_binary_op = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (binary_op_node)
            is_binary_op = .true.
        end select
    end function is_binary_op_node

    function is_function_def_node(arena, index) result(is_function_def)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_function_def

        is_function_def = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (function_def_node)
            is_function_def = .true.
        end select
    end function is_function_def_node

    function is_subroutine_def_node(arena, index) result(is_subroutine_def)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_subroutine_def

        is_subroutine_def = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (subroutine_def_node)
            is_subroutine_def = .true.
        end select
    end function is_subroutine_def_node

    function is_identifier_node(arena, index) result(is_identifier)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_identifier

        is_identifier = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (identifier_node)
            is_identifier = .true.
        end select
    end function is_identifier_node

    function is_literal_node(arena, index) result(is_literal)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_literal

        is_literal = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (literal_node)
            is_literal = .true.
        end select
    end function is_literal_node

    function is_declaration_node(arena, index) result(is_declaration)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_declaration

        is_declaration = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (declaration_node)
            is_declaration = .true.
        end select
    end function is_declaration_node

    function is_if_node(arena, index) result(is_if)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_if

        is_if = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (if_node)
            is_if = .true.
        end select
    end function is_if_node

    function is_do_loop_node(arena, index) result(is_do_loop)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_do_loop

        is_do_loop = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (do_loop_node)
            is_do_loop = .true.
        end select
    end function is_do_loop_node

    function is_do_while_node(arena, index) result(is_do_while)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_do_while

        is_do_while = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (do_while_node)
            is_do_while = .true.
        end select
    end function is_do_while_node

    function is_call_or_subscript_node(arena, index) result(is_call_or_subscript)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_call_or_subscript

        is_call_or_subscript = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (call_or_subscript_node)
            is_call_or_subscript = .true.
        end select
    end function is_call_or_subscript_node

    function is_subroutine_call_node(arena, index) result(is_subroutine_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_subroutine_call

        is_subroutine_call = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (subroutine_call_node)
            is_subroutine_call = .true.
        end select
    end function is_subroutine_call_node

    function is_print_statement_node(arena, index) result(is_print_statement)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_print_statement

        is_print_statement = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (print_statement_node)
            is_print_statement = .true.
        end select
    end function is_print_statement_node

    function is_use_statement_node(arena, index) result(is_use_statement)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_use_statement

        is_use_statement = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (use_statement_node)
            is_use_statement = .true.
        end select
    end function is_use_statement_node

    function is_select_case_node(arena, index) result(is_select_case)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_select_case

        is_select_case = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (select_case_node)
            is_select_case = .true.
        end select
    end function is_select_case_node

    function is_derived_type_node(arena, index) result(is_derived_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_derived_type

        is_derived_type = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (derived_type_node)
            is_derived_type = .true.
        end select
    end function is_derived_type_node

    function is_module_node(arena, index) result(is_module)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_module

        is_module = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (module_node)
            is_module = .true.
        end select
    end function is_module_node

    function is_interface_block_node(arena, index) result(is_interface_block)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_interface_block

        is_interface_block = .false.
        if (.not. arena%has_node_at(index)) return

        select type (n => arena%entries(index)%node)
        type is (interface_block_node)
            is_interface_block = .true.
        end select
    end function is_interface_block_node

end module ast_traversal_predicates
