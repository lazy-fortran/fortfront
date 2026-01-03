module frontend_location_validation
    ! AST Location Validation Pass
    ! Verifies that parser and standardizer properly populate source locations
    ! Catches missing location data before it degrades diagnostics

    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_visitor, only: ast_visitor_t
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
                              identifier_node, literal_node, &
                              call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_data, only: declaration_node, derived_type_node, module_node, &
                              submodule_node
    use ast_nodes_misc, only: interface_block_node, use_statement_node, &
                              visibility_statement_node, &
                              include_statement_node
    use ast_nodes_generics, only: template_block_node, instantiate_statement_node
    use ast_introspection, only: visit_node_at
    implicit none
    private

    public :: validate_ast_locations
    public :: location_validation_visitor_t

    ! Visitor that validates location data
    type, extends(ast_visitor_t) :: location_validation_visitor_t
        integer :: violations = 0
        integer :: nodes_checked = 0
        logical :: report_violations = .true.
        ! Allow synthesized nodes to skip validation
        logical :: allow_synthesized = .true.
        ! Flag nodes that keep default positions (line=1, column=1)
        logical :: detect_default_positions = .false.
    contains
        procedure :: visit_program => validate_visit_program
        procedure :: visit_assignment => validate_visit_assignment
        procedure :: visit_binary_op => validate_visit_binary_op
        procedure :: visit_function_def => validate_visit_function_def
        procedure :: visit_subroutine_def => validate_visit_subroutine_def
        procedure :: visit_subroutine_call => validate_visit_subroutine_call
        procedure :: visit_identifier => validate_visit_identifier
        procedure :: visit_literal => validate_visit_literal
        procedure :: visit_declaration => validate_visit_declaration
        procedure :: visit_print_statement => validate_visit_print_statement
        procedure :: visit_if => validate_visit_if
        procedure :: visit_do_loop => validate_visit_do_loop
        procedure :: visit_do_while => validate_visit_do_while
        procedure :: visit_select_case => validate_visit_select_case
        procedure :: visit_derived_type => validate_visit_derived_type
        procedure :: visit_interface_block => validate_visit_interface_block
        procedure :: visit_module => validate_visit_module
        procedure :: visit_submodule => validate_visit_submodule
        procedure :: visit_use_statement => validate_visit_use_statement
        procedure :: visit_visibility_statement &
            => validate_visit_visibility_statement
        procedure :: visit_include_statement => validate_visit_include_statement
        procedure :: visit_call_or_subscript &
            => validate_visit_call_or_subscript
        procedure :: visit_template_block => validate_visit_template_block
        procedure :: visit_instantiate_statement &
            => validate_visit_instantiate_statement
        procedure, private :: check_location
        procedure, private :: is_synthesized_node
    end type location_validation_visitor_t

contains

    ! Main entry point: validate all nodes in arena
    subroutine validate_ast_locations(arena, strict_mode, violations_count)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(in), optional :: strict_mode
        integer, intent(out), optional :: violations_count
        type(location_validation_visitor_t) :: visitor
        integer :: i
        logical :: strict

        strict = .false.
        if (present(strict_mode)) strict = strict_mode

        visitor%violations = 0
        visitor%nodes_checked = 0
        visitor%allow_synthesized = .not. strict
        visitor%detect_default_positions = strict

        ! Walk all nodes in arena
        do i = 1, arena%size
            call visit_node_at(arena, i, visitor)
        end do

        if (present(violations_count)) then
            violations_count = visitor%violations
        end if

        ! Report summary if violations found
        if (visitor%violations > 0 .and. visitor%report_violations) then
            write (error_unit, '(A,I0,A,I0,A)') &
                "Location validation: ", visitor%violations, &
                " violations in ", visitor%nodes_checked, " nodes"
        end if
    end subroutine validate_ast_locations

    ! Check if a node's location is valid
    subroutine check_location(this, node, node_kind)
        class(location_validation_visitor_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in) :: node_kind
        logical :: is_valid

        this%nodes_checked = this%nodes_checked + 1

        ! Allow synthesized nodes to have default locations
        if (this%allow_synthesized .and. this%is_synthesized_node(node_kind)) then
            return
        end if

        ! Check for default/invalid location values (line/column must be > 0)
        is_valid = node%line > 0 .and. node%column > 0
        if (.not. is_valid) then
            this%violations = this%violations + 1
            if (this%report_violations) then
                write (error_unit, '(A,A,A,I0,A,I0)') &
                    "Missing/default location in ", node_kind, " at ", &
                    node%line, ":", node%column
            end if
            return
        end if

        if (this%detect_default_positions) then
            if (node%line == 1 .and. node%column == 1) then
                this%violations = this%violations + 1
                if (this%report_violations) then
                    write (error_unit, '(A,A,A,I0,A,I0)') &
                        "Missing/default location in ", node_kind, " at ", &
                        node%line, ":", node%column
                end if
            end if
        end if
    end subroutine check_location

    ! Check if a node type is typically synthesized (created by compiler)
    pure function is_synthesized_node(this, node_kind) result(is_synth)
        class(location_validation_visitor_t), intent(in) :: this
        character(len=*), intent(in) :: node_kind
        logical :: is_synth

        ! These node types are often synthesized by the compiler
        ! and legitimately have no source location
        select case (node_kind)
        case ("contains", "implicit_statement")
            is_synth = .true.
        case default
            is_synth = .false.
        end select
    end function is_synthesized_node

    ! Visitor implementations for all node types
    subroutine validate_visit_program(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(program_node), intent(in) :: node
        call this%check_location(node, "program")
    end subroutine validate_visit_program

    subroutine validate_visit_assignment(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
        call this%check_location(node, "assignment")
    end subroutine validate_visit_assignment

    subroutine validate_visit_binary_op(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node
        call this%check_location(node, "binary_op")
    end subroutine validate_visit_binary_op

    subroutine validate_visit_function_def(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node
        call this%check_location(node, "function_def")
    end subroutine validate_visit_function_def

    subroutine validate_visit_subroutine_def(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node
        call this%check_location(node, "subroutine_def")
    end subroutine validate_visit_subroutine_def

    subroutine validate_visit_subroutine_call(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node
        call this%check_location(node, "subroutine_call")
    end subroutine validate_visit_subroutine_call

    subroutine validate_visit_identifier(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
        call this%check_location(node, "identifier")
    end subroutine validate_visit_identifier

    subroutine validate_visit_literal(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(literal_node), intent(in) :: node
        call this%check_location(node, "literal")
    end subroutine validate_visit_literal

    subroutine validate_visit_template_block(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(template_block_node), intent(in) :: node
        call this%check_location(node, "template_block")
    end subroutine validate_visit_template_block

    subroutine validate_visit_instantiate_statement(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(instantiate_statement_node), intent(in) :: node
        call this%check_location(node, "instantiate_statement")
    end subroutine validate_visit_instantiate_statement

    subroutine validate_visit_declaration(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node
        call this%check_location(node, "declaration")
    end subroutine validate_visit_declaration

    subroutine validate_visit_print_statement(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node
        call this%check_location(node, "print_statement")
    end subroutine validate_visit_print_statement

    subroutine validate_visit_if(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(if_node), intent(in) :: node
        call this%check_location(node, "if")
    end subroutine validate_visit_if

    subroutine validate_visit_do_loop(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node
        call this%check_location(node, "do_loop")
    end subroutine validate_visit_do_loop

    subroutine validate_visit_do_while(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node
        call this%check_location(node, "do_while")
    end subroutine validate_visit_do_while

    subroutine validate_visit_select_case(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node
        call this%check_location(node, "select_case")
    end subroutine validate_visit_select_case

    subroutine validate_visit_derived_type(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node
        call this%check_location(node, "derived_type")
    end subroutine validate_visit_derived_type

    subroutine validate_visit_interface_block(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node
        call this%check_location(node, "interface_block")
    end subroutine validate_visit_interface_block

    subroutine validate_visit_module(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(module_node), intent(in) :: node
        call this%check_location(node, "module")
    end subroutine validate_visit_module

    subroutine validate_visit_submodule(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(submodule_node), intent(in) :: node
        call this%check_location(node, "submodule")
    end subroutine validate_visit_submodule

    subroutine validate_visit_use_statement(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node
        call this%check_location(node, "use_statement")
    end subroutine validate_visit_use_statement

    subroutine validate_visit_visibility_statement(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(visibility_statement_node), intent(in) :: node
        call this%check_location(node, "visibility_statement")
    end subroutine validate_visit_visibility_statement

    subroutine validate_visit_include_statement(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node
        call this%check_location(node, "include_statement")
    end subroutine validate_visit_include_statement

    subroutine validate_visit_call_or_subscript(this, node)
        class(location_validation_visitor_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node
        call this%check_location(node, "call_or_subscript")
    end subroutine validate_visit_call_or_subscript

end module frontend_location_validation
