module ast_visitor_base
    ! Abstract visitor base providing type-safe visitor pattern without class(*)
    use ast_base, only: ast_visitor_base_t
    implicit none
    private

    ! Abstract visitor interface defining all visit methods
    type, extends(ast_visitor_base_t), abstract, public :: ast_visitor_t
    contains
        ! Deferred procedures for all AST node types
        procedure(visit_program_interface), deferred :: visit_program
        procedure(visit_assignment_interface), deferred :: visit_assignment
        procedure(visit_binary_op_interface), deferred :: visit_binary_op
        procedure(visit_function_def_interface), deferred :: visit_function_def
        procedure(visit_subroutine_def_interface), deferred :: visit_subroutine_def
        procedure(visit_call_or_subscript_interface), deferred :: &
                                                    visit_call_or_subscript
        procedure(visit_subroutine_call_interface), deferred :: &
                                                    visit_subroutine_call
        procedure(visit_identifier_interface), deferred :: visit_identifier
        procedure(visit_literal_interface), deferred :: visit_literal
        procedure(visit_declaration_interface), deferred :: visit_declaration
        procedure(visit_print_statement_interface), deferred :: &
                                                    visit_print_statement
        procedure(visit_if_interface), deferred :: visit_if
        procedure(visit_do_loop_interface), deferred :: visit_do_loop
        procedure(visit_do_while_interface), deferred :: visit_do_while
        procedure(visit_select_case_interface), deferred :: visit_select_case
        procedure(visit_derived_type_interface), deferred :: visit_derived_type
        procedure(visit_interface_block_interface), deferred :: &
                                                    visit_interface_block
        procedure(visit_module_interface), deferred :: visit_module
        procedure(visit_use_statement_interface), deferred :: visit_use_statement
        procedure(visit_include_statement_interface), deferred :: &
                                                    visit_include_statement
    end type ast_visitor_t

    ! Abstract interfaces for visitor methods
    abstract interface
        subroutine visit_program_interface(this, node)
            import :: ast_visitor_t
            ! Forward declaration - will be fully defined when node types available
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with program_node
        end subroutine visit_program_interface

        subroutine visit_assignment_interface(this, node)
            import :: ast_visitor_t  
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with assignment_node
        end subroutine visit_assignment_interface

        subroutine visit_binary_op_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with binary_op_node
        end subroutine visit_binary_op_interface

        subroutine visit_function_def_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with function_def_node
        end subroutine visit_function_def_interface

        subroutine visit_subroutine_def_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with subroutine_def_node
        end subroutine visit_subroutine_def_interface

        subroutine visit_call_or_subscript_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with call_or_subscript_node
        end subroutine visit_call_or_subscript_interface

        subroutine visit_subroutine_call_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with subroutine_call_node
        end subroutine visit_subroutine_call_interface

        subroutine visit_identifier_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with identifier_node
        end subroutine visit_identifier_interface

        subroutine visit_literal_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with literal_node
        end subroutine visit_literal_interface

        subroutine visit_declaration_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with declaration_node
        end subroutine visit_declaration_interface

        subroutine visit_print_statement_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with print_statement_node
        end subroutine visit_print_statement_interface

        subroutine visit_if_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with if_node
        end subroutine visit_if_interface

        subroutine visit_do_loop_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with do_loop_node
        end subroutine visit_do_loop_interface

        subroutine visit_do_while_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with do_while_node
        end subroutine visit_do_while_interface

        subroutine visit_select_case_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with select_case_node
        end subroutine visit_select_case_interface

        subroutine visit_derived_type_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with derived_type_node
        end subroutine visit_derived_type_interface

        subroutine visit_interface_block_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with interface_block_node
        end subroutine visit_interface_block_interface

        subroutine visit_module_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with module_node
        end subroutine visit_module_interface

        subroutine visit_use_statement_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with use_statement_node
        end subroutine visit_use_statement_interface

        subroutine visit_include_statement_interface(this, node)
            import :: ast_visitor_t
            class(ast_visitor_t), intent(inout) :: this
            class(*), intent(in) :: node  ! Temporary - will replace with include_statement_node
        end subroutine visit_include_statement_interface
    end interface

    ! Public interface
    public :: ast_visitor_t

end module ast_visitor_base