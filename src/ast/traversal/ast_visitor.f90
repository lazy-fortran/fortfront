module ast_visitor
    use ast_core
    implicit none
    private

    ! Abstract visitor interface for AST traversal
    type, abstract :: ast_visitor_t
    contains
        procedure(visit_program_interface), deferred :: visit_program
        procedure(visit_assignment_interface), deferred :: visit_assignment
        procedure(visit_binary_op_interface), deferred :: visit_binary_op
        procedure(visit_function_def_interface), deferred :: visit_function_def
        procedure(visit_subroutine_def_interface), deferred :: visit_subroutine_def
       procedure(visit_call_or_subscript_interface), deferred :: visit_call_or_subscript
        procedure(visit_subroutine_call_interface), deferred :: visit_subroutine_call
        procedure(visit_identifier_interface), deferred :: visit_identifier
        procedure(visit_literal_interface), deferred :: visit_literal
        procedure(visit_declaration_interface), deferred :: visit_declaration
        procedure(visit_print_statement_interface), deferred :: visit_print_statement
        procedure(visit_if_interface), deferred :: visit_if
        procedure(visit_do_loop_interface), deferred :: visit_do_loop
        procedure(visit_do_while_interface), deferred :: visit_do_while
        procedure(visit_select_case_interface), deferred :: visit_select_case
        procedure(visit_derived_type_interface), deferred :: visit_derived_type
        procedure(visit_interface_block_interface), deferred :: visit_interface_block
        procedure(visit_module_interface), deferred :: visit_module
        procedure(visit_use_statement_interface), deferred :: visit_use_statement
       procedure(visit_include_statement_interface), deferred :: visit_include_statement
    end type ast_visitor_t

    ! Concrete visitor implementation for debugging/printing
    type, extends(ast_visitor_t) :: debug_visitor_t
        integer :: indent_level = 0
        character(len=:), allocatable :: output
    contains
        procedure :: visit_program => debug_visit_program
        procedure :: visit_assignment => debug_visit_assignment
        procedure :: visit_binary_op => debug_visit_binary_op
        procedure :: visit_function_def => debug_visit_function_def
        procedure :: visit_subroutine_def => debug_visit_subroutine_def
        procedure :: visit_subroutine_call => debug_visit_subroutine_call
        procedure :: visit_identifier => debug_visit_identifier
        procedure :: visit_literal => debug_visit_literal
        procedure :: visit_declaration => debug_visit_declaration
        procedure :: visit_print_statement => debug_visit_print_statement
        procedure :: visit_if => debug_visit_if
        procedure :: visit_do_loop => debug_visit_do_loop
        procedure :: visit_do_while => debug_visit_do_while
        procedure :: visit_select_case => debug_visit_select_case
        procedure :: visit_derived_type => debug_visit_derived_type
        procedure :: visit_interface_block => debug_visit_interface_block
        procedure :: visit_module => debug_visit_module
        procedure :: visit_use_statement => debug_visit_use_statement
        procedure :: visit_include_statement => debug_visit_include_statement
        procedure :: visit_call_or_subscript => debug_visit_call_or_subscript
    end type debug_visitor_t

    ! Abstract interfaces for visitor methods
    abstract interface
        subroutine visit_program_interface(this, node)
            import :: ast_visitor_t, program_node
            class(ast_visitor_t), intent(inout) :: this
            class(program_node), intent(in) :: node
        end subroutine visit_program_interface

        subroutine visit_assignment_interface(this, node)
            import :: ast_visitor_t, assignment_node
            class(ast_visitor_t), intent(inout) :: this
            class(assignment_node), intent(in) :: node
        end subroutine visit_assignment_interface

        subroutine visit_binary_op_interface(this, node)
            import :: ast_visitor_t, binary_op_node
            class(ast_visitor_t), intent(inout) :: this
            class(binary_op_node), intent(in) :: node
        end subroutine visit_binary_op_interface

        subroutine visit_function_def_interface(this, node)
            import :: ast_visitor_t, function_def_node
            class(ast_visitor_t), intent(inout) :: this
            class(function_def_node), intent(in) :: node
        end subroutine visit_function_def_interface

        subroutine visit_subroutine_def_interface(this, node)
            import :: ast_visitor_t, subroutine_def_node
            class(ast_visitor_t), intent(inout) :: this
            class(subroutine_def_node), intent(in) :: node
        end subroutine visit_subroutine_def_interface

        subroutine visit_call_or_subscript_interface(this, node)
            import :: ast_visitor_t, call_or_subscript_node
            class(ast_visitor_t), intent(inout) :: this
            class(call_or_subscript_node), intent(in) :: node
        end subroutine visit_call_or_subscript_interface

        subroutine visit_subroutine_call_interface(this, node)
            import :: ast_visitor_t, subroutine_call_node
            class(ast_visitor_t), intent(inout) :: this
            class(subroutine_call_node), intent(in) :: node
        end subroutine visit_subroutine_call_interface

        subroutine visit_identifier_interface(this, node)
            import :: ast_visitor_t, identifier_node
            class(ast_visitor_t), intent(inout) :: this
            class(identifier_node), intent(in) :: node
        end subroutine visit_identifier_interface

        subroutine visit_literal_interface(this, node)
            import :: ast_visitor_t, literal_node
            class(ast_visitor_t), intent(inout) :: this
            class(literal_node), intent(in) :: node
        end subroutine visit_literal_interface

        subroutine visit_declaration_interface(this, node)
            import :: ast_visitor_t, declaration_node
            class(ast_visitor_t), intent(inout) :: this
            class(declaration_node), intent(in) :: node
        end subroutine visit_declaration_interface

        subroutine visit_print_statement_interface(this, node)
            import :: ast_visitor_t, print_statement_node
            class(ast_visitor_t), intent(inout) :: this
            class(print_statement_node), intent(in) :: node
        end subroutine visit_print_statement_interface

        subroutine visit_if_interface(this, node)
            import :: ast_visitor_t, if_node
            class(ast_visitor_t), intent(inout) :: this
            class(if_node), intent(in) :: node
        end subroutine visit_if_interface

        subroutine visit_do_loop_interface(this, node)
            import :: ast_visitor_t, do_loop_node
            class(ast_visitor_t), intent(inout) :: this
            class(do_loop_node), intent(in) :: node
        end subroutine visit_do_loop_interface

        subroutine visit_do_while_interface(this, node)
            import :: ast_visitor_t, do_while_node
            class(ast_visitor_t), intent(inout) :: this
            class(do_while_node), intent(in) :: node
        end subroutine visit_do_while_interface

        subroutine visit_select_case_interface(this, node)
            import :: ast_visitor_t, select_case_node
            class(ast_visitor_t), intent(inout) :: this
            class(select_case_node), intent(in) :: node
        end subroutine visit_select_case_interface

        subroutine visit_derived_type_interface(this, node)
            import :: ast_visitor_t, derived_type_node
            class(ast_visitor_t), intent(inout) :: this
            class(derived_type_node), intent(in) :: node
        end subroutine visit_derived_type_interface

        subroutine visit_interface_block_interface(this, node)
            import :: ast_visitor_t, interface_block_node
            class(ast_visitor_t), intent(inout) :: this
            class(interface_block_node), intent(in) :: node
        end subroutine visit_interface_block_interface

        subroutine visit_module_interface(this, node)
            import :: ast_visitor_t, module_node
            class(ast_visitor_t), intent(inout) :: this
            class(module_node), intent(in) :: node
        end subroutine visit_module_interface

        subroutine visit_use_statement_interface(this, node)
            import :: ast_visitor_t, use_statement_node
            class(ast_visitor_t), intent(inout) :: this
            class(use_statement_node), intent(in) :: node
        end subroutine visit_use_statement_interface

        subroutine visit_include_statement_interface(this, node)
            import :: ast_visitor_t, include_statement_node
            class(ast_visitor_t), intent(inout) :: this
            class(include_statement_node), intent(in) :: node
        end subroutine visit_include_statement_interface
    end interface

    ! Public interface
    public :: ast_visitor_t, debug_visitor_t
    public :: program_accept, assignment_accept, binary_op_accept
    public :: function_def_accept, subroutine_def_accept
    public :: call_or_subscript_accept, subroutine_call_accept, &
              identifier_accept, literal_accept
    public :: use_statement_accept, include_statement_accept, print_statement_accept
    public :: declaration_accept, do_loop_accept, do_while_accept, if_accept
public :: select_case_accept, derived_type_accept, interface_block_accept, module_accept

contains

    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_program(this)
        end select
    end subroutine program_accept

    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_assignment(this)
        end select
    end subroutine assignment_accept

    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_binary_op(this)
        end select
    end subroutine binary_op_accept

    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_function_def(this)
        end select
    end subroutine function_def_accept

    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_subroutine_def(this)
        end select
    end subroutine subroutine_def_accept

    subroutine subroutine_call_accept(this, visitor)
        class(subroutine_call_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_subroutine_call(this)
        end select
    end subroutine subroutine_call_accept

    subroutine call_or_subscript_accept(this, visitor)
        class(call_or_subscript_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_call_or_subscript(this)
        end select
    end subroutine call_or_subscript_accept

    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_identifier(this)
        end select
    end subroutine identifier_accept

    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_literal(this)
        end select
    end subroutine literal_accept

    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_use_statement(this)
        end select
    end subroutine use_statement_accept

    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_include_statement(this)
        end select
    end subroutine include_statement_accept

    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_print_statement(this)
        end select
    end subroutine print_statement_accept

    subroutine declaration_accept(this, visitor)
        class(declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_declaration(this)
        end select
    end subroutine declaration_accept

    subroutine do_loop_accept(this, visitor)
        class(do_loop_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_do_loop(this)
        end select
    end subroutine do_loop_accept

    subroutine do_while_accept(this, visitor)
        class(do_while_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_do_while(this)
        end select
    end subroutine do_while_accept

    subroutine if_accept(this, visitor)
        class(if_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_if(this)
        end select
    end subroutine if_accept

    subroutine select_case_accept(this, visitor)
        class(select_case_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_select_case(this)
        end select
    end subroutine select_case_accept

    subroutine derived_type_accept(this, visitor)
        class(derived_type_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_derived_type(this)
        end select
    end subroutine derived_type_accept

    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_interface_block(this)
        end select
    end subroutine interface_block_accept

    subroutine module_accept(this, visitor)
        class(module_node), intent(in) :: this
        class(*), intent(inout) :: visitor

        select type (vis => visitor)
        class is (ast_visitor_t)
            call vis%visit_module(this)
        end select
    end subroutine module_accept

    ! Debug visitor implementations
    subroutine debug_visit_program(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(program_node), intent(in) :: node

        call append_debug(this, "program: "//node%name)
        this%indent_level = this%indent_level + 1
       call append_debug(this, "body_indices: "//int_array_to_string(node%body_indices))
        this%indent_level = this%indent_level - 1
    end subroutine debug_visit_program

    subroutine debug_visit_assignment(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node

     call append_debug(this, "assignment: target="//int_to_string(node%target_index)// &
                          " value="//int_to_string(node%value_index))
    end subroutine debug_visit_assignment

    subroutine debug_visit_binary_op(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node

        call append_debug(this, "binary_op: "//node%operator//" left="// &
                          int_to_string(node%left_index)//" right="// &
                          int_to_string(node%right_index))
    end subroutine debug_visit_binary_op

    subroutine debug_visit_function_def(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node

        call append_debug(this, "function_def: "//node%name)
        this%indent_level = this%indent_level + 1
        if (allocated(node%return_type)) then
            call append_debug(this, "return_type: "//node%return_type)
        end if
        if (allocated(node%param_indices)) then
     call append_debug(this, "param_indices: "//int_array_to_string(node%param_indices))
        end if
        this%indent_level = this%indent_level - 1
    end subroutine debug_visit_function_def

    subroutine debug_visit_subroutine_def(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node

        call append_debug(this, "subroutine_def: "//node%name)
        this%indent_level = this%indent_level + 1
        if (allocated(node%param_indices)) then
     call append_debug(this, "param_indices: "//int_array_to_string(node%param_indices))
        end if
        this%indent_level = this%indent_level - 1
    end subroutine debug_visit_subroutine_def

    subroutine debug_visit_subroutine_call(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node

        call append_debug(this, "subroutine_call: "//node%name)
        if (allocated(node%arg_indices)) then
         call append_debug(this, "arg_indices: "//int_array_to_string(node%arg_indices))
        end if
    end subroutine debug_visit_subroutine_call

    subroutine debug_visit_identifier(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node

        call append_debug(this, "identifier: "//node%name)
    end subroutine debug_visit_identifier

    subroutine debug_visit_literal(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(literal_node), intent(in) :: node

        call append_debug(this, "literal: "//node%value//" kind="// &
            int_to_string(node%literal_kind))
    end subroutine debug_visit_literal

    subroutine debug_visit_declaration(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node

        call append_debug(this, "declaration: "//node%type_name//" "//node%var_name)
    end subroutine debug_visit_declaration

    subroutine debug_visit_print_statement(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node

        call append_debug(this, "print_statement: "//node%format_spec)
        if (allocated(node%expression_indices)) then
         call append_debug(this, "expression_indices: "// &
            int_array_to_string(node%expression_indices))
        end if
    end subroutine debug_visit_print_statement

    subroutine debug_visit_if(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(if_node), intent(in) :: node

call append_debug(this, "if_statement: condition="//int_to_string(node%condition_index))
    end subroutine debug_visit_if

    subroutine debug_visit_do_loop(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node

        call append_debug(this, "do_loop: "//node%var_name//" start="// &
                          int_to_string(node%start_expr_index)//" end="// &
                          int_to_string(node%end_expr_index))
    end subroutine debug_visit_do_loop

    subroutine debug_visit_do_while(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node

    call append_debug(this, "do_while: condition="//int_to_string(node%condition_index))
    end subroutine debug_visit_do_while

    subroutine debug_visit_select_case(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node

        call append_debug(this, "select_case")
    end subroutine debug_visit_select_case

    subroutine debug_visit_derived_type(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node

        call append_debug(this, "derived_type: "//node%name)
    end subroutine debug_visit_derived_type

    subroutine debug_visit_interface_block(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node

        call append_debug(this, "interface_block")
    end subroutine debug_visit_interface_block

    subroutine debug_visit_module(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(module_node), intent(in) :: node

        call append_debug(this, "module: "//node%name)
    end subroutine debug_visit_module

    subroutine debug_visit_use_statement(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node

        call append_debug(this, "use_statement: "//node%module_name)
    end subroutine debug_visit_use_statement

    subroutine debug_visit_include_statement(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node

        call append_debug(this, "include_statement: "//node%filename)
    end subroutine debug_visit_include_statement

    subroutine debug_visit_call_or_subscript(this, node)
        class(debug_visitor_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node

        call append_debug(this, "call_or_subscript")
    end subroutine debug_visit_call_or_subscript

    ! Helper functions
    subroutine append_debug(this, text)
        class(debug_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: indent
        integer :: i

        allocate (character(len=this%indent_level*2) :: indent)
        do i = 1, this%indent_level*2
            indent(i:i) = " "
        end do

        if (allocated(this%output)) then
            this%output = this%output//new_line('a')//indent//text
        else
            this%output = indent//text
        end if
    end subroutine append_debug

    function int_to_string(val) result(str)
        integer, intent(in) :: val
        character(len=20) :: str
        write (str, '(I0)') val
    end function int_to_string

    function int_array_to_string(arr) result(str)
        integer, intent(in) :: arr(:)
        character(len=:), allocatable :: str
        integer :: i
        character(len=20) :: temp

        if (size(arr) == 0) then
            str = "[]"
            return
        end if

        str = "["
        do i = 1, size(arr)
            write (temp, '(I0)') arr(i)
            if (i > 1) str = str//","
            str = str//trim(temp)
        end do
        str = str//"]"
    end function int_array_to_string

end module ast_visitor
