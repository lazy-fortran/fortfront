program implicit_none_checker_example
    ! Example demonstrating how to use the AST visitor pattern to check for implicit none
    use fortfront
    use ast_visitor
    use ast_traversal
    implicit none
    
    ! Example visitor that checks for implicit none in programs
    type, extends(ast_visitor_t) :: implicit_none_checker_t
        logical :: found_implicit_none = .false.
        logical :: in_program = .false.
        logical :: in_module = .false.
        character(len=:), allocatable :: current_scope_name
    contains
        procedure :: visit_program => check_visit_program
        procedure :: visit_module => check_visit_module
        procedure :: visit_assignment => check_visit_assignment
        procedure :: visit_binary_op => check_visit_binary_op
        procedure :: visit_function_def => check_visit_function_def
        procedure :: visit_subroutine_def => check_visit_subroutine_def
        procedure :: visit_call_or_subscript => check_visit_call_or_subscript
        procedure :: visit_subroutine_call => check_visit_subroutine_call
        procedure :: visit_identifier => check_visit_identifier
        procedure :: visit_literal => check_visit_literal
        procedure :: visit_declaration => check_visit_declaration
        procedure :: visit_print_statement => check_visit_print_statement
        procedure :: visit_if => check_visit_if
        procedure :: visit_do_loop => check_visit_do_loop
        procedure :: visit_do_while => check_visit_do_while
        procedure :: visit_select_case => check_visit_select_case
        procedure :: visit_derived_type => check_visit_derived_type
        procedure :: visit_interface_block => check_visit_interface_block
        procedure :: visit_use_statement => check_visit_use_statement
        procedure :: visit_include_statement => check_visit_include_statement
    end type implicit_none_checker_t
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index
    character(len=:), allocatable :: error_msg
    type(implicit_none_checker_t) :: checker
    
    ! Example 1: Code with implicit none
    print *, "=== Example 1: Code WITH implicit none ==="
    source_code = &
        "program test" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "    x = 42" // new_line('A') // &
        "end program test"
    
    call analyze_code(source_code)
    
    ! Example 2: Code without implicit none
    print *, ""
    print *, "=== Example 2: Code WITHOUT implicit none ==="
    source_code = &
        "program test" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "    x = 42" // new_line('A') // &
        "end program test"
    
    call analyze_code(source_code)
    
    ! Example 3: Module with implicit none
    print *, ""
    print *, "=== Example 3: Module WITH implicit none ==="
    source_code = &
        "module mymod" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "    integer :: modvar" // new_line('A') // &
        "end module mymod"
    
    call analyze_code(source_code)
    
contains
    
    subroutine analyze_code(code)
        character(len=*), intent(in) :: code
        
        ! Lex the source
        call lex_source(code, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "Lexer error:", error_msg
            return
        end if
        
        ! Parse to AST
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            print *, "Parser error:", error_msg
            return
        end if
        
        ! Run the implicit none checker
        checker = implicit_none_checker_t()
        call traverse_ast_visitor(arena, prog_index, checker)
        
        ! Report results
        if (checker%found_implicit_none) then
            print *, "✓ PASS: Found 'implicit none' in scope:", trim(checker%current_scope_name)
        else
            print *, "✗ FAIL: Missing 'implicit none' in scope:", trim(checker%current_scope_name)
            print *, "  Consider adding 'implicit none' after program/module statement"
        end if
    end subroutine analyze_code
    
    ! Visitor method implementations
    subroutine check_visit_program(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(program_node), intent(in) :: node
        
        this%in_program = .true.
        this%current_scope_name = node%name
        this%found_implicit_none = .false.
        
        ! Check if program has implicit none
        ! In a real implementation, we would check the program's specification part
        ! For this example, we check if the program node has specifications
        if (allocated(node%specifications)) then
            if (index(node%specifications, "implicit none") > 0) then
                this%found_implicit_none = .true.
            end if
        end if
    end subroutine check_visit_program
    
    subroutine check_visit_module(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(module_node), intent(in) :: node
        
        this%in_module = .true.
        this%current_scope_name = node%name
        this%found_implicit_none = .false.
        
        ! In a real implementation, check module's specification part
        ! This is a simplified example
    end subroutine check_visit_module
    
    ! Empty implementations for other node types
    subroutine check_visit_assignment(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
    end subroutine check_visit_assignment
    
    subroutine check_visit_binary_op(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node
    end subroutine check_visit_binary_op
    
    subroutine check_visit_function_def(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node
    end subroutine check_visit_function_def
    
    subroutine check_visit_subroutine_def(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node
    end subroutine check_visit_subroutine_def
    
    subroutine check_visit_call_or_subscript(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node
    end subroutine check_visit_call_or_subscript
    
    subroutine check_visit_subroutine_call(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node
    end subroutine check_visit_subroutine_call
    
    subroutine check_visit_identifier(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
    end subroutine check_visit_identifier
    
    subroutine check_visit_literal(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(literal_node), intent(in) :: node
    end subroutine check_visit_literal
    
    subroutine check_visit_declaration(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node
        
        ! Check if this is an implicit none statement
        if (this%in_program .or. this%in_module) then
            if (node%type_name == "implicit" .and. node%var_name == "none") then
                this%found_implicit_none = .true.
            end if
        end if
    end subroutine check_visit_declaration
    
    subroutine check_visit_print_statement(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node
    end subroutine check_visit_print_statement
    
    subroutine check_visit_if(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(if_node), intent(in) :: node
    end subroutine check_visit_if
    
    subroutine check_visit_do_loop(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node
    end subroutine check_visit_do_loop
    
    subroutine check_visit_do_while(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node
    end subroutine check_visit_do_while
    
    subroutine check_visit_select_case(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node
    end subroutine check_visit_select_case
    
    subroutine check_visit_derived_type(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node
    end subroutine check_visit_derived_type
    
    subroutine check_visit_interface_block(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node
    end subroutine check_visit_interface_block
    
    subroutine check_visit_use_statement(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node
    end subroutine check_visit_use_statement
    
    subroutine check_visit_include_statement(this, node)
        class(implicit_none_checker_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node
    end subroutine check_visit_include_statement
    
end program implicit_none_checker_example