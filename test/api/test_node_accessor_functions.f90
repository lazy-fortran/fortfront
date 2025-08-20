program test_node_accessor_functions
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    integer :: prog_index, assign_index, id_index, lit_index
    character(len=:), allocatable :: prog_name, id_name, lit_value, lit_type
    integer, allocatable :: body_indices(:)
    integer :: target_idx, value_idx
    character(len=:), allocatable :: operator
    logical :: found
    
    print *, "=== Node Accessor Functions Test ==="
    
    ! Create arena
    arena = create_ast_arena()
    
    ! Create test AST: program with assignment
    call create_test_ast(arena, prog_index, assign_index, id_index, lit_index)
    
    ! Test program node accessor
    print *, "Testing get_program_info..."
    found = get_program_info(arena, prog_index, prog_name, body_indices)
    if (found) then
        print *, "  ✓ Program name:", prog_name
        print *, "  ✓ Body indices count:", size(body_indices)
        if (size(body_indices) > 0) then
            print *, "  ✓ First body index:", body_indices(1)
        end if
    else
        print *, "  ✗ Failed to get program info"
    end if
    
    ! Test assignment node accessor
    print *, "Testing get_assignment_indices..."
    found = get_assignment_indices(arena, assign_index, target_idx, value_idx, operator)
    if (found) then
        print *, "  ✓ Assignment operator:", operator
        print *, "  ✓ Target index:", target_idx
        print *, "  ✓ Value index:", value_idx
    else
        print *, "  ✗ Failed to get assignment info"
    end if
    
    ! Test identifier node accessor
    print *, "Testing get_identifier_name..."
    found = get_identifier_name(arena, id_index, id_name)
    if (found) then
        print *, "  ✓ Identifier name:", id_name
    else
        print *, "  ✗ Failed to get identifier name"
    end if
    
    ! Test literal node accessor
    print *, "Testing get_literal_value..."
    found = get_literal_value(arena, lit_index, lit_value, lit_type)
    if (found) then
        print *, "  ✓ Literal value:", lit_value
        print *, "  ✓ Literal type:", lit_type
    else
        print *, "  ✗ Failed to get literal value"
    end if
    
    ! Test binary operation and call nodes
    call test_advanced_nodes(arena)
    
    ! Test declaration nodes
    call test_declaration_nodes(arena)
    
    ! Test error handling
    print *, "Testing error handling..."
    found = get_identifier_name(arena, 999, id_name)
    if (.not. found) then
        print *, "  ✓ Correctly handled invalid node index"
    else
        print *, "  ✗ Should have failed on invalid index"
    end if
    
    ! Test wrong node type access
    found = get_binary_op_info(arena, id_index, target_idx, value_idx, operator)
    if (.not. found) then
        print *, "  ✓ Correctly rejected wrong node type"
    else
        print *, "  ✗ Should have failed on wrong node type"
    end if
    
    print *, "All accessor function tests completed!"
    
contains
    
    subroutine create_test_ast(arena, prog_idx, assign_idx, id_idx, lit_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_idx, assign_idx, id_idx, lit_idx
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: id
        type(literal_node) :: lit
        
        ! Create literal node: 42
        lit%value = "42"
        lit%literal_type = "integer"
        call arena%push(lit, "literal")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push literal node to arena"
            stop 1
        end if
        lit_idx = arena%size
        
        ! Create identifier node: x
        id%name = "x"
        call arena%push(id, "identifier")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push identifier node to arena"
            stop 1
        end if
        id_idx = arena%size
        
        ! Create assignment node: x = 42
        assign%target_index = id_idx
        assign%value_index = lit_idx
        assign%operator = "="
        call arena%push(assign, "assignment")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push assignment node to arena"
            stop 1
        end if
        assign_idx = arena%size
        
        ! Create program node
        prog%name = "test_program"
        allocate(prog%body_indices(1))
        prog%body_indices(1) = assign_idx
        call arena%push(prog, "program")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push program node to arena"
            stop 1
        end if
        prog_idx = arena%size
    end subroutine create_test_ast
    
    subroutine test_advanced_nodes(arena)
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node) :: binop
        type(call_or_subscript_node) :: call_node
        type(array_literal_node) :: array_node
        type(literal_node) :: lit1, lit2
        integer :: binop_idx, call_idx, array_idx
        integer :: left_idx, right_idx
        character(len=:), allocatable :: operator, name
        integer, allocatable :: arg_indices(:), element_indices(:)
        character(len=:), allocatable :: element_type
        logical :: found
        
        print *, "Testing advanced node accessors..."
        
        ! Create binary operation: 1 + 2
        lit1%value = "1"
        lit1%literal_type = "integer"
        call arena%push(lit1, "literal")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push lit1 node to arena"
            stop 1
        end if
        left_idx = arena%size
        
        lit2%value = "2" 
        lit2%literal_type = "integer"
        call arena%push(lit2, "literal")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push lit2 node to arena"
            stop 1
        end if
        right_idx = arena%size
        
        binop%left_index = left_idx
        binop%right_index = right_idx
        binop%operator = "+"
        call arena%push(binop, "binary_op")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push binary_op node to arena"
            stop 1
        end if
        binop_idx = arena%size
        
        ! Test binary operation accessor
        found = get_binary_op_info(arena, binop_idx, left_idx, right_idx, operator)
        if (found) then
            print *, "  ✓ Binary op operator:", operator
            print *, "  ✓ Left operand index:", left_idx
            print *, "  ✓ Right operand index:", right_idx
        else
            print *, "  ✗ Failed to get binary operation info"
        end if
        
        ! Create function call: sin(x)
        call_node%name = "sin"
        allocate(call_node%arg_indices(1))
        call_node%arg_indices(1) = left_idx  ! Reuse lit1 as argument
        call arena%push(call_node, "call_or_subscript")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push call_or_subscript node to arena"
            stop 1
        end if
        call_idx = arena%size
        
        ! Test call accessor
        found = get_call_info(arena, call_idx, name, arg_indices)
        if (found) then
            print *, "  ✓ Function name:", name
            print *, "  ✓ Argument count:", size(arg_indices)
            if (size(arg_indices) > 0) then
                print *, "  ✓ First argument index:", arg_indices(1)
            end if
        else
            print *, "  ✗ Failed to get call info"
        end if
        
        ! Create array literal: [1, 2]
        array_node%element_type = "integer"
        allocate(array_node%element_indices(2))
        array_node%element_indices(1) = left_idx   ! lit1
        array_node%element_indices(2) = right_idx  ! lit2
        call arena%push(array_node, "array_literal")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push array_literal node to arena"
            stop 1
        end if
        array_idx = arena%size
        
        ! Test array literal accessor
        found = get_array_literal_info(arena, array_idx, element_indices, element_type)
        if (found) then
            print *, "  ✓ Array element type:", element_type
            print *, "  ✓ Element count:", size(element_indices)
            if (size(element_indices) >= 2) then
                print *, "  ✓ First element index:", element_indices(1)
                print *, "  ✓ Second element index:", element_indices(2)
            end if
        else
            print *, "  ✗ Failed to get array literal info"
        end if
    end subroutine test_advanced_nodes
    
    subroutine test_declaration_nodes(arena)
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_node) :: decl_node
        type(parameter_declaration_node) :: param_node
        integer :: decl_idx, param_idx
        character(len=:), allocatable :: var_names(:), type_spec, attributes(:)
        character(len=:), allocatable :: values(:)
        logical :: found
        
        print *, "Testing declaration node accessors..."
        
        ! Create declaration node: integer :: a, b
        decl_node%type_name = "integer"
        decl_node%is_multi_declaration = .true.
        allocate(character(len=256) :: decl_node%var_names(2))
        decl_node%var_names(1) = "a"
        decl_node%var_names(2) = "b"
        decl_node%has_intent = .true.
        decl_node%intent = "in"
        call arena%push(decl_node, "declaration")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push declaration node to arena"
            stop 1
        end if
        decl_idx = arena%size
        
        ! Test declaration accessor
        found = get_declaration_info(arena, decl_idx, var_names, type_spec, attributes)
        if (found) then
            print *, "  ✓ Declaration type:", type_spec
            print *, "  ✓ Variable count:", size(var_names)
            if (size(var_names) >= 2) then
                print *, "  ✓ First variable:", var_names(1)
                print *, "  ✓ Second variable:", var_names(2)
            end if
            print *, "  ✓ Attributes count:", size(attributes)
            if (size(attributes) > 0) then
                print *, "  ✓ First attribute:", attributes(1)
            end if
        else
            print *, "  ✗ Failed to get declaration info"
        end if
        
        ! Create parameter declaration: parameter :: pi = 3.14
        param_node%type_name = "real"
        param_node%name = "pi"
        call arena%push(param_node, "parameter_declaration")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push parameter_declaration node to arena"
            stop 1
        end if
        param_idx = arena%size
        
        ! Test parameter declaration accessor
        found = get_parameter_declaration_info(arena, param_idx, var_names, values, type_spec)
        if (found) then
            print *, "  ✓ Parameter type:", type_spec
            print *, "  ✓ Parameter count:", size(var_names)
            if (size(var_names) > 0) then
                print *, "  ✓ Parameter name:", var_names(1)
            end if
            if (size(values) > 0) then
                print *, "  ✓ Parameter value:", values(1)
            end if
        else
            print *, "  ✗ Failed to get parameter declaration info"
        end if
    end subroutine test_declaration_nodes
    
end program test_node_accessor_functions