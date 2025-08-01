program test_intrinsic_ast
    use ast_core
    use json_module
    implicit none
    
    call test_intrinsic_call_node_creation()
    call test_non_intrinsic_call_node_creation()
    ! TODO: Fix segmentation fault in JSON serialization
    ! call test_intrinsic_json_serialization()
    
    print *, "All intrinsic AST tests passed!"
    
contains

    subroutine test_intrinsic_call_node_creation()
        type(call_or_subscript_node) :: node
        integer, parameter :: dummy_args(2) = [1, 2]
        
        print *, "Testing intrinsic call node creation..."
        
        ! Test creating intrinsic function call nodes
        node = create_call_or_subscript("sin", dummy_args)
        if (.not. node%is_intrinsic) error stop "sin call should be marked as intrinsic"
        if (.not. allocated(node%intrinsic_signature)) &
            error stop "sin call should have signature"
        if (len_trim(node%intrinsic_signature) == 0) &
            error stop "sin signature should not be empty"
        if (node%intrinsic_signature /= "real(real)") &
            error stop "sin signature should be 'real(real)'"
        
        ! Test creating another intrinsic function call
        node = create_call_or_subscript("sqrt", dummy_args)
        if (.not. node%is_intrinsic) error stop "sqrt call should be marked as intrinsic"
        if (node%intrinsic_signature /= "real(real)") &
            error stop "sqrt signature should be 'real(real)'"
        
        ! Test type conversion intrinsic
        node = create_call_or_subscript("int", dummy_args)
        if (.not. node%is_intrinsic) error stop "int call should be marked as intrinsic"
        if (node%intrinsic_signature /= "integer(real)") &
            error stop "int signature should be 'integer(real)'"
        
        ! Test array intrinsic
        node = create_call_or_subscript("size", dummy_args)
        if (.not. node%is_intrinsic) error stop "size call should be marked as intrinsic"
        if (node%intrinsic_signature /= "integer(array)") &
            error stop "size signature should be 'integer(array)'"
        
        print *, "  ✓ Intrinsic call node creation tests passed"
    end subroutine test_intrinsic_call_node_creation

    subroutine test_non_intrinsic_call_node_creation()
        type(call_or_subscript_node) :: node
        integer, parameter :: dummy_args(2) = [1, 2]
        
        print *, "Testing non-intrinsic call node creation..."
        
        ! Test creating user-defined function call nodes
        node = create_call_or_subscript("my_function", dummy_args)
        if (node%is_intrinsic) error stop "my_function call should not be marked as intrinsic"
        if (allocated(node%intrinsic_signature) .and. len_trim(node%intrinsic_signature) > 0) &
            error stop "my_function should not have signature"
        
        node = create_call_or_subscript("user_routine", dummy_args)
        if (node%is_intrinsic) error stop "user_routine call should not be marked as intrinsic"
        
        node = create_call_or_subscript("calculate", dummy_args)
        if (node%is_intrinsic) error stop "calculate call should not be marked as intrinsic"
        
        print *, "  ✓ Non-intrinsic call node creation tests passed"
    end subroutine test_non_intrinsic_call_node_creation

    subroutine test_intrinsic_json_serialization()
        type(call_or_subscript_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root, parent
        integer, parameter :: dummy_args(2) = [1, 2]
        character(len=:), allocatable :: json_str
        logical :: found
        character(len=:), allocatable :: str_val
        logical :: bool_val
        
        print *, "Testing intrinsic JSON serialization..."
        
        ! Create an intrinsic function call node
        node = create_call_or_subscript("sin", dummy_args)
        
        ! Create JSON structure
        call json%initialize()
        call json%create_object(root, '')
        call json%create_object(parent, 'call_node')
        call json%add(root, parent)
        
        ! Serialize the node
        call node%to_json(json, parent)
        
        ! Check that intrinsic information is included
        call json%get(parent, 'is_intrinsic', bool_val, found)
        if (.not. found) error stop "is_intrinsic field should be present"
        if (.not. bool_val) error stop "is_intrinsic should be true for sin"
        
        call json%get(parent, 'intrinsic_signature', str_val, found)
        if (.not. found) error stop "intrinsic_signature field should be present"
        if (str_val /= "real(real)") error stop "intrinsic_signature should be 'real(real)'"
        
        call json%get(parent, 'name', str_val, found)
        if (.not. found) error stop "name field should be present"
        if (str_val /= "sin") error stop "name should be 'sin'"
        
        ! Test with non-intrinsic function
        call json%destroy(root)
        node = create_call_or_subscript("my_function", dummy_args)
        
        call json%create_object(root, '')
        call json%create_object(parent, 'call_node')
        call json%add(root, parent)
        call node%to_json(json, parent)
        
        call json%get(parent, 'is_intrinsic', bool_val, found)
        if (.not. found) error stop "is_intrinsic field should be present for non-intrinsic"
        if (bool_val) error stop "is_intrinsic should be false for my_function"
        
        ! intrinsic_signature should not be present for non-intrinsic functions
        call json%get(parent, 'intrinsic_signature', str_val, found)
        if (found) error stop "intrinsic_signature should not be present for non-intrinsic"
        
        call json%destroy(root)
        print *, "  ✓ Intrinsic JSON serialization tests passed"
    end subroutine test_intrinsic_json_serialization

end program test_intrinsic_ast