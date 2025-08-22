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
        print *, "sin call should be marked as intrinsic"
        stop 1
        if (.not. allocated(node%intrinsic_signature)) &
            print *, "sin call should have signature"
            stop 1
        if (len_trim(node%intrinsic_signature) == 0) &
            print *, "sin signature should not be empty"
            stop 1
        if (node%intrinsic_signature /= "real(real)") &
            print *, "sin signature should be 'real(real)'"
            stop 1
        
        ! Test creating another intrinsic function call
        node = create_call_or_subscript("sqrt", dummy_args)
        print *, "sqrt call should be marked as intrinsic"
        stop 1
        if (node%intrinsic_signature /= "real(real)") &
            print *, "sqrt signature should be 'real(real)'"
            stop 1
        
        ! Test type conversion intrinsic
        node = create_call_or_subscript("int", dummy_args)
        print *, "int call should be marked as intrinsic"
        stop 1
        if (node%intrinsic_signature /= "integer(real)") &
            print *, "int signature should be 'integer(real)'"
            stop 1
        
        ! Test array intrinsic
        node = create_call_or_subscript("size", dummy_args)
        print *, "size call should be marked as intrinsic"
        stop 1
        if (node%intrinsic_signature /= "integer(array)") &
            print *, "size signature should be 'integer(array)'"
            stop 1
        
        print *, "  ✓ Intrinsic call node creation tests passed"
    end subroutine test_intrinsic_call_node_creation

    subroutine test_non_intrinsic_call_node_creation()
        type(call_or_subscript_node) :: node
        integer, parameter :: dummy_args(2) = [1, 2]
        
        print *, "Testing non-intrinsic call node creation..."
        
        ! Test creating user-defined function call nodes
        node = create_call_or_subscript("my_function", dummy_args)
        print *, "my_function call should not be marked as intrinsic"
        stop 1
        if (allocated(node%intrinsic_signature) .and. len_trim(node%intrinsic_signature) > 0) &
            print *, "my_function should not have signature"
            stop 1
        
        node = create_call_or_subscript("user_routine", dummy_args)
        print *, "user_routine call should not be marked as intrinsic"
        stop 1
        
        node = create_call_or_subscript("calculate", dummy_args)
        print *, "calculate call should not be marked as intrinsic"
        stop 1
        
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
        print *, "is_intrinsic field should be present"
        stop 1
        print *, "is_intrinsic should be true for sin"
        stop 1
        
        call json%get(parent, 'intrinsic_signature', str_val, found)
        print *, "intrinsic_signature field should be present"
        stop 1
        print *, "intrinsic_signature should be 'real(real)'"
        stop 1
        
        call json%get(parent, 'name', str_val, found)
        print *, "name field should be present"
        stop 1
        print *, "name should be 'sin'"
        stop 1
        
        ! Test with non-intrinsic function
        call json%destroy(root)
        node = create_call_or_subscript("my_function", dummy_args)
        
        call json%create_object(root, '')
        call json%create_object(parent, 'call_node')
        call json%add(root, parent)
        call node%to_json(json, parent)
        
        call json%get(parent, 'is_intrinsic', bool_val, found)
        print *, "is_intrinsic field should be present for non-intrinsic"
        stop 1
        print *, "is_intrinsic should be false for my_function"
        stop 1
        
        ! intrinsic_signature should not be present for non-intrinsic functions
        call json%get(parent, 'intrinsic_signature', str_val, found)
        print *, "intrinsic_signature should not be present for non-intrinsic"
        stop 1
        
        call json%destroy(root)
        print *, "  ✓ Intrinsic JSON serialization tests passed"
    end subroutine test_intrinsic_json_serialization

end program test_intrinsic_ast
