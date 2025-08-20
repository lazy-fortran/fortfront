program test_self_assignment_safety
    ! Test that self-assignment doesn't cause memory corruption
    use dependency_graph
    use variable_collection
    implicit none

    logical :: all_tests_passed = .true.

    print *, "Testing self-assignment safety..."
    print *, "================================="
    
    call test_dependency_node_self_assignment()
    call test_dependency_graph_self_assignment()
    call test_variable_info_self_assignment()
    call test_variable_collection_self_assignment()
    
    if (all_tests_passed) then
        print *, ""
        print *, "SUCCESS: All self-assignment tests passed!"
    else
        print *, ""
        print *, "FAILURE: Some self-assignment tests failed"
        error stop 1
    end if

contains

    subroutine test_dependency_node_self_assignment()
        type(dependency_node_t) :: node
        logical :: test_passed = .true.
        
        print *, ""
        print *, "Test: dependency_node_t self-assignment safety"
        
        ! Initialize node
        node%name = "test_node"
        allocate(node%dependencies(2))
        node%dependencies(1) = "dep1"
        node%dependencies(2) = "dep2"
        node%visited = .true.
        node%in_progress = .false.
        
        ! Perform self-assignment
        node = node
        
        ! Verify node is still valid after self-assignment
        if (.not. allocated(node%dependencies)) then
            print *, "  FAIL: dependencies deallocated after self-assignment"
            test_passed = .false.
        else if (size(node%dependencies) /= 2) then
            print *, "  FAIL: dependencies size changed after self-assignment"
            test_passed = .false.
        else if (node%dependencies(1) /= "dep1" .or. node%dependencies(2) /= "dep2") then
            print *, "  FAIL: dependencies values corrupted after self-assignment"
            test_passed = .false.
        else if (node%name /= "test_node") then
            print *, "  FAIL: name corrupted after self-assignment"
            test_passed = .false.
        else
            print *, "  PASS: Self-assignment handled safely"
        end if
        
        if (test_passed) then
            print *, "  PASS: dependency_node_t self-assignment safe"
        else
            all_tests_passed = .false.
        end if
    end subroutine test_dependency_node_self_assignment

    subroutine test_dependency_graph_self_assignment()
        type(dependency_graph_t) :: graph
        character(len=32) :: deps(2)
        logical :: test_passed = .true.
        
        print *, ""
        print *, "Test: dependency_graph_t self-assignment safety"
        
        ! Initialize graph
        graph = create_dependency_graph()
        deps(1) = "base"
        deps(2) = "core"
        call graph%add_node("module1", deps)
        
        ! Perform self-assignment
        graph = graph
        
        ! Verify graph is still valid after self-assignment
        if (.not. allocated(graph%nodes)) then
            print *, "  FAIL: nodes deallocated after self-assignment"
            test_passed = .false.
        else if (graph%node_count /= 1) then
            print *, "  FAIL: node_count changed after self-assignment"
            test_passed = .false.
        else if (graph%nodes(1)%name /= "module1") then
            print *, "  FAIL: node data corrupted after self-assignment"
            test_passed = .false.
        else
            print *, "  PASS: Self-assignment handled safely"
        end if
        
        if (test_passed) then
            print *, "  PASS: dependency_graph_t self-assignment safe"
        else
            all_tests_passed = .false.
        end if
    end subroutine test_dependency_graph_self_assignment

    subroutine test_variable_info_self_assignment()
        type(variable_info_t) :: var
        logical :: test_passed = .true.
        
        print *, ""
        print *, "Test: variable_info_t self-assignment safety"
        
        ! Initialize var
        var%name = "test_var"
        var%type_name = "integer"
        var%is_declared = .true.
        var%usage_count = 5
        
        ! Perform self-assignment
        var = var
        
        ! Verify var is still valid after self-assignment
        if (.not. allocated(var%name)) then
            print *, "  FAIL: name deallocated after self-assignment"
            test_passed = .false.
        else if (var%name /= "test_var") then
            print *, "  FAIL: name corrupted after self-assignment"
            test_passed = .false.
        else if (var%type_name /= "integer") then
            print *, "  FAIL: type_name corrupted after self-assignment"
            test_passed = .false.
        else if (var%usage_count /= 5) then
            print *, "  FAIL: usage_count changed after self-assignment"
            test_passed = .false.
        else
            print *, "  PASS: Self-assignment handled safely"
        end if
        
        if (test_passed) then
            print *, "  PASS: variable_info_t self-assignment safe"
        else
            all_tests_passed = .false.
        end if
    end subroutine test_variable_info_self_assignment

    subroutine test_variable_collection_self_assignment()
        type(variable_collection_t) :: coll
        type(variable_info_t) :: var_info
        logical :: test_passed = .true.
        
        print *, ""
        print *, "Test: variable_collection_t self-assignment safety"
        
        ! Initialize collection
        coll = create_variable_collection(10)
        
        ! Add variable
        var_info%name = "var1"
        var_info%type_name = "real"
        var_info%is_declared = .true.
        var_info%usage_count = 3
        call coll%add_variable(var_info)
        
        ! Perform self-assignment
        coll = coll
        
        ! Verify collection is still valid after self-assignment
        if (.not. allocated(coll%variables)) then
            print *, "  FAIL: variables deallocated after self-assignment"
            test_passed = .false.
        else if (coll%count /= 1) then
            print *, "  FAIL: count changed after self-assignment"
            test_passed = .false.
        else if (coll%variables(1)%name /= "var1") then
            print *, "  FAIL: variable data corrupted after self-assignment"
            test_passed = .false.
        else
            print *, "  PASS: Self-assignment handled safely"
        end if
        
        if (test_passed) then
            print *, "  PASS: variable_collection_t self-assignment safe"
        else
            all_tests_passed = .false.
        end if
    end subroutine test_variable_collection_self_assignment

end program test_self_assignment_safety