program test_dependency_graph
    use dependency_graph, only: dependency_graph_t, create_dependency_graph
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test dependency graph functionality
    call test_topological_sort()
    call test_cycle_detection()
    call test_simple_dependencies()
    
    if (tests_passed) then
        print *, "TEST PASSED: dependency graph module"
    else
        print *, "TEST FAILED: dependency graph module"
        error stop 1
    end if

contains

    subroutine test_simple_dependencies()
        type(dependency_graph_t) :: graph
        character(len=32), allocatable :: order(:)
        character(len=32), allocatable :: no_deps(:)
        
        ! Test simple case: no dependencies
        graph = create_dependency_graph()
        
        allocate(no_deps(0))
        call graph%add_node("simple_analyzer", no_deps)
        
        order = graph%get_execution_order()
        
        if (size(order) /= 1) then
            print *, "ERROR: Expected 1 node in execution order"
            tests_passed = .false.
            return
        end if
        
        if (trim(order(1)) /= "simple_analyzer") then
            print *, "ERROR: Expected simple_analyzer in order"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Simple dependencies work"
    end subroutine

    subroutine test_topological_sort()
        type(dependency_graph_t) :: graph
        character(len=32), allocatable :: order(:)
        character(len=32) :: deps_a(1), deps_b(1), no_deps(0)
        
        ! Test: A depends on B, B depends on C
        ! Expected order: [C, B, A]
        graph = create_dependency_graph()
        
        ! Add nodes
        deps_a(1) = "B"
        call graph%add_node("A", deps_a)
        
        deps_b(1) = "C"  
        call graph%add_node("B", deps_b)
        
        call graph%add_node("C", no_deps)
        
        order = graph%get_execution_order()
        
        if (size(order) /= 3) then
            print *, "ERROR: Expected 3 nodes in execution order"
            tests_passed = .false.
            return
        end if
        
        ! C should come first (no dependencies)
        ! B should come second (depends on C)
        ! A should come last (depends on B)
        if (trim(order(1)) /= "C" .or. trim(order(2)) /= "B" .or. &
            trim(order(3)) /= "A") then
            print *, "ERROR: Incorrect topological order"
            print *, "Expected: C, B, A"
            print *, "Got:", trim(order(1)), trim(order(2)), trim(order(3))
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Topological sort works correctly"
    end subroutine

    subroutine test_cycle_detection()
        type(dependency_graph_t) :: graph
        character(len=32), allocatable :: order(:)
        character(len=32) :: deps_a(1), deps_b(1)
        
        ! Test: A depends on B, B depends on A (cycle)
        graph = create_dependency_graph()
        
        deps_a(1) = "B"
        call graph%add_node("A", deps_a)
        
        deps_b(1) = "A"
        call graph%add_node("B", deps_b)
        
        ! Should detect cycle and return empty order
        order = graph%get_execution_order()
        
        if (size(order) /= 0) then
            print *, "ERROR: Expected empty order due to cycle"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Cycle detection works correctly"
    end subroutine

end program test_dependency_graph