program test_ast_hierarchical_factory
    use ast_hierarchical_factory, only: hierarchical_factory_t, create_hierarchical_factory, &
                                       factory_stats_t
    use ast_base, only: LITERAL_INTEGER, LITERAL_STRING
    implicit none

    write(*,*) "=== AST Hierarchical Factory Test ==="
    
    call test_factory_creation()
    call test_node_creation()
    call test_reference_counting()
    call test_memory_management()
    call test_performance_pools()
    
    write(*,*) "=== ALL TESTS PASSED ==="
    write(*,*) "Hierarchical factory architecture working correctly!"

contains

    subroutine test_factory_creation()
        type(hierarchical_factory_t) :: factory
        type(factory_stats_t) :: stats
        
        write(*,*) "Test 1: Factory Creation and Initialization"
        
        ! Test factory creation with default capacity
        factory = create_hierarchical_factory()
        
        ! Check initial state
        stats = factory%get_memory_stats()
        if (stats%nodes_created /= 0) then
            print *, "FAIL: Initial nodes_created should be 0"
            stop 1
        end if
        if (stats%nodes_reused /= 0) then
            print *, "FAIL: Initial nodes_reused should be 0"
            stop 1
        end if
        
        write(*,*) "PASS: Factory creation and initialization"
    end subroutine

    subroutine test_node_creation()
        type(hierarchical_factory_t) :: factory
        integer :: id_node_id, lit_node_id, assign_node_id
        
        write(*,*) "Test 2: Node Creation and Reference Management"
        
        factory = create_hierarchical_factory()
        
        ! Create identifier node
        id_node_id = factory%create_identifier_node("test_var", line=1, column=1)
        if (id_node_id <= 0) then
            print *, "FAIL: identifier node creation failed"
            stop 1
        end if
        
        ! Create literal node
        lit_node_id = factory%create_literal_node("42", LITERAL_INTEGER, line=1, column=10)
        if (lit_node_id <= 0) then
            print *, "FAIL: literal node creation failed"
            stop 1
        end if
        
        ! Create assignment node with child references
        assign_node_id = factory%create_assignment_node(id_node_id, lit_node_id, line=1, column=1)
        if (assign_node_id <= 0) then
            print *, "FAIL: assignment node creation failed"
            stop 1
        end if
        
        write(*,*) "PASS: Node creation and reference management"
    end subroutine

    subroutine test_reference_counting()
        type(hierarchical_factory_t) :: factory
        integer :: node_id
        class(*), allocatable :: node_ref
        
        write(*,*) "Test 3: Reference Counting and Memory Management"
        
        factory = create_hierarchical_factory()
        
        ! Create a node and get reference
        node_id = factory%create_identifier_node("ref_test", line=1, column=1)
        node_ref = factory%get_node_reference(node_id)
        
        if (.not. allocated(node_ref)) then
            print *, "FAIL: node reference should be valid"
            stop 1
        end if
        
        ! Release reference
        call factory%release_node_reference(node_id)
        
        write(*,*) "PASS: Reference counting and memory management"
    end subroutine

    subroutine test_memory_management()
        type(hierarchical_factory_t) :: factory
        type(factory_stats_t) :: stats_before, stats_after
        integer :: i, node_id
        
        write(*,*) "Test 4: Memory Management and Garbage Collection"
        
        factory = create_hierarchical_factory()
        stats_before = factory%get_memory_stats()
        
        ! Create many temporary nodes
        do i = 1, 10
            node_id = factory%create_literal_node("temp", LITERAL_STRING, line=i, column=1)
        end do
        
        ! Trigger garbage collection
        call factory%collect_garbage()
        
        stats_after = factory%get_memory_stats()
        
        if (stats_after%nodes_created /= 10) then
            print *, "FAIL: should have created 10 nodes"
            stop 1
        end if
        
        if (stats_after%gc_cycles == 0) then
            print *, "FAIL: garbage collection should have run"
            stop 1
        end if
        
        write(*,*) "PASS: Memory management and garbage collection"
    end subroutine

    subroutine test_performance_pools()
        type(hierarchical_factory_t) :: factory
        type(factory_stats_t) :: stats
        integer :: node_id1, node_id2
        
        write(*,*) "Test 5: Performance Pools and Optimization"
        
        factory = create_hierarchical_factory()
        
        ! Create multiple similar nodes (should use pool optimization)
        node_id1 = factory%create_identifier_node("pooled1", line=1, column=1)
        node_id2 = factory%create_identifier_node("pooled2", line=2, column=1)
        
        if (node_id1 <= 0 .or. node_id2 <= 0) then
            print *, "FAIL: pooled node creation failed"
            stop 1
        end if
        
        ! Optimize memory (should use pools)
        call factory%optimize_memory()
        
        stats = factory%get_memory_stats()
        if (stats%nodes_created /= 2) then
            print *, "FAIL: should have created 2 nodes"
            stop 1
        end if
        
        write(*,*) "PASS: Performance pools and optimization"
    end subroutine

end program test_ast_hierarchical_factory
