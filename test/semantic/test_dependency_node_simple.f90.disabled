program test_dependency_node_simple
    use dependency_graph
    implicit none
    
    type(dependency_node_t) :: node1, node2
    
    ! Initialize node1
    node1%name = "test_node"
    allocate(node1%dependencies(2))
    node1%dependencies(1) = "dep1"
    node1%dependencies(2) = "dep2"
    
    print *, "Before assignment:"
    print *, "node1%dependencies(1) = '", trim(node1%dependencies(1)), "'"
    print *, "node1%dependencies(2) = '", trim(node1%dependencies(2)), "'"
    
    ! Test assignment
    node2 = node1
    
    print *, ""
    print *, "After assignment:"
    print *, "node2%dependencies(1) = '", trim(node2%dependencies(1)), "'"
    print *, "node2%dependencies(2) = '", trim(node2%dependencies(2)), "'"
    
    ! Modify original
    node1%dependencies(1) = "modified"
    
    print *, ""
    print *, "After modifying node1:"
    print *, "node1%dependencies(1) = '", trim(node1%dependencies(1)), "'"
    print *, "node2%dependencies(1) = '", trim(node2%dependencies(1)), "'"
    
    if (trim(node2%dependencies(1)) == "dep1") then
        print *, ""
        print *, "SUCCESS: Deep copy works - node2 is independent"
    else
        print *, ""
        print *, "FAILURE: Shallow copy - node2 was affected"
    end if
    
end program test_dependency_node_simple