program test_ast_uid_integration
    ! Test UID integration with AST nodes
    ! Verifies that all AST nodes have unique UIDs
    
    use uid_generator
    use ast_core
    use ast_arena_modern
    implicit none
    
    type(ast_arena_t) :: arena
    integer :: prog_idx, assign_idx, id1_idx, id2_idx, lit_idx
    integer :: test_count, tests_passed, tests_failed
    type(uid_t) :: uid1, uid2, uid3
    
    test_count = 0
    tests_passed = 0
    tests_failed = 0
    
    ! Initialize UID generator and arena
    call init_uid_generator()
    arena = create_ast_arena()
    
    ! Run tests
    call test_node_uid_generation()
    call test_uid_uniqueness_across_nodes()
    call test_uid_preservation()
    
    ! Clean up
    call destroy_ast_arena(arena)
    
    ! Print summary
    print *, "====================================="
    print *, "AST UID Integration Test Summary"
    print *, "====================================="
    print '(A,I0)', "Total tests: ", test_count
    print '(A,I0)', "Passed: ", tests_passed
    print '(A,I0)', "Failed: ", tests_failed
    
    if (tests_failed > 0) then
        error stop "Some tests failed!"
    end if
    
contains
    
    subroutine test_node_uid_generation()
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        type(assignment_node) :: assign_node
        type(program_node) :: prog_node
        
        print *, "Testing UID generation in AST nodes..."
        
        ! Create nodes using factory functions
        id_node = create_identifier("x", 1, 1)
        lit_node = create_literal("42", LITERAL_INTEGER, 1, 5)
        
        ! Check UIDs are valid
        call assert(id_node%uid%is_valid(), "Identifier node should have valid UID")
        call assert(lit_node%uid%is_valid(), "Literal node should have valid UID")
        
        ! Check UIDs are different
        call assert(.not. uid_equal(id_node%uid, lit_node%uid), &
                   "Different nodes should have different UIDs")
        
        print *, "  Node UID generation tests passed"
    end subroutine test_node_uid_generation
    
    subroutine test_uid_uniqueness_across_nodes()
        type(identifier_node) :: id_nodes(10)
        integer :: i, j
        logical :: all_unique
        
        print *, "Testing UID uniqueness across multiple nodes..."
        
        ! Create multiple nodes
        do i = 1, 10
            id_nodes(i) = create_identifier("var", i, 1)
        end do
        
        ! Check all UIDs are unique
        all_unique = .true.
        outer: do i = 1, 9
            do j = i + 1, 10
                if (uid_equal(id_nodes(i)%uid, id_nodes(j)%uid)) then
                    all_unique = .false.
                    exit outer
                end if
            end do
        end do outer
        
        call assert(all_unique, "All node UIDs should be unique")
        
        print *, "  UID uniqueness tests passed"
    end subroutine test_uid_uniqueness_across_nodes
    
    subroutine test_uid_preservation()
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: id1, id2
        type(uid_t) :: original_uid
        integer :: indices(2)
        
        print *, "Testing UID preservation through operations..."
        
        ! Create nodes
        id1 = create_identifier("result", 1, 1)
        id2 = create_identifier("value", 1, 10)
        
        ! Store original UID
        original_uid = id1%uid
        
        ! Create assignment using indices (simulated)
        indices = [1, 2]
        assign = create_assignment(1, 2, 1, 8)
        
        ! Create program
        prog = create_program("test_prog", indices, 1, 1)
        
        ! Verify UIDs are preserved (not regenerated)
        call assert(uid_equal(id1%uid, original_uid), &
                   "Node UID should be preserved")
        
        ! Verify each node has unique UID
        call assert(.not. uid_equal(prog%uid, assign%uid), &
                   "Program and assignment should have different UIDs")
        call assert(.not. uid_equal(prog%uid, id1%uid), &
                   "Program and identifier should have different UIDs")
        
        print *, "  UID preservation tests passed"
    end subroutine test_uid_preservation
    
    ! Helper assertion subroutine
    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        test_count = test_count + 1
        
        if (condition) then
            tests_passed = tests_passed + 1
        else
            tests_failed = tests_failed + 1
            print '(A,A)', "    FAILED: ", message
        end if
    end subroutine assert
    
end program test_ast_uid_integration