program test_ast_core_comprehensive
    use ast_core
    use ast_factory
    use json_module
    implicit none
    
    integer :: test_count, pass_count
    logical :: is_windows
    
    test_count = 0
    pass_count = 0
    
    ! Detect if we're on Windows  
    is_windows = check_if_windows()
    
    print *, "=== AST Core Comprehensive Tests ==="
    
    if (is_windows) then
        print *, "INFO: Running on Windows - using reduced memory profile"
    end if
    
    ! Test arena operations
    call test_arena_operations()
    
    ! Test node creation
    call test_node_creation()
    
    ! Test arena stats
    call test_arena_stats()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All AST core comprehensive tests passed!"
        stop 0
    else
        print *, "Some AST core comprehensive tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_arena_operations()
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: stats
        integer :: i, idx
        
        call test_start("Arena initialization and growth")
        
        ! Initialize arena
        arena = create_ast_stack()
        
        ! Test automatic growth (reduced for Windows compatibility)  
        do i = 1, 50  ! Sufficient to test growth without exhausting stack
            idx = push_identifier(arena, "var" // char(48 + mod(i, 10)))
        end do
        
        if (arena%size == 50) then
            call test_pass()
        else
            call test_fail("Arena growth failed")
        end if
        
        call test_start("Arena clear operation")
        
        ! Clear
        call arena%clear()
        
        if (arena%size == 0) then
            call test_pass()
        else
            call test_fail("Arena clear failed")
        end if
        
        call test_start("Arena pop operation")
        
        ! Add some nodes
        idx = push_identifier(arena, "x")
        idx = push_identifier(arena, "y")
        idx = push_identifier(arena, "z")
        
        ! Pop one
        call arena%pop()
        
        if (arena%size == 2) then
            call test_pass()
        else
            call test_fail("Pop operation failed")
        end if
    end subroutine test_arena_operations
    
    subroutine test_node_creation()
        type(ast_arena_t) :: arena
        integer :: prog_idx, var_idx, val_idx, assign_idx
        integer, allocatable :: body(:)
        
        call test_start("Node creation via factory")
        
        arena = create_ast_stack()
        
        ! Create nodes
        allocate(body(0))
        prog_idx = push_program(arena, "test_prog", body)
        var_idx = push_identifier(arena, "x")
        val_idx = push_literal(arena, "42", LITERAL_INTEGER)
        assign_idx = push_assignment(arena, var_idx, val_idx)
        
        if (arena%size == 4) then
            call test_pass()
        else
            call test_fail("Wrong number of nodes")
        end if
        
        call test_start("Find by type operation")
        
        block
            integer, allocatable :: found_indices(:)
            
            ! Find all identifier nodes
            found_indices = arena%find_by_type("identifier")
            
            if (allocated(found_indices)) then
                if (size(found_indices) == 1) then  ! We have 1 identifier
                    call test_pass()
                else
                    call test_fail("Wrong number of identifiers found")
                end if
            else
                call test_fail("Find operation failed")
            end if
        end block
    end subroutine test_node_creation
    
    subroutine test_arena_stats()
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: stats
        integer :: i, idx
        
        call test_start("Arena stats tracking")
        
        arena = create_ast_stack()
        
        ! Add some nodes
        do i = 1, 10
            idx = push_identifier(arena, "node" // char(48 + mod(i, 10)))
        end do
        
        ! Get stats
        stats = arena%get_stats()
        
        if (stats%total_nodes == 10 .and. &
            stats%capacity >= 10 .and. &
            stats%memory_usage > 0) then
            call test_pass()
        else
            call test_fail("Stats incorrect")
        end if
    end subroutine test_arena_stats
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat
        
        ! Try to detect Windows through environment variable
        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')
        
        ! Alternative: check for Windows-specific env var
        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows
    
end program test_ast_core_comprehensive