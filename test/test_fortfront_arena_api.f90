program test_fortfront_arena_api
    ! Test the AST arena public API methods exposed through fortfront
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: failures = 0
    
    ! Test the enhanced arena API methods
    call test_find_by_type()
    call test_traverse_depth()
    call test_arena_node_type_query()
    call test_typed_node_access()
    
    if (failures == 0) then
        print *, "All fortfront arena API tests passed!"
    else
        print *, "fortfront arena API tests failed with", failures, "failures"
        stop 1
    end if
    
contains
    
    subroutine test_find_by_type()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: found_nodes(:)
        character(len=*), parameter :: source = &
            "program test" // char(10) // &
            "    integer :: x = 5" // char(10) // &
            "    real :: y = 3.14" // char(10) // &
            "    x = x + 1" // char(10) // &
            "end program test"
        
        print *, "Testing find_by_type..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        ! Test finding assignment nodes
        found_nodes = find_nodes_by_type(arena, "assignment")
        if (size(found_nodes) /= 1) then  ! Only the x = x + 1 assignment
            print *, "Expected 1 assignment node, found", size(found_nodes)
            failures = failures + 1
        end if
        
        ! Test finding literal nodes
        found_nodes = find_nodes_by_type(arena, "literal")
        if (size(found_nodes) < 3) then  ! At least 5, 3.14, and 1
            print *, "Expected at least 3 literal nodes, found", size(found_nodes)
            failures = failures + 1
        end if
        
        print *, "  find_by_type test completed"
    end subroutine test_find_by_type
    
    subroutine test_traverse_depth()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: max_depth
        character(len=*), parameter :: source = &
            "program test" // char(10) // &
            "    if (x > 0) then" // char(10) // &
            "        if (y > 0) then" // char(10) // &
            "            z = x + y" // char(10) // &
            "        end if" // char(10) // &
            "    end if" // char(10) // &
            "end program test"
        
        print *, "Testing traverse_depth..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        ! Get depth statistics
        max_depth = get_max_depth(arena, prog_index)
        if (max_depth < 1) then  ! At least the program node itself
            print *, "Expected depth >= 1, got", max_depth
            failures = failures + 1
        end if
        
        print *, "  traverse_depth test completed"
    end subroutine test_traverse_depth
    
    subroutine test_arena_node_type_query()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: node_type
        character(len=*), parameter :: source = &
            "program test" // char(10) // &
            "    x = 42" // char(10) // &
            "end program test"
        
        print *, "Testing get_node_type..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        ! Test getting node type
        node_type = get_node_type(arena, prog_index)
        if (node_type /= NODE_PROGRAM) then
            print *, "Expected NODE_PROGRAM, got", node_type
            failures = failures + 1
        end if
        
        print *, "  get_node_type test completed"
    end subroutine test_arena_node_type_query
    
    subroutine test_typed_node_access()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(program_node), pointer :: prog
        type(assignment_node), pointer :: assign
        integer, allocatable :: children(:)
        character(len=*), parameter :: source = &
            "program test" // char(10) // &
            "    x = 42" // char(10) // &
            "end program test"
        
        print *, "Testing typed node access..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        ! Get program node
        prog => get_node_as_program(arena, prog_index)
        if (.not. associated(prog)) then
            print *, "Failed to get program node"
            failures = failures + 1
            return
        end if
        
        if (prog%name /= "main") then
            print *, "Expected program name 'main', got '", trim(prog%name), "'"
            failures = failures + 1
        end if
        
        ! Get children and test assignment access
        children = get_children(arena, prog_index)
        if (size(children) > 0) then
            assign => get_node_as_assignment(arena, children(1))
            if (.not. associated(assign)) then
                print *, "Failed to get assignment node"
                failures = failures + 1
            end if
        end if
        
        print *, "  typed node access test completed"
    end subroutine test_typed_node_access
    
end program test_fortfront_arena_api