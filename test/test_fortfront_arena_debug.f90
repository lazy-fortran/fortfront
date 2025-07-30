program test_fortfront_arena_debug
    ! Debug program to check what's in the arena
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg
    type(ast_arena_stats_t) :: stats
    character(len=*), parameter :: source = &
        "program test" // char(10) // &
        "    integer :: x = 5" // char(10) // &
        "    real :: y = 3.14" // char(10) // &
        "    x = x + 1" // char(10) // &
        "end program test"
    
    ! Lex and parse
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Lex error:", trim(error_msg)
            stop 1
        end if
    end if
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Parse error:", trim(error_msg)
            stop 1
        end if
    end if
    
    ! Get arena stats
    stats = get_arena_stats(arena)
    print *, "Arena statistics:"
    print *, "  Total nodes:", stats%total_nodes
    print *, "  Max depth:", stats%max_depth
    print *, "  Size:", arena%size
    
    ! Print all node types in the arena with parent and children info
    print *, ""
    print *, "Node types in arena with relationships:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node_type)) then
            print '(A,I4,A,A,A,I3)', "  Node ", i, ": ", trim(arena%entries(i)%node_type), &
                ", parent=", arena%entries(i)%parent_index
            if (allocated(arena%entries(i)%child_indices)) then
                print '(A,*(I3,1X))', "    Children: ", &
                    arena%entries(i)%child_indices(1:arena%entries(i)%child_count)
            end if
        end if
    end do
    
end program test_fortfront_arena_debug