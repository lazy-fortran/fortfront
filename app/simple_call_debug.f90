program simple_call_debug
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    type(call_graph_t) :: graph
    character(len=:), allocatable :: unused(:)
    integer :: i
    
    ! Test case that should show multiply as unused (EXACT copy from test)
    source = 'module math_utils' // new_line('a') // &
            'implicit none' // new_line('a') // &
            'contains' // new_line('a') // &
            'function add(a, b) result(c)' // new_line('a') // &
            '    real :: a, b, c' // new_line('a') // &
            '    c = a + b' // new_line('a') // &
            'end function add' // new_line('a') // &
            'function multiply(a, b) result(c)' // new_line('a') // &
            '    real :: a, b, c' // new_line('a') // &
            '    c = a * b' // new_line('a') // &
            'end function multiply' // new_line('a') // &
            'end module math_utils' // new_line('a') // &
            '' // new_line('a') // &
            'program test' // new_line('a') // &
            'use math_utils' // new_line('a') // &
            'implicit none' // new_line('a') // &
            'real :: x' // new_line('a') // &
            'x = add(2.0, 3.0)' // new_line('a') // &
            'end program test'
    
    ! Direct call to build call graph
    block
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        integer :: root_index
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "LEX ERROR:", error_msg
            stop 1
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (root_index <= 0) then
            print *, "PARSE ERROR:", error_msg
            stop 1
        end if
        
        print *, "Root index:", root_index
        print *, "Arena size:", arena%size
        
        graph = build_call_graph_from_arena(arena, root_index)
    end block
    
    print *, "=== GRAPH INFO ==="
    print *, "Procedures:", graph%proc_count
    print *, "Calls:", graph%call_count
    
    print *, "=== PROCEDURES ==="
    do i = 1, graph%proc_count
        print *, i, ":", trim(graph%procedures(i)%name)
    end do
    
    print *, "=== CALLS ==="
    do i = 1, graph%call_count
        print *, i, ":", trim(graph%calls(i)%caller), "->", trim(graph%calls(i)%callee)
    end do
    
    unused = get_unused_procedures(graph)
    print *, "=== UNUSED (", size(unused), ") ==="
    do i = 1, size(unused)
        print *, i, ":", trim(unused(i))
    end do
    
end program simple_call_debug
