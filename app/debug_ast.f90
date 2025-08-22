program debug_ast
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    integer :: root_index, i
    
    ! Same test case  
    source = 'module math_utils' // new_line('a') // &
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
            'program test' // new_line('a') // &
            'use math_utils' // new_line('a') // &
            'real :: x' // new_line('a') // &
            'x = add(2.0, 3.0)' // new_line('a') // &
            'end program test'
    
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
    
    print *, "=== AST STRUCTURE ==="
    print *, "Root index:", root_index
    print *, "Arena size:", arena%size
    
    ! Show all nodes in arena
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, i, ":", trim(arena%entries(i)%node_type), "allocated"
            select case (arena%entries(i)%node_type)
            case ("module", "module_node")
                select type (node => arena%entries(i)%node)
                type is (module_node)
                    print *, "  Module name:", trim(node%name)
                    if (allocated(node%procedure_indices)) then
                        print *, "  Procedure indices:", node%procedure_indices
                    else
                        print *, "  No procedure indices"
                    end if
                    if (allocated(node%declaration_indices)) then
                        print *, "  Declaration indices:", node%declaration_indices  
                    else
                        print *, "  No declaration indices"
                    end if
                end select
            case ("program")
                select type (node => arena%entries(i)%node)
                type is (program_node)
                    print *, "  Program name:", trim(node%name)
                    if (allocated(node%body_indices)) then
                        print *, "  Body indices:", node%body_indices
                    else
                        print *, "  No body indices"
                    end if
                end select
            case ("function_def")
                select type (node => arena%entries(i)%node)
                type is (function_def_node)
                    print *, "  Function name:", trim(node%name)
                end select
            end select
        else
            print *, i, ": (unallocated)"
        end if
    end do
    
end program debug_ast
