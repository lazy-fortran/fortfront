program test_where_forall_constructs
    use iso_fortran_env, only: error_unit
    use frontend, only: compile_source, compilation_options_t
    use ast_types
    implicit none

    call test_simple_where_construct()
    call test_where_with_elsewhere()
    call test_where_with_multiple_elsewhere()
    call test_single_line_where()
    call test_simple_forall()
    call test_forall_with_mask()
    call test_forall_with_multiple_indices()
    call test_nested_where_forall()
    
    print *, "All WHERE/FORALL construct tests passed!"

contains

    subroutine test_simple_where_construct()
        character(len=:), allocatable :: source, error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=*), parameter :: test_file = "test_where_simple.f90"
        
        print *, "Testing simple WHERE construct..."
        
        source = "program test" // new_line('a') // &
                "where (a > 0)" // new_line('a') // &
                "    b = sqrt(a)" // new_line('a') // &
                "end where" // new_line('a') // &
                "end program test"
        
        ! Write to file
        open(newunit=unit, file=test_file, status='replace', action='write')
        write(unit, '(A)') source
        close(unit)
        
        ! Compile
        options%parse_only = .true.
        options%output_file = "test_where_simple_out.f90"
        call compile_source(test_file, options, error_msg)
        
        if (allocated(error_msg)) then
            error stop "Failed to parse simple WHERE construct: " // error_msg
        end if
        
        print *, "  ✓ Simple WHERE construct parsed correctly"
    end subroutine test_simple_where_construct
    
    subroutine test_where_with_elsewhere()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing WHERE with ELSEWHERE..."
        
        source = "program test" // new_line('a') // &
                "where (a > 0)" // new_line('a') // &
                "    b = sqrt(a)" // new_line('a') // &
                "elsewhere" // new_line('a') // &
                "    b = 0" // new_line('a') // &
                "end where" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse WHERE with ELSEWHERE"
        end if
        
        print *, "  ✓ WHERE with ELSEWHERE parsed correctly"
    end subroutine test_where_with_elsewhere
    
    subroutine test_where_with_multiple_elsewhere()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing WHERE with multiple ELSEWHERE clauses..."
        
        source = "program test" // new_line('a') // &
                "where (temperature > 100.0)" // new_line('a') // &
                "    state = 'gas'" // new_line('a') // &
                "elsewhere (temperature < 0.0)" // new_line('a') // &
                "    state = 'solid'" // new_line('a') // &
                "elsewhere" // new_line('a') // &
                "    state = 'liquid'" // new_line('a') // &
                "end where" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse WHERE with multiple ELSEWHERE"
        end if
        
        print *, "  ✓ WHERE with multiple ELSEWHERE parsed correctly"
    end subroutine test_where_with_multiple_elsewhere
    
    subroutine test_single_line_where()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing single-line WHERE statement..."
        
        source = "program test" // new_line('a') // &
                "where (mask) a = b + c" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse single-line WHERE"
        end if
        
        print *, "  ✓ Single-line WHERE parsed correctly"
    end subroutine test_single_line_where
    
    subroutine test_simple_forall()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing simple FORALL construct..."
        
        source = "program test" // new_line('a') // &
                "forall (i=1:n)" // new_line('a') // &
                "    a(i) = b(i) + c(i)" // new_line('a') // &
                "end forall" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse simple FORALL"
        end if
        
        print *, "  ✓ Simple FORALL parsed correctly"
    end subroutine test_simple_forall
    
    subroutine test_forall_with_mask()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing FORALL with mask..."
        
        source = "program test" // new_line('a') // &
                "forall (i=1:n, j=1:m, i+j <= n)" // new_line('a') // &
                "    a(i,j) = b(j,i) + c(i)" // new_line('a') // &
                "end forall" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse FORALL with mask"
        end if
        
        print *, "  ✓ FORALL with mask parsed correctly"
    end subroutine test_forall_with_mask
    
    subroutine test_forall_with_multiple_indices()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing FORALL with multiple indices..."
        
        source = "program test" // new_line('a') // &
                "forall (i=1:n:2, j=1:m)" // new_line('a') // &
                "    matrix(i,j) = i + j" // new_line('a') // &
                "end forall" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse FORALL with multiple indices"
        end if
        
        print *, "  ✓ FORALL with multiple indices parsed correctly"
    end subroutine test_forall_with_multiple_indices
    
    subroutine test_nested_where_forall()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        print *, "Testing nested WHERE inside FORALL..."
        
        source = "program test" // new_line('a') // &
                "forall (i=1:n)" // new_line('a') // &
                "    where (a(i) > 0)" // new_line('a') // &
                "        b(i) = sqrt(a(i))" // new_line('a') // &
                "    elsewhere" // new_line('a') // &
                "        b(i) = 0" // new_line('a') // &
                "    end where" // new_line('a') // &
                "end forall" // new_line('a') // &
                "end program test"
        
        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Parse
        prog_index = parse_tokens(tokens, arena)
        
        if (prog_index <= 0) then
            error stop "Failed to parse nested WHERE/FORALL"
        end if
        
        print *, "  ✓ Nested WHERE/FORALL parsed correctly"
    end subroutine test_nested_where_forall
    
    function verify_where_node(arena, prog_index) result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        logical :: success
        integer :: i, where_index
        character(len=:), allocatable :: node_type
        
        success = .false.
        
        ! Find WHERE node in the program
        do i = 1, arena%size
            node_type = get_node_type(arena, i)
            if (node_type == "where" .or. node_type == "where_stmt") then
                where_index = i
                success = .true.
                exit
            end if
        end do
        
    end function verify_where_node

end program test_where_forall_constructs