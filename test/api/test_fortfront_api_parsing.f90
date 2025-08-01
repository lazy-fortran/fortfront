program test_fortfront_api_parsing
    ! Test the public API parsing functionality
    use fortfront, only: parse_tokens, ast_arena_t, create_ast_arena, &
                        lex_source, token_t, ast_node, &
                        program_node, assignment_node, function_def_node, &
                        if_node, do_loop_node
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== fortfront Public API Parsing Tests ==='
    print *
    
    ! Test parsing functionality
    if (.not. test_basic_parsing()) all_passed = .false.
    if (.not. test_lazy_fortran_parsing()) all_passed = .false.
    if (.not. test_function_parsing()) all_passed = .false.
    if (.not. test_control_flow_parsing()) all_passed = .false.
    if (.not. test_error_handling()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All fortfront API parsing tests passed!'
        stop 0
    else
        print *, 'Some fortfront API parsing tests failed!'
        stop 1
    end if
    
contains
    
    logical function test_basic_parsing()
        test_basic_parsing = .true.
        print *, 'Testing basic parse_tokens functionality...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg
            integer :: prog_index
            class(ast_node), allocatable :: node
            
            ! Lex simple code
            call lex_source('x = 42', tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing error: ', error_msg
                test_basic_parsing = .false.
                return
            end if
            
            ! Parse tokens
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_basic_parsing = .false.
                return
            end if
            
            if (prog_index <= 0) then
                print *, '  FAIL: Invalid program index:', prog_index
                test_basic_parsing = .false.
                return
            end if
            
            ! Verify we got a program node
            if (prog_index <= 0 .or. prog_index > arena%size) then
                print *, '  FAIL: Invalid program node index'
                test_basic_parsing = .false.
                return
            end if
            
            if (.not. allocated(arena%entries(prog_index)%node)) then
                print *, '  FAIL: Could not get program node'
                test_basic_parsing = .false.
                return
            end if
            
            select type (node => arena%entries(prog_index)%node)
            type is (program_node)
                if (size(node%body_indices) == 0) then
                    print *, '  FAIL: Program has no body'
                    test_basic_parsing = .false.
                    return
                end if
                
                ! Check first statement is assignment
                block
                    integer :: stmt_index
                    stmt_index = node%body_indices(1)
                    
                    if (stmt_index <= 0 .or. stmt_index > arena%size) then
                        print *, '  FAIL: Invalid statement index'
                        test_basic_parsing = .false.
                        return
                    end if
                    
                    select type (stmt => arena%entries(stmt_index)%node)
                    type is (assignment_node)
                        ! Success
                    class default
                        print *, '  FAIL: Expected assignment node'
                        test_basic_parsing = .false.
                        return
                    end select
                end block
                
            class default
                print *, '  FAIL: Expected program_node type'
                test_basic_parsing = .false.
                return
            end select
            
            print *, '  PASS: Basic parsing'
        end block
    end function test_basic_parsing
    
    logical function test_lazy_fortran_parsing()
        test_lazy_fortran_parsing = .true.
        print *, 'Testing lazy Fortran parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg
            integer :: prog_index
            
            ! Type inference assignment
            call lex_source('x = 3.14', tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing error: ', error_msg
                test_lazy_fortran_parsing = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_lazy_fortran_parsing = .false.
                return
            end if
            
            ! Multiple statements without explicit program
            call lex_source('x = 1' // new_line('A') // 'y = 2', tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing multi-statement error: ', error_msg
                test_lazy_fortran_parsing = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing multi-statement error: ', error_msg
                test_lazy_fortran_parsing = .false.
                return
            end if
            
            print *, '  PASS: Lazy Fortran parsing'
        end block
    end function test_lazy_fortran_parsing
    
    logical function test_function_parsing()
        test_function_parsing = .true.
        print *, 'Testing function parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg, source
            integer :: prog_index
            class(ast_node), allocatable :: node
            
            ! Simple function
            source = 'real function square(x)' // new_line('A') // &
                     '    real :: x' // new_line('A') // &
                     '    square = x * x' // new_line('A') // &
                     'end function square'
            
            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing error: ', error_msg
                test_function_parsing = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_function_parsing = .false.
                return
            end if
            
            ! Should get function node directly for explicit function
            if (prog_index <= 0 .or. prog_index > arena%size) then
                print *, '  FAIL: Invalid function node index'
                test_function_parsing = .false.
                return
            end if
            
            if (.not. allocated(arena%entries(prog_index)%node)) then
                print *, '  FAIL: Could not get parsed node'
                test_function_parsing = .false.
                return
            end if
            
            select type (node => arena%entries(prog_index)%node)
            type is (function_def_node)
                if (node%name /= 'square') then
                    print *, '  FAIL: Expected function name "square", got "', node%name, '"'
                    test_function_parsing = .false.
                    return
                end if
                
                if (node%return_type /= 'real') then
                    print *, '  FAIL: Expected return type "real", got "', node%return_type, '"'
                    test_function_parsing = .false.
                    return
                end if
                
                if (size(node%param_indices) /= 1) then
                    print *, '  FAIL: Expected 1 parameter, got', size(node%param_indices)
                    test_function_parsing = .false.
                    return
                end if
                
            type is (program_node)
                ! For lazy fortran, might wrap in program
                if (size(node%body_indices) > 0) then
                    ! Check the first body item is a function
                    if (node%body_indices(1) > 0 .and. node%body_indices(1) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(1))%node)) then
                            select type (func_node => arena%entries(node%body_indices(1))%node)
                            type is (function_def_node)
                                ! OK - function is in program body
                            class default
                                print *, '  FAIL: Expected function in program body'
                                test_function_parsing = .false.
                                return
                            end select
                        else
                            print *, '  FAIL: Function node not allocated'
                            test_function_parsing = .false.
                            return
                        end if
                    else
                        print *, '  FAIL: Invalid function index'
                        test_function_parsing = .false.
                        return
                    end if
                end if
                
            class default
                print *, '  FAIL: Expected function_def_node or program_node'
                test_function_parsing = .false.
                return
            end select
            
            print *, '  PASS: Function parsing'
        end block
    end function test_function_parsing
    
    logical function test_control_flow_parsing()
        test_control_flow_parsing = .true.
        print *, 'Testing control flow parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg, source
            integer :: prog_index
            
            ! If statement
            source = 'if (x > 0) then' // new_line('A') // &
                     '    y = 1' // new_line('A') // &
                     'end if'
            
            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing if statement error: ', error_msg
                test_control_flow_parsing = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing if statement error: ', error_msg
                test_control_flow_parsing = .false.
                return
            end if
            
            ! Do loop
            source = 'do i = 1, 10' // new_line('A') // &
                     '    print *, i' // new_line('A') // &
                     'end do'
            
            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing do loop error: ', error_msg
                test_control_flow_parsing = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing do loop error: ', error_msg
                test_control_flow_parsing = .false.
                return
            end if
            
            print *, '  PASS: Control flow parsing'
        end block
    end function test_control_flow_parsing
    
    logical function test_error_handling()
        test_error_handling = .true.
        print *, 'Testing parsing error handling...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg
            integer :: prog_index
            
            ! Incomplete if statement
            call lex_source('if (x > 0) then', tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            ! Parser might handle this gracefully or produce an error
            if (error_msg /= "") then
                print *, '  Note: Parser error for incomplete if: ', trim(error_msg)
            end if
            
            ! Mismatched end
            call lex_source('do i = 1, 10' // new_line('A') // 'end if', tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  Note: Parser error for mismatched end: ', trim(error_msg)
            end if
            
            print *, '  PASS: Error handling tested'
        end block
    end function test_error_handling
    
end program test_fortfront_api_parsing