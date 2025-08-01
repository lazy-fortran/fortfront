program test_fortfront_api_json
    ! Test the public API JSON serialization functionality
    use fortfront, only: lex_source, parse_tokens, token_t, &
                        ast_arena_t, create_ast_arena, &
                        ast_to_json, ast_node, &
                        program_node, assignment_node, identifier_node, &
                        literal_node, LITERAL_INTEGER
    use ast_factory, only: push_program, push_assignment, push_identifier, push_literal
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== fortfront Public API JSON Serialization Tests ==='
    print *
    
    ! Test JSON serialization functionality
    if (.not. test_basic_json()) all_passed = .false.
    if (.not. test_complex_ast_json()) all_passed = .false.
    if (.not. test_null_node_json()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All fortfront API JSON tests passed!'
        stop 0
    else
        print *, 'Some fortfront API JSON tests failed!'
        stop 1
    end if
    
contains
    
    logical function test_basic_json()
        test_basic_json = .true.
        print *, 'Testing basic AST to JSON conversion...'
        
        block
            type(ast_arena_t) :: arena
            integer :: prog_index, assign_index, id_index, lit_index
            integer, allocatable :: body_indices(:)
            character(len=:), allocatable :: json_str
            
            arena = create_ast_arena()
            
            ! Build simple AST: x = 42
            id_index = push_identifier(arena, "x", 1, 1)
            lit_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
            assign_index = push_assignment(arena, id_index, lit_index, 1, 1)
            body_indices = [assign_index]
            prog_index = push_program(arena, "test", body_indices, 1, 1)
            
            ! Convert to JSON
            call ast_to_json(arena, prog_index, json_str)
            
            if (.not. allocated(json_str)) then
                print *, '  FAIL: JSON string not allocated'
                test_basic_json = .false.
                return
            end if
            
            if (len(json_str) == 0) then
                print *, '  FAIL: Empty JSON string'
                test_basic_json = .false.
                return
            end if
            
            ! Check for expected content
            if (index(json_str, '"type"') == 0) then
                print *, '  FAIL: Missing type field in JSON'
                test_basic_json = .false.
                return
            end if
            
            if (index(json_str, '"program"') == 0) then
                print *, '  FAIL: Missing program type in JSON'
                test_basic_json = .false.
                return
            end if
            
            print *, '  PASS: Basic JSON conversion'
            print *, '  Full JSON output:'
            print *, json_str
        end block
    end function test_basic_json
    
    logical function test_complex_ast_json()
        test_complex_ast_json = .true.
        print *, 'Testing complex AST to JSON conversion...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: prog_index
            character(len=:), allocatable :: error_msg, json_str
            character(len=:), allocatable :: source
            
            source = 'program test' // new_line('A') // &
                     '    integer :: x, y' // new_line('A') // &
                     '    x = 10' // new_line('A') // &
                     '    y = x + 5' // new_line('A') // &
                     'end program test'
            
            ! Lex and parse
            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexing error: ', error_msg
                test_complex_ast_json = .false.
                return
            end if
            
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_complex_ast_json = .false.
                return
            end if
            
            ! Convert to JSON
            call ast_to_json(arena, prog_index, json_str)
            
            if (.not. allocated(json_str)) then
                print *, '  FAIL: JSON string not allocated'
                test_complex_ast_json = .false.
                return
            end if
            
            ! Check for expected content - the JSON will have stack indices
            if (index(json_str, '"stack_index"') == 0) then
                print *, '  FAIL: Missing stack_index in JSON'
                test_complex_ast_json = .false.
                return
            end if
            
            if (index(json_str, '"body"') == 0) then
                print *, '  FAIL: Missing body array in JSON'
                test_complex_ast_json = .false.
                return
            end if
            
            print *, '  PASS: Complex AST JSON conversion'
        end block
    end function test_complex_ast_json
    
    logical function test_null_node_json()
        test_null_node_json = .true.
        print *, 'Testing null node JSON conversion...'
        
        block
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: json_str
            
            arena = create_ast_arena()
            
            ! Convert null node (index 0) to JSON
            call ast_to_json(arena, 0, json_str)
            
            if (.not. allocated(json_str)) then
                print *, '  FAIL: JSON string not allocated for null node'
                test_null_node_json = .false.
                return
            end if
            
            if (index(json_str, 'null') == 0) then
                print *, '  FAIL: Expected null in JSON for invalid node'
                test_null_node_json = .false.
                return
            end if
            
            print *, '  PASS: Null node JSON conversion'
        end block
    end function test_null_node_json
    
end program test_fortfront_api_json