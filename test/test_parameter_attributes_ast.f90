program test_parameter_attributes_ast
    use fortfront
    use ast_core
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              INTENT_IN, INTENT_OUT, INTENT_INOUT, &
                              intent_type_to_string
    implicit none
    
    logical :: all_passed = .true.
    
    print *, '=== Testing Parameter Attributes in AST ==='
    print *
    
    if (.not. test_ast_parameter_attributes()) all_passed = .false.
    
    if (all_passed) then
        print *, 'PASS: AST parameter attribute tests passed'
        stop 0
    else
        print *, 'FAIL: AST parameter attribute tests failed'
        stop 1
    end if
    
contains
    
    function test_ast_parameter_attributes() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        type(compilation_options_t) :: options
        type(semantic_context_t) :: semantic_ctx
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg
        
        passed = .true.
        
        ! Simple test case
        source = &
            "subroutine test(a, b, c)" // new_line('a') // &
            "    integer, intent(in) :: a" // new_line('a') // &
            "    integer, intent(in), optional :: b" // new_line('a') // &
            "    integer, intent(out) :: c" // new_line('a') // &
            "end subroutine test"
        
        ! Lex the source
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "ERROR: Lexing failed: ", trim(error_msg)
            passed = .false.
            return
        end if
        if (.not. allocated(tokens) .or. size(tokens) == 0) then
            print *, "ERROR: No tokens generated"
            passed = .false.
            return
        end if
        
        ! Parse the tokens
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "ERROR: Parsing failed: ", trim(error_msg)
            passed = .false.
            return
        end if
        
        ! Analyze the AST
        call analyze_ast_parameters(arena, root_index, passed)
        
    end function test_ast_parameter_attributes
    
    subroutine analyze_ast_parameters(arena, root_index, test_passed)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        logical, intent(inout) :: test_passed
        integer :: i
        integer :: found_params
        
        found_params = 0
        
        ! First look for declaration nodes in the body
        print *, "Looking for declaration nodes:"
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (declaration_node)
                    print *, "  Found declaration: name='", trim(node%var_name), &
                             "' type='", trim(node%type_name), &
                             "' intent=", node%has_intent, &
                             " intent_str='", trim(node%intent), &
                             "' optional=", node%is_optional
                end select
            end if
        end do
        
        ! Look through the arena for parameter nodes
        print *, "Looking for parameter nodes:"
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (parameter_declaration_node)
                    found_params = found_params + 1
                    print *, "Found parameter: ", trim(node%name)
                    print *, "  Type: ", trim(node%type_name)
                    print *, "  Intent (type=", node%intent_type, "): ", intent_type_to_string(node%intent_type)
                    print *, "  Optional: ", node%is_optional
                    
                    ! Check specific parameters
                    if (node%name == "a") then
                        if (node%intent_type /= INTENT_IN) then
                            print *, "  ERROR: Parameter 'a' should have intent(in)"
                            test_passed = .false.
                        end if
                        if (node%is_optional) then
                            print *, "  ERROR: Parameter 'a' should not be optional"
                            test_passed = .false.
                        end if
                    else if (node%name == "b") then
                        if (node%intent_type /= INTENT_IN) then
                            print *, "  ERROR: Parameter 'b' should have intent(in)"
                            test_passed = .false.
                        end if
                        if (.not. node%is_optional) then
                            print *, "  ERROR: Parameter 'b' should be optional"
                            test_passed = .false.
                        end if
                    else if (node%name == "c") then
                        if (node%intent_type /= INTENT_OUT) then
                            print *, "  ERROR: Parameter 'c' should have intent(out)"
                            test_passed = .false.
                        end if
                        if (node%is_optional) then
                            print *, "  ERROR: Parameter 'c' should not be optional"
                            test_passed = .false.
                        end if
                    end if
                end select
            end if
        end do
        
        if (found_params == 0) then
            print *, "ERROR: No parameter declaration nodes found in AST"
            test_passed = .false.
        else
            print *, "Total parameter declaration nodes found:", found_params
        end if
        
    end subroutine analyze_ast_parameters
    
end program test_parameter_attributes_ast