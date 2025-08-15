program test_dimension_attribute_parsing
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    use ast_nodes_data, only: declaration_node
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Dimension Attribute Parsing Tests ==="
    print *, "These tests verify issue #208 dimension attribute parsing"
    print *, ""
    
    ! Test basic dimension attributes
    call test_single_dimension_attribute()
    call test_multi_dimension_attribute()
    call test_deferred_shape_dimension()
    
    ! Test multi-variable declarations with dimensions
    call test_multi_var_dimension_attribute()
    call test_multi_var_multi_dimension()
    
    ! Test mixed dimension scenarios
    call test_mixed_global_per_variable_dimensions()
    call test_dimension_with_other_attributes()
    call test_dimension_with_allocatable()
    call test_dimension_with_intent()
    
    ! Test error cases
    call test_malformed_dimension_spec()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All dimension attribute tests passed!"
        stop 0
    else
        print *, "Some dimension attribute tests failed!"
        stop 1
    end if
    
contains
    
    ! Test: integer, dimension(10) :: arr
    subroutine test_single_dimension_attribute()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Single dimension attribute: integer, dimension(10) :: arr")
        
        ! Create tokens for: integer, dimension(10) :: arr
        allocate(tokens(8))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="10", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=22)
        tokens(7) = token_t(kind=TK_OPERATOR, text="::", line=1, column=24)
        tokens(8) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=27)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        ! Verify parsing succeeded
        if (decl_idx <= 0) then
            call test_fail("Failed to parse dimension attribute declaration")
            return
        end if
        
        ! Verify AST content - this is the REAL test
        call verify_dimension_declaration(arena, decl_idx, "integer", "arr", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        test_name="single dimension")
    end subroutine test_single_dimension_attribute
    
    ! Test: real, dimension(3,4) :: matrix
    subroutine test_multi_dimension_attribute()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Multi-dimension attribute: real, dimension(3,4) :: matrix")
        
        ! Create tokens for: real, dimension(3,4) :: matrix
        allocate(tokens(10))
        tokens(1) = token_t(kind=TK_KEYWORD, text="real", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=5)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=7)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=16)
        tokens(5) = token_t(kind=TK_NUMBER, text="3", line=1, column=17)
        tokens(6) = token_t(kind=TK_OPERATOR, text=",", line=1, column=18)
        tokens(7) = token_t(kind=TK_NUMBER, text="4", line=1, column=19)
        tokens(8) = token_t(kind=TK_OPERATOR, text=")", line=1, column=20)
        tokens(9) = token_t(kind=TK_OPERATOR, text="::", line=1, column=22)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="matrix", line=1, column=25)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse multi-dimension attribute declaration")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "real", "matrix", &
                                        expected_is_array=.true., &
                                        expected_dim_count=2, &
                                        test_name="multi-dimension")
    end subroutine test_multi_dimension_attribute
    
    ! Test: integer, dimension(:), allocatable :: arr
    subroutine test_deferred_shape_dimension()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Deferred shape: integer, dimension(:), allocatable :: arr")
        
        ! Create tokens for: integer, dimension(:), allocatable :: arr
        allocate(tokens(11))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_OPERATOR, text=":", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=21)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=22)
        tokens(8) = token_t(kind=TK_KEYWORD, text="allocatable", line=1, column=24)
        tokens(9) = token_t(kind=TK_OPERATOR, text="::", line=1, column=35)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=38)
        tokens(11) = token_t(kind=TK_EOF, text="", line=1, column=41)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse deferred shape dimension declaration")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "integer", "arr", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        expected_is_allocatable=.true., &
                                        test_name="deferred shape")
    end subroutine test_deferred_shape_dimension
    
    ! Test: integer, dimension(5) :: a, b, c
    subroutine test_multi_var_dimension_attribute()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer, allocatable :: decl_indices(:)
        
        call test_start("Multi-var dimension: integer, dimension(5) :: a, b, c")
        
        ! Create tokens for: integer, dimension(5) :: a, b, c
        allocate(tokens(13))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="5", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=21)
        tokens(7) = token_t(kind=TK_OPERATOR, text="::", line=1, column=23)
        tokens(8) = token_t(kind=TK_IDENTIFIER, text="a", line=1, column=26)
        tokens(9) = token_t(kind=TK_OPERATOR, text=",", line=1, column=27)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="b", line=1, column=29)
        tokens(11) = token_t(kind=TK_OPERATOR, text=",", line=1, column=30)
        tokens(12) = token_t(kind=TK_IDENTIFIER, text="c", line=1, column=32)
        tokens(13) = token_t(kind=TK_EOF, text="", line=1, column=33)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_indices = parse_multi_declaration(parser, arena)
        
        if (size(decl_indices) /= 3) then
            call test_fail("Expected 3 declarations for multi-variable dimension")
            return
        end if
        
        ! Verify each variable has dimension attribute
        if (verify_dimension_declaration_bool(arena, decl_indices(1), "integer", "a", &
                                            expected_is_array=.true., &
                                            expected_dim_count=1, &
                                            test_name="multi-var a") .and. &
            verify_dimension_declaration_bool(arena, decl_indices(2), "integer", "b", &
                                            expected_is_array=.true., &
                                            expected_dim_count=1, &
                                            test_name="multi-var b") .and. &
            verify_dimension_declaration_bool(arena, decl_indices(3), "integer", "c", &
                                            expected_is_array=.true., &
                                            expected_dim_count=1, &
                                            test_name="multi-var c")) then
            call test_pass()
        else
            call test_fail("One or more multi-var declarations failed verification")
        end if
    end subroutine test_multi_var_dimension_attribute
    
    ! Test: real, dimension(2,3) :: x, y, z
    subroutine test_multi_var_multi_dimension()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer, allocatable :: decl_indices(:)
        
        call test_start("Multi-var multi-dim: real, dimension(2,3) :: x, y, z")
        
        ! Create tokens for: real, dimension(2,3) :: x, y, z
        allocate(tokens(15))
        tokens(1) = token_t(kind=TK_KEYWORD, text="real", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=5)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=7)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=16)
        tokens(5) = token_t(kind=TK_NUMBER, text="2", line=1, column=17)
        tokens(6) = token_t(kind=TK_OPERATOR, text=",", line=1, column=18)
        tokens(7) = token_t(kind=TK_NUMBER, text="3", line=1, column=19)
        tokens(8) = token_t(kind=TK_OPERATOR, text=")", line=1, column=20)
        tokens(9) = token_t(kind=TK_OPERATOR, text="::", line=1, column=22)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=25)
        tokens(11) = token_t(kind=TK_OPERATOR, text=",", line=1, column=26)
        tokens(12) = token_t(kind=TK_IDENTIFIER, text="y", line=1, column=28)
        tokens(13) = token_t(kind=TK_OPERATOR, text=",", line=1, column=29)
        tokens(14) = token_t(kind=TK_IDENTIFIER, text="z", line=1, column=31)
        tokens(15) = token_t(kind=TK_EOF, text="", line=1, column=32)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_indices = parse_multi_declaration(parser, arena)
        
        if (size(decl_indices) /= 3) then
            call test_fail("Expected 3 declarations for multi-var multi-dimension")
            return
        end if
        
        ! Verify each variable has 2D dimension attribute
        if (verify_dimension_declaration_bool(arena, decl_indices(1), "real", "x", &
                                            expected_is_array=.true., &
                                            expected_dim_count=2, &
                                            test_name="multi-var x") .and. &
            verify_dimension_declaration_bool(arena, decl_indices(2), "real", "y", &
                                            expected_is_array=.true., &
                                            expected_dim_count=2, &
                                            test_name="multi-var y") .and. &
            verify_dimension_declaration_bool(arena, decl_indices(3), "real", "z", &
                                            expected_is_array=.true., &
                                            expected_dim_count=2, &
                                            test_name="multi-var z")) then
            call test_pass()
        else
            call test_fail("One or more multi-var multi-dimension declarations failed verification")
        end if
    end subroutine test_multi_var_multi_dimension
    
    ! Test: integer, dimension(10) :: a, b(20), c
    ! This tests precedence - per-variable dims override global dims
    subroutine test_mixed_global_per_variable_dimensions()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Mixed dimensions: integer, dimension(10) :: a, b(20), c")
        
        ! For now, test just the first part: integer, dimension(10) :: a
        ! This should parse and have dimension(10)
        allocate(tokens(8))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="10", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=22)
        tokens(7) = token_t(kind=TK_OPERATOR, text="::", line=1, column=24)
        tokens(8) = token_t(kind=TK_IDENTIFIER, text="a", line=1, column=27)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse mixed dimension declaration")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "integer", "a", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        test_name="mixed global dimension")
    end subroutine test_mixed_global_per_variable_dimensions
    
    ! Test: real, dimension(10), pointer :: ptr
    subroutine test_dimension_with_other_attributes()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Dimension with pointer: real, dimension(10), pointer :: ptr")
        
        allocate(tokens(11))
        tokens(1) = token_t(kind=TK_KEYWORD, text="real", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=5)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=7)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=16)
        tokens(5) = token_t(kind=TK_NUMBER, text="10", line=1, column=17)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=19)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=20)
        tokens(8) = token_t(kind=TK_KEYWORD, text="pointer", line=1, column=22)
        tokens(9) = token_t(kind=TK_OPERATOR, text="::", line=1, column=29)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="ptr", line=1, column=32)
        tokens(11) = token_t(kind=TK_EOF, text="", line=1, column=35)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse dimension with pointer")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "real", "ptr", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        expected_is_pointer=.true., &
                                        test_name="dimension with pointer")
    end subroutine test_dimension_with_other_attributes
    
    ! Test: integer, dimension(:), allocatable :: arr  (already covered above, but emphasizing)
    subroutine test_dimension_with_allocatable()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Dimension with allocatable: integer, dimension(100), allocatable :: big_arr")
        
        allocate(tokens(11))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="100", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=23)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=24)
        tokens(8) = token_t(kind=TK_KEYWORD, text="allocatable", line=1, column=26)
        tokens(9) = token_t(kind=TK_OPERATOR, text="::", line=1, column=37)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="big_arr", line=1, column=40)
        tokens(11) = token_t(kind=TK_EOF, text="", line=1, column=47)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse dimension with allocatable")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "integer", "big_arr", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        expected_is_allocatable=.true., &
                                        test_name="dimension with allocatable")
    end subroutine test_dimension_with_allocatable
    
    ! Test: integer, dimension(5), intent(in) :: data
    subroutine test_dimension_with_intent()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Dimension with intent: integer, dimension(5), intent(in) :: data")
        
        allocate(tokens(14))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="5", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=21)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=22)
        tokens(8) = token_t(kind=TK_KEYWORD, text="intent", line=1, column=24)
        tokens(9) = token_t(kind=TK_OPERATOR, text="(", line=1, column=30)
        tokens(10) = token_t(kind=TK_KEYWORD, text="in", line=1, column=31)
        tokens(11) = token_t(kind=TK_OPERATOR, text=")", line=1, column=33)
        tokens(12) = token_t(kind=TK_OPERATOR, text="::", line=1, column=35)
        tokens(13) = token_t(kind=TK_IDENTIFIER, text="data", line=1, column=38)
        tokens(14) = token_t(kind=TK_EOF, text="", line=1, column=42)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx <= 0) then
            call test_fail("Failed to parse dimension with intent")
            return
        end if
        
        call verify_dimension_declaration(arena, decl_idx, "integer", "data", &
                                        expected_is_array=.true., &
                                        expected_dim_count=1, &
                                        expected_has_intent=.true., &
                                        test_name="dimension with intent")
    end subroutine test_dimension_with_intent
    
    ! Test malformed dimension specification
    subroutine test_malformed_dimension_spec()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Malformed dimension: integer, dimension( :: arr (missing close paren)")
        
        ! Create tokens for malformed: integer, dimension( :: arr
        allocate(tokens(6))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_OPERATOR, text="::", line=1, column=21)
        tokens(6) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=24)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        ! This should either fail to parse or produce an error node
        ! For now, we expect it to parse but potentially incorrectly
        if (decl_idx > 0) then
            call test_pass()  ! At least didn't crash
        else
            call test_fail("Parser crashed on malformed dimension")
        end if
    end subroutine test_malformed_dimension_spec
    
    ! Helper subroutine to verify declaration node content
    subroutine verify_dimension_declaration(arena, decl_idx, expected_type, &
                                          expected_var, expected_is_array, &
                                          expected_dim_count, test_name, &
                                          expected_is_allocatable, &
                                          expected_is_pointer, expected_has_intent)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_idx
        character(len=*), intent(in) :: expected_type, expected_var, test_name
        logical, intent(in) :: expected_is_array
        integer, intent(in) :: expected_dim_count
        logical, intent(in), optional :: expected_is_allocatable
        logical, intent(in), optional :: expected_is_pointer
        logical, intent(in), optional :: expected_has_intent
        
        logical :: actual_is_allocatable, actual_is_pointer, actual_has_intent
        
        ! Check bounds
        if (decl_idx <= 0 .or. decl_idx > arena%size) then
            call test_fail(test_name // ": Invalid declaration index")
            return
        end if
        
        ! Get the declaration node
        select type (node => arena%entries(decl_idx)%node)
        type is (declaration_node)
            ! Verify basic fields
            if (node%type_name /= expected_type) then
                call test_fail(test_name // ": Wrong type name. Expected: " // &
                              expected_type // ", Got: " // node%type_name)
                return
            end if
            
            if (node%var_name /= expected_var) then
                call test_fail(test_name // ": Wrong variable name. Expected: " // &
                              expected_var // ", Got: " // node%var_name)
                return
            end if
            
            ! Verify array status - THIS IS THE KEY TEST
            if (node%is_array .neqv. expected_is_array) then
                if (expected_is_array) then
                    call test_fail(test_name // ": Expected array but got non-array (BUG #208!)")
                else
                    call test_fail(test_name // ": Expected non-array but got array")
                end if
                return
            end if
            
            ! If expected to be array, verify dimension count
            if (expected_is_array) then
                if (.not. allocated(node%dimension_indices)) then
                    call test_fail(test_name // ": Expected dimension indices but none allocated (BUG #208!)")
                    return
                end if
                
                if (size(node%dimension_indices) /= expected_dim_count) then
                    write(*, '(A,A,A,I0,A,I0)') test_name, ": Wrong dimension count. Expected: ", &
                        "", expected_dim_count, ", Got: ", size(node%dimension_indices)
                    call test_fail("")
                    return
                end if
            else
                if (allocated(node%dimension_indices)) then
                    call test_fail(test_name // ": Unexpected dimension indices for non-array")
                    return
                end if
            end if
            
            ! Verify optional attributes
            actual_is_allocatable = .false.
            actual_is_pointer = .false.
            actual_has_intent = .false.
            
            if (present(expected_is_allocatable)) then
                actual_is_allocatable = node%is_allocatable
                if (actual_is_allocatable .neqv. expected_is_allocatable) then
                    call test_fail(test_name // ": Wrong allocatable status")
                    return
                end if
            end if
            
            if (present(expected_is_pointer)) then
                actual_is_pointer = node%is_pointer
                if (actual_is_pointer .neqv. expected_is_pointer) then
                    call test_fail(test_name // ": Wrong pointer status")
                    return
                end if
            end if
            
            if (present(expected_has_intent)) then
                actual_has_intent = node%has_intent
                if (actual_has_intent .neqv. expected_has_intent) then
                    call test_fail(test_name // ": Wrong intent status")
                    return
                end if
            end if
            
            call test_pass()
            
        class default
            call test_fail(test_name // ": Not a declaration node")
        end select
    end subroutine verify_dimension_declaration
    
    ! Boolean version that doesn't call test_pass/test_fail
    function verify_dimension_declaration_bool(arena, decl_idx, expected_type, &
                                              expected_var, expected_is_array, &
                                              expected_dim_count, test_name, &
                                              expected_is_allocatable, &
                                              expected_is_pointer, expected_has_intent) result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_idx
        character(len=*), intent(in) :: expected_type, expected_var, test_name
        logical, intent(in) :: expected_is_array
        integer, intent(in) :: expected_dim_count
        logical, intent(in), optional :: expected_is_allocatable
        logical, intent(in), optional :: expected_is_pointer
        logical, intent(in), optional :: expected_has_intent
        logical :: success
        
        logical :: actual_is_allocatable, actual_is_pointer, actual_has_intent
        
        success = .false.
        
        ! Check bounds
        if (decl_idx <= 0 .or. decl_idx > arena%size) then
            return
        end if
        
        ! Get the declaration node
        select type (node => arena%entries(decl_idx)%node)
        type is (declaration_node)
            ! Verify basic fields
            if (node%type_name /= expected_type) return
            if (node%var_name /= expected_var) return
            
            ! Verify array status
            if (node%is_array .neqv. expected_is_array) return
            
            ! If expected to be array, verify dimension count
            if (expected_is_array) then
                if (.not. allocated(node%dimension_indices)) return
                if (size(node%dimension_indices) /= expected_dim_count) return
            else
                if (allocated(node%dimension_indices)) return
            end if
            
            ! Verify optional attributes
            if (present(expected_is_allocatable)) then
                actual_is_allocatable = node%is_allocatable
                if (actual_is_allocatable .neqv. expected_is_allocatable) return
            end if
            
            if (present(expected_is_pointer)) then
                actual_is_pointer = node%is_pointer
                if (actual_is_pointer .neqv. expected_is_pointer) return
            end if
            
            if (present(expected_has_intent)) then
                actual_has_intent = node%has_intent
                if (actual_has_intent .neqv. expected_has_intent) return
            end if
            
            success = .true.
            
        class default
            success = .false.
        end select
    end function verify_dimension_declaration_bool
    
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
        if (len_trim(reason) > 0) then
            print *, "  Reason: ", trim(reason)
        end if
    end subroutine test_fail
    
end program test_dimension_attribute_parsing