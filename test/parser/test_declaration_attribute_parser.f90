program test_declaration_attribute_parser
    use lexer_api, only: lex_source
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declaration_attributes_module, only: parse_declaration_attributes
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use declaration_attribute_utils, only: declaration_attribute_info_t
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
        ', pointer, dimension(1:10), intent(inout), optional'
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    type(declaration_attribute_info_t) :: attr_info

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: lexer error:', trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    parser = create_parser_state(tokens)

    call parse_declaration_attributes(parser, arena, attr_info)

    if (.not. attr_info%is_pointer) then
        print *, 'FAIL: pointer attribute missing'
        stop 1
    end if

    if (.not. attr_info%has_global_dimensions) then
        print *, 'FAIL: dimension attribute missing'
        stop 1
    end if

    if (.not. attr_info%has_intent) then
        print *, 'FAIL: intent attribute missing'
        stop 1
    end if

    if (trim(attr_info%intent) /= 'inout') then
        print *, 'FAIL: intent value incorrect:', trim(attr_info%intent)
        stop 1
    end if

    if (.not. attr_info%is_optional) then
        print *, 'FAIL: optional attribute missing'
        stop 1
    end if

    if (.not. allocated(attr_info%global_dimension_indices)) then
        print *, 'FAIL: dimension indices not allocated'
        stop 1
    end if

    if (size(attr_info%global_dimension_indices) /= 1) then
        print *, 'FAIL: expected 1 dimension range, got', &
            size(attr_info%global_dimension_indices)
        stop 1
    end if

    if (attr_info%global_dimension_indices(1) <= 0) then
        print *, 'FAIL: dimension range not recorded'
        stop 1
    end if

    print *, 'PASS: parse_declaration_attributes handles multiple attributes'
end program test_declaration_attribute_parser
