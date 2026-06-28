program test_derived_type_specifier_ast
    use lexer_api, only: lex_source
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_type_specifier, type_specifier_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use fortfront_utils, only: get_node_type
    use fortfront_types, only: NODE_IDENTIFIER
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
        "type(point_mod::point_t(3)) :: value"
    character(len=*), parameter :: unlimited_source = "class(*), pointer :: p"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(parser_state_t) :: parser
    type(type_specifier_t) :: type_spec
    type(ast_arena_t) :: arena
    integer :: node_type

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    parser = create_parser_state(tokens)

    type_spec = parse_type_specifier(parser, arena)

    if (.not. type_spec%is_derived_type) then
        print *, "FAIL: derived type flag not set"
        stop 1
    end if

    if (type_spec%derived_type_identifier <= 0) then
        print *, "FAIL: derived type identifier missing"
        stop 1
    end if

    node_type = get_node_type(arena, type_spec%derived_type_identifier)
    if (node_type /= NODE_IDENTIFIER) then
        print *, "FAIL: derived type identifier is not NODE_IDENTIFIER"
        stop 1
    end if

    if (type_spec%derived_type_name /= "point_t") then
        print *, "FAIL: derived type name mismatch:", trim(type_spec%derived_type_name)
        stop 1
    end if

    if (type_spec%derived_type_module /= "point_mod") then
        print *, "FAIL: module qualifier lost:", trim(type_spec%derived_type_module)
        stop 1
    end if

    if (.not. type_spec%has_derived_type_parameters) then
        print *, "FAIL: derived type parameters not detected"
        stop 1
    end if

    if (.not. allocated(type_spec%derived_parameter_nodes)) then
        print *, "FAIL: derived parameter nodes not allocated"
        stop 1
    end if

    if (size(type_spec%derived_parameter_nodes) /= 1) then
        print *, "FAIL: expected 1 parameter node, got", &
            size(type_spec%derived_parameter_nodes)
        stop 1
    end if

    print *, "PASS: derived type specifier produces structured AST"

    call lex_source(unlimited_source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error for unlimited source:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    parser = create_parser_state(tokens)
    type_spec = parse_type_specifier(parser, arena)

    if (type_spec%is_derived_type) then
        print *, "FAIL: unlimited polymorphic flagged as derived type"
        stop 1
    end if

    if (len_trim(type_spec%derived_type_name) /= 0) then
        print *, "FAIL: unlimited polymorphic captured derived name"
        stop 1
    end if

    if (len_trim(type_spec%derived_type_module) /= 0) then
        print *, "FAIL: unlimited polymorphic captured module name"
        stop 1
    end if

    if (type_spec%derived_type_identifier /= 0) then
        print *, "FAIL: unlimited polymorphic created identifier node"
        stop 1
    end if

    if (allocated(type_spec%derived_parameter_nodes)) then
        print *, "FAIL: unlimited polymorphic recorded parameter nodes"
        stop 1
    end if

    if (type_spec%type_name /= "class(*)") then
        print *, "FAIL: unlimited polymorphic type name mismatch:", &
            trim(type_spec%type_name)
        stop 1
    end if

    print *, "PASS: unlimited polymorphic spec parsed without derived metadata"
end program test_derived_type_specifier_ast
