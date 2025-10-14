program test_parser_type_hints
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_declaration
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use parser_type_hooks_module, only: type_annotation_t
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: decl_index
    type(semantic_context_t) :: ctx
    type(type_annotation_t) :: hint
    logical :: found

    print *, '=== Test: parser type hooks capture declaration metadata ==='

    source = 'real(kind=8), allocatable :: mat(:,:)' // new_line('a')

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(64)

    decl_index = parse_declaration(parser, arena)
    if (decl_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_declaration did not return a valid node'
        stop 1
    end if

    call create_semantic_context(ctx)

    if (.not. allocated(ctx%parser_type_hints)) then
        write (error_unit, '(a)') 'ERROR: semantic context did not receive parser hints'
        stop 1
    end if

    if (size(ctx%parser_type_hints) /= 1) then
        write (error_unit, '(a,i0)') 'ERROR: expected a single type annotation, found ', &
            size(ctx%parser_type_hints)
        stop 1
    end if

    if (trim(ctx%parser_type_hints(1)%type_name) /= 'real') then
        write (error_unit, '(a)') 'ERROR: recorded type name is incorrect'
        stop 1
    end if

    if (.not. ctx%parser_type_hints(1)%is_allocatable) then
        write (error_unit, '(a)') 'ERROR: allocatable attribute missing from annotation'
        stop 1
    end if

    if (.not. ctx%parser_type_hints(1)%has_dimensions) then
        write (error_unit, '(a)') 'ERROR: dimension metadata missing from annotation'
        stop 1
    end if

    if (size(ctx%parser_type_hints(1)%var_names) /= 1) then
        write (error_unit, '(a,i0)') 'ERROR: expected one variable name, found ', &
            size(ctx%parser_type_hints(1)%var_names)
        stop 1
    end if

    found = ctx%get_type_hint(decl_index, hint)
    if (.not. found) then
        write (error_unit, '(a)') 'ERROR: context lookup for type hint failed'
        stop 1
    end if

    if (.not. hint%has_dimensions) then
        write (error_unit, '(a)') 'ERROR: lookup annotation missing dimensions'
        stop 1
    end if

    if (hint%dimension_indices(1) /= ctx%parser_type_hints(1)%dimension_indices(1)) then
        write (error_unit, '(a)') 'ERROR: dimension metadata mismatch'
        stop 1
    end if

    print *, 'PASS: parser type annotations propagated to semantic context'

end program test_parser_type_hints
