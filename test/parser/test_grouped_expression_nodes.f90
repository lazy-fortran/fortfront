program test_grouped_expression_nodes
    use lexer_core, only: token_t, tokenize_core
    use parser_expressions_module, only: parse_expression
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: literal_node, binary_op_node, call_or_subscript_node, &
        array_literal_node
    use ast_nodes_misc, only: complex_literal_node
    implicit none
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: source
    integer :: i, root

    source = '1'
    do i = 1, 8
        source = '(1+' // source // ')'
    end do
    call parse(source, root)
    if (integer_value(root) /= 9) error stop 'grouped arithmetic changed value'
    if (arena%entry_count /= 17) error stop 'ordinary groups left duplicate AST nodes'

    call parse('(1.0_8, -2.0_8)', root)
    call require_complex(root)
    call parse('(1.0 + 2.0, 3.0 * 4.0)', root)
    call require_complex(root)
    call parse('((1.0, 2.0))', root)
    call require_complex(root)
    call parse('(f(1, 2) + 3)', root)
    select type (node => arena%entries(root)%node)
        type is (binary_op_node)
        if (node%operator /= '+') error stop 'grouped call lost addition'
        select type (left => arena%entries(node%left_index)%node)
            type is (call_or_subscript_node)
        class default
            error stop 'nested comma became complex separator'
        end select
    class default
        error stop 'grouped call became complex literal'
    end select
    call parse('([1, 2])', root)
    select type (node => arena%entries(root)%node)
        type is (array_literal_node)
    class default
        error stop 'bracket comma became complex separator'
    end select
    call parse('((/1, 2/))', root)
    select type (node => arena%entries(root)%node)
        type is (array_literal_node)
    class default
        error stop 'legacy array changed grouping'
    end select
    call parse("('a,b')", root)
    select type (node => arena%entries(root)%node)
        type is (literal_node)
    class default
        error stop 'string comma became complex separator'
    end select
    call parse('(size([1, 2]))', root)
    select type (node => arena%entries(root)%node)
        type is (call_or_subscript_node)
    class default
        error stop 'array comma became complex separator'
    end select
    call parse("(index('),(', ','))", root)
    select type (node => arena%entries(root)%node)
        type is (call_or_subscript_node)
    class default
        error stop 'string punctuation changed grouping'
    end select
    print *, 'PASS: grouped expressions and complex separators'

contains

    subroutine parse(text, index)
        character(len=*), intent(in) :: text
        integer, intent(out) :: index
        type(token_t), allocatable :: tokens(:)
        call tokenize_core(text, tokens)
        call arena%clear()
        arena = create_ast_arena()
        index = parse_expression(tokens, arena)
        if (index <= 0) error stop 'expression was not parsed'
    end subroutine parse

    subroutine require_complex(index)
        integer, intent(in) :: index
        select type (node => arena%entries(index)%node)
            type is (complex_literal_node)
            if (node%real_index <= 0 .or. node%imag_index <= 0) &
                error stop 'complex literal lost a component'
        class default
            error stop 'complex literal lost its node'
        end select
    end subroutine require_complex

    recursive integer function integer_value(index) result(value)
        integer, intent(in) :: index
        select type (node => arena%entries(index)%node)
            type is (literal_node)
            read (node%value, *) value
            type is (binary_op_node)
            if (node%operator /= '+') error stop 'unexpected arithmetic operator'
            value = integer_value(node%left_index) + integer_value(node%right_index)
        class default
            error stop 'unexpected arithmetic node'
        end select
    end function integer_value
end program test_grouped_expression_nodes
