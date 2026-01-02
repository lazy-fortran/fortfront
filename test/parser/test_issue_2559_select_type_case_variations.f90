program test_issue_2559_select_type_case_variations
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_execution_statements_module, only: parse_program_statement
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_conditional, only: select_type_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    logical :: has_select_type

    print *, '=== Test: SELECT TYPE handles mixed-case keywords ==='

    call read_example('examples/f90/issue_2559_select_type_case_variations.f90', &
                      source)

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(256)

    prog_index = parse_program_statement(parser, arena)
    if (prog_index <= 0) then
        write (error_unit, '(a)') &
            'ERROR: parse_program_statement returned invalid index'
        stop 1
    end if

    if (parser%has_errors()) then
        write (error_unit, '(a)') 'ERROR: parser reported errors'
        stop 1
    end if

    has_select_type = .false.
    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
        type is (select_type_node)
            has_select_type = .true.
            exit
        class default
            cycle
        end select
    end do

    if (.not. has_select_type) then
        write (error_unit, '(a)') 'ERROR: expected select type node not found'
        stop 1
    end if

    print *, 'PASS: mixed-case SELECT TYPE parsed'


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_2559_select_type_case_variations
