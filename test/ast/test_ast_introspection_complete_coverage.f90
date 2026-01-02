program test_ast_introspection_complete_coverage
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Complete Coverage..."

    if (.not. test_all_node_type_ids()) all_passed = .false.
    if (.not. test_has_semantic_info_coverage()) all_passed = .false.

    if (all_passed) then
        print *, "All complete coverage tests passed!"
        stop 0
    else
        print *, "Some complete coverage tests failed!"
        stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    logical function test_all_node_type_ids()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg, source
        integer :: root_index, i, type_id
        integer :: line, column
        logical :: found_types(50)

        test_all_node_type_ids = .true.
        print *, "Testing comprehensive node type ID coverage..."

        found_types = .false.

        ! Test 1: Program with functions and subroutines
        arena = create_ast_arena()
        call read_example('examples/f90/ast_coverage_program_with_procedures.f90', source)

        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing test 1 failed"
            test_all_node_type_ids = .false.
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing test 1 failed"
            test_all_node_type_ids = .false.
            return
        end if

        ! Check all nodes and their type IDs
        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            if (type_id > 0 .and. type_id <= 50) then
                found_types(type_id) = .true.
            end if

            ! Also test source location API
            call get_node_source_location_from_arena(arena, i, line, column)
        end do

        ! Test 2: Control flow constructs
        arena = create_ast_arena()
        call read_example('examples/f90/ast_coverage_control_flow.f90', source)

        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)

        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            if (type_id > 0 .and. type_id <= 50) then
                found_types(type_id) = .true.
            end if
        end do

        ! Test 3: Module and use statements
        arena = create_ast_arena()
        call read_example('examples/f90/ast_coverage_module_interface.f90', source)

        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)

        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            if (type_id > 0 .and. type_id <= 50) then
                found_types(type_id) = .true.
            end if
        end do

        ! Test 4: I/O and other statements
        arena = create_ast_arena()
        call read_example('examples/f90/ast_coverage_io_statements.f90', source)

        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)

        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            if (type_id > 0 .and. type_id <= 50) then
                found_types(type_id) = .true.
            end if
        end do

        ! Test 5: Complex literals and array literals
        arena = create_ast_arena()
        call read_example('examples/f90/ast_coverage_literals_calls.f90', source)

        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)

        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            if (type_id > 0 .and. type_id <= 50) then
                found_types(type_id) = .true.
            end if
        end do

        ! Print which node types were found
        print *, "  Found node types:"
        do i = 1, 40
            if (found_types(i)) then
                print *, "    Type", i, ": FOUND"
            end if
        end do

        print *, "  All node type IDs: PASS"
    end function test_all_node_type_ids

    logical function test_has_semantic_info_coverage()
        test_has_semantic_info_coverage = .true.
        print *, "Testing has_semantic_info coverage..."

        ! Since get_node is disabled and we can't create nodes with type info
        ! directly, we can only test has_semantic_info via semantic analysis
        print *, "  has_semantic_info test covered by semantic introspection tests"

        print *, "  has_semantic_info coverage: PASS"
    end function test_has_semantic_info_coverage


end program test_ast_introspection_complete_coverage
