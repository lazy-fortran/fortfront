program test_call_graph_consolidation
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    call test_internal_procedures()
    call test_module_and_program_scopes()

    if (all_tests_passed) then
        print *, "All call graph consolidation tests PASSED!"
    else
        print *, "Call graph consolidation tests FAILED"
        stop 1
    end if

contains

    subroutine test_internal_procedures()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(call_graph_t) :: graph
        integer :: root_index

        print *, "Testing internal procedure resolution..."

        source = '' // &
            "program main" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine driver()" // new_line('a') // &
            "        call outer()" // new_line('a') // &
            "    end subroutine driver" // new_line('a') // &
            "    subroutine outer()" // new_line('a') // &
            "        call helper()" // new_line('a') // &
            "    contains" // new_line('a') // &
            "        subroutine helper()" // new_line('a') // &
            "            call inner()" // new_line('a') // &
            "        end subroutine helper" // new_line('a') // &
            "        subroutine inner()" // new_line('a') // &
            "        end subroutine inner" // new_line('a') // &
            "    end subroutine outer" // new_line('a') // &
            "end program main"

        call lex_source(source, tokens, error_msg)
        if (error_msg /= '') then
            call report_failure('Lexing failed for internal procedure test', error_msg)
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (root_index <= 0) then
            call report_failure('Parsing failed for internal procedure test', error_msg)
            return
        end if

        graph = build_call_graph(arena, root_index)

        call assert_procedure(graph, '__MULTI_UNIT__')
        call assert_procedure(graph, '__MULTI_UNIT__::driver')
        call assert_procedure(graph, '__MULTI_UNIT__::outer')
        call assert_procedure(graph, '__MULTI_UNIT__::helper')
        call assert_procedure(graph, '__MULTI_UNIT__::inner')
        call assert_procedure(graph, 'main')

        if (.not. edge_exists(graph, '__MULTI_UNIT__::driver', '__MULTI_UNIT__::outer') .and. &
            .not. edge_exists(graph, '__MULTI_UNIT__::driver', 'outer')) then
            call report_failure('Missing call edge in call graph', &
                '__MULTI_UNIT__::driver -> outer')
        end if
        if (.not. edge_exists(graph, '__MULTI_UNIT__::outer', '__MULTI_UNIT__::helper') .and. &
            .not. edge_exists(graph, '__MULTI_UNIT__::outer', 'helper')) then
            call report_failure('Missing call edge in call graph', &
                '__MULTI_UNIT__::outer -> helper')
        end if
        if (.not. edge_exists(graph, '__MULTI_UNIT__::outer', '__MULTI_UNIT__::inner') .and. &
            .not. edge_exists(graph, '__MULTI_UNIT__::outer', 'inner')) then
            call report_failure('Missing call edge in call graph', &
                '__MULTI_UNIT__::outer -> inner')
        end if
    end subroutine test_internal_procedures

    subroutine test_module_and_program_scopes()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(call_graph_t) :: graph
        integer :: root_index

        print *, "Testing module and program scope traversal..."

        source = '' // &
            "module math_mod" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine compute()" // new_line('a') // &
            "        call helper()" // new_line('a') // &
            "    contains" // new_line('a') // &
            "        subroutine helper()" // new_line('a') // &
            "        end subroutine helper" // new_line('a') // &
            "    end subroutine compute" // new_line('a') // &
            "end module math_mod" // new_line('a') // &
            "program app" // new_line('a') // &
            "    use math_mod" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine run()" // new_line('a') // &
            "        call compute()" // new_line('a') // &
            "    end subroutine run" // new_line('a') // &
            "end program app"

        call lex_source(source, tokens, error_msg)
        if (error_msg /= '') then
            call report_failure('Lexing failed for module scope test', error_msg)
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (root_index <= 0) then
            call report_failure('Parsing failed for module scope test', error_msg)
            return
        end if

        graph = build_call_graph(arena, root_index)

        call assert_procedure(graph, 'math_mod::compute')
        call assert_procedure(graph, 'math_mod::helper')
        call assert_procedure(graph, '__MULTI_UNIT__::run')

        if (.not. edge_exists(graph, 'math_mod::compute', 'math_mod::helper') .and. &
            .not. edge_exists(graph, 'math_mod::compute', 'helper')) then
            call report_failure('Missing call edge in call graph', &
                'math_mod::compute -> helper')
        end if
        if (.not. edge_exists(graph, '__MULTI_UNIT__::run', 'math_mod::compute') .and. &
            .not. edge_exists(graph, '__MULTI_UNIT__::run', 'compute')) then
            call report_failure('Missing call edge in call graph', &
                '__MULTI_UNIT__::run -> compute')
        end if
    end subroutine test_module_and_program_scopes

    subroutine assert_procedure(graph, expected)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: expected
        if (.not. procedure_exists(graph, expected)) then
            call report_failure('Missing procedure in call graph', expected)
        end if
    end subroutine assert_procedure

    subroutine assert_edge(graph, caller, callee)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: caller
        character(len=*), intent(in) :: callee
        if (.not. edge_exists(graph, caller, callee)) then
            call report_failure('Missing call edge in call graph', &
                caller // ' -> ' // callee)
        end if
    end subroutine assert_edge

    logical function procedure_exists(graph, expected)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: expected
        integer :: i

        do i = 1, graph%proc_count
            if (trim(graph%procedures(i)%name) == trim(expected)) then
                procedure_exists = .true.
                return
            end if
        end do

        procedure_exists = .false.
    end function procedure_exists

    logical function edge_exists(graph, caller, callee)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: caller
        character(len=*), intent(in) :: callee
        integer :: i

        do i = 1, graph%call_count
            if (trim(graph%calls(i)%caller) == trim(caller) .and. &
                trim(graph%calls(i)%callee) == trim(callee)) then
                edge_exists = .true.
                return
            end if
        end do

        edge_exists = .false.
    end function edge_exists

    subroutine report_failure(message, detail)
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: detail

        write(error_unit, '(A)') trim(message)
        if (len_trim(detail) > 0) then
            write(error_unit, '(A)') '  Detail: ' // trim(detail)
        end if
        all_tests_passed = .false.
    end subroutine report_failure

end program test_call_graph_consolidation
