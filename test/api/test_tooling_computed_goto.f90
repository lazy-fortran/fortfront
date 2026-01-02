program test_tooling_computed_goto
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
                         ast_arena_t, token_t
    use ast_nodes_transfer, only: goto_node
    use ast_nodes_core, only: identifier_node
    implicit none

    type(tooling_parse_options_t) :: options
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: source, error_msg
    integer :: root_index, selector_index, i
    logical :: found

    print *, '=== Test: tooling API preserves computed goto ==='

    call read_example('examples/f90/issue_1583_computed_goto.f90', source)

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                      options, tokens)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a,1x,a)') 'FAIL: tooling_load_ast_from_string:', &
                trim(error_msg)
            stop 1
        end if
    end if

    if (root_index <= 0 .or. arena%size <= 0) then
        write (error_unit, '(a)') 'FAIL: AST root missing'
        stop 1
    end if

    found = .false.
    selector_index = -1

    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
        type is (goto_node)
            if (.not. allocated(node%label_list)) cycle
            if (node%selector_index <= 0) cycle
            found = .true.
            selector_index = node%selector_index
            exit
        class default
        end select
    end do

    if (.not. found) then
        write (error_unit, '(a)') 'FAIL: goto node with selector not found'
        stop 1
    end if

    select type (selector => arena%entries(selector_index)%node)
    type is (identifier_node)
        if (trim(selector%name) /= 'choice') then
            write (error_unit, '(a,1x,a)') 'FAIL: selector identifier mismatch:', &
                trim(selector%name)
            stop 1
        end if
    class default
        write (error_unit, '(a)') 'FAIL: selector node not identifier'
        stop 1
    end select

    print *, 'PASS: tooling API preserves computed goto selector'
contains

    include '../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example
end program test_tooling_computed_goto
