program test_issue_760_named_do_nesting
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_loops, only: do_loop_node
    implicit none
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: source, invalid
    integer :: outer, middle, inner, ending

    call read_example('examples/f90/issue_760_named_do_depth_three.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'depth-three named DO is accepted')
    outer = 0
    select type (root => result%arena%entries(result%root_index)%node)
        type is (program_node)
        outer = first_loop(result%arena, root%body_indices)
    end select
    call require(outer > 0, 'outer DO is present')
    call loop_child(result%arena, outer, 'i', middle)
    call require(middle > 0, 'middle DO belongs to outer DO')
    call loop_child(result%arena, middle, 'j', inner)
    call require(inner > 0, 'named DO belongs to middle DO')
    select type (node => result%arena%entries(inner)%node)
        type is (do_loop_node)
        call require(node%var_name == 'k', 'inner loop variable')
        call require(allocated(node%label), 'inner DO retains its construct name')
        call require(node%label == 'c', 'inner construct name is c')
        call require(size(node%body_indices) == 2, 'inner statements stay in its body')
    end select

    ending = index(source, 'end do c')
    call require(ending > 0, 'fixture has named terminator')
    invalid = source(:ending - 1)//'end do wrong'//source(ending + 8:)
    call compile_frontend_from_string(invalid, result, options)
    call require(.not. result%parse_ok, 'wrong closing construct name is refused')
    call require(index(result%diagnostic_text, 'wrong') > 0, &
        'closing-name diagnostic identifies the offending name')
    invalid = source(:ending - 1)//'end do'//source(ending + 8:)
    call compile_frontend_from_string(invalid, result, options)
    call require(.not. result%parse_ok, 'missing closing construct name is refused')
    print *, 'PASS: named DO depth and closing-name validation'

contains

    integer function first_loop(arena, indices) result(index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        integer :: i

        index = 0
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
                type is (do_loop_node)
                index = indices(i)
                return
            end select
        end do
    end function first_loop

    subroutine loop_child(arena, loop, variable, child)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: loop
        character(len=*), intent(in) :: variable
        integer, intent(out) :: child

        child = 0
        select type (node => arena%entries(loop)%node)
            type is (do_loop_node)
            call require(node%var_name == variable, 'enclosing loop variable')
            child = first_loop(arena, node%body_indices)
        end select
    end subroutine loop_child

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a)') 'FAIL: '//message
        stop 1
    end subroutine require

    include '../common/read_example.inc'

end program test_issue_760_named_do_nesting
