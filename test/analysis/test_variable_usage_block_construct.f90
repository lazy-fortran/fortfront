program test_variable_usage_block_construct
    use fortfront, only: ast_arena_t, create_ast_arena, lex_source, parse_tokens, &
        token_t
    use variable_usage_tracker_module, only: count_variable_usage
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    integer :: procedure_index

    call read_example('examples/f90/f2003_block_construct.f90', source)
    call lex_source(source, tokens, error_msg)
    call assert_no_error('lexing', error_msg)

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (root_index <= 0) then
        call fail('parsing failed: ' // trim(error_msg))
    end if

    procedure_index = find_first_node_type('subroutine_def')
    if (procedure_index <= 0) then
        call fail('subroutine not found in block construct example')
    end if

    call assert_count('partial_sum', 4)
    call assert_count('total', 5)

    print '(a)', "Variable usage block construct test passed"

contains

    include '../common/read_example.inc'

    subroutine assert_count(name, expected)
        character(len=*), intent(in) :: name
        integer, intent(in) :: expected
        integer :: actual

        actual = count_variable_usage(arena, procedure_index, name)
        if (actual /= expected) then
            write (error_unit, '(a,a,a,i0,a,i0)') 'FAIL: expected ', &
                trim(name), ' count ', expected, ', got ', actual
            error stop 1
        end if
    end subroutine assert_count

    subroutine assert_no_error(stage, message)
        character(len=*), intent(in) :: stage
        character(len=*), intent(in) :: message

        if (len_trim(message) > 0) then
            call fail(trim(stage) // ' failed: ' // trim(message))
        end if
    end subroutine assert_no_error

    integer function find_first_node_type(node_type)
        character(len=*), intent(in) :: node_type
        integer :: i

        find_first_node_type = 0

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. allocated(arena%entries(i)%node_type)) cycle
            if (arena%entries(i)%node_type == node_type) then
                find_first_node_type = i
                return
            end if
        end do
    end function find_first_node_type

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: ' // trim(message)
        error stop 1
    end subroutine fail

end program test_variable_usage_block_construct
