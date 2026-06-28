program test_integer_expression_detection
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node
    use standardizer, only: collect_assignment_vars
    implicit none

    character(len=*), parameter :: nl = new_line('a')

    print *, '=== Integer Expression Detection Tests ==='

    call run_case('deep integer operations', &
        'program case1' // nl // &
        'integer :: a' // nl // &
        'a = ((1 + 2) * (3 + 4)) - (5 - 6)' // nl // &
        'end program case1', &
        'a', 'integer')

    call run_case('division downgrade', &
        'program case2' // nl // &
        'integer :: b' // nl // &
        'b = ((1 + 2) * (3 - 4)) / (5 + 6)' // nl // &
        'end program case2', &
        'b', 'real')

    print *, 'All integer expression detection tests passed.'

contains

    subroutine run_case(test_name, source_code, expected_name, expected_type)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: source_code
        character(len=*), intent(in) :: expected_name
        character(len=*), intent(in) :: expected_type
        type(token_t), allocatable :: tokens(:)
        character(:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: assignment_index
        character(len=64) :: var_names(8)
        character(len=64) :: var_types(8)
        logical :: var_declared(8)
        integer :: var_count
        integer :: i
        character(len=64), allocatable :: function_names(:)

        call lex_source(source_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, 'FAIL:', trim(test_name), 'lex error', trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, 'FAIL:', trim(test_name), 'parse error', trim(error_msg)
            error stop 1
        end if

        assignment_index = 0
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (assignment_node)
                assignment_index = i
                exit
            end select
        end do

        if (assignment_index == 0) then
            print *, 'FAIL:', trim(test_name), 'assignment not found'
            error stop 1
        end if

        var_names = ''
        var_types = ''
        var_declared = .false.
        var_count = 0
        allocate (function_names(0))

        call collect_assignment_vars(arena, assignment_index, var_names, var_types, &
            var_declared, var_count, function_names, 0)

        if (var_count /= 1) then
            print *, 'FAIL:', trim(test_name), 'unexpected var count', var_count
            error stop 1
        end if

        if (trim(var_names(1)) /= trim(expected_name)) then
            print *, 'FAIL:', trim(test_name), 'expected variable', trim(expected_name), &
                'got', trim(var_names(1))
            error stop 1
        end if

        if (trim(var_types(1)) /= trim(expected_type)) then
            print *, 'FAIL:', trim(test_name), 'expected type', trim(expected_type), &
                'got', trim(var_types(1))
            error stop 1
        end if

        print *, 'PASS:', trim(test_name)
    end subroutine run_case

end program test_integer_expression_detection
