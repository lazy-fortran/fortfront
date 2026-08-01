program test_f2003_associate_block
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    use ast_nodes_associate, only: associate_node
    use ast_nodes_loops, only: do_loop_node
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    type(transform_context_t) :: ctx
    logical :: all_passed

    all_passed = .true.

    call test_associate_construct(all_passed)
    call test_associate_with_do_construct(all_passed)
    call test_associate_nested_do_ast(all_passed)
    call test_block_construct(all_passed)

    if (all_passed) then
        print *, 'PASS: F2003 associate and block constructs parsed correctly'
    else
        error stop 1
    end if

contains

    include '../common/read_example.inc'


    subroutine test_associate_construct(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lower_output
        type(transform_context_t) :: ctx

        call read_example('examples/f90/f2003_associate_construct.f90', source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'f2003_associate_construct'

        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: associate transform error: ' &
                & // trim(error_msg)
            all_passed = .false.
            return
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: associate no output produced'
            all_passed = .false.
            return
        end if

        lower_output = to_lower(output_code)

        call assert_contains(lower_output, 'associate', &
            & 'FAIL: associate keyword not preserved', all_passed)

        call assert_contains(lower_output, 'end associate', &
            & 'FAIL: end associate not preserved', all_passed)

        call assert_contains(lower_output, 'px => pt%x', &
            & 'FAIL: associate with component access not preserved', all_passed)

        call assert_contains(lower_output, 'py => pt%y', &
            & 'FAIL: second associate binding not preserved', all_passed)

        call assert_contains(lower_output, 'scaled_val => val', &
            & 'FAIL: associate with expression not preserved', all_passed)

    end subroutine test_associate_construct

    subroutine test_associate_with_do_construct(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lower_output
        type(transform_context_t) :: ctx

        source_code = 'module associate_do_test'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine run()'//new_line('a')// &
            '    integer :: f, b'//new_line('a')// &
            '    associate (l1 => f)'//new_line('a')// &
            '      do f = 1, 2'//new_line('a')// &
            '        associate (l2 => b)'//new_line('a')// &
            '          do b = 1, 2'//new_line('a')// &
            '          end do'//new_line('a')// &
            '        end associate'//new_line('a')// &
            '      end do'//new_line('a')// &
            '    end associate'//new_line('a')// &
            '  end subroutine run'//new_line('a')// &
            'end module associate_do_test'//new_line('a')

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'associate_do_test'

        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: associate DO transform error: ' &
                & // trim(error_msg)
            all_passed = .false.
            return
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: associate DO no output produced'
            all_passed = .false.
            return
        end if

        lower_output = to_lower(output_code)
        call assert_contains(lower_output, 'do f = 1, 2', &
            & 'FAIL: DO loop in associate body not preserved', all_passed)
        call assert_contains(lower_output, 'do b = 1, 2', &
            & 'FAIL: nested DO loop in associate body not preserved', all_passed)
    end subroutine test_associate_with_do_construct

    subroutine test_associate_nested_do_ast(all_passed)
        logical, intent(inout) :: all_passed
        character(len=*), parameter :: source = &
            'MODULE associate_ast_test'//new_line('a')// &
            'TYPE outer_t'//new_line('a')// &
            ' INTEGER :: n'//new_line('a')// &
            ' INTEGER, ALLOCATABLE :: values(:)'//new_line('a')// &
            'END TYPE'//new_line('a')// &
            'TYPE(outer_t) :: outer'//new_line('a')// &
            'CONTAINS'//new_line('a')// &
            'SUBROUTINE run()'//new_line('a')// &
            ' INTEGER :: i, j'//new_line('a')// &
            ' ASSOCIATE(l1 => outer%values)'//new_line('a')// &
            '  DO i = 1, outer%n'//new_line('a')// &
            '   ASSOCIATE(l2 => l1)'//new_line('a')// &
            '    DO j = 1, 2'//new_line('a')// &
            '    END DO'//new_line('a')// &
            '   END ASSOCIATE'//new_line('a')// &
            '  END DO'//new_line('a')// &
            ' END ASSOCIATE'//new_line('a')// &
            'END SUBROUTINE'//new_line('a')// &
            'END MODULE'
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        logical :: found_inner, found_inner_do
        integer :: i, j

        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        call compile_frontend_from_string(source, result, options)
        if (.not. result%parse_ok) then
            write (error_unit, '(A)') &
                'FAIL: uppercase nested ASSOCIATE/DO did not parse'
            all_passed = .false.
            return
        end if

        found_inner = .false.
        found_inner_do = .false.
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (associate_node)
                if (.not. allocated(node%associations)) cycle
                if (size(node%associations) /= 1) cycle
                if (trim(node%associations(1)%name) /= 'l2') cycle
                found_inner = .true.
                if (.not. allocated(node%body_indices)) cycle
                do j = 1, size(node%body_indices)
                    if (.not. result%arena%has_node_at(node%body_indices(j))) cycle
                    select type (body_node => &
                        result%arena%entries(node%body_indices(j))%node)
                    type is (do_loop_node)
                        found_inner_do = .true.
                    class default
                    end select
                end do
            class default
            end select
        end do

        if (.not. found_inner) then
            write (error_unit, '(A)') &
                'FAIL: inner ASSOCIATE node missing from parsed AST'
            all_passed = .false.
        else if (.not. found_inner_do) then
            write (error_unit, '(A)') &
                'FAIL: inner ASSOCIATE body lost its nested DO node'
            all_passed = .false.
        end if
    end subroutine test_associate_nested_do_ast

    subroutine test_block_construct(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lower_output
        type(transform_context_t) :: ctx
        integer :: block_count
        integer :: pos
        integer :: search_start

        call read_example('examples/f90/f2003_block_construct.f90', source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'f2003_block_construct'

        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: block transform error: ' &
                & // trim(error_msg)
            all_passed = .false.
            return
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: block no output produced'
            all_passed = .false.
            return
        end if

        lower_output = to_lower(output_code)

        block_count = 0
        search_start = 1
        do
            pos = index(lower_output(search_start:), 'end block')
            if (pos == 0) exit
            block_count = block_count + 1
            search_start = search_start + pos + 8
        end do

        if (block_count < 2) then
            write (error_unit, '(A,I0,A)') &
                'FAIL: expected 2 block constructs, found ', &
                & block_count, ''
            all_passed = .false.
            return
        end if

        call assert_contains(lower_output, 'real :: partial_sum', &
            & 'FAIL: block-local variable declaration not preserved', all_passed)

    end subroutine test_block_construct

    subroutine assert_contains(text, pattern, failure_message, passed)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message
        logical, intent(inout) :: passed

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            passed = .false.
        end if
    end subroutine assert_contains

end program test_f2003_associate_block
