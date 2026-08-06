program test_issue_669_nested_character_substring
    use fortfront_compiler, only: compiler_frontend_options_t, &
                                  compiler_frontend_result_t, &
                                  compile_frontend_from_string, &
                                  INPUT_MODE_STANDARD
    use ast_nodes_bounds, only: array_slice_node
    use ast_nodes_core, only: call_or_subscript_node, literal_node
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: source
    integer :: index, slice_count
    logical :: all_passed

    call read_example('examples/f90/issue_669_nested_character_substring.f90', &
                      source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)

    all_passed = result%success()
    if (.not. all_passed) then
        print *, 'FAIL: frontend rejected nested character substring:'
        print *, trim(result%diagnostic_text)
        error stop 1
    end if

    slice_count = 0
    do index = 1, result%arena%size
        if (.not. allocated(result%arena%entries(index)%node)) cycle
        select type (slice => result%arena%entries(index)%node)
        type is (array_slice_node)
            if (slice%num_dimensions /= 1) cycle
            if (slice%array_index <= 0) cycle
            if (.not. allocated(result%arena%entries(slice%array_index)%node)) cycle
            select type (base => result%arena%entries(slice%array_index)%node)
            type is (call_or_subscript_node)
                if (allocated(base%name) .and. trim(base%name) == 'c' .and. &
                    allocated(base%arg_indices) .and. &
                    size(base%arg_indices) == 1) then
                    select type (arg => result%arena%entries( &
                        base%arg_indices(1))%node)
                    type is (literal_node)
                        if (trim(arg%value) == '2') slice_count = slice_count + 1
                    end select
                end if
            end select
        end select
    end do

    if (slice_count /= 4) then
        print *, 'FAIL: expected four c(2) substring slices, found', slice_count
        error stop 1
    end if
    print *, 'PASS: nested character-array substrings retain their designator base'

contains

    include '../common/read_example.inc'

end program test_issue_669_nested_character_substring
