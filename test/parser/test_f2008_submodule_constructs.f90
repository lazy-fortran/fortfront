program test_f2008_submodule_constructs
    ! Test Fortran 2008 submodule parsing and JSON serialization
    ! ISO/IEC 1539-1:2008 Section 11.2
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
                         ast_arena_t, token_t, get_node_type_at, ast_to_json
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    character(len=:), allocatable :: json_output
    type(transform_context_t) :: ctx
    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    logical :: all_passed

    all_passed = .true.
    print *, 'Testing F2008 submodule constructs (ISO/IEC 1539-1:2008 Section 11.2)'

    call test_simple_submodule(all_passed)
    call test_submodule_with_contains(all_passed)
    call test_submodule_json_serialization(all_passed)

    if (all_passed) then
        print *, 'PASS: F2008 submodule constructs parsed correctly'
    else
        error stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


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

    subroutine test_simple_submodule(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, output, err

        print *, '  Testing simple submodule parsing...'

        call read_example('examples/f90/issue_1827_submodule_simple.f90', source)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'submodule_simple'

        call transform_with_context(source, output, err, ctx)

        if (allocated(err) .and. len_trim(err) > 0) then
            write (error_unit, '(A)') 'FAIL: simple submodule transform error: ' // &
                trim(err)
            passed = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: simple submodule no output produced'
            passed = .false.
            return
        end if

        lower_output = to_lower(output)

        call assert_contains(lower_output, 'submodule', &
            & 'FAIL: submodule keyword not preserved', passed)
        call assert_contains(lower_output, 'parent_module', &
            & 'FAIL: parent module reference not preserved', passed)
        call assert_contains(lower_output, 'child_submodule', &
            & 'FAIL: submodule name not preserved', passed)
        call assert_contains(lower_output, 'end submodule', &
            & 'FAIL: end submodule not preserved', passed)

        if (passed) print *, '    PASS: simple submodule'
    end subroutine test_simple_submodule

    subroutine test_submodule_with_contains(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, output, err

        print *, '  Testing submodule with contains section...'

        call read_example('examples/f90/issue_1827_submodule_with_contents.f90', &
                          source)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'submodule_with_contents'

        call transform_with_context(source, output, err, ctx)

        if (allocated(err) .and. len_trim(err) > 0) then
            write (error_unit, '(A)') 'FAIL: submodule contains transform error: ' // &
                trim(err)
            passed = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: submodule contains no output produced'
            passed = .false.
            return
        end if

        lower_output = to_lower(output)

        call assert_contains(lower_output, 'submodule', &
            & 'FAIL: submodule keyword not preserved', passed)
        call assert_contains(lower_output, 'contains', &
            & 'FAIL: contains section not preserved', passed)
        call assert_contains(lower_output, 'module subroutine test', &
            & 'FAIL: module subroutine not preserved', passed)
        call assert_contains(lower_output, 'end subroutine', &
            & 'FAIL: end subroutine not preserved', passed)

        if (passed) print *, '    PASS: submodule with contains'
    end subroutine test_submodule_with_contains

    subroutine test_submodule_json_serialization(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, '  Testing submodule JSON serialization...'

        call read_example('examples/f90/issue_1827_submodule_simple.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          options, tokens)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: submodule AST load error: ' // &
                trim(error_msg)
            passed = .false.
            return
        end if

        if (root_index <= 0) then
            write (error_unit, '(A)') 'FAIL: submodule root index not set'
            passed = .false.
            return
        end if

        call ast_to_json(arena, root_index, json_output)

        if (.not. allocated(json_output)) then
            write (error_unit, '(A)') 'FAIL: submodule JSON output not produced'
            passed = .false.
            return
        end if

        call assert_contains(json_output, '"type"', &
            & 'FAIL: JSON missing type field', passed)
        call assert_contains(json_output, 'submodule', &
            & 'FAIL: JSON missing submodule type', passed)
        call assert_contains(json_output, '"root":', &
            & 'FAIL: JSON missing root field', passed)
        call assert_contains(json_output, '"nodes":', &
            & 'FAIL: JSON missing nodes array', passed)

        if (passed) print *, '    PASS: submodule JSON serialization'
    end subroutine test_submodule_json_serialization

end program test_f2008_submodule_constructs
