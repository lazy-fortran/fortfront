program test_array_slicing_codegen
    use fortfront, only: analyze_semantics, ast_arena_t, create_ast_arena, &
        emit_fortran, lex_source, parse_tokens, token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_basic_slices()) all_passed = .false.
    if (.not. test_empty_bounds()) all_passed = .false.
    if (.not. test_multidim_slices()) all_passed = .false.
    if (.not. test_slice_type_inference()) all_passed = .false.

    if (all_passed) then
        print *, 'All array slicing codegen tests passed!'
    else
        print *, 'Some array slicing codegen tests failed!'
        stop 1
    end if

contains

    include '../common/read_example.inc'

    logical function test_basic_slices()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root

        test_basic_slices = .true.

        call read_example('examples/f90/array_slicing_basic.f90', source)
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_basic_slices = .false.
                return
            end if
        end if

        call emit_fortran(arena, root, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_basic_slices = .false.
            return
        end if

        if (index(code, 'arr(2:4)') == 0) then
            print *, '  FAIL: missing slice arr(2:4) in code'
            test_basic_slices = .false.
            return
        end if

        if (index(code, '[10, 20, 30]') == 0 .and. &
            index(code, '(/10, 20, 30/)') == 0) then
            print *, '  FAIL: missing array constructor for RHS'
            test_basic_slices = .false.
            return
        end if
    end function test_basic_slices

    logical function test_empty_bounds()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root

        test_empty_bounds = .true.

        call read_example('examples/f90/array_slicing_empty_bounds.f90', source)
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_empty_bounds = .false.
                return
            end if
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_empty_bounds = .false.
            return
        end if

        if (index(code, 'arr(:3)') == 0) then
            print *, '  FAIL: missing arr(:3)'
            test_empty_bounds = .false.
            return
        end if

        if (index(code, 'arr(2:)') == 0) then
            print *, '  FAIL: missing arr(2:)'
            test_empty_bounds = .false.
            return
        end if

        if (index(code, 'arr(:)') == 0) then
            print *, '  FAIL: missing arr(:)'
            test_empty_bounds = .false.
            return
        end if
    end function test_empty_bounds

    logical function test_multidim_slices()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root

        test_multidim_slices = .true.

        call read_example('examples/f90/array_slicing_multidim.f90', source)
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_multidim_slices = .false.
                return
            end if
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_multidim_slices = .false.
            return
        end if

        if (index(code, 'b(1:2, :3)') == 0 .and. &
            index(code, 'b(1:2,:3)') == 0) then
            print *, '  FAIL: missing multidimensional slice b(1:2, :3)'
            test_multidim_slices = .false.
            return
        end if
    end function test_multidim_slices

    logical function test_slice_type_inference()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root

        test_slice_type_inference = .true.

        call read_example('examples/f90/array_slicing_type_inference.f90', source)
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_slice_type_inference = .false.
                return
            end if
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_slice_type_inference = .false.
            return
        end if

        if (index(code, 'integer :: subset(3)') == 0 .and. &
            index(code, 'integer, dimension(3) :: subset') == 0) then
            print *, '  FAIL: subset slice missing explicit extent'
            test_slice_type_inference = .false.
        end if

        if (index(code, 'allocatable :: subset') /= 0) then
            print *, '  FAIL: subset slice incorrectly marked allocatable'
            test_slice_type_inference = .false.
        end if

        if (index(code, 'real :: subset') /= 0 .or. &
            index(code, 'real(dp) :: subset') /= 0 .or. &
            index(code, 'real(8) :: subset') /= 0) then
            print *, '  FAIL: subset slice still inferred as real'
            test_slice_type_inference = .false.
        end if
    end function test_slice_type_inference


end program test_array_slicing_codegen
