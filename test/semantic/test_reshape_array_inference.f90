program test_reshape_array_inference
    use frontend, only: compile_source, compilation_options_t
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, assignment_node, call_or_subscript_node
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Reshape Array Inference Tests ==='
    print *

    ! Test reshape() array dimension inference
    if (.not. test_reshape_2d_literal_shape()) all_passed = .false.
    if (.not. test_reshape_3d_literal_shape()) all_passed = .false.
    if (.not. test_reshape_1d_literal_shape()) all_passed = .false.
    if (.not. test_reshape_variable_shape()) all_passed = .false.
    if (.not. test_reshape_complex_source()) all_passed = .false.
    if (.not. test_multiple_reshape_calls()) all_passed = .false.

    ! Test standardizer integration for reshape declarations
    if (.not. test_reshape_standardizer_2d()) all_passed = .false.
    if (.not. test_reshape_standardizer_3d()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All reshape array inference tests passed!'
        stop 0
    else
        print *, 'Some reshape array inference tests failed!'
        stop 1
    end if

contains

    logical function test_reshape_2d_literal_shape()
        test_reshape_2d_literal_shape = .true.
        print *, 'Testing reshape 2D literal shape inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Given: Fortran code with reshape() call using literal 2D shape
            call tokenize_core("matrix = reshape([1,2,3,4], [2,2])", tokens)

            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_reshape_2d_literal_shape = .false.
                return
            end if

            ! When: Semantic analysis and type inference occurs
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Then: Variable should be inferred as 2D array
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        ! Check if type indicates 2D array (rank 2)
                        if (index(node%inferred_type_name, 'rank=2') > 0 .or. &
                            index(node%inferred_type_name, '2D') > 0 .or. &
                            index(node%inferred_type_name, 'array(2)') > 0) then
                            print *, '  PASS: 2D array type inferred from reshape'
                            print *, '  INFO: Inferred type: ', node%inferred_type_name
                        else
                            print *, '  FAIL: Expected 2D array type, got: ', &
                                   node%inferred_type_name
                            test_reshape_2d_literal_shape = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for reshape result'
                        test_reshape_2d_literal_shape = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_reshape_2d_literal_shape = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_reshape_2d_literal_shape = .false.
            end if
        end block

    end function test_reshape_2d_literal_shape

    logical function test_reshape_3d_literal_shape()
        test_reshape_3d_literal_shape = .true.
        print *, 'Testing reshape 3D literal shape inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Given: Fortran code with reshape() call using literal 3D shape
            call tokenize_core("cube = reshape([1,2,3,4,5,6,7,8], [2,2,2])", tokens)

            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_reshape_3d_literal_shape = .false.
                return
            end if

            ! When: Semantic analysis and type inference occurs
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Then: Variable should be inferred as 3D array
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        ! Check if type indicates 3D array (rank 3)
                        if (index(node%inferred_type_name, 'rank=3') > 0 .or. &
                            index(node%inferred_type_name, '3D') > 0 .or. &
                            index(node%inferred_type_name, 'array(3)') > 0) then
                            print *, '  PASS: 3D array type inferred from reshape'
                            print *, '  INFO: Inferred type: ', node%inferred_type_name
                        else
                            print *, '  FAIL: Expected 3D array type, got: ', &
                                   node%inferred_type_name
                            test_reshape_3d_literal_shape = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for reshape result'
                        test_reshape_3d_literal_shape = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_reshape_3d_literal_shape = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_reshape_3d_literal_shape = .false.
            end if
        end block

    end function test_reshape_3d_literal_shape

    logical function test_reshape_1d_literal_shape()
        test_reshape_1d_literal_shape = .true.
        print *, 'Testing reshape 1D literal shape inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Given: Fortran code with reshape() call using literal 1D shape
            call tokenize_core("vector = reshape([1,2,3,4], [4])", tokens)

            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_reshape_1d_literal_shape = .false.
                return
            end if

            ! When: Semantic analysis and type inference occurs
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Then: Variable should be inferred as 1D array
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        ! Check if type indicates 1D array (rank 1)
                        if (index(node%inferred_type_name, 'rank=1') > 0 .or. &
                            index(node%inferred_type_name, '1D') > 0 .or. &
                            index(node%inferred_type_name, 'array(1)') > 0) then
                            print *, '  PASS: 1D array type inferred from reshape'
                            print *, '  INFO: Inferred type: ', node%inferred_type_name
                        else
                            print *, '  FAIL: Expected 1D array type, got: ', &
                                   node%inferred_type_name
                            test_reshape_1d_literal_shape = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for reshape result'
                        test_reshape_1d_literal_shape = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_reshape_1d_literal_shape = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_reshape_1d_literal_shape = .false.
            end if
        end block

    end function test_reshape_1d_literal_shape

    logical function test_reshape_variable_shape()
        test_reshape_variable_shape = .true.
        print *, 'Testing reshape variable shape inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Given: Fortran code with reshape() using variable shape reference
            call tokenize_core("result = reshape(source_array, shape_var)", tokens)

            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_reshape_variable_shape = .false.
                return
            end if

            ! When: Semantic analysis and type inference occurs
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Then: Variable should be inferred as array (rank unknown from var)
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        ! For variable shape, we expect array type but unknown rank
                        if (index(node%inferred_type_name, 'array') > 0 .or. &
                            index(node%inferred_type_name, 'unknown_rank') > 0) then
                            print *, '  PASS: Array type inferred for variable shape'
                            print *, '  INFO: Inferred type: ', node%inferred_type_name
                        else
                            print *, '  FAIL: Expected array type, got: ', &
                                   node%inferred_type_name
                            test_reshape_variable_shape = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for reshape result'
                        test_reshape_variable_shape = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_reshape_variable_shape = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_reshape_variable_shape = .false.
            end if
        end block

    end function test_reshape_variable_shape

    logical function test_reshape_complex_source()
        test_reshape_complex_source = .true.
        print *, 'Testing reshape complex source expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Given: Complex source array expression in reshape
            call tokenize_core("matrix = reshape(a + b * 2, [3,3])", tokens)

            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_reshape_complex_source = .false.
                return
            end if

            ! When: Semantic analysis and type inference occurs  
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Then: Should infer array type from shape regardless of source
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        ! Check for 2D array from [3,3] shape
                        if (index(node%inferred_type_name, 'rank=2') > 0 .or. &
                            index(node%inferred_type_name, '2D') > 0 .or. &
                            index(node%inferred_type_name, 'array(2)') > 0) then
                            print *, '  PASS: 2D array inferred from complex source'
                            print *, '  INFO: Inferred type: ', node%inferred_type_name
                        else
                            print *, '  FAIL: Expected 2D array type, got: ', &
                                   node%inferred_type_name
                            test_reshape_complex_source = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for reshape result'
                        test_reshape_complex_source = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_reshape_complex_source = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_reshape_complex_source = .false.
            end if
        end block

    end function test_reshape_complex_source

    logical function test_multiple_reshape_calls()
        test_multiple_reshape_calls = .true.
        print *, 'Testing multiple reshape calls with different dimensions...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt1_index, stmt2_index, stmt3_index
            logical :: first_2d, second_3d, third_1d

            first_2d = .false.
            second_3d = .false.
            third_1d = .false.

            arena = create_ast_arena()
            
            ! Given: Multiple reshape calls with different dimensions
            call tokenize_core("mat2d = reshape([1,2,3,4], [2,2])", tokens)
            stmt1_index = parse_statement_dispatcher(tokens, arena)
            
            call tokenize_core("cube3d = reshape([1,2,3,4,5,6,7,8], [2,2,2])", tokens)
            stmt2_index = parse_statement_dispatcher(tokens, arena)
            
            call tokenize_core("vec1d = reshape([1,2,3,4,5], [5])", tokens)
            stmt3_index = parse_statement_dispatcher(tokens, arena)

            if (stmt1_index <= 0 .or. stmt2_index <= 0 .or. stmt3_index <= 0) then
                print *, '  FAIL: Parse failed for one or more statements'
                test_multiple_reshape_calls = .false.
                return
            end if

            ! When: Semantic analysis processes all statements
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt1_index)
            call analyze_program(ctx, arena, stmt2_index)
            call analyze_program(ctx, arena, stmt3_index)

            ! Then: Each variable should have correct array rank inferred
            ! Check first statement (2D)
            if (allocated(arena%entries(stmt1_index)%node)) then
                select type (node => arena%entries(stmt1_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        if (index(node%inferred_type_name, 'rank=2') > 0 .or. &
                            index(node%inferred_type_name, '2D') > 0) then
                            first_2d = .true.
                        end if
                    end if
                end select
            end if

            ! Check second statement (3D)
            if (allocated(arena%entries(stmt2_index)%node)) then
                select type (node => arena%entries(stmt2_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        if (index(node%inferred_type_name, 'rank=3') > 0 .or. &
                            index(node%inferred_type_name, '3D') > 0) then
                            second_3d = .true.
                        end if
                    end if
                end select
            end if

            ! Check third statement (1D)
            if (allocated(arena%entries(stmt3_index)%node)) then
                select type (node => arena%entries(stmt3_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        if (index(node%inferred_type_name, 'rank=1') > 0 .or. &
                            index(node%inferred_type_name, '1D') > 0) then
                            third_1d = .true.
                        end if
                    end if
                end select
            end if

            if (first_2d .and. second_3d .and. third_1d) then
                print *, '  PASS: All reshape calls inferred correct dimensions'
            else
                if (.not. first_2d) print *, '  FAIL: First reshape not 2D'
                if (.not. second_3d) print *, '  FAIL: Second reshape not 3D' 
                if (.not. third_1d) print *, '  FAIL: Third reshape not 1D'
                test_multiple_reshape_calls = .false.
            end if
        end block

    end function test_multiple_reshape_calls

    logical function test_reshape_standardizer_2d()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_allocatable_2d

        test_reshape_standardizer_2d = .true.
        print *, 'Testing reshape 2D array standardizer integration...'

        ! Given: Fortran code with 2D reshape call
        input_file = 'test_reshape_2d.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'matrix = reshape([1,2,3,4], [2,2])'
        write (unit, '(a)') 'print *, matrix'
        close (unit)

        ! When: Standardization occurs via compilation
        output_file = 'test_reshape_2d_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_reshape_standardizer_2d = .false.
            return
        end if

        ! Then: Should generate allocatable 2D array declaration
        found_allocatable_2d = .false.

        open (newunit=unit, file=output_file, status='old')
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) > 0) print *, '  DEBUG:', trim(line)
            ! Look for allocatable 2D array declaration
            if ((index(line, 'matrix') > 0) .and. &
                (index(line, 'allocatable') > 0) .and. &
                (index(line, '(:,:)') > 0)) then
                found_allocatable_2d = .true.
                print *, '  Found 2D allocatable:', trim(line)
            end if
        end do
        close (unit)

        if (found_allocatable_2d) then
            print *, '  PASS: 2D allocatable array declared for reshape'
        else
            print *, '  FAIL: Expected allocatable(:,:) declaration not found'
            test_reshape_standardizer_2d = .false.
        end if

    end function test_reshape_standardizer_2d

    logical function test_reshape_standardizer_3d()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_allocatable_3d

        test_reshape_standardizer_3d = .true.
        print *, 'Testing reshape 3D array standardizer integration...'

        ! Given: Fortran code with 3D reshape call  
        input_file = 'test_reshape_3d.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'cube = reshape([1,2,3,4,5,6,7,8], [2,2,2])'
        write (unit, '(a)') 'print *, cube'
        close (unit)

        ! When: Standardization occurs via compilation
        output_file = 'test_reshape_3d_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_reshape_standardizer_3d = .false.
            return
        end if

        ! Then: Should generate allocatable 3D array declaration
        found_allocatable_3d = .false.

        open (newunit=unit, file=output_file, status='old')
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) > 0) print *, '  DEBUG:', trim(line)
            ! Look for allocatable 3D array declaration
            if ((index(line, 'cube') > 0) .and. &
                (index(line, 'allocatable') > 0) .and. &
                (index(line, '(:,:,:)') > 0)) then
                found_allocatable_3d = .true.
                print *, '  Found 3D allocatable:', trim(line)
            end if
        end do
        close (unit)

        if (found_allocatable_3d) then
            print *, '  PASS: 3D allocatable array declared for reshape'
        else
            print *, '  FAIL: Expected allocatable(:,:,:) declaration not found'
            test_reshape_standardizer_3d = .false.
        end if

    end function test_reshape_standardizer_3d

end program test_reshape_array_inference