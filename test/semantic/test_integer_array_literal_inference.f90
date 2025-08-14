program test_integer_array_literal_inference
    use frontend, only: compile_source, compilation_options_t
    use semantic_analyzer, only: analyze_program, create_semantic_context, &
                                 semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, assignment_node
    use lexer_core, only: tokenize_core, token_t
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Integer Array Literal Type Inference Tests ==='
    print *, 'Testing issue #206: Type inference defaults to real(8) for '// &
             'integer array literals'
    print *

    ! Test semantic analysis level
    if (.not. test_simple_integer_array_literal()) all_passed = .false.
    if (.not. test_single_element_integer_array()) all_passed = .false.
    if (.not. test_integer_variable_array()) all_passed = .false.
    if (.not. test_mixed_integer_expressions()) all_passed = .false.
    if (.not. test_negative_integer_literals()) all_passed = .false.
    if (.not. test_integer_vs_real_arrays()) all_passed = .false.

    ! Test end-to-end standardizer output  
    if (.not. test_standardizer_integer_slice_declarations()) &
        all_passed = .false.
    if (.not. test_standardizer_real_slice_declarations()) &
        all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All integer array literal inference tests passed!'
        stop 0
    else
        print *, 'Some integer array literal inference tests failed!'
        print *, 'Issue #206 is NOT YET FIXED'
        stop 1
    end if

contains

    logical function test_simple_integer_array_literal()
        ! Given: Fortran code with simple integer array literal
        ! When: Semantic analysis occurs
        ! Then: Array should be inferred as integer type, not real(8)
        
        test_simple_integer_array_literal = .true.
        print *, 'Testing simple integer array literal semantic inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [1, 2, 3]"
            call tokenize_core("arr = [1, 2, 3]", tokens)
            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  INFO: Inferred type: ', &
                                 node%inferred_type_name
                        if (index(node%inferred_type_name, "integer") > 0) then
                            print *, '  PASS: Correctly inferred as integer'
                        else
                            print *, '  FAIL: Expected integer, got: ', &
                                     node%inferred_type_name
                            test_simple_integer_array_literal = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred'
                        test_simple_integer_array_literal = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_simple_integer_array_literal = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_simple_integer_array_literal = .false.
            end if
            
            call arena%clear()
        end block

    end function test_simple_integer_array_literal

    logical function test_single_element_integer_array()
        ! Given: Fortran code with single element integer array literal
        ! When: Semantic analysis occurs
        ! Then: Array should be inferred as integer type, not real(8)
        
        test_single_element_integer_array = .true.
        print *, 'Testing single element integer array inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [10]"
            call tokenize_core("arr = [10]", tokens)
            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  INFO: Inferred type: ', &
                                 node%inferred_type_name
                        if (index(node%inferred_type_name, "integer") > 0) then
                            print *, '  PASS: Single element correctly ' // &
                                     'inferred as integer'
                        else
                            print *, '  FAIL: Expected integer for [10], ' // &
                                     'got: ', node%inferred_type_name
                            test_single_element_integer_array = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for single element'
                        test_single_element_integer_array = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_single_element_integer_array = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_single_element_integer_array = .false.
            end if
            
            call arena%clear()
        end block

    end function test_single_element_integer_array

    logical function test_integer_variable_array()
        ! Given: Fortran code with integer variables in array literal
        ! When: Semantic analysis occurs  
        ! Then: Array should be inferred as integer type, not real(8)
        
        test_integer_variable_array = .true.
        print *, 'Testing integer variable array inference...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg, line
            type(compilation_options_t) :: options
            integer :: unit, iostat
            logical :: found_v_int, found_arr_real, found_arr_int

            ! Create test with integer variable in array
            input_file = 'test_var_arr.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'v = 5'
            write (unit, '(a)') 'arr = [v, v**2]'
            close (unit)

            output_file = 'test_var_arr_out.f90'
            options%output_file = output_file
            call compile_source(input_file, options, error_msg)

            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_integer_variable_array = .false.
                return
            end if

            found_v_int = .false.
            found_arr_real = .false.
            found_arr_int = .false.

            open (newunit=unit, file=output_file, status='old')
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'integer') > 0 .and. index(line, ' v') > 0) then
                    found_v_int = .true.
                end if
                if (index(line, 'real(8)') > 0 .and. index(line, 'arr') > 0) then
                    found_arr_real = .true.
                end if
                if (index(line, 'integer') > 0 .and. index(line, 'arr') > 0) then
                    found_arr_int = .true.
                end if
            end do
            close (unit)

            if (found_arr_real) then
                print *, '  FAIL: Variable array incorrectly inferred ' // &
                         'as real(8)'
                test_integer_variable_array = .false.
            else if (found_arr_int) then
                print *, '  PASS: Variable array correctly inferred ' // &
                         'as integer'
            else
                print *, '  UNCLEAR: Array type not determined'
                test_integer_variable_array = .false.
            end if
        end block

    end function test_integer_variable_array

    logical function test_mixed_integer_expressions()
        ! Given: Fortran code with complex integer expressions in array
        ! When: Semantic analysis occurs
        ! Then: Array should be inferred as integer type, not real(8)
        
        test_mixed_integer_expressions = .true.
        print *, 'Testing mixed integer expressions array inference...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg, line
            type(compilation_options_t) :: options
            integer :: unit, iostat
            logical :: found_real_arr, found_int_arr

            input_file = 'test_mixed_expr.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'a = 10'
            write (unit, '(a)') 'b = 20'
            write (unit, '(a)') 'result = [a + b, a * 2, b - a]'
            close (unit)

            output_file = 'test_mixed_expr_out.f90'
            options%output_file = output_file
            call compile_source(input_file, options, error_msg)

            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_mixed_integer_expressions = .false.
                return
            end if

            found_real_arr = .false.
            found_int_arr = .false.

            open (newunit=unit, file=output_file, status='old')
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'real(8)') > 0 .and. &
                    index(line, 'result') > 0) then
                    found_real_arr = .true.
                end if
                if (index(line, 'integer') > 0 .and. &
                    index(line, 'result') > 0) then
                    found_int_arr = .true.
                end if
            end do
            close (unit)

            if (found_real_arr) then
                print *, '  FAIL: Mixed integer expressions incorrectly ' // &
                         'inferred as real(8)'
                test_mixed_integer_expressions = .false.
            else if (found_int_arr) then
                print *, '  PASS: Mixed integer expressions correctly ' // &
                         'inferred as integer'
            else
                print *, '  UNCLEAR: Expression array type not determined'
                test_mixed_integer_expressions = .false.
            end if
        end block

    end function test_mixed_integer_expressions

    logical function test_negative_integer_literals()
        ! Given: Fortran code with negative integer literals in array
        ! When: Semantic analysis occurs
        ! Then: Array should be inferred as integer type, not real(8)
        
        test_negative_integer_literals = .true.
        print *, 'Testing negative integer literals array inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [-1, -2, -3]"
            call tokenize_core("arr = [-1, -2, -3]", tokens)
            arena = create_ast_arena()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  INFO: Inferred type: ', &
                                 node%inferred_type_name
                        if (index(node%inferred_type_name, "integer") > 0) then
                            print *, '  PASS: Negative integers correctly ' // &
                                     'inferred as integer'
                        else
                            print *, '  FAIL: Expected integer for ' // &
                                     '[-1, -2, -3], got: ', &
                                     node%inferred_type_name
                            test_negative_integer_literals = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred for negative ' // &
                                 'literals'
                        test_negative_integer_literals = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_negative_integer_literals = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_negative_integer_literals = .false.
            end if
            
            call arena%clear()
        end block

    end function test_negative_integer_literals

    logical function test_integer_vs_real_arrays()
        ! Given: Both integer and real array literals
        ! When: Semantic analysis occurs
        ! Then: Integer arrays should be integer, real arrays should be real
        
        test_integer_vs_real_arrays = .true.
        print *, 'Testing integer vs real array comparison...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg, line
            type(compilation_options_t) :: options
            integer :: unit, iostat
            logical :: found_int_arr, found_real_arr

            input_file = 'test_int_vs_real.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'int_arr = [1, 2, 3]'
            write (unit, '(a)') 'real_arr = [1.0, 2.0, 3.0]'
            close (unit)

            output_file = 'test_int_vs_real_out.f90'
            options%output_file = output_file
            call compile_source(input_file, options, error_msg)

            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_integer_vs_real_arrays = .false.
                return
            end if

            found_int_arr = .false.
            found_real_arr = .false.

            open (newunit=unit, file=output_file, status='old')
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'integer') > 0 .and. &
                    index(line, 'int_arr') > 0) then
                    found_int_arr = .true.
                end if
                if (index(line, 'real') > 0 .and. &
                    index(line, 'real_arr') > 0) then
                    found_real_arr = .true.
                end if
            end do
            close (unit)

            if (found_int_arr .and. found_real_arr) then
                print *, '  PASS: Both integer and real arrays correctly ' // &
                         'distinguished'
            else
                if (.not. found_int_arr) then
                    print *, '  FAIL: Integer array not correctly inferred'
                end if
                if (.not. found_real_arr) then
                    print *, '  FAIL: Real array not correctly inferred'
                end if
                test_integer_vs_real_arrays = .false.
            end if
        end block

    end function test_integer_vs_real_arrays

    logical function test_standardizer_integer_slice_declarations()
        ! Given: Integer array slice assignment
        ! When: Standardization occurs
        ! Then: Should generate integer, allocatable :: slice(:), 
        !       NOT real(8), allocatable
        
        test_standardizer_integer_slice_declarations = .true.
        print *, 'Testing standardizer integer slice declarations ' // &
                 '(MAIN BUG)...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg, line
            type(compilation_options_t) :: options
            integer :: unit, iostat
            logical :: found_slice_real, found_slice_int

            input_file = 'test_int_slice_std.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'arr = [1, 2, 3, 4, 5]'
            write (unit, '(a)') 'slice = arr(2:4)'  ! This is the failing case
            close (unit)

            output_file = 'test_int_slice_std_out.f90'
            options%output_file = output_file
            call compile_source(input_file, options, error_msg)

            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_standardizer_integer_slice_declarations = .false.
                return
            end if

            found_slice_real = .false.
            found_slice_int = .false.

            open (newunit=unit, file=output_file, status='old')
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                print *, '  DEBUG:', trim(line)
                if (index(line, 'real(8)') > 0 .and. &
                    index(line, 'slice') > 0) then
                    found_slice_real = .true.
                    print *, '  BUG DETECTED: slice declared as real(8)'
                end if
                if (index(line, 'integer') > 0 .and. &
                    index(line, 'slice') > 0) then
                    found_slice_int = .true.
                end if
            end do
            close (unit)

            ! This test MUST FAIL until the bug is fixed
            if (found_slice_real) then
                print *, '  FAIL: Integer slice incorrectly declared ' // &
                         'as real(8) - BUG CONFIRMED'
                test_standardizer_integer_slice_declarations = .false.
            else if (found_slice_int) then
                print *, '  PASS: Integer slice correctly declared ' // &
                         'as integer - BUG FIXED!'
            else
                print *, '  UNCLEAR: Slice declaration not found'
                test_standardizer_integer_slice_declarations = .false.
            end if
        end block

    end function test_standardizer_integer_slice_declarations

    logical function test_standardizer_real_slice_declarations()
        ! Given: Real array slice assignment
        ! When: Standardization occurs
        ! Then: Should still generate real(8), allocatable :: slice(:)
        
        test_standardizer_real_slice_declarations = .true.
        print *, 'Testing standardizer real slice declarations ' // &
                 '(should work)...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg, line
            type(compilation_options_t) :: options
            integer :: unit, iostat
            logical :: found_slice_real

            input_file = 'test_real_slice_std.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'arr = [1.0, 2.5, 3.14, 4.2, 5.1]'
            write (unit, '(a)') 'slice = arr(2:4)'  ! Real slice should work
            close (unit)

            output_file = 'test_real_slice_std_out.f90'
            options%output_file = output_file
            call compile_source(input_file, options, error_msg)

            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_standardizer_real_slice_declarations = .false.
                return
            end if

            found_slice_real = .false.

            open (newunit=unit, file=output_file, status='old')
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'real') > 0 .and. index(line, 'slice') > 0) then
                    found_slice_real = .true.
                end if
            end do
            close (unit)

            if (found_slice_real) then
                print *, '  PASS: Real slice correctly declared as real'
            else
                print *, '  FAIL: Real slice declaration not found'
                test_standardizer_real_slice_declarations = .false.
            end if
        end block

    end function test_standardizer_real_slice_declarations

end program test_integer_array_literal_inference