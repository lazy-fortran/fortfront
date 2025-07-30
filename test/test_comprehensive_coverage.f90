program test_comprehensive_coverage
    use frontend, only: lex_file, parse_tokens, analyze_semantics, emit_fortran
    use ast_core
    use lexer_core, only: token_t
    use codegen_core, only: generate_code_from_arena
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Comprehensive Coverage Tests ==='
    print *

    ! Run tests that exercise major code paths
    if (.not. test_lexer_coverage()) all_passed = .false.
    if (.not. test_parser_coverage()) all_passed = .false.
    if (.not. test_semantic_coverage()) all_passed = .false.
    if (.not. test_codegen_coverage()) all_passed = .false.
    if (.not. test_full_pipeline_coverage()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All comprehensive coverage tests passed!'
        stop 0
    else
        print *, 'Some comprehensive coverage tests failed!'
        stop 1
    end if

contains

    logical function test_lexer_coverage()
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        character(len=256) :: error_msg
        
        test_lexer_coverage = .true.
        print *, 'Testing lexer coverage...'
        
        ! Test comprehensive Fortran code that exercises lexer paths
        code = &
            "module math_operations" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    private" // new_line('a') // &  
            "    public :: add, subtract, multiply, divide" // new_line('a') // &
            "" // new_line('a') // &
            "    interface operator(+)" // new_line('a') // &
            "        module procedure vector_add" // new_line('a') // &
            "    end interface" // new_line('a') // &
            "" // new_line('a') // &
            "    type :: vector3d" // new_line('a') // &
            "        real :: x = 0.0_8, y = 0.0_8, z = 0.0_8" // new_line('a') // &
            "    end type vector3d" // new_line('a') // &
            "" // new_line('a') // &
            "contains" // new_line('a') // &
            "" // new_line('a') // &
            "    pure elemental function add(a, b) result(c)" // new_line('a') // &
            "        real, intent(in) :: a, b" // new_line('a') // &
            "        real :: c" // new_line('a') // &
            "        c = a + b  ! Arithmetic" // new_line('a') // &
            "    end function add" // new_line('a') // &
            "" // new_line('a') // &
            "    function vector_add(v1, v2) result(v3)" // new_line('a') // &
            "        type(vector3d), intent(in) :: v1, v2" // new_line('a') // &
            "        type(vector3d) :: v3" // new_line('a') // &
            "        v3%x = v1%x + v2%x" // new_line('a') // &
            "        v3%y = v1%y + v2%y" // new_line('a') // &
            "        v3%z = v1%z + v2%z" // new_line('a') // &
            "    end function vector_add" // new_line('a') // &
            "" // new_line('a') // &
            "    subroutine compute(arr, n, result)" // new_line('a') // &
            "        integer, intent(in) :: n" // new_line('a') // &
            "        real, dimension(n), intent(in) :: arr" // new_line('a') // &
            "        real, intent(out) :: result" // new_line('a') // &
            "        integer :: i" // new_line('a') // &
            "        real :: sum_val = 0.0" // new_line('a') // &
            "" // new_line('a') // &
            "        ! Complex expressions and control flow" // new_line('a') // &
            "        do i = 1, n" // new_line('a') // &
            "            if (arr(i) > 0.0 .and. arr(i) < 100.0) then" // new_line('a') // &
            "                sum_val = sum_val + arr(i) ** 2" // new_line('a') // &
            "            else if (arr(i) <= 0.0) then" // new_line('a') // &
            "                sum_val = sum_val - abs(arr(i))" // new_line('a') // &
            "            else" // new_line('a') // &
            "                sum_val = sum_val + sqrt(arr(i))" // new_line('a') // &
            "            end if" // new_line('a') // &
            "        end do" // new_line('a') // &
            "" // new_line('a') // &
            "        select case (n)" // new_line('a') // &
            "        case (1:10)" // new_line('a') // &
            "            result = sum_val" // new_line('a') // &
            "        case (11:100)" // new_line('a') // &
            "            result = sum_val / real(n)" // new_line('a') // &
            "        case default" // new_line('a') // &
            "            result = 0.0" // new_line('a') // &
            "        end select" // new_line('a') // &
            "" // new_line('a') // &
            "        ! WHERE construct" // new_line('a') // &
            "        where (arr > 50.0)" // new_line('a') // &
            "            ! This exercises array operations" // new_line('a') // &
            "        end where" // new_line('a') // &
            "" // new_line('a') // &
            "    end subroutine compute" // new_line('a') // &
            "" // new_line('a') // &
            "end module math_operations"
        
        call lex_file(code, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Lexer error:', trim(error_msg)
            test_lexer_coverage = .false.
        else
            print *, '  PASSED: Lexer handled comprehensive code (' // &
                    trim(int_to_string(size(tokens))) // ' tokens)'
        end if
    end function

    logical function test_parser_coverage()
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        character(len=256) :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        test_parser_coverage = .true.
        print *, 'Testing parser coverage...'
        
        ! Test comprehensive parsing
        code = &
            "program parser_test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i, j, k" // new_line('a') // &
            "    real, dimension(10) :: array = [(real(i), i=1,10)]" // new_line('a') // &
            "    real, allocatable :: dynamic(:,:)" // new_line('a') // &
            "    logical :: flag = .true." // new_line('a') // &
            "    character(len=*), parameter :: msg = 'Test'" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Test various constructs and expressions" // new_line('a') // &
            "    allocate(dynamic(5, 5))" // new_line('a') // &
            "    dynamic = 0.0" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Nested loops and complex conditions" // new_line('a') // &
            "    outer: do i = 1, 10" // new_line('a') // &
            "        inner: do j = 1, 5" // new_line('a') // &
            "            if (array(i) > 5.0 .and. flag) then" // new_line('a') // &
            "                k = int(array(i) * sin(real(j)))" // new_line('a') // &
            "                if (k > 10) exit outer" // new_line('a') // &
            "                if (k < 0) cycle inner" // new_line('a') // &
            "                dynamic(i,j) = real(k)" // new_line('a') // &
            "            end if" // new_line('a') // &
            "        end do inner" // new_line('a') // &
            "    end do outer" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Function calls and array operations" // new_line('a') // &
            "    array(1:5) = array(6:10) + sqrt(array(1:5))" // new_line('a') // &
            "    k = maxval(array) + minval(dynamic)" // new_line('a') // &
            "" // new_line('a') // &
            "    ! String operations" // new_line('a') // &
            "    print *, msg // ' completed with result: ', k" // new_line('a') // &
            "" // new_line('a') // &
            "    deallocate(dynamic)" // new_line('a') // &
            "end program parser_test"
        
        call lex_file(code, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Lexer error for parser test:', trim(error_msg)
            test_parser_coverage = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Parser error:', trim(error_msg)
            test_parser_coverage = .false.
        else
            print *, '  PASSED: Parser handled comprehensive constructs'
        end if
    end function

    logical function test_semantic_coverage()
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        character(len=256) :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        test_semantic_coverage = .true.
        print *, 'Testing semantic analysis coverage...'
        
        ! Test semantic analysis with type inference challenges
        code = &
            "program semantic_test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Mixed type assignments and type inference" // new_line('a') // &
            "    a = 42" // new_line('a') // &  ! Integer inference
            "    b = 3.14159" // new_line('a') // &  ! Real inference
            "    c = a + b" // new_line('a') // &  ! Type promotion
            "    d = .true." // new_line('a') // &  ! Logical inference
            "    e = 'hello world'" // new_line('a') // &  ! String inference
            "" // new_line('a') // &
            "    ! Array type inference" // new_line('a') // &
            "    arr1 = [1, 2, 3, 4, 5]" // new_line('a') // &  ! Integer array
            "    arr2 = [1.0, 2.0, 3.0]" // new_line('a') // &  ! Real array  
            "    arr3 = arr1 + arr2" // new_line('a') // &  ! Mixed array operations
            "" // new_line('a') // &
            "    ! Function calls with type inference" // new_line('a') // &
            "    f = sin(b)" // new_line('a') // &  ! Real function
            "    g = abs(a)" // new_line('a') // &  ! Integer function
            "    h = sqrt(c)" // new_line('a') // &  ! Type promotion in function
            "" // new_line('a') // &
            "    ! Complex expressions" // new_line('a') // &
            "    result = (a * b + sqrt(c)) / (f + g)" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Array intrinsic functions" // new_line('a') // &
            "    sum_val = sum(arr1)" // new_line('a') // &
            "    max_val = maxval(arr2)" // new_line('a') // &
            "    size_val = size(arr3)" // new_line('a') // &
            "" // new_line('a') // &
            "    print *, 'Results:', result, sum_val, max_val, size_val" // new_line('a') // &
            "end program semantic_test"
        
        call lex_file(code, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Lexer error for semantic test:', trim(error_msg)
            test_semantic_coverage = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Parser error for semantic test:', trim(error_msg)
            test_semantic_coverage = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_semantics(arena, prog_index)
        
        print *, '  PASSED: Semantic analysis completed without errors'
    end function

    logical function test_codegen_coverage()
        character(len=:), allocatable :: code, generated_code
        type(token_t), allocatable :: tokens(:)
        character(len=256) :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        test_codegen_coverage = .true.
        print *, 'Testing code generation coverage...'
        
        ! Test comprehensive code generation
        code = &
            "program codegen_test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i, n = 10" // new_line('a') // &
            "    real, dimension(10) :: data" // new_line('a') // &
            "    real :: sum_value = 0.0" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Initialize array" // new_line('a') // &
            "    do i = 1, n" // new_line('a') // &
            "        data(i) = real(i) * 1.5" // new_line('a') // &
            "    end do" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Compute sum with conditions" // new_line('a') // &
            "    do i = 1, n" // new_line('a') // &
            "        if (data(i) > 5.0) then" // new_line('a') // &
            "            sum_value = sum_value + data(i)" // new_line('a') // &
            "        else" // new_line('a') // &
            "            sum_value = sum_value + data(i) / 2.0" // new_line('a') // &
            "        end if" // new_line('a') // &
            "    end do" // new_line('a') // &
            "" // new_line('a') // &
            "    ! Output result" // new_line('a') // &
            "    write(*, '(a,f10.2)') 'Total sum: ', sum_value" // new_line('a') // &
            "end program codegen_test"
        
        call lex_file(code, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Lexer error for codegen test:', trim(error_msg)
            test_codegen_coverage = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Parser error for codegen test:', trim(error_msg)
            test_codegen_coverage = .false.
            return
        end if
        
        call analyze_semantics(arena, prog_index)
        
        ! Generate code
        call emit_fortran(arena, prog_index, generated_code)
        
        if (len(generated_code) > 0) then
            print *, '  PASSED: Code generation produced output (' // &
                    trim(int_to_string(len(generated_code))) // ' chars)'
        else
            print *, '  FAILED: Code generation produced no output'
            test_codegen_coverage = .false.
        end if
    end function

    logical function test_full_pipeline_coverage()
        character(len=:), allocatable :: code, generated_code
        type(token_t), allocatable :: tokens(:)
        character(len=256) :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        test_full_pipeline_coverage = .true.
        print *, 'Testing full pipeline coverage...'
        
        ! Test a realistic Fortran module through the full pipeline
        code = &
            "module statistics" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    private" // new_line('a') // &
            "    public :: mean, variance, standard_deviation" // new_line('a') // &
            "" // new_line('a') // &
            "contains" // new_line('a') // &
            "" // new_line('a') // &
            "    function mean(data) result(avg)" // new_line('a') // &
            "        real, dimension(:), intent(in) :: data" // new_line('a') // &
            "        real :: avg" // new_line('a') // &
            "        avg = sum(data) / real(size(data))" // new_line('a') // &
            "    end function mean" // new_line('a') // &
            "" // new_line('a') // &
            "    function variance(data) result(var)" // new_line('a') // &
            "        real, dimension(:), intent(in) :: data" // new_line('a') // &
            "        real :: var, avg" // new_line('a') // &
            "        integer :: i, n" // new_line('a') // &
            "        " // new_line('a') // &
            "        n = size(data)" // new_line('a') // &
            "        avg = mean(data)" // new_line('a') // &
            "        var = 0.0" // new_line('a') // &
            "        " // new_line('a') // &
            "        do i = 1, n" // new_line('a') // &
            "            var = var + (data(i) - avg)**2" // new_line('a') // &
            "        end do" // new_line('a') // &
            "        " // new_line('a') // &
            "        var = var / real(n - 1)" // new_line('a') // &
            "    end function variance" // new_line('a') // &
            "" // new_line('a') // &
            "    function standard_deviation(data) result(std)" // new_line('a') // &
            "        real, dimension(:), intent(in) :: data" // new_line('a') // &
            "        real :: std" // new_line('a') // &
            "        std = sqrt(variance(data))" // new_line('a') // &
            "    end function standard_deviation" // new_line('a') // &
            "" // new_line('a') // &
            "end module statistics" // new_line('a') // &
            "" // new_line('a') // &
            "program test_stats" // new_line('a') // &
            "    use statistics" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    " // new_line('a') // &
            "    real, parameter :: data_set(5) = [1.0, 2.0, 3.0, 4.0, 5.0]" // new_line('a') // &
            "    real :: avg, var, std" // new_line('a') // &
            "    " // new_line('a') // &
            "    avg = mean(data_set)" // new_line('a') // &
            "    var = variance(data_set)" // new_line('a') // &
            "    std = standard_deviation(data_set)" // new_line('a') // &
            "    " // new_line('a') // &
            "    print *, 'Mean:', avg" // new_line('a') // &
            "    print *, 'Variance:', var" // new_line('a') // &
            "    print *, 'Standard Deviation:', std" // new_line('a') // &
            "    " // new_line('a') // &
            "end program test_stats"
        
        ! Full pipeline: Lex -> Parse -> Semantic -> Codegen
        call lex_file(code, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Lexer error in full pipeline:', trim(error_msg)
            test_full_pipeline_coverage = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAILED: Parser error in full pipeline:', trim(error_msg)
            test_full_pipeline_coverage = .false.
            return
        end if
        
        call analyze_semantics(arena, prog_index)
        call emit_fortran(arena, prog_index, generated_code)
        
        if (len(generated_code) > 100) then
            print *, '  PASSED: Full pipeline completed successfully'
        else
            print *, '  FAILED: Full pipeline produced insufficient output'
            test_full_pipeline_coverage = .false.
        end if
    end function

    ! Helper function to convert integer to string
    function int_to_string(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(i0)') i
        str = trim(str)
    end function int_to_string

end program test_comprehensive_coverage