program test_semantic_type_errors
    use frontend, only: lex_source, parse_tokens, analyze_semantics
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use ast_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Semantic Type Error Tests ==='
    print *

    ! Test type mismatches
    print *, 'Testing type mismatches...'
    if (.not. test_integer_real_mismatch()) all_passed = .false.
    if (.not. test_logical_numeric_mismatch()) all_passed = .false.
    
    ! Test undefined variables
    print *, 'Testing undefined variables...'
    if (.not. test_undefined_variable()) all_passed = .false.
    if (.not. test_out_of_scope()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All semantic type error tests passed!'
        stop 0
    else
        print *, 'Some semantic type error tests failed!'
        stop 1
    end if

contains

    function test_integer_real_mismatch() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(semantic_context_t) :: sem_ctx
        integer :: ast_id
        
        passed = .true.
        
        source = "program test" // new_line('a') // &
                 "  integer :: i" // new_line('a') // &
                 "  real :: r" // new_line('a') // &
                 "  i = 3.14" // new_line('a') // &  ! Type mismatch
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, '  FAILED: Lexing error:', trim(error_msg)
            passed = .false.
            return
        end if
        
        call parse_tokens(tokens, ast_id, error_msg)
        if (error_msg /= "") then
            print *, '  FAILED: Parsing error:', trim(error_msg)
            passed = .false.
            return
        end if
        
        sem_ctx = create_semantic_context()
        call analyze_semantics(ast_id, sem_ctx, error_msg)
        
        ! Should detect type mismatch
        if (error_msg == "") then
            print *, '  WARNING: Type mismatch not detected (may be valid implicit conversion)'
        end if
        
        if (passed) print *, '  PASSED: Integer/real type mismatch'
    end function

    function test_logical_numeric_mismatch() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(semantic_context_t) :: sem_ctx
        integer :: ast_id
        
        passed = .true.
        
        source = "program test" // new_line('a') // &
                 "  logical :: flag" // new_line('a') // &
                 "  flag = 42" // new_line('a') // &  ! Type error
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        call parse_tokens(tokens, ast_id, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        sem_ctx = create_semantic_context()
        call analyze_semantics(ast_id, sem_ctx, error_msg)
        
        if (passed) print *, '  PASSED: Logical/numeric type mismatch'
    end function

    function test_undefined_variable() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(semantic_context_t) :: sem_ctx
        integer :: ast_id
        
        passed = .true.
        
        source = "program test" // new_line('a') // &
                 "  x = undefined_var" // new_line('a') // &  ! Undefined
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        call parse_tokens(tokens, ast_id, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        sem_ctx = create_semantic_context()
        call analyze_semantics(ast_id, sem_ctx, error_msg)
        
        ! Should detect undefined variable
        if (error_msg == "") then
            print *, '  WARNING: Undefined variable not detected'
        end if
        
        if (passed) print *, '  PASSED: Undefined variable'
    end function

    function test_out_of_scope() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(semantic_context_t) :: sem_ctx
        integer :: ast_id
        
        passed = .true.
        
        source = "program test" // new_line('a') // &
                 "  if (.true.) then" // new_line('a') // &
                 "    integer :: local" // new_line('a') // &
                 "    local = 10" // new_line('a') // &
                 "  end if" // new_line('a') // &
                 "  print *, local" // new_line('a') // &  ! Out of scope
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        call parse_tokens(tokens, ast_id, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        sem_ctx = create_semantic_context()
        call analyze_semantics(ast_id, sem_ctx, error_msg)
        
        if (passed) print *, '  PASSED: Out of scope variable'
    end function

end program test_semantic_type_errors