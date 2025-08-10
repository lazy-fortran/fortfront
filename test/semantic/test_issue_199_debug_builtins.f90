program test_issue_199_debug_builtins
    ! Test to investigate if the issue is related to builtin function detection
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: ctx
    logical :: result
    integer :: i
    character(len=20), dimension(10) :: test_names = [ &
        "undefined_variable  ", "some_random_name    ", "x                   ", &
        "y                   ", "z                   ", "sin                 ", &
        "cos                 ", "sqrt                ", "nonexistent_func    ", &
        "real                "]
    
    print *, "=== Testing Issue #199: Debugging Builtin Function Detection ==="
    print *, ""
    
    arena = create_ast_arena()
    ctx = create_semantic_context()
    
    ! Enter a scope and define a few variables
    call ctx%scopes%enter_block()
    call define_test_variables(ctx)
    
    print *, "Testing various identifiers:"
    print *, ""
    
    do i = 1, size(test_names)
        result = is_identifier_defined_direct(arena, ctx, trim(test_names(i)))
        print *, "  ", trim(test_names(i)), ":", result
    end do
    
    print *, ""
    print *, "Builtin function analysis:"
    call test_builtin_detection(ctx)
    
contains
    
    subroutine define_test_variables(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t) :: int_type, real_type
        type(poly_type_t) :: int_scheme, real_scheme
        
        ! Define some variables
        int_type%kind = TINT
        int_scheme%mono = int_type
        call ctx%scopes%define("x", int_scheme)
        call ctx%scopes%define("y", int_scheme)
        
        real_type%kind = TREAL  
        real_scheme%mono = real_type
        call ctx%scopes%define("z", real_scheme)
        
        print *, "✓ Defined test variables: x (int), y (int), z (real)"
    end subroutine define_test_variables
    
    subroutine test_builtin_detection(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t) :: builtin_type
        character(len=20), dimension(6) :: builtin_names = [ &
            "sin                 ", "cos                 ", "sqrt                ", &
            "undefined_var       ", "random_name         ", "exp                 "]
        integer :: i
        
        print *, ""
        print *, "Direct builtin function type detection:"
        
        do i = 1, size(builtin_names)
            builtin_type = ctx%get_builtin_function_type(trim(builtin_names(i)))
            print *, "  get_builtin_function_type('", trim(builtin_names(i)), "'):"
            print *, "    kind =", builtin_type%kind, "(0=undefined)"
            if (builtin_type%kind /= 0) then
                print *, "    -> BUILTIN DETECTED"
            else
                print *, "    -> NOT A BUILTIN"
            end if
        end do
    end subroutine test_builtin_detection
    
end program test_issue_199_debug_builtins