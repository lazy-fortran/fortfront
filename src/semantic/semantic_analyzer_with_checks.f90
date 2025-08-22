module semantic_analyzer_with_checks
    ! Enhanced semantic analyzer with validation checks
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use ast_core, only: ast_arena_t
    implicit none
    
    public :: analyze_with_checks
    
contains
    
    ! Analyze with additional validation checks
    subroutine analyze_with_checks(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        
        ! For now, just delegate to standard analyzer
        call analyze_program(ctx, arena, root_index)
    end subroutine analyze_with_checks
    
end module semantic_analyzer_with_checks