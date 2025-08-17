module semantic_pipeline_integration
    ! Integration layer between new semantic pipeline and existing API
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                analyze_program
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    implicit none
    private

    public :: analyze_semantics_with_pipeline
    public :: create_default_semantic_pipeline

contains

    ! Enhanced semantic analysis using plugin pipeline
    subroutine analyze_semantics_with_pipeline(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(semantic_context_t), intent(out), optional :: context
        
        type(semantic_pipeline_t) :: pipeline
        type(symbol_analyzer_t) :: symbol_analyzer
        
        ! Create pipeline with default analyzers
        pipeline = create_default_semantic_pipeline()
        
        ! Run analysis
        call pipeline%run_analysis(arena, root_index)
        
        ! Return context if requested
        if (present(context)) then
            ! Get context from symbol analyzer
            block
                class(*), allocatable :: results
                results = symbol_analyzer%get_results()
                select type(results)
                type is (semantic_context_t)
                    context = results
                end select
            end block
        end if
    end subroutine

    ! Create default semantic pipeline with built-in analyzers
    function create_default_semantic_pipeline() result(pipeline)
        type(semantic_pipeline_t) :: pipeline
        
        ! Create pipeline
        pipeline = create_pipeline()
        
        ! Register built-in analyzers in dependency order
        call pipeline%register_analyzer(symbol_analyzer_t())
        call pipeline%register_analyzer(scope_analyzer_t())
        call pipeline%register_analyzer(type_analyzer_t())
    end function

    ! Backward-compatible wrapper for existing analyze_semantics
    subroutine analyze_semantics_legacy_wrapper(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        
        ! Use new pipeline but maintain same interface
        call analyze_semantics_with_pipeline(arena, root_index)
    end subroutine

end module semantic_pipeline_integration