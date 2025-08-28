module semantic_pipeline_integration
    ! Integration layer for semantic pipeline with fortfront
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    implicit none
    private
    
    public :: analyze_semantics_with_pipeline, create_default_semantic_pipeline
    
contains
    
    ! Run semantic analysis using pipeline
    subroutine analyze_semantics_with_pipeline(pipeline, arena, node_index, context)
        type(semantic_pipeline_t), intent(inout) :: pipeline
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(semantic_context_t), intent(inout), optional :: context
        type(semantic_context_t) :: local_context
        
        ! Use provided context or create default
        if (present(context)) then
            local_context = context
        else
            local_context = create_semantic_context()
        end if
        
        ! Run traditional semantic analysis first for compatibility
        call analyze_program(local_context, arena, node_index)
        
        ! Run pipeline analyzers - temporary skip due to type mismatch
        ! TODO: Fix semantic_context_t to extend semantic_context_base_t
        ! call pipeline%run_analysis(arena, node_index, local_context)
        
        ! Return context if provided
        if (present(context)) then
            context = local_context
        end if
    end subroutine analyze_semantics_with_pipeline
    
    ! Create default semantic analysis pipeline
    subroutine create_default_semantic_pipeline(pipeline)
        type(semantic_pipeline_t), intent(out) :: pipeline
        type(symbol_analyzer_t) :: symbol_analyzer
        type(type_analyzer_t) :: type_analyzer
        type(scope_analyzer_t) :: scope_analyzer
        
        ! Create pipeline
        pipeline = create_pipeline()
        
        ! Add core analyzers in dependency order
        call pipeline%add_analyzer(symbol_analyzer, "symbol_analyzer")
        call pipeline%add_analyzer(scope_analyzer, "scope_analyzer")
        call pipeline%add_analyzer(type_analyzer, "type_analyzer")
    end subroutine create_default_semantic_pipeline
    
end module semantic_pipeline_integration