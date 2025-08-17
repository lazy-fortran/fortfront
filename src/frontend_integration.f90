module frontend_integration
    ! Higher-level compilation interface for fortfront
    ! Provides a unified compilation result type and interface for integration testing
    
    use frontend, only: transform_lazy_fortran_string
    use ast_core, only: ast_arena_t, init_ast_arena
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    implicit none
    private
    
    public :: compilation_result_t, compile_source
    
    ! Unified compilation result type
    type :: compilation_result_t
        logical :: success = .false.
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: generated_code
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_context
    end type compilation_result_t
    
contains
    
    function compile_source(source_code) result(result)
        character(len=*), intent(in) :: source_code
        type(compilation_result_t) :: result
        
        character(len=:), allocatable :: output_code, error_msg
        
        ! Initialize the arena and semantic context
        call init_ast_arena(result%arena)
        result%semantic_context = create_semantic_context()
        
        ! Call the frontend transformation function
        call transform_lazy_fortran_string(source_code, output_code, error_msg)
        
        ! Store results
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            result%success = .false.
            result%error_message = error_msg
        else
            result%success = .true.
            if (allocated(output_code)) then
                result%generated_code = output_code
            end if
        end if
        
    end function compile_source
    
end module frontend_integration