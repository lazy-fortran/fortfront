module frontend_integration
    ! Higher-level compilation interface for fortfront
    ! Provides a unified compilation result type and interface for integration testing
    
    use frontend, only: transform_lazy_fortran_string_with_arena
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
    contains
        procedure :: assign => compilation_result_assign
        generic :: assignment(=) => assign
    end type compilation_result_t
    
contains
    
    function compile_source(source_code) result(result)
        character(len=*), intent(in) :: source_code
        type(compilation_result_t) :: result
        
        character(len=:), allocatable :: output_code, error_msg
        integer :: prog_index
        
        ! Call the frontend transformation function with arena exposure
        call transform_lazy_fortran_string_with_arena(source_code, output_code, error_msg, &
                                                      result%arena, prog_index, &
                                                      result%semantic_context)
        
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
    
    ! Assignment operator for compilation_result_t
    subroutine compilation_result_assign(lhs, rhs)
        class(compilation_result_t), intent(out) :: lhs
        type(compilation_result_t), intent(in) :: rhs
        
        lhs%success = rhs%success
        
        if (allocated(rhs%error_message)) then
            lhs%error_message = rhs%error_message
        else
            if (allocated(lhs%error_message)) deallocate(lhs%error_message)
        end if
        
        if (allocated(rhs%generated_code)) then
            lhs%generated_code = rhs%generated_code
        else
            if (allocated(lhs%generated_code)) deallocate(lhs%generated_code)
        end if
        
        lhs%arena = rhs%arena                    ! Uses ast_arena_t assignment
        lhs%semantic_context = rhs%semantic_context  ! Uses semantic_context_t assignment
    end subroutine compilation_result_assign
    
end module frontend_integration