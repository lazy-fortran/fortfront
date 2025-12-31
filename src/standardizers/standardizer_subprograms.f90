module standardizer_subprograms
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use standardizer_function, only: standardize_function_def
    use standardizer_function, only: standardize_function_parameters
    use standardizer_function, only: standardize_function_result
    use standardizer_parameter, only: infer_parameter_type
    use standardizer_subroutine, only: standardize_subroutine_def
    use standardizer_subroutine, only: standardize_subroutine_parameters
    use standardizer_wrapping, only: wrap_function_in_program, &
                                     wrap_subroutine_in_program
    implicit none
    private
    public :: standardize_subprograms
    public :: standardize_function_def
    public :: standardize_subroutine_def
    public :: standardize_function_parameters
    public :: standardize_subroutine_parameters
    public :: wrap_function_in_program
    public :: wrap_subroutine_in_program
    public :: infer_parameter_type
    public :: standardize_function_result
contains

    subroutine standardize_subprograms(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

        ! CRITICAL: Copy indices before loop to prevent dangling pointer access
  ! standardize_function_def and standardize_subroutine_def call push_implicit_statement
  ! which modifies arena, potentially invalidating the prog selector used in select type
        block
            integer, allocatable :: local_indices(:)
            allocate (local_indices(size(prog%body_indices)))
            local_indices = prog%body_indices

            do i = 1, size(local_indices)
                if (local_indices(i) > 0 .and. local_indices(i) <= arena%size) then
                    if (allocated(arena%entries(local_indices(i))%node)) then
                        select type (stmt => arena%entries(local_indices(i))%node)
                        type is (function_def_node)
                            call standardize_function_def(arena, stmt, local_indices(i))
                        type is (subroutine_def_node)
                            call standardize_subroutine_def(arena, stmt, &
                                                            local_indices(i))
                        end select
                    end if
                end if
            end do

            deallocate (local_indices)
        end block
    end subroutine standardize_subprograms

end module standardizer_subprograms
