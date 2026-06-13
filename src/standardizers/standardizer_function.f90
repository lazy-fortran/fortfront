module standardizer_function
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use ast_nodes_procedure, only: function_def_node
    use standardizer_parameter, only: get_standardizer_type_standardization
    use standardizer_function_parameters, only: standardize_function_parameters
    use standardizer_function_result_utils, only: apply_result_variable
    use standardizer_function_result_utils, only: determine_preferred_result_name
    use standardizer_function_result_utils, only: sync_result_declaration
    use standardizer_interface_utils, only: function_in_interface_block
    implicit none
    private
    public :: standardize_function_def
    public :: standardize_function_parameters
    public :: standardize_function_result
contains

    subroutine standardize_function_def(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i
        character(len=:), allocatable :: return_type_str
        logical :: standardizer_type_standardization_enabled
        logical :: skip_result_standardization
        ! Local snapshot taken before any arena push that may reallocate entries.
        ! When this routine is called from a select-type block whose selector
        ! aliases arena%entries(func_index)%node, push_implicit_statement can
        ! trigger move_alloc on the entries array.  move_entries_fast transfers
        ! the polymorphic node allocation via move_alloc, leaving the original
        ! storage unallocated.  Any subsequent read of func_def (which may be an
        ! alias into that freed storage) is undefined behaviour.  Copying into a
        ! plain local variable up front guarantees all fields remain valid.
        type(function_def_node) :: local_def

        local_def = func_def

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)
        skip_result_standardization = function_in_interface_block(arena, &
                                                                  func_index)
        if (skip_result_standardization) then
            arena%entries(func_index)%node = local_def
            return
        end if

        ! Standardize return type
        if (allocated(local_def%return_type)) then
            if (local_def%return_type == "real") then
                if (standardizer_type_standardization_enabled) then
                    local_def%return_type = "real(dp)"
                else
                    local_def%return_type = "real"
                end if
            end if
        end if

        ! Add implicit none at the beginning of function body.
        ! push_implicit_statement may reallocate arena%entries; local_def is
        ! already a safe copy so nothing here can corrupt it.
        if (allocated(local_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=func_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(local_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(local_def%body_indices)
                new_body_indices(i + 1) = local_def%body_indices(i)
            end do
            local_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_function_parameters(arena, local_def, func_index)

        ! Ensure function result variable is standardized and declared
        call standardize_function_result(arena, local_def, func_index)

        ! Write back to arena and propagate to caller.
        arena%entries(func_index)%node = local_def
        func_def = local_def

    end subroutine standardize_function_def

    subroutine standardize_function_result(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical :: type_std_enabled
        character(len=64) :: preferred_name

        call get_standardizer_type_standardization(type_std_enabled)
        call determine_preferred_result_name(arena, func_def, func_index, &
                                             preferred_name)
        call apply_result_variable(arena, func_def, func_index, preferred_name)

        if (.not. allocated(func_def%result_variable)) return
        if (len_trim(func_def%result_variable) == 0) return

        call sync_result_declaration(arena, func_def, func_index, type_std_enabled)

    end subroutine standardize_function_result

end module standardizer_function
