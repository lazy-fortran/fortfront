module standardizer_function
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_misc, only: implicit_statement_node, comment_node
    use ast_nodes_misc, only: blank_line_node, end_statement_node
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
        logical :: skip_full_standardization

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)
        skip_result_standardization = function_in_interface_block(arena, &
                                                                  func_index)
        skip_full_standardization = skip_result_standardization
        if (.not. skip_full_standardization) then
            skip_full_standardization = function_has_only_spec_statements( &
                arena, func_def)
        end if

        if (skip_full_standardization) then
            arena%entries(func_index)%node = func_def
            return
        end if

        ! Standardize return type
        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "real") then
                if (standardizer_type_standardization_enabled) then
                    func_def%return_type = "real(8)"
                else
                    func_def%return_type = "real"
                end if
            end if
        end if

        ! Add implicit none at the beginning of function body
        if (allocated(func_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=func_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(func_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(func_def%body_indices)
                new_body_indices(i + 1) = func_def%body_indices(i)
            end do
            func_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_function_parameters(arena, func_def, func_index)

        ! Ensure function result variable is standardized and declared
        if (.not. skip_result_standardization) then
            call standardize_function_result(arena, func_def, func_index)
        end if

        ! Update the arena entry
        arena%entries(func_index)%node = func_def

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

    logical function function_has_only_spec_statements(arena, func_def) &
        result(only_specs)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        integer :: i, idx

        if (.not. allocated(func_def%body_indices)) then
            only_specs = .true.
            return
        end if

        only_specs = .true.
        do i = 1, size(func_def%body_indices)
            idx = func_def%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (body_node => arena%entries(idx)%node)
            type is (declaration_node)
                cycle
            type is (parameter_declaration_node)
                cycle
            type is (implicit_statement_node)
                cycle
            type is (comment_node)
                cycle
            type is (blank_line_node)
                cycle
            type is (end_statement_node)
                cycle
            class default
                only_specs = .false.
                return
            end select
        end do
    end function function_has_only_spec_statements
end module standardizer_function
