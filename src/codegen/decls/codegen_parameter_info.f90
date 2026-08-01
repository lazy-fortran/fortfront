module codegen_parameter_info
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: identifier_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    implicit none
    private

    type, public :: parameter_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent_str
        logical :: is_optional
        logical :: is_target
        logical :: is_pointer
        logical :: is_mutated
    end type parameter_info_t

    public :: find_parameter_info
    public :: is_function_parameter
    public :: is_parameter_name

contains

    function find_parameter_info(param_map, var_name) result(param_idx)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: var_name
        integer :: param_idx
        integer :: i

        param_idx = 0
        do i = 1, size(param_map)
            if (allocated(param_map(i)%name)) then
                if (trim(param_map(i)%name) == trim(var_name)) then
                    param_idx = i
                    return
                end if
            end if
        end do
    end function find_parameter_info

    logical function check_param_indices_for_name(arena, param_indices, var_name) &
            result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: var_name
        integer :: i

        found = .false.
        do i = 1, size(param_indices)
            if (param_indices(i) > 0 .and. param_indices(i) <= arena%size) then
                if (allocated(arena%entries(param_indices(i))%node)) then
                    select type (param_node => arena%entries(param_indices(i))%node)
                        type is (identifier_node)
                        if (param_node%name == var_name) then
                            found = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function check_param_indices_for_name

    function is_function_parameter(var_name, arena, proc_node) result(is_param)
        character(len=*), intent(in) :: var_name
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        logical :: is_param

        is_param = .false.

        select type (proc_node)
            type is (function_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            is_param = check_param_indices_for_name(arena, proc_node%param_indices, &
                var_name)
            type is (subroutine_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            is_param = check_param_indices_for_name(arena, proc_node%param_indices, &
                var_name)
        end select
    end function is_function_parameter

    function is_parameter_name(var_name, param_names) result(is_param)
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: param_names(:)
        logical :: is_param
        integer :: i

        is_param = .false.
        do i = 1, size(param_names)
            if (trim(param_names(i)) == trim(var_name)) then
                is_param = .true.
                return
            end if
        end do
    end function is_parameter_name

end module codegen_parameter_info
