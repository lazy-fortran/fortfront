module codegen_program_decl_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: exists_in_list, build_function_return_type_table

contains

    logical function exists_in_list(list, count, name)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i
        character(len=64) :: normalized_target
        character(len=64) :: normalized_entry

        exists_in_list = .false.
        normalized_target = trim(to_lower(name))
        do i = 1, count
            normalized_entry = trim(to_lower(list(i)))
            if (trim(normalized_entry) == trim(normalized_target)) then
                exists_in_list = .true.
                return
            end if
        end do
    end function exists_in_list

    subroutine build_function_return_type_table(arena, func_names, func_types, &
                                                count)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(inout) :: func_names(:)
        character(len=*), intent(inout) :: func_types(:)
        integer, intent(out) :: count
        integer :: i
        character(len=64) :: func_name

        count = 0
        func_names = ""
        func_types = ""

        do i = 1, arena%size
            if (count >= size(func_names)) exit
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(func%name)) cycle
                func_name = trim(func%name)
                if (len_trim(func_name) == 0) cycle
                if (exists_in_list(func_names, count, func_name)) cycle
                count = count + 1
                func_names(count) = trim(to_lower(func_name))
                if (allocated(func%return_type)) then
                    if (len_trim(func%return_type) > 0) then
                        func_types(count) = trim(func%return_type)
                    end if
                end if
            end select
        end do
    end subroutine build_function_return_type_table

end module codegen_program_decl_utils
