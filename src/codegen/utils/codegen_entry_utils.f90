module codegen_entry_utils
    use ast_nodes_transfer, only: entry_node
    use ast_nodes_data, only: declaration_node
    implicit none
    private

    public :: collect_entry_param_names, is_entry_parameter_decl

contains

    subroutine collect_entry_param_names(entry_stmt, param_list, num_params)
        type(entry_node), intent(in) :: entry_stmt
        character(len=64), allocatable, intent(inout) :: param_list(:)
        integer, intent(inout) :: num_params
        character(len=:), allocatable :: params_text
        character(len=:), allocatable :: clean_params
        integer :: start_pos
        integer :: end_pos
        integer :: comma_pos
        character(len=64) :: param_name

        if (.not. allocated(entry_stmt%params_text)) return

        params_text = entry_stmt%params_text

        start_pos = index(params_text, '(')
        if (start_pos == 0) return
        end_pos = index(params_text, ')')
        if (end_pos == 0) return

        clean_params = params_text(start_pos+1:end_pos-1)
        clean_params = adjustl(clean_params)

        if (len_trim(clean_params) == 0) return

        if (.not. allocated(param_list)) then
            allocate (param_list(10))
            param_list = ""
            num_params = 0
        end if

        do while (len_trim(clean_params) > 0)
            comma_pos = index(clean_params, ',')
            if (comma_pos > 0) then
                param_name = trim(adjustl(clean_params(1:comma_pos-1)))
                clean_params = adjustl(clean_params(comma_pos+1:))
            else
                param_name = trim(adjustl(clean_params))
                clean_params = ""
            end if

            if (len_trim(param_name) > 0) then
                num_params = num_params + 1
                if (num_params > size(param_list)) then
                    call expand_param_list(param_list, num_params)
                end if
                param_list(num_params) = param_name
            end if
        end do
    end subroutine collect_entry_param_names

    subroutine expand_param_list(param_list, new_size)
        character(len=64), allocatable, intent(inout) :: param_list(:)
        integer, intent(in) :: new_size
        character(len=64), allocatable :: temp(:)
        integer :: old_size

        old_size = size(param_list)
        allocate (temp(old_size))
        temp = param_list
        deallocate (param_list)
        allocate (param_list(new_size * 2))
        param_list = ""
        param_list(1:old_size) = temp
        deallocate (temp)
    end subroutine expand_param_list

    logical function is_entry_parameter_decl(decl, entry_params, num_params) &
            result(is_entry_param)
        type(declaration_node), intent(in) :: decl
        character(len=64), allocatable, intent(in) :: entry_params(:)
        integer, intent(in) :: num_params
        integer :: i
        integer :: max_params

        is_entry_param = .false.

        if (.not. allocated(entry_params)) return
        if (num_params <= 0) return
        max_params = min(num_params, size(entry_params))
        if (max_params <= 0) return

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            do i = 1, size(decl%var_names)
                if (is_in_entry_params(decl%var_names(i), entry_params, &
                    max_params)) then
                    is_entry_param = .true.
                    return
                end if
            end do
        else if (allocated(decl%var_name)) then
            is_entry_param = is_in_entry_params(decl%var_name, entry_params, &
                max_params)
        end if
    end function is_entry_parameter_decl

    logical function is_in_entry_params(var_name, entry_params, num_params) &
            result(found)
        character(len=*), intent(in) :: var_name
        character(len=64), allocatable, intent(in) :: entry_params(:)
        integer, intent(in) :: num_params
        integer :: i
        integer :: max_params

        found = .false.
        if (.not. allocated(entry_params)) return
        if (num_params <= 0) return

        max_params = min(num_params, size(entry_params))
        do i = 1, max_params
            if (trim(var_name) == trim(entry_params(i))) then
                found = .true.
                return
            end if
        end do
    end function is_in_entry_params

end module codegen_entry_utils
