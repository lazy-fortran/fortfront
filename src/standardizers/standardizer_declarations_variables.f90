module standardizer_declarations_variables
    use ast_nodes_core, only: identifier_node
    use standardizer_declarations_state, only: get_standardizer_type_standardization
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    implicit none
    private

    public :: add_variable
    public :: mark_variable_declared
    public :: collect_identifier_var_with_type
    public :: collect_identifier_var

contains

    subroutine add_variable(var_name, var_type, var_names, var_types, &
            var_declared, var_count, &
            function_names, func_count)
        character(len=*), intent(in) :: var_name, var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function
        character(len=64) :: normalized_input
        character(len=64) :: normalized_existing
        character(len=64) :: normalized_function

        normalized_input = to_lower(trim(var_name))

        is_function = .false.
            do i = 1, func_count
                normalized_function = to_lower(trim(function_names(i)))
                    if (trim(normalized_function) == trim(normalized_input)) then
                        is_function = .true.
                            exit
                        end if
                    end do

                    if (is_function) return

                    found = .false.
                    do i = 1, var_count
                        normalized_existing = to_lower(trim(var_names(i)))
                        if (trim(normalized_existing) == trim(normalized_input)) then
                            found = .true.
                            exit
                        end if
                    end do

                    if (.not. found) then
                        var_count = var_count + 1
                        if (var_count <= size(var_names)) then
                            var_names(var_count) = normalized_input
                            var_types(var_count) = var_type
                            var_declared(var_count) = .true.
                        end if
                    end if
                end subroutine add_variable

                subroutine mark_variable_declared(var_name, var_names, var_declared, var_count)
                    character(len=*), intent(in) :: var_name
                    character(len=64), intent(in) :: var_names(:)
                    logical, intent(inout) :: var_declared(:)
                    integer, intent(in) :: var_count
                    integer :: i
                    character(len=64) :: normalized_input
                    character(len=64) :: normalized_existing

                    normalized_input = to_lower(trim(var_name))

                    do i = 1, var_count
                        normalized_existing = to_lower(trim(var_names(i)))
                        if (trim(normalized_existing) == trim(normalized_input)) then
                            var_declared(i) = .false.
                            return
                        end if
                    end do
                end subroutine mark_variable_declared

                subroutine collect_identifier_var_with_type(identifier, var_type, &
                        var_names, var_types, var_declared, &
                        var_count, &
                        function_names, func_count)
                    type(identifier_node), intent(in) :: identifier
                    character(len=*), intent(in) :: var_type
                    character(len=64), intent(inout) :: var_names(:)
                    character(len=64), intent(inout) :: var_types(:)
                    logical, intent(inout) :: var_declared(:)
                    integer, intent(inout) :: var_count
                    character(len=64), intent(in) :: function_names(:)
                    integer, intent(in) :: func_count

                    call add_variable(identifier%name, var_type, var_names, var_types, &
                        var_declared, var_count, function_names, func_count)
                end subroutine collect_identifier_var_with_type

                subroutine collect_identifier_var(identifier, var_names, var_types, &
                        var_declared, var_count, &
                        function_names, func_count)
                    type(identifier_node), intent(in) :: identifier
                    character(len=64), intent(inout) :: var_names(:)
                    character(len=64), intent(inout) :: var_types(:)
                    logical, intent(inout) :: var_declared(:)
                    integer, intent(inout) :: var_count
                    character(len=64), intent(in) :: function_names(:)
                    integer, intent(in) :: func_count
                    character(len=:), allocatable :: inferred_type
                    logical :: success_flag
                    logical :: standardize_flag
                    character(len=1) :: first_char
                    character(len=64) :: implicit_type

                    call get_standardizer_type_standardization(standardize_flag)

                    if (identifier%inferred_type%kind > 0) then
                        inferred_type = mono_type_to_string( &
                            identifier%inferred_type, include_shape=.true., &
                            prefer_len_zero_char=.true., &
                            standardize_real=standardize_flag, &
                            success=success_flag)
                        if (success_flag) then
                            if (len_trim(inferred_type) > 0) then
                                call collect_identifier_var_with_type( &
                                    identifier, trim(inferred_type), var_names, var_types, &
                                    var_declared, var_count, function_names, func_count)
                                return
                            end if
                        end if
                    end if

                    ! Apply Fortran's implicit typing rules when no type is inferred
                    ! Variables starting with I-N are INTEGER, others are REAL
                    first_char = to_lower(identifier%name(1:1))
                    if (first_char >= 'i' .and. first_char <= 'n') then
                        implicit_type = "integer"
                    else
                        implicit_type = "real"
                    end if

                    call add_variable(identifier%name, implicit_type, var_names, var_types, &
                        var_declared, var_count, function_names, func_count)
                end subroutine collect_identifier_var

            end module standardizer_declarations_variables
