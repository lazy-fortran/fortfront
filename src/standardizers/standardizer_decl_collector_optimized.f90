module standardizer_decl_collector_optimized
    ! Optimized declaration collection using hash tables instead of linear arrays
    ! This module provides a drop-in replacement for the old array-based system
    use standardizer_decl_table_mod
    use string_utils_mod, only: to_lower
    implicit none
    private

    ! Public API - compatible with old array-based interface
    public :: opt_decl_state_t
    public :: opt_decl_init
    public :: opt_decl_destroy
    public :: opt_add_variable
    public :: opt_add_or_update
    public :: opt_to_arrays

    ! Optimized declaration state
    type :: opt_decl_state_t
        type(decl_table_t) :: table
        logical :: initialized = .false.
    end type opt_decl_state_t

contains

    ! Initialize optimized declaration state
    subroutine opt_decl_init(state, initial_capacity)
        type(opt_decl_state_t), intent(inout) :: state
        integer, intent(in), optional :: initial_capacity

        if (present(initial_capacity)) then
            call state%table%init(initial_capacity)
        else
            call state%table%init()
        end if
        state%initialized = .true.
    end subroutine opt_decl_init

    ! Destroy optimized declaration state
    subroutine opt_decl_destroy(state)
        type(opt_decl_state_t), intent(inout) :: state

        if (state%initialized) then
            call state%table%destroy()
            state%initialized = .false.
        end if
    end subroutine opt_decl_destroy

    ! Add variable to hash table (O(1) expected instead of O(n))
    subroutine opt_add_variable(state, var_name, var_type, function_names, func_count)
        type(opt_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: var_type
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: is_function
        character(len=64) :: normalized_input, normalized_function

        if (.not. state%initialized) call opt_decl_init(state)

        ! Check if this is a function name (skip if so)
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

        ! Add to hash table (O(1) lookup and insert)
        call state%table%add_or_update(var_name, var_type, is_declared=.false.)
    end subroutine opt_add_variable

    ! Add or update variable in hash table (replaces add_or_update_alloc_var)
    subroutine opt_add_or_update(state, name, var_type)
        type(opt_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: var_type

        if (.not. state%initialized) call opt_decl_init(state)

        ! O(1) hash table insert/update instead of O(n) array search + realloc
        call state%table%add_or_update(name, var_type, is_declared=.true.)
    end subroutine opt_add_or_update

    ! Convert hash table to arrays (for compatibility with code that needs arrays)
    subroutine opt_to_arrays(state, var_names, var_types, var_declared, var_count, max_size)
        type(opt_decl_state_t), intent(in) :: state
        character(len=64), intent(out) :: var_names(:)
        character(len=64), intent(out) :: var_types(:)
        logical, intent(out) :: var_declared(:)
        integer, intent(out) :: var_count
        integer, intent(in) :: max_size

        if (.not. state%initialized) then
            var_count = 0
            return
        end if

        call state%table%to_arrays(var_names, var_types, var_declared, var_count, max_size)
    end subroutine opt_to_arrays

end module standardizer_decl_collector_optimized
