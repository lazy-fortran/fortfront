module symbol_table_api
    ! Symbol Table Query API for external tools (fluff, LSP, etc.)
    ! Provides functions to query symbols from semantic_context_t
    use fortfront_types, only: symbol_info_t, scope_info_t
    use type_system_unified, only: mono_type_t, poly_type_t
    use scope_manager, only: scope_stack_t, scope_t
    use identifier_table, only: identifier_table_get
    implicit none
    private

    public :: get_symbols_in_scope
    public :: get_all_symbols
    public :: is_symbol_defined
    public :: lookup_symbol
    public :: get_scope_info
    public :: get_current_scope_depth

contains

    function get_symbols_in_scope(scopes, scope_level) result(symbols)
        ! Get all symbols defined in a specific scope level
        type(scope_stack_t), intent(in) :: scopes
        integer, intent(in), optional :: scope_level
        type(symbol_info_t), allocatable :: symbols(:)

        integer :: level, i, count
        type(scope_t) :: scope
        character(len=:), allocatable :: name

        if (present(scope_level)) then
            level = scope_level
        else
            level = scopes%depth
        end if

        if (level < 1 .or. level > scopes%depth) then
            allocate (symbols(0))
            return
        end if

        scope = scopes%scopes(level)
        count = scope%env%count

        if (count <= 0) then
            allocate (symbols(0))
            return
        end if

        allocate (symbols(count))

        do i = 1, count
            if (associated(scope%identifiers)) then
                name = identifier_table_get(scope%identifiers, &
                    scope%env%name_ids(i))
            else
                name = ""
            end if
            symbols(i)%name = name
            symbols(i)%scope_level = level
            symbols(i)%is_defined = .true.
            call extract_mono_type(scope%env%schemes(i), symbols(i)%type_info)
        end do
    end function get_symbols_in_scope

    function get_all_symbols(scopes) result(symbols)
        ! Get all symbols from all scopes (flattened)
        type(scope_stack_t), intent(in) :: scopes
        type(symbol_info_t), allocatable :: symbols(:)

        type(symbol_info_t), allocatable :: level_symbols(:)
        integer :: total_count, level, i, offset

        total_count = 0
        do level = 1, scopes%depth
            total_count = total_count + scopes%scopes(level)%env%count
        end do

        if (total_count <= 0) then
            allocate (symbols(0))
            return
        end if

        allocate (symbols(total_count))
        offset = 0

        do level = 1, scopes%depth
            level_symbols = get_symbols_in_scope(scopes, level)
            do i = 1, size(level_symbols)
                offset = offset + 1
                symbols(offset) = level_symbols(i)
            end do
        end do
    end function get_all_symbols

    function is_symbol_defined(scopes, name) result(defined)
        ! Check if a symbol is defined anywhere in the scope hierarchy
        type(scope_stack_t), intent(in) :: scopes
        character(len=*), intent(in) :: name
        logical :: defined

        type(poly_type_t), allocatable :: scheme

        call scopes%lookup(name, scheme)
        defined = allocated(scheme)
    end function is_symbol_defined

    function lookup_symbol(scopes, name) result(info)
        ! Lookup a symbol by name and return its info
        type(scope_stack_t), intent(in) :: scopes
        character(len=*), intent(in) :: name
        type(symbol_info_t) :: info

        type(poly_type_t), allocatable :: scheme
        integer :: level

        info%name = name
        info%is_defined = .false.

        call scopes%lookup(name, scheme)
        if (.not. allocated(scheme)) return

        info%is_defined = .true.
        call extract_mono_type(scheme, info%type_info)

        do level = scopes%depth, 1, -1
            block
                type(poly_type_t), allocatable :: local_scheme
                call scopes%scopes(level)%lookup(name, local_scheme)
                if (allocated(local_scheme)) then
                    info%scope_level = level
                    exit
                end if
            end block
        end do
    end function lookup_symbol

    function get_scope_info(scopes, scope_level) result(info)
        ! Get information about a specific scope
        type(scope_stack_t), intent(in) :: scopes
        integer, intent(in), optional :: scope_level
        type(scope_info_t) :: info

        integer :: level

        if (present(scope_level)) then
            level = scope_level
        else
            level = scopes%depth
        end if

        if (level < 1 .or. level > scopes%depth) then
            info%level = 0
            info%scope_type = 0
            info%symbol_count = 0
            return
        end if

        info%level = level
        info%scope_type = scopes%scopes(level)%scope_type
        info%symbol_count = scopes%scopes(level)%env%count
        if (allocated(scopes%scopes(level)%name)) then
            info%name = scopes%scopes(level)%name
        else
            info%name = ""
        end if
    end function get_scope_info

    function get_current_scope_depth(scopes) result(depth)
        ! Get the current scope depth (number of nested scopes)
        type(scope_stack_t), intent(in) :: scopes
        integer :: depth

        depth = scopes%depth
    end function get_current_scope_depth

    subroutine extract_mono_type(scheme, mono)
        ! Extract mono_type_t from poly_type_t
        type(poly_type_t), intent(inout) :: scheme
        type(mono_type_t), intent(out) :: mono

        mono = scheme%get_mono()
    end subroutine extract_mono_type

end module symbol_table_api
