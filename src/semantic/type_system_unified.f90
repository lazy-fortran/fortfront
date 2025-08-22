module type_system_unified
    ! Unified type system using arena-based storage
    ! Provides compatibility layer for legacy API while using efficient arena storage
    
    use type_system_arena
    implicit none
    private

    ! Re-export arena types and constants
    public :: type_arena_t, mono_handle_t, poly_handle_t, args_handle_t
    public :: arena_mono_type_t, arena_poly_type_t
    public :: create_type_arena, destroy_type_arena
    
    ! Type kind constants (compatible with legacy system)
    integer, parameter, public :: TVAR = 1      ! Type variable
    integer, parameter, public :: TINT = 2      ! Integer type  
    integer, parameter, public :: TREAL = 3     ! Real type
    integer, parameter, public :: TCHAR = 4     ! Character type
    integer, parameter, public :: TLOGICAL = 5  ! Logical type
    integer, parameter, public :: TFUN = 6      ! Function type
    integer, parameter, public :: TARRAY = 7    ! Array type

    ! Compatibility layer types (lightweight wrappers around arena handles)
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t

    type :: type_var_t
        integer :: id = 0
        character(len=64) :: name = ""  ! Fixed size to avoid allocatable issues
    contains
        procedure :: assign => type_var_assign
        generic :: assignment(=) => assign
    end type type_var_t

    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_t

    ! Arena-backed types providing legacy API
    type :: mono_type_t
        type(mono_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null()  ! Reference to parent arena
        ! Cached values for compatibility (updated lazily)
        integer :: kind = 0
        type(type_var_t) :: var
        integer :: size = 0  
        type(allocation_info_t) :: alloc_info
    contains
        procedure :: to_string => mono_type_to_string
        procedure :: assign => mono_type_assign
        procedure :: get_kind => mono_type_get_kind
        procedure :: get_size => mono_type_get_size
        procedure :: get_alloc_info => mono_type_get_alloc_info
        procedure :: sync_from_arena => mono_type_sync_from_arena
        procedure :: has_args => mono_type_has_args
        procedure :: get_arg => mono_type_get_arg
        procedure :: get_args_count => mono_type_get_args_count
        generic :: assignment(=) => assign
    end type mono_type_t

    type :: poly_type_t
        type(poly_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null()
        ! Cached mono type for compatibility - simplified version without arena linkage
        integer :: mono_kind = 0
        integer :: mono_size = 0
        type(type_var_t) :: mono_var
        type(allocation_info_t) :: mono_alloc_info
        logical :: mono_synced = .false.
    contains
        procedure :: assign => poly_type_assign
        procedure :: sync_mono => poly_type_sync_mono
        procedure :: get_mono => poly_type_get_mono
        generic :: assignment(=) => assign
    end type poly_type_t

    type :: substitution_t
        integer :: count = 0
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
    contains
        procedure :: add => substitution_add
        procedure :: apply => substitution_apply
        procedure :: assign => substitution_assign
        generic :: assignment(=) => assign
    end type substitution_t

    type :: type_env_t
        integer :: count = 0
        integer :: capacity = 0
        character(len=:), allocatable :: names(:)
        type(poly_type_t), allocatable :: schemes(:)
    contains
        procedure :: extend => type_env_extend
        procedure :: assign => type_env_assign
        generic :: assignment(=) => assign
    end type type_env_t

    ! Global arena for type operations - initialized by create functions
    type(type_arena_t), target, save :: global_arena
    logical, save :: arena_initialized = .false.

    ! Public API functions (compatibility with legacy system)
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: compose_substitutions, occurs_check, free_type_vars
    
    ! Compatibility wrapper functions for type_checker
    public :: type_has_args, type_get_arg, type_get_args_count
    
    ! Additional compatibility functions for semantic analyzer
    public :: type_args_allocated, type_args_size, type_args_element

contains

    ! Initialize global arena if needed
    subroutine ensure_arena_initialized()
        if (.not. arena_initialized) then
            global_arena = create_type_arena(65536)  ! 64KB for types
            arena_initialized = .true.
        end if
    end subroutine ensure_arena_initialized

    ! Create type variable (compatibility function)
    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_t) :: tv

        tv%id = id
        if (present(name)) then
            tv%name = name
        else
            write(tv%name, '("t", I0)') id
        end if
    end function create_type_var

    ! Create monomorphic type (compatibility function)
    function create_mono_type(kind, var, args, char_size, array_size) result(mt)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size, array_size
        type(mono_type_t) :: mt

        type(arena_mono_type_t) :: arena_type
        type(args_handle_t) :: args_handle
        type(mono_handle_t), allocatable :: arg_handles(:)
        integer :: i

        call ensure_arena_initialized()

        ! Build arena type
        arena_type%kind = kind
        arena_type%size = 0
        if (present(char_size)) arena_type%size = char_size
        if (present(array_size)) arena_type%size = array_size

        if (present(var)) then
            arena_type%var_id = var%id
            arena_type%var_name = var%name
        end if

        ! Handle arguments for function and array types
        if (present(args)) then
            allocate(arg_handles(size(args)))
            do i = 1, size(args)
                arg_handles(i) = args(i)%handle
            end do
            args_handle = store_type_args(global_arena, arg_handles)
            arena_type%args = args_handle
        else
            arena_type%args = null_args_handle()
        end if

        ! Store in arena
        mt%handle = store_mono_type(global_arena, arena_type)
        mt%arena => global_arena
        
        ! Cache values for immediate access
        mt%kind = kind
        mt%size = arena_type%size
        if (present(var)) mt%var = var
        
        ! Set allocation info if needed
        mt%alloc_info%is_allocatable = arena_type%is_allocatable
        mt%alloc_info%is_pointer = arena_type%is_pointer  
        mt%alloc_info%is_target = arena_type%is_target
    end function create_mono_type

    ! Create polymorphic type (compatibility function)
    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt

        type(arena_poly_type_t) :: arena_poly
        type(args_handle_t) :: vars_handle
        type(mono_handle_t), allocatable :: var_handles(:)
        integer :: i

        call ensure_arena_initialized()

        ! Convert type vars to handles (simplified - we'll store as empty for now)
        allocate(var_handles(0))  ! Empty for now - full implementation would convert forall_vars
        vars_handle = store_type_args(global_arena, var_handles)

        arena_poly%forall_vars = vars_handle
        arena_poly%mono = mono%handle

        pt%handle = store_poly_type(global_arena, arena_poly)
        pt%arena => global_arena
        
        ! Initialize cached mono type fields
        pt%mono_kind = mono%kind
        pt%mono_size = mono%size
        pt%mono_var = mono%var
        pt%mono_alloc_info = mono%alloc_info
        pt%mono_synced = .true.
    end function create_poly_type

    ! Create function type (compatibility function)
    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type

        type(mono_type_t) :: args(2)

        args(1) = arg_type
        args(2) = result_type
        fun_type = create_mono_type(TFUN, args=args)
    end function create_fun_type

    ! Compose substitutions (stub for now)
    function compose_substitutions(s1, s2) result(composed)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: composed

        ! Simple implementation - just copy s1 for now
        composed = s1
        ! TODO: Implement proper composition
    end function compose_substitutions

    ! Occurs check (stub for now)
    function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        logical :: occurs

        occurs = .false.  ! Safe default
        ! TODO: Implement proper occurs check
    end function occurs_check

    ! Free type variables (stub for now)
    subroutine free_type_vars(typ, vars)
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable, intent(out) :: vars(:)

        allocate(vars(0))  ! Empty for now
        ! TODO: Implement proper free variable collection
    end subroutine free_type_vars

    ! Type variable assignment
    subroutine type_var_assign(lhs, rhs)
        class(type_var_t), intent(out) :: lhs
        type(type_var_t), intent(in) :: rhs

        lhs%id = rhs%id
        lhs%name = rhs%name
    end subroutine type_var_assign

    ! Mono type assignment
    subroutine mono_type_assign(lhs, rhs)
        class(mono_type_t), intent(out) :: lhs
        type(mono_type_t), intent(in) :: rhs

        lhs%handle = rhs%handle
        lhs%arena => rhs%arena
        
        ! Copy cached values
        lhs%kind = rhs%kind
        lhs%var = rhs%var
        lhs%size = rhs%size
        lhs%alloc_info = rhs%alloc_info
    end subroutine mono_type_assign

    ! Poly type assignment  
    subroutine poly_type_assign(lhs, rhs)
        class(poly_type_t), intent(out) :: lhs
        type(poly_type_t), intent(in) :: rhs

        lhs%handle = rhs%handle
        lhs%arena => rhs%arena
        lhs%mono_kind = rhs%mono_kind
        lhs%mono_size = rhs%mono_size
        lhs%mono_var = rhs%mono_var
        lhs%mono_alloc_info = rhs%mono_alloc_info
        lhs%mono_synced = rhs%mono_synced
    end subroutine poly_type_assign

    ! Substitution assignment
    subroutine substitution_assign(lhs, rhs)
        class(substitution_t), intent(out) :: lhs
        type(substitution_t), intent(in) :: rhs

        lhs%count = rhs%count
        if (allocated(rhs%vars)) then
            allocate(lhs%vars(size(rhs%vars)))
            lhs%vars = rhs%vars
        end if
        if (allocated(rhs%types)) then
            allocate(lhs%types(size(rhs%types)))
            lhs%types = rhs%types
        end if
    end subroutine substitution_assign

    ! Type environment assignment
    subroutine type_env_assign(lhs, rhs)
        class(type_env_t), intent(out) :: lhs
        type(type_env_t), intent(in) :: rhs

        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        if (allocated(rhs%names)) then
            allocate(character(len=len(rhs%names)) :: lhs%names(size(rhs%names)))
            lhs%names = rhs%names
        end if
        if (allocated(rhs%schemes)) then
            allocate(lhs%schemes(size(rhs%schemes)))
            lhs%schemes = rhs%schemes
        end if
    end subroutine type_env_assign

    ! Mono type helper functions
    function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=64) :: str

        type(arena_mono_type_t) :: arena_type

        if (.not. is_valid_mono_handle(this%handle) .or. .not. associated(this%arena)) then
            str = "invalid_type"
            return
        end if

        arena_type = get_mono_type(this%arena, this%handle)

        select case (arena_type%kind)
        case (TVAR)
            write(str, '("''", A)') trim(arena_type%var_name)
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real"
        case (TCHAR)
            if (arena_type%size > 0) then
                write(str, '("character(len=", I0, ")")') arena_type%size
            else
                str = "character"
            end if
        case (TLOGICAL)
            str = "logical"
        case (TFUN)
            str = "function"  ! Simplified
        case (TARRAY)
            str = "array"    ! Simplified
        case default
            str = "unknown"
        end select
    end function mono_type_to_string

    function mono_type_get_kind(this) result(kind)
        class(mono_type_t), intent(in) :: this
        integer :: kind

        ! Use cached value for performance
        kind = this%kind
    end function mono_type_get_kind

    function mono_type_get_size(this) result(size)
        class(mono_type_t), intent(in) :: this
        integer :: size

        ! Use cached value for performance
        size = this%size
    end function mono_type_get_size

    function mono_type_get_alloc_info(this) result(alloc_info)
        class(mono_type_t), intent(in) :: this
        type(allocation_info_t) :: alloc_info

        ! Use cached value for performance
        alloc_info = this%alloc_info
    end function mono_type_get_alloc_info

    ! Sync cached values from arena (called when needed)
    subroutine mono_type_sync_from_arena(this)
        class(mono_type_t), intent(inout) :: this
        type(arena_mono_type_t) :: arena_type

        if (.not. is_valid_mono_handle(this%handle) .or. .not. associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        this%kind = arena_type%kind
        this%size = arena_type%size
        this%var%id = arena_type%var_id
        this%var%name = arena_type%var_name
        this%alloc_info%is_allocatable = arena_type%is_allocatable
        this%alloc_info%is_pointer = arena_type%is_pointer
        this%alloc_info%is_target = arena_type%is_target
    end subroutine mono_type_sync_from_arena

    ! Check if mono type has arguments
    function mono_type_has_args(this) result(has_args)
        class(mono_type_t), intent(in) :: this
        logical :: has_args

        type(arena_mono_type_t) :: arena_type

        has_args = .false.
        if (.not. is_valid_mono_handle(this%handle) .or. .not. associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        has_args = is_valid_args_handle(arena_type%args) .and. arena_type%args%count > 0
    end function mono_type_has_args

    ! Get argument count
    function mono_type_get_args_count(this) result(count)
        class(mono_type_t), intent(in) :: this
        integer :: count

        type(arena_mono_type_t) :: arena_type

        count = 0
        if (.not. is_valid_mono_handle(this%handle) .or. .not. associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        if (is_valid_args_handle(arena_type%args)) then
            count = arena_type%args%count
        end if
    end function mono_type_get_args_count

    ! Get specific argument by index
    function mono_type_get_arg(this, index) result(arg_type)
        class(mono_type_t), intent(in) :: this
        integer, intent(in) :: index
        type(mono_type_t) :: arg_type

        type(arena_mono_type_t) :: arena_type
        type(mono_handle_t), allocatable :: arg_handles(:)

        ! Return invalid type if out of bounds or invalid
        arg_type%kind = 0
        if (.not. is_valid_mono_handle(this%handle) .or. .not. associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        if (.not. is_valid_args_handle(arena_type%args)) return
        if (index < 1 .or. index > arena_type%args%count) return

        ! Get the arguments array
        call get_type_args(this%arena, arena_type%args, arg_handles)
        if (allocated(arg_handles) .and. size(arg_handles) >= index) then
            arg_type%handle = arg_handles(index)
            arg_type%arena => this%arena
            call arg_type%sync_from_arena()
        end if
    end function mono_type_get_arg

    ! Substitution helper functions
    subroutine substitution_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ

        ! Extend arrays if needed
        if (.not. allocated(this%vars)) then
            allocate(this%vars(10))
            allocate(this%types(10))
        else if (this%count >= size(this%vars)) then
            ! Grow arrays
            block
                type(type_var_t), allocatable :: temp_vars(:)
                type(mono_type_t), allocatable :: temp_types(:)
                integer :: new_size
                
                new_size = size(this%vars) * 2
                allocate(temp_vars(new_size))
                allocate(temp_types(new_size))
                temp_vars(1:this%count) = this%vars(1:this%count)
                temp_types(1:this%count) = this%types(1:this%count)
                call move_alloc(temp_vars, this%vars)
                call move_alloc(temp_types, this%types)
            end block
        end if

        this%count = this%count + 1
        this%vars(this%count) = var
        this%types(this%count) = typ
    end subroutine substitution_add

    subroutine substitution_apply(this, input, output)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: input
        type(mono_type_t), intent(out) :: output

        ! For now, just copy input to output
        output = input
        ! TODO: Implement proper substitution application
    end subroutine substitution_apply

    ! Type environment helper functions
    subroutine type_env_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        ! Initialize if needed
        if (.not. allocated(this%names)) then
            this%capacity = 10
            allocate(character(len=256) :: this%names(this%capacity))
            allocate(this%schemes(this%capacity))
            this%count = 0
        end if

        ! Grow if needed
        if (this%count >= this%capacity) then
            block
                character(len=:), allocatable :: temp_names(:)
                type(poly_type_t), allocatable :: temp_schemes(:)
                integer :: new_capacity

                new_capacity = this%capacity * 2
                allocate(character(len=len(this%names)) :: temp_names(new_capacity))
                allocate(temp_schemes(new_capacity))
                temp_names(1:this%count) = this%names(1:this%count)
                temp_schemes(1:this%count) = this%schemes(1:this%count)
                deallocate(this%names, this%schemes)
                call move_alloc(temp_names, this%names)
                call move_alloc(temp_schemes, this%schemes)
                this%capacity = new_capacity
            end block
        end if

        this%count = this%count + 1
        this%names(this%count) = name
        this%schemes(this%count) = scheme
    end subroutine type_env_extend

    ! Public wrapper functions for compatibility with type_checker
    function type_has_args(typ) result(has_args)
        type(mono_type_t), intent(in) :: typ
        logical :: has_args
        has_args = typ%has_args()
    end function type_has_args

    function type_get_args_count(typ) result(count)
        type(mono_type_t), intent(in) :: typ
        integer :: count
        count = typ%get_args_count()
    end function type_get_args_count

    function type_get_arg(typ, index) result(arg_type)
        type(mono_type_t), intent(in) :: typ
        integer, intent(in) :: index
        type(mono_type_t) :: arg_type
        arg_type = typ%get_arg(index)
    end function type_get_arg

    ! Additional semantic analyzer compatibility functions
    function type_args_allocated(typ) result(allocated)
        type(mono_type_t), intent(in) :: typ
        logical :: allocated
        allocated = typ%has_args()
    end function type_args_allocated

    function type_args_size(typ) result(size)
        type(mono_type_t), intent(in) :: typ
        integer :: size
        size = typ%get_args_count()
    end function type_args_size

    function type_args_element(typ, index) result(element_type)
        type(mono_type_t), intent(in) :: typ
        integer, intent(in) :: index
        type(mono_type_t) :: element_type
        element_type = typ%get_arg(index)
    end function type_args_element

    ! Poly type mono sync function
    subroutine poly_type_sync_mono(this)
        class(poly_type_t), intent(inout) :: this
        type(arena_poly_type_t) :: arena_poly
        type(arena_mono_type_t) :: arena_mono

        if (.not. is_valid_poly_handle(this%handle) .or. .not. associated(this%arena)) return
        if (this%mono_synced) return

        arena_poly = get_poly_type(this%arena, this%handle)
        if (is_valid_mono_handle(arena_poly%mono)) then
            arena_mono = get_mono_type(this%arena, arena_poly%mono)
            this%mono_kind = arena_mono%kind
            this%mono_size = arena_mono%size
            this%mono_var%id = arena_mono%var_id
            this%mono_var%name = arena_mono%var_name
            this%mono_alloc_info%is_allocatable = arena_mono%is_allocatable
            this%mono_alloc_info%is_pointer = arena_mono%is_pointer
            this%mono_alloc_info%is_target = arena_mono%is_target
        end if
        this%mono_synced = .true.
    end subroutine poly_type_sync_mono

    ! Get mono type from poly type
    function poly_type_get_mono(this) result(mono)
        class(poly_type_t), intent(inout) :: this
        type(mono_type_t) :: mono

        if (.not. this%mono_synced) then
            call this%sync_mono()
        end if

        mono%kind = this%mono_kind
        mono%size = this%mono_size
        mono%var = this%mono_var
        mono%alloc_info = this%mono_alloc_info
    end function poly_type_get_mono

end module type_system_unified