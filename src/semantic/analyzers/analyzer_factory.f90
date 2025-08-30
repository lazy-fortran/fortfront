module analyzer_factory
    use base_analyzer, only: base_analyzer_t
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: analyzer_factory_t, analyzer_registry_t, &
              create_global_registry, get_global_registry

    ! Abstract factory interface for creating analyzers
    abstract interface
        function analyzer_creator_interface() result(analyzer)
            import :: base_analyzer_t
            class(base_analyzer_t), allocatable :: analyzer
        end function analyzer_creator_interface
    end interface

    ! Factory type that holds a creator function
    type :: analyzer_factory_t
        procedure(analyzer_creator_interface), pointer, nopass :: creator => null()
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: description
    contains
        procedure :: create => factory_create_analyzer
        procedure :: is_valid => factory_is_valid
    end type analyzer_factory_t

    ! Registry for managing analyzer factories
    type :: analyzer_registry_t
        character(len=32), allocatable :: names(:)
        type(analyzer_factory_t), allocatable :: factories(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: register_factory
        procedure :: create_analyzer
        procedure :: is_registered
        procedure :: get_factory_names
        procedure :: clear
        procedure :: expand_capacity
    end type analyzer_registry_t

    ! Global registry instance
    type(analyzer_registry_t), target, save :: global_registry

contains

    ! Factory methods
    function factory_create_analyzer(this) result(analyzer)
        class(analyzer_factory_t), intent(in) :: this
        class(base_analyzer_t), allocatable :: analyzer
        
        if (associated(this%creator)) then
            analyzer = this%creator()
        else
            write(error_unit, '(A)') "ERROR [analyzer_factory]: Factory creator function not set - returning null"
            ! Don't allocate analyzer on error - leave unallocated
        end if
    end function factory_create_analyzer

    function factory_is_valid(this) result(valid)
        class(analyzer_factory_t), intent(in) :: this
        logical :: valid
        
        valid = associated(this%creator) .and. allocated(this%type_name)
    end function factory_is_valid

    ! Registry methods
    subroutine register_factory(this, name, factory)
        class(analyzer_registry_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(analyzer_factory_t), intent(in) :: factory
        
        ! Check if already registered
        if (this%is_registered(name)) then
            write(error_unit, '(A)') "ERROR [analyzer_factory]: Analyzer factory already registered: " &
                // name // " - ignoring registration"
            return  ! Skip registration
        end if
        
        ! Expand capacity if needed
        if (this%count >= this%capacity) then
            call this%expand_capacity()
        end if
        
        ! Add to registry
        this%count = this%count + 1
        this%names(this%count) = name
        this%factories(this%count) = factory
    end subroutine register_factory

    function create_analyzer(this, name) result(analyzer)
        class(analyzer_registry_t), intent(in) :: this
        character(len=*), intent(in) :: name
        class(base_analyzer_t), allocatable :: analyzer
        integer :: i
        
        ! Find factory by name
        do i = 1, this%count
            if (trim(this%names(i)) == trim(name)) then
                analyzer = this%factories(i)%create()
                return
            end if
        end do
        
        write(error_unit, '(A)') "ERROR [analyzer_factory]: Unknown analyzer type: " // name // " - returning null"
        ! Don't allocate analyzer on error - leave unallocated
    end function create_analyzer

    function is_registered(this, name) result(registered)
        class(analyzer_registry_t), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: registered
        integer :: i
        
        registered = .false.
        do i = 1, this%count
            if (trim(this%names(i)) == trim(name)) then
                registered = .true.
                return
            end if
        end do
    end function is_registered

    function get_factory_names(this) result(names)
        class(analyzer_registry_t), intent(in) :: this
        character(len=32), allocatable :: names(:)
        
        if (this%count > 0) then
            allocate(names(this%count))
            names = this%names(1:this%count)
        else
            allocate(names(0))
        end if
    end function get_factory_names

    subroutine clear(this)
        class(analyzer_registry_t), intent(inout) :: this
        
        if (allocated(this%names)) deallocate(this%names)
        if (allocated(this%factories)) deallocate(this%factories)
        this%count = 0
        this%capacity = 0
    end subroutine clear

    subroutine expand_capacity(this)
        class(analyzer_registry_t), intent(inout) :: this
        character(len=32), allocatable :: temp_names(:)
        type(analyzer_factory_t), allocatable :: temp_factories(:)
        integer :: new_capacity
        
        if (this%capacity == 0) then
            new_capacity = 10
        else
            new_capacity = this%capacity * 2
        end if
        
        ! Copy existing data
        if (this%count > 0) then
            allocate(temp_names(this%count))
            allocate(temp_factories(this%count))
            temp_names = this%names(1:this%count)
            temp_factories = this%factories(1:this%count)
        end if
        
        ! Reallocate with new capacity
        if (allocated(this%names)) deallocate(this%names)
        if (allocated(this%factories)) deallocate(this%factories)
        allocate(this%names(new_capacity))
        allocate(this%factories(new_capacity))
        this%capacity = new_capacity
        
        ! Restore existing data
        if (this%count > 0) then
            this%names(1:this%count) = temp_names
            this%factories(1:this%count) = temp_factories
        end if
    end subroutine expand_capacity

    ! Global registry functions
    function create_global_registry() result(registry)
        type(analyzer_registry_t) :: registry
        ! Initialize empty registry
        registry%count = 0
        registry%capacity = 0
    end function create_global_registry

    function get_global_registry() result(registry)
        type(analyzer_registry_t), pointer :: registry
        registry => global_registry
    end function get_global_registry

end module analyzer_factory