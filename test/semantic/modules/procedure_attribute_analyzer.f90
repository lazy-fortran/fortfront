module procedure_attribute_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use semantic_result_types, only: semantic_result_base_t
    use ast_core, only: ast_arena_t
    implicit none
    private
    
    public :: procedure_attribute_analyzer_t, procedure_info_t, procedure_registry_t
    
    ! Parameter attribute information
    type :: parameter_attribute_t
        character(:), allocatable :: name
        character(len=10) :: intent = ""         ! IN, OUT, INOUT, or empty
        logical :: is_optional = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        character(:), allocatable :: type_spec
    end type
    
    ! Comprehensive procedure information
    type :: procedure_info_t
        character(:), allocatable :: name
        integer :: node_index                    ! AST node reference
        integer :: declaration_line              ! Source location
        
        ! Core attributes
        logical :: is_pure = .false.
        logical :: is_elemental = .false.
        logical :: is_recursive = .false.
        logical :: is_module_procedure = .false.
        logical :: is_abstract = .false.
        logical :: is_interface = .false.
        
        ! C interoperability
        logical :: has_bind_c = .false.
        character(:), allocatable :: bind_name   ! BIND(C, name="...")
        
        ! Parameter attributes
        type(parameter_attribute_t), allocatable :: parameters(:)
        
        ! Function-specific
        logical :: is_function = .false.
        character(:), allocatable :: result_name
        character(:), allocatable :: result_type
        
        ! Validation status
        logical :: attributes_validated = .false.
        character(:), allocatable :: validation_errors(:)
    end type
    
    ! Efficient procedure registry
    type, extends(semantic_result_base_t) :: procedure_registry_t
        type(procedure_info_t), allocatable :: procedures(:)
        integer :: procedure_count = 0
    contains
        procedure :: add_procedure
        procedure :: find_by_name
        procedure :: find_by_node
        procedure :: get_all_procedures
        procedure :: get_result_type => registry_get_result_type
        procedure :: clone_result => registry_clone_result
        procedure :: merge_results => registry_merge_results
        procedure :: assign => registry_assign
        generic :: assignment(=) => assign
    end type
    
    ! Procedure attribute analyzer plugin
    type, extends(semantic_analyzer_t) :: procedure_attribute_analyzer_t
        type(procedure_registry_t) :: registry
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_procedure_attributes
        procedure :: get_results => get_procedure_attribute_results
        procedure :: get_name => get_procedure_attribute_analyzer_name
        procedure :: assign => assign_procedure_attribute_analyzer
        procedure :: get_dependencies => get_procedure_attribute_dependencies
        
        ! Public API methods for fluff rules
        procedure :: extract_procedure_attributes
        procedure :: has_attribute
        procedure :: get_all_attributes
        procedure :: get_procedure_info
        procedure :: validate_attribute_consistency
    end type

contains

    subroutine analyze_procedure_attributes(this, shared_context, arena, node_index)
        class(procedure_attribute_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Basic implementation - traverse arena for procedure declarations
        call traverse_for_procedures(this%registry, arena, node_index)
        call extract_all_attributes(this%registry, arena)
        call validate_all_procedures(this%registry)
        this%analysis_complete = .true.
        
        ! Avoid unused variable warnings
        associate(dummy => shared_context)
        end associate
    end subroutine
    
    function get_procedure_attribute_results(this) result(results)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        ! Return the procedure registry
        allocate(procedure_registry_t :: results)
        select type(results)
        type is (procedure_registry_t)
            results = this%registry
        end select
    end function
    
    function get_procedure_attribute_analyzer_name(this) result(name)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "procedure_attribute_analyzer"
        
        associate(dummy => this)
        end associate
    end function
    
    subroutine assign_procedure_attribute_analyzer(lhs, rhs)
        class(procedure_attribute_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (procedure_attribute_analyzer_t)
            lhs%registry = rhs%registry
            lhs%analysis_complete = rhs%analysis_complete
        class default
            error stop "Type mismatch in procedure_attribute_analyzer assignment"
        end select
    end subroutine
    
    function get_procedure_attribute_dependencies(this) result(deps)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! No dependencies for procedure attribute analysis
        allocate(character(len=0) :: deps(0))
        
        ! Avoid unused variable warning
        associate(dummy => this)
        end associate
    end function
    
    ! Public API methods
    function has_attribute(this, proc_name, attribute) result(has_attr)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        character(*), intent(in) :: proc_name, attribute
        logical :: has_attr
        
        type(procedure_info_t) :: proc_info
        logical :: found
        
        call this%registry%find_by_name(proc_name, proc_info, found)
        if (.not. found) then
            has_attr = .false.
            return
        end if
        
        select case (trim(attribute))
        case ("PURE")
            has_attr = proc_info%is_pure
        case ("ELEMENTAL")
            has_attr = proc_info%is_elemental
        case ("RECURSIVE")
            has_attr = proc_info%is_recursive
        case ("BIND_C")
            has_attr = proc_info%has_bind_c
        case default
            has_attr = .false.
        end select
    end function
    
    function get_procedure_info(this, proc_name) result(proc_info)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        character(*), intent(in) :: proc_name
        type(procedure_info_t) :: proc_info
        
        logical :: found
        call this%registry%find_by_name(proc_name, proc_info, found)
        ! If not found, proc_info will have default values
    end function
    
    function get_all_attributes(this, proc_name) result(attributes)
        class(procedure_attribute_analyzer_t), intent(in) :: this
        character(*), intent(in) :: proc_name
        character(:), allocatable :: attributes(:)
        
        type(procedure_info_t) :: proc_info
        character(len=20), allocatable :: temp_attrs(:)
        integer :: attr_count
        logical :: found
        
        call this%registry%find_by_name(proc_name, proc_info, found)
        if (.not. found) then
            allocate(character(len=20) :: attributes(0))
            return
        end if
        
        ! Build attribute list
        allocate(temp_attrs(10))  ! Maximum possible attributes
        attr_count = 0
        
        if (proc_info%is_pure) then
            attr_count = attr_count + 1
            temp_attrs(attr_count) = "PURE"
        end if
        if (proc_info%is_elemental) then
            attr_count = attr_count + 1
            temp_attrs(attr_count) = "ELEMENTAL"
        end if
        if (proc_info%is_recursive) then
            attr_count = attr_count + 1
            temp_attrs(attr_count) = "RECURSIVE"
        end if
        if (proc_info%has_bind_c) then
            attr_count = attr_count + 1
            temp_attrs(attr_count) = "BIND_C"
        end if
        
        ! Copy to result array
        allocate(character(len=20) :: attributes(attr_count))
        if (attr_count > 0) then
            attributes(1:attr_count) = temp_attrs(1:attr_count)
        end if
    end function
    
    subroutine extract_procedure_attributes(this, arena, node_index)
        class(procedure_attribute_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Placeholder implementation
        call traverse_for_procedures(this%registry, arena, node_index)
    end subroutine
    
    subroutine validate_attribute_consistency(this)
        class(procedure_attribute_analyzer_t), intent(inout) :: this
        
        ! Placeholder for validation logic
        call validate_all_procedures(this%registry)
    end subroutine
    
    ! Registry methods
    subroutine add_procedure(this, proc_info)
        class(procedure_registry_t), intent(inout) :: this
        type(procedure_info_t), intent(in) :: proc_info
        
        type(procedure_info_t), allocatable :: temp_procedures(:)
        integer :: i
        
        ! Grow procedures array
        allocate(temp_procedures(this%procedure_count + 1))
        
        ! Copy existing procedures
        do i = 1, this%procedure_count
            temp_procedures(i) = this%procedures(i)
        end do
        
        ! Add new procedure
        temp_procedures(this%procedure_count + 1) = proc_info
        
        ! Update registry
        call move_alloc(temp_procedures, this%procedures)
        this%procedure_count = this%procedure_count + 1
    end subroutine
    
    subroutine find_by_name(this, name, proc_info, found)
        class(procedure_registry_t), intent(in) :: this
        character(*), intent(in) :: name
        type(procedure_info_t), intent(out) :: proc_info
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        do i = 1, this%procedure_count
            if (trim(this%procedures(i)%name) == trim(name)) then
                proc_info = this%procedures(i)
                found = .true.
                return
            end if
        end do
    end subroutine
    
    subroutine find_by_node(this, node_index, proc_info, found)
        class(procedure_registry_t), intent(in) :: this
        integer, intent(in) :: node_index
        type(procedure_info_t), intent(out) :: proc_info
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        do i = 1, this%procedure_count
            if (this%procedures(i)%node_index == node_index) then
                proc_info = this%procedures(i)
                found = .true.
                return
            end if
        end do
    end subroutine
    
    function get_all_procedures(this) result(procedures)
        class(procedure_registry_t), intent(in) :: this
        type(procedure_info_t), allocatable :: procedures(:)
        
        allocate(procedures(this%procedure_count))
        if (this%procedure_count > 0) then
            procedures = this%procedures(1:this%procedure_count)
        end if
    end function
    
    ! Helper subroutines (placeholder implementations)
    subroutine traverse_for_procedures(registry, arena, node_index)
        type(procedure_registry_t), intent(inout) :: registry
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Placeholder - would traverse AST to find procedure declarations
        associate(dummy1 => registry, dummy2 => arena, dummy3 => node_index)
        end associate
    end subroutine
    
    subroutine extract_all_attributes(registry, arena)
        type(procedure_registry_t), intent(inout) :: registry
        type(ast_arena_t), intent(in) :: arena
        
        ! Placeholder - would extract attributes from found procedures
        associate(dummy1 => registry, dummy2 => arena)
        end associate
    end subroutine
    
    subroutine validate_all_procedures(registry)
        type(procedure_registry_t), intent(inout) :: registry
        
        ! Placeholder - would validate attribute combinations
        associate(dummy => registry)
        end associate
    end subroutine

    ! Implementation of required abstract interfaces for procedure_registry_t
    function registry_get_result_type(this) result(type_name)
        class(procedure_registry_t), intent(in) :: this
        character(:), allocatable :: type_name
        
        type_name = "procedure_registry_t"
        associate(dummy => this)
        end associate
    end function

    function registry_clone_result(this) result(cloned)
        class(procedure_registry_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        
        allocate(procedure_registry_t :: cloned)
        select type(cloned)
        type is (procedure_registry_t)
            cloned = this
        end select
    end function

    subroutine registry_merge_results(this, other)
        class(procedure_registry_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type(other)
        type is (procedure_registry_t)
            ! Merge other registry into this one
            ! For now, just copy errors and warnings
            this%has_errors = this%has_errors .or. other%has_errors
            this%has_warnings = this%has_warnings .or. other%has_warnings
        end select
    end subroutine

    subroutine registry_assign(this, other)
        class(procedure_registry_t), intent(out) :: this
        class(procedure_registry_t), intent(in) :: other
        
        ! Copy all components
        this%procedures = other%procedures
        this%procedure_count = other%procedure_count
        
        ! Copy base class components
        this%result_id = other%result_id
        this%result_type_name = other%result_type_name
        this%has_errors = other%has_errors
        this%has_warnings = other%has_warnings
        this%summary = other%summary
    end subroutine

end module procedure_attribute_analyzer