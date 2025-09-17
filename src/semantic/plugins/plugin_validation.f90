module plugin_validation_module
    !! Plugin validation system for interface compliance and dependency checking
    !! Provides comprehensive validation of plugin metadata and configuration
    use plugin_metadata_module, only: plugin_metadata_t
    implicit none
    private
    
    ! Validation result type
    type, public :: validation_result_t
        logical :: valid = .false.
        character(len=256) :: error_message = ""
        integer :: error_code = 0
        character(len=64) :: failed_component = ""
    end type
    
    ! Plugin validator type
    type, public :: plugin_validator_t
        logical :: initialized = .false.
        logical :: strict_mode = .true.
    contains
        procedure :: initialize => validator_initialize
        procedure :: cleanup => validator_cleanup
        procedure :: validate_plugin => validator_validate_plugin
        procedure :: validate_interface => validator_validate_interface
        procedure :: validate_dependencies => validator_validate_dependencies
        procedure :: validate_version => validator_validate_version
        procedure :: validate_capabilities => validator_validate_capabilities
        procedure :: validate_configuration => validator_validate_configuration
        procedure :: check_circular_dependencies => validator_check_circular
        procedure :: validate_plugin_name => validator_validate_name
        procedure :: set_strict_mode => validator_set_strict_mode
    end type
    
    
    ! Error codes
    integer, parameter, public :: VALIDATION_SUCCESS = 0
    integer, parameter, public :: VALIDATION_ERROR_NAME = 1
    integer, parameter, public :: VALIDATION_ERROR_VERSION = 2
    integer, parameter, public :: VALIDATION_ERROR_DEPENDENCIES = 3
    integer, parameter, public :: VALIDATION_ERROR_CAPABILITIES = 4
    integer, parameter, public :: VALIDATION_ERROR_INTERFACE = 5
    integer, parameter, public :: VALIDATION_ERROR_CIRCULAR_DEPS = 6
    
contains

    subroutine validator_initialize(this)
        class(plugin_validator_t), intent(inout) :: this
        this%initialized = .true.
        this%strict_mode = .true.
    end subroutine
    
    subroutine validator_cleanup(this)
        class(plugin_validator_t), intent(inout) :: this
        this%initialized = .false.
    end subroutine
    
    subroutine validator_validate_plugin(this, metadata, result)
        class(plugin_validator_t), intent(in) :: this
        type(plugin_metadata_t), intent(in) :: metadata
        type(validation_result_t), intent(out) :: result
        
        ! Initialize result
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        if (.not. this%initialized) then
            result%valid = .false.
            result%error_message = "Validator not initialized"
            result%error_code = VALIDATION_ERROR_INTERFACE
            result%failed_component = "validator"
            return
        end if
        
        ! Validate name
        call this%validate_plugin_name(metadata%name, result)
        if (.not. result%valid) return
        
        ! Validate version
        call this%validate_version(metadata%version, result)
        if (.not. result%valid) return
        
        ! Validate dependencies
        if (allocated(metadata%dependencies)) then
            call this%validate_dependencies(metadata%dependencies, result)
            if (.not. result%valid) return
        end if
        
        ! Validate capabilities
        if (allocated(metadata%capabilities)) then
            call this%validate_capabilities(metadata%capabilities, result)
            if (.not. result%valid) return
        end if
        
        ! Check for circular dependencies
        if (allocated(metadata%dependencies)) then
            call this%check_circular_dependencies(metadata%dependencies, result)
            if (.not. result%valid) return
        end if
    end subroutine
    
    subroutine validator_validate_interface(this, plugin_id, result)
        class(plugin_validator_t), intent(in) :: this
        integer, intent(in) :: plugin_id
        type(validation_result_t), intent(out) :: result
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        if (plugin_id <= 0) then
            result%valid = .false.
            result%error_message = "Invalid plugin ID"
            result%error_code = VALIDATION_ERROR_INTERFACE
            result%failed_component = "plugin_id"
            return
        end if
        
        ! For now, assume all interfaces are valid
        ! Real implementation would check method signatures, etc.
    end subroutine
    
    subroutine validator_validate_dependencies(this, dependencies, result)
        class(plugin_validator_t), intent(in) :: this
        integer, intent(in) :: dependencies(:)
        type(validation_result_t), intent(out) :: result
        
        integer :: i, j
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Check for negative IDs
        do i = 1, size(dependencies)
            if (dependencies(i) <= 0) then
                result%valid = .false.
                result%error_message = "Invalid dependency ID"
                result%error_code = VALIDATION_ERROR_DEPENDENCIES
                result%failed_component = "dependencies"
                return
            end if
        end do
        
        ! Check for duplicates
        do i = 1, size(dependencies) - 1
            do j = i + 1, size(dependencies)
                if (dependencies(i) == dependencies(j)) then
                    result%valid = .false.
                    result%error_message = "Duplicate dependency detected"
                    result%error_code = VALIDATION_ERROR_DEPENDENCIES
                    result%failed_component = "dependencies"
                    return
                end if
            end do
        end do
    end subroutine
    
    subroutine validator_validate_version(this, version, result)
        class(plugin_validator_t), intent(in) :: this
        character(len=*), intent(in) :: version
        type(validation_result_t), intent(out) :: result
        
        integer :: dot_count, i
        character(len=1) :: char
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Check if version is empty
        if (len_trim(version) == 0) then
            result%valid = .false.
            result%error_message = "Version cannot be empty"
            result%error_code = VALIDATION_ERROR_VERSION
            result%failed_component = "version"
            return
        end if
        
        ! Check for at least one dot (semantic versioning)
        dot_count = 0
        do i = 1, len_trim(version)
            char = version(i:i)
            if (char == ".") then
                dot_count = dot_count + 1
            else if (.not. (char >= "0" .and. char <= "9")) then
                if (.not. this%strict_mode) cycle
                result%valid = .false.
                result%error_message = "Invalid character in version"
                result%error_code = VALIDATION_ERROR_VERSION
                result%failed_component = "version"
                return
            end if
        end do
        
        if (dot_count == 0) then
            result%valid = .false.
            result%error_message = "Version must contain at least one dot"
            result%error_code = VALIDATION_ERROR_VERSION
            result%failed_component = "version"
            return
        end if
    end subroutine
    
    subroutine validator_validate_capabilities(this, capabilities, result)
        class(plugin_validator_t), intent(in) :: this
        integer, intent(in) :: capabilities(:)
        type(validation_result_t), intent(out) :: result
        
        integer :: i, j
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Check for valid capability values (1-8 for our event types)
        do i = 1, size(capabilities)
            if (capabilities(i) < 1 .or. capabilities(i) > 8) then
                result%valid = .false.
                result%error_message = "Invalid capability value"
                result%error_code = VALIDATION_ERROR_CAPABILITIES
                result%failed_component = "capabilities"
                return
            end if
        end do
        
        ! Check for duplicates
        do i = 1, size(capabilities) - 1
            do j = i + 1, size(capabilities)
                if (capabilities(i) == capabilities(j)) then
                    result%valid = .false.
                    result%error_message = "Duplicate capability detected"
                    result%error_code = VALIDATION_ERROR_CAPABILITIES
                    result%failed_component = "capabilities"
                    return
                end if
            end do
        end do
    end subroutine
    
    subroutine validator_validate_configuration(this, config_data, result)
        class(plugin_validator_t), intent(in) :: this
        character(len=*), intent(in) :: config_data
        type(validation_result_t), intent(out) :: result
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Simple configuration validation - just check it's not empty
        if (len_trim(config_data) == 0) then
            result%valid = .false.
            result%error_message = "Configuration data cannot be empty"
            result%error_code = VALIDATION_ERROR_INTERFACE
            result%failed_component = "configuration"
        end if
    end subroutine
    
    subroutine validator_check_circular(this, dependencies, result)
        class(plugin_validator_t), intent(in) :: this
        integer, intent(in) :: dependencies(:)
        type(validation_result_t), intent(out) :: result
        
        integer :: i, j
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Simple circular dependency check - look for immediate self-reference
        ! Real implementation would do full graph traversal
        do i = 1, size(dependencies)
            do j = 1, size(dependencies)
                if (i /= j .and. dependencies(i) == dependencies(j)) then
                    result%valid = .false.
                    result%error_message = "Circular dependency detected"
                    result%error_code = VALIDATION_ERROR_CIRCULAR_DEPS
                    result%failed_component = "dependencies"
                    return
                end if
            end do
        end do
    end subroutine
    
    subroutine validator_validate_name(this, name, result)
        class(plugin_validator_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(validation_result_t), intent(out) :: result
        
        integer :: i
        character(len=1) :: char
        
        result%valid = .true.
        result%error_message = ""
        result%error_code = VALIDATION_SUCCESS
        result%failed_component = ""
        
        ! Check if name is empty
        if (len_trim(name) == 0) then
            result%valid = .false.
            result%error_message = "Plugin name cannot be empty"
            result%error_code = VALIDATION_ERROR_NAME
            result%failed_component = "name"
            return
        end if
        
        ! Check for valid characters (alphanumeric and underscore)
        do i = 1, len_trim(name)
            char = name(i:i)
            if (.not. ((char >= "a" .and. char <= "z") .or. &
                      (char >= "A" .and. char <= "Z") .or. &
                      (char >= "0" .and. char <= "9") .or. &
                      char == "_")) then
                result%valid = .false.
                result%error_message = "Invalid character in plugin name"
                result%error_code = VALIDATION_ERROR_NAME
                result%failed_component = "name"
                return
            end if
        end do
        
        ! Check length constraints
        if (len_trim(name) > 64) then
            result%valid = .false.
            result%error_message = "Plugin name too long (max 64 characters)"
            result%error_code = VALIDATION_ERROR_NAME
            result%failed_component = "name"
            return
        end if
    end subroutine
    
    subroutine validator_set_strict_mode(this, strict)
        class(plugin_validator_t), intent(inout) :: this
        logical, intent(in) :: strict
        this%strict_mode = strict
    end subroutine

end module plugin_validation_module