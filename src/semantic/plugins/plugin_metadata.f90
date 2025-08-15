module plugin_metadata_module
    !! Plugin metadata management for versioning and dependencies
    !! Provides metadata validation, version comparison, and dependency tracking
    implicit none
    private
    
    ! Re-export from plugin_registry_module for convenience
    public :: plugin_metadata_t
    
    ! Metadata management type
    type, public :: metadata_manager_t
        logical :: initialized = .false.
    contains
        procedure :: initialize => metadata_initialize
        procedure :: cleanup => metadata_cleanup
        procedure :: validate_metadata => metadata_validate
        procedure :: compare_versions => metadata_compare_versions
        procedure :: is_compatible_version => metadata_is_compatible
        procedure :: parse_version => metadata_parse_version
        procedure :: format_metadata => metadata_format
    end type
    
    ! Import the metadata type from registry module
    type :: plugin_metadata_t
        character(len=64) :: name = ""
        character(len=32) :: version = ""
        character(len=256) :: description = ""
        integer, allocatable :: dependencies(:)
        integer, allocatable :: capabilities(:)
        logical :: initialized = .false.
        logical :: enabled = .true.
    end type
    
contains

    subroutine metadata_initialize(this)
        class(metadata_manager_t), intent(inout) :: this
        this%initialized = .true.
    end subroutine
    
    subroutine metadata_cleanup(this)
        class(metadata_manager_t), intent(inout) :: this
        this%initialized = .false.
    end subroutine
    
    subroutine metadata_validate(this, metadata, valid, error_msg)
        class(metadata_manager_t), intent(in) :: this
        type(plugin_metadata_t), intent(in) :: metadata
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_msg
        
        valid = .true.
        error_msg = ""
        
        ! Validate name
        if (len_trim(metadata%name) == 0) then
            valid = .false.
            error_msg = "Plugin name cannot be empty"
            return
        end if
        
        ! Validate version format
        if (index(metadata%version, ".") == 0) then
            valid = .false.
            error_msg = "Invalid version format"
            return
        end if
        
        ! Validate description
        if (len_trim(metadata%description) == 0) then
            valid = .false.
            error_msg = "Plugin description cannot be empty"
            return
        end if
    end subroutine
    
    integer function metadata_compare_versions(this, version1, version2) result(result)
        class(metadata_manager_t), intent(in) :: this
        character(len=*), intent(in) :: version1, version2
        
        ! Simple lexicographic comparison for now
        if (version1 < version2) then
            result = -1
        else if (version1 > version2) then
            result = 1
        else
            result = 0
        end if
    end function
    
    logical function metadata_is_compatible(this, required_version, &
                                              available_version) result(compatible)
        class(metadata_manager_t), intent(in) :: this
        character(len=*), intent(in) :: required_version, available_version
        
        ! Simple compatibility check - exact match for now
        compatible = (trim(required_version) == trim(available_version))
    end function
    
    subroutine metadata_parse_version(this, version_string, major, minor, &
                                       patch, success)
        class(metadata_manager_t), intent(in) :: this
        character(len=*), intent(in) :: version_string
        integer, intent(out) :: major, minor, patch
        logical, intent(out) :: success
        
        integer :: dot1, dot2
        character(len=32) :: work_string
        
        success = .false.
        major = 0
        minor = 0
        patch = 0
        
        work_string = trim(version_string)
        
        ! Find first dot
        dot1 = index(work_string, ".")
        if (dot1 == 0) return
        
        ! Find second dot
        dot2 = index(work_string(dot1+1:), ".")
        if (dot2 == 0) return
        dot2 = dot1 + dot2
        
        ! Parse major
        read(work_string(1:dot1-1), *, iostat=dot1) major
        if (dot1 /= 0) return
        
        ! Parse minor
        read(work_string(dot1+1:dot2-1), *, iostat=dot1) minor
        if (dot1 /= 0) return
        
        ! Parse patch
        read(work_string(dot2+1:), *, iostat=dot1) patch
        if (dot1 /= 0) return
        
        success = .true.
    end subroutine
    
    subroutine metadata_format(this, metadata, formatted_string)
        class(metadata_manager_t), intent(in) :: this
        type(plugin_metadata_t), intent(in) :: metadata
        character(len=*), intent(out) :: formatted_string
        
        character(len=16) :: dep_count, cap_count
        
        write(dep_count, '(I0)') 0
        write(cap_count, '(I0)') 0
        
        if (allocated(metadata%dependencies)) then
            write(dep_count, '(I0)') size(metadata%dependencies)
        end if
        
        if (allocated(metadata%capabilities)) then
            write(cap_count, '(I0)') size(metadata%capabilities)
        end if
        
        formatted_string = trim(metadata%name) // " v" // trim(metadata%version) // &
                          " (deps:" // trim(dep_count) // ", caps:" // &
                          trim(cap_count) // ")"
    end subroutine

end module plugin_metadata_module