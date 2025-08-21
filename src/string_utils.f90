module string_utils
    ! String utility module for simplifying common string operations
    ! Provides ergonomic wrappers around character(len=:), allocatable patterns
    implicit none
    private
    
    ! Public procedures
    public :: string_copy
    public :: string_concat
    public :: string_append
    public :: string_trim
    public :: string_ltrim
    public :: string_equals
    public :: string_starts_with
    public :: string_ends_with
    public :: string_contains
    public :: string_allocate
    public :: string_deallocate
    public :: string_length
    public :: string_is_empty
    
contains
    
    ! ========================================================================
    ! Basic string operations
    ! ========================================================================
    
    subroutine string_copy(dest, src)
        ! Safely copy string with automatic allocation
        character(len=:), allocatable, intent(out) :: dest
        character(len=*), intent(in) :: src
        
        ! intent(out) automatically deallocates dest, so no self-assignment issue
        allocate(character(len=len(src)) :: dest)
        dest = src
    end subroutine string_copy
    
    function string_concat(str1, str2) result(result_str)
        ! Concatenate two strings with automatic allocation
        character(len=*), intent(in) :: str1, str2
        character(len=:), allocatable :: result_str
        
        allocate(character(len=len(str1)+len(str2)) :: result_str)
        result_str = str1 // str2
    end function string_concat
    
    subroutine string_append(dest, src)
        ! Append src to dest, reallocating as needed
        character(len=:), allocatable, intent(inout) :: dest
        character(len=*), intent(in) :: src
        character(len=:), allocatable :: temp
        
        ! Self-assignment protection - create temp copy of src first
        allocate(character(len=len(src)) :: temp)
        temp = src
        
        if (.not. allocated(dest)) then
            call string_copy(dest, temp)
        else
            ! Use the temp copy to avoid aliasing issues
            dest = dest // temp
        end if
    end subroutine string_append
    
    ! ========================================================================
    ! String trimming operations
    ! ========================================================================
    
    function string_trim(str) result(trimmed)
        ! Remove trailing spaces
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: trimmed
        
        allocate(character(len=len_trim(str)) :: trimmed)
        trimmed = trim(str)
    end function string_trim
    
    function string_ltrim(str) result(trimmed)
        ! Remove leading spaces
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: trimmed
        
        allocate(character(len=len_trim(adjustl(str))) :: trimmed)
        trimmed = trim(adjustl(str))
    end function string_ltrim
    
    
    ! ========================================================================
    ! String comparison operations
    ! ========================================================================
    
    function string_equals(str1, str2) result(equal)
        ! Case-sensitive string comparison
        character(len=*), intent(in) :: str1, str2
        logical :: equal
        
        equal = (str1 == str2)
    end function string_equals
    
    function string_starts_with(str, prefix) result(starts)
        ! Check if string starts with prefix
        character(len=*), intent(in) :: str, prefix
        logical :: starts
        
        if (len(prefix) > len(str)) then
            starts = .false.
        else
            starts = (str(1:len(prefix)) == prefix)
        end if
    end function string_starts_with
    
    function string_ends_with(str, suffix) result(ends)
        ! Check if string ends with suffix
        character(len=*), intent(in) :: str, suffix
        logical :: ends
        integer :: str_len, suffix_len
        
        str_len = len(str)
        suffix_len = len(suffix)
        
        if (suffix_len > str_len) then
            ends = .false.
        else
            ends = (str(str_len-suffix_len+1:str_len) == suffix)
        end if
    end function string_ends_with
    
    function string_contains(str, substring) result(contains)
        ! Check if string contains substring
        character(len=*), intent(in) :: str, substring
        logical :: contains
        
        contains = (index(str, substring) > 0)
    end function string_contains
    
    ! ========================================================================
    ! String allocation utilities
    ! ========================================================================
    
    subroutine string_allocate(str, length, initial_value)
        ! Allocate string with specified length and optional initial value
        character(len=:), allocatable, intent(out) :: str
        integer, intent(in) :: length
        character(len=*), intent(in), optional :: initial_value
        integer :: init_len
        
        ! Input validation - reject negative lengths
        if (length < 0) then
            ! For negative length, leave str unallocated
            if (allocated(str)) deallocate(str)
            return
        end if
        
        if (allocated(str)) deallocate(str)
        
        ! Handle zero-length allocation case
        if (length == 0) then
            allocate(character(len=0) :: str)
            return
        end if
        
        allocate(character(len=length) :: str)
        
        ! Initialize with spaces
        str = repeat(' ', length)
        
        if (present(initial_value)) then
            init_len = min(len(initial_value), length)
            if (init_len > 0) then
                str(1:init_len) = initial_value(1:init_len)
            end if
        end if
    end subroutine string_allocate
    
    subroutine string_deallocate(str)
        ! Safely deallocate string
        character(len=:), allocatable, intent(inout) :: str
        
        if (allocated(str)) deallocate(str)
    end subroutine string_deallocate
    
    ! ========================================================================
    ! String query utilities
    ! ========================================================================
    
    function string_length(str) result(length)
        ! Get length of string (handles unallocated)
        character(len=:), allocatable, intent(in) :: str
        integer :: length
        
        if (allocated(str)) then
            length = len(str)
        else
            length = 0
        end if
    end function string_length
    
    function string_is_empty(str) result(empty)
        ! Check if string is empty or unallocated
        character(len=:), allocatable, intent(in) :: str
        logical :: empty
        
        if (.not. allocated(str)) then
            empty = .true.
        else
            empty = (len_trim(str) == 0)
        end if
    end function string_is_empty
    
end module string_utils