module url_utilities
    implicit none
    private
    
    public :: extract_module_from_url
    
contains
    
    ! Extract module name from URL following Go-style import conventions
    ! Examples:
    !   "https://github.com/user/repo/module.f90" -> "module"
    !   "https://example.com/path/math.f90@v1.2.3" -> "math"
    subroutine extract_module_from_url(url, module_name, is_valid)
        character(*), intent(in) :: url
        character(:), allocatable, intent(out) :: module_name
        logical, intent(out) :: is_valid
        
        integer :: last_slash, dot_pos, at_pos, start_pos, end_pos
        character(:), allocatable :: filename
        
        is_valid = .false.
        module_name = ""
        
        ! Basic URL validation - must start with http:// or https://
        if (len(url) < 8) return
        if (url(1:7) /= "http://" .and. url(1:8) /= "https://") return
        
        ! Find last slash to get filename
        last_slash = 0
        do start_pos = len(url), 1, -1
            if (url(start_pos:start_pos) == '/') then
                last_slash = start_pos
                exit
            end if
        end do
        
        if (last_slash == 0 .or. last_slash == len(url)) return
        
        ! Extract filename part
        filename = url(last_slash + 1:len(url))
        
        ! Remove version specifier if present (everything after @)
        at_pos = 0
        do start_pos = 1, len(filename)
            if (filename(start_pos:start_pos) == '@') then
                at_pos = start_pos
                exit
            end if
        end do
        
        if (at_pos > 0) then
            filename = filename(1:at_pos - 1)
        end if
        
        ! Find .f90 extension and extract module name
        if (len(filename) < 5) return
        if (filename(len(filename)-3:len(filename)) /= ".f90") return
        
        ! Module name is everything before .f90
        module_name = filename(1:len(filename) - 4)
        
        ! Validate module name (simple check - starts with letter, &
        !   contains only alphanumeric and underscore)
        if (len(module_name) == 0) return
        if (.not. is_valid_identifier(module_name)) return
        
        is_valid = .true.
    end subroutine extract_module_from_url
    
    ! Simple identifier validation
    function is_valid_identifier(name) result(valid)
        character(*), intent(in) :: name
        logical :: valid
        integer :: i
        character :: ch
        
        valid = .false.
        if (len(name) == 0) return
        
        ! First character must be a letter
        ch = name(1:1)
        if (.not. ((ch >= 'a' .and. ch <= 'z') .or. &
                   (ch >= 'A' .and. ch <= 'Z'))) return
        
        ! Remaining characters must be alphanumeric or underscore
        do i = 2, len(name)
            ch = name(i:i)
            if (.not. ((ch >= 'a' .and. ch <= 'z') .or. &
                       (ch >= 'A' .and. ch <= 'Z') .or. &
                       (ch >= '0' .and. ch <= '9') .or. &
                       ch == '_')) return
        end do
        
        valid = .true.
    end function is_valid_identifier
    
end module url_utilities