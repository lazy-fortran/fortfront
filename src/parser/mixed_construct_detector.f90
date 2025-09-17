module mixed_construct_detector
    ! Mixed construct detection for Issue #511 support
    ! Analyzes token stream to identify mixed constructs requiring module generation
    
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_EOF, TK_NEWLINE
    implicit none
    private

    ! Mixed construct analysis result
    type, public :: mixed_construct_result_t
        logical :: has_mixed_constructs = .false.
        integer, allocatable :: implicit_ranges(:, :)  ! [start, end] pairs
        integer, allocatable :: explicit_ranges(:, :)   ! [start, end] pairs
        integer :: num_implicit_ranges = 0
        integer :: num_explicit_ranges = 0
    end type mixed_construct_result_t

    public :: detect_mixed_constructs
    public :: is_top_level_declaration
    public :: is_explicit_program_unit

contains

    ! Main detection routine
    subroutine detect_mixed_constructs(tokens, result)
        type(token_t), intent(in) :: tokens(:)
        type(mixed_construct_result_t), intent(out) :: result
        
        integer :: i, range_start, range_end
        logical :: in_implicit_construct, in_explicit_construct
        
        ! Initialize result
        result%has_mixed_constructs = .false.
        result%num_implicit_ranges = 0
        result%num_explicit_ranges = 0
        allocate(result%implicit_ranges(100, 2))  ! Max 100 ranges
        allocate(result%explicit_ranges(100, 2))
        
        i = 1
        do while (i <= size(tokens))
            ! Skip EOF tokens between lines
            if (tokens(i)%kind == TK_EOF .or. tokens(i)%kind == TK_NEWLINE) then
                i = i + 1
                cycle
            end if
            
            ! Detect construct type
            if (is_top_level_declaration(tokens, i)) then
                ! Found implicit declaration - find its range
                range_start = i
                call find_declaration_range(tokens, i, range_end)
                
                ! Add to implicit ranges
                result%num_implicit_ranges = result%num_implicit_ranges + 1
                result%implicit_ranges(result%num_implicit_ranges, 1) = range_start
                result%implicit_ranges(result%num_implicit_ranges, 2) = range_end
                
                i = range_end + 1
                
            else if (is_explicit_program_unit(tokens, i)) then
                ! Found explicit program unit - find its range  
                range_start = i
                call find_program_unit_range(tokens, i, range_end)
                
                ! Add to explicit ranges
                result%num_explicit_ranges = result%num_explicit_ranges + 1
                result%explicit_ranges(result%num_explicit_ranges, 1) = range_start
                result%explicit_ranges(result%num_explicit_ranges, 2) = range_end
                
                i = range_end + 1
                
            else
                ! Unknown construct - skip
                i = i + 1
            end if
        end do
        
        ! Check if we have mixed constructs
        result%has_mixed_constructs = (result%num_implicit_ranges > 0 .and. &
                                     result%num_explicit_ranges > 0)
        
        ! Resize arrays to actual size
        if (result%num_implicit_ranges > 0) then
            result%implicit_ranges = result%implicit_ranges(1:result%num_implicit_ranges, :)
        else
            deallocate(result%implicit_ranges)
            allocate(result%implicit_ranges(0, 2))
        end if
        
        if (result%num_explicit_ranges > 0) then
            result%explicit_ranges = result%explicit_ranges(1:result%num_explicit_ranges, :)
        else
            deallocate(result%explicit_ranges)
            allocate(result%explicit_ranges(0, 2))
        end if
    end subroutine detect_mixed_constructs

    ! Check if token sequence represents a top-level declaration
    function is_top_level_declaration(tokens, start_pos) result(is_declaration)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: is_declaration
        
        is_declaration = .false.
        
        if (start_pos > size(tokens)) return
        
        ! Check for type declarations: type, integer, real, character, etc.
        if (tokens(start_pos)%kind == TK_KEYWORD) then
            select case (trim(tokens(start_pos)%text))
            case ("type")
                ! Could be "type :: name" or "type(name)" - both are declarations
                is_declaration = .true.
            case ("integer", "real", "character", "logical", "complex")
                ! Basic type declarations
                is_declaration = .true.
            case ("parameter")
                ! Parameter declarations
                is_declaration = .true.
            end select
        end if
    end function is_top_level_declaration

    ! Check if token sequence represents explicit program unit
    function is_explicit_program_unit(tokens, start_pos) result(is_program_unit)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: is_program_unit
        
        is_program_unit = .false.
        
        if (start_pos > size(tokens)) return
        
        ! Check for explicit program unit starters
        if (tokens(start_pos)%kind == TK_KEYWORD) then
            select case (trim(tokens(start_pos)%text))
            case ("program", "module", "subroutine", "function")
                is_program_unit = .true.
            end select
        end if
    end function is_explicit_program_unit

    ! Find the end of a declaration construct
    subroutine find_declaration_range(tokens, start_pos, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: end_pos
        
        integer :: i, depth
        
        i = start_pos
        depth = 0
        
        ! For type declarations, find "end type"
        if (i <= size(tokens) .and. tokens(i)%kind == TK_KEYWORD .and. &
            trim(tokens(i)%text) == "type") then
            
            ! Find matching "end type"
            do i = start_pos + 1, size(tokens)
                if (tokens(i)%kind == TK_KEYWORD) then
                    if (trim(tokens(i)%text) == "type") then
                        depth = depth + 1
                    else if (trim(tokens(i)%text) == "end") then
                        ! Check next token for "type"
                        if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD .and. &
                            trim(tokens(i + 1)%text) == "type") then
                            if (depth == 0) then
                                end_pos = i + 1
                                return
                            else
                                depth = depth - 1
                            end if
                        end if
                    end if
                end if
            end do
        else
            ! For simple declarations, just find end of statement
            do i = start_pos + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    end_pos = i
                    return
                end if
                ! Check for start of new construct
                if (is_top_level_declaration(tokens, i) .or. &
                    is_explicit_program_unit(tokens, i)) then
                    end_pos = i - 1
                    return
                end if
            end do
        end if
        
        ! Default to end of tokens
        end_pos = size(tokens)
    end subroutine find_declaration_range

    ! Find the end of a program unit construct
    subroutine find_program_unit_range(tokens, start_pos, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: end_pos
        
        integer :: i
        character(len=:), allocatable :: start_keyword, end_keyword
        
        if (start_pos > size(tokens)) then
            end_pos = start_pos
            return
        end if
        
        start_keyword = trim(tokens(start_pos)%text)
        
        ! Determine expected end keyword
        select case (start_keyword)
        case ("program")
            end_keyword = "program"
        case ("module")
            end_keyword = "module" 
        case ("subroutine")
            end_keyword = "subroutine"
        case ("function")
            end_keyword = "function"
        case default
            end_pos = start_pos
            return
        end select
        
        ! Find matching end statement
        do i = start_pos + 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD .and. trim(tokens(i)%text) == "end") then
                ! Check if next token matches our end keyword
                if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD .and. &
                    trim(tokens(i + 1)%text) == end_keyword) then
                    end_pos = i + 1
                    return
                end if
            end if
        end do
        
        ! Default to end of tokens if no matching end found
        end_pos = size(tokens)
    end subroutine find_program_unit_range

end module mixed_construct_detector