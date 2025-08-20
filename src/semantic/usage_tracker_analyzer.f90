module usage_tracker_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use variable_usage_tracker_module, only: variable_usage_info_t, &
                                             create_variable_usage_info, &
                                             get_identifiers_in_subtree, &
                                             is_variable_used_in_expression
    implicit none
    private

    public :: usage_tracker_analyzer_t

    ! Variable usage tracking result
    type :: usage_analysis_result_t
        character(:), allocatable :: unused_variables(:)
        character(:), allocatable :: undefined_variables(:)
        type(variable_usage_info_t) :: usage_info
        integer, allocatable :: unused_node_indices(:)
        integer, allocatable :: undefined_node_indices(:)
    contains
        procedure :: assign_usage_result
        generic :: assignment(=) => assign_usage_result
    end type

    ! Usage tracker analyzer plugin  
    type, extends(semantic_analyzer_t) :: usage_tracker_analyzer_t
        type(usage_analysis_result_t) :: result
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_variable_usage
        procedure :: get_results => get_usage_results
        procedure :: get_name => get_usage_analyzer_name
        procedure :: assign => assign_usage_tracker_analyzer
        procedure :: get_dependencies => get_usage_dependencies
        procedure :: reset_state => reset_usage_tracker_state
        
        ! Analysis methods for fluff rules
        procedure :: find_unused_variables
        procedure :: find_undefined_variables
        procedure :: get_variable_usage_info
        procedure :: is_variable_used
        procedure :: get_usage_locations
    end type

contains

    subroutine analyze_variable_usage(this, shared_context, arena, node_index)
        class(usage_tracker_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        
        ! Reset state before analysis to handle repeated use
        call this%reset_state()
        
        ! Initialize usage info
        this%result%usage_info = create_variable_usage_info()
        
        ! Analyze variable usage throughout the AST
        call collect_variable_usage(this%result, arena, node_index)
        
        ! Identify unused and undefined variables
        call identify_unused_variables(this%result, arena, node_index)
        call identify_undefined_variables(this%result, arena, node_index)
        
        this%analysis_complete = .true.
    end subroutine

    function get_usage_results(this) result(results)
        class(usage_tracker_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
        ! Return the usage analysis result
        allocate(usage_analysis_result_t :: results)
        select type(results)
        type is (usage_analysis_result_t)
            results = this%result
        end select
    end function

    function get_usage_analyzer_name(this) result(name)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "usage_tracker_analyzer"
    end function

    subroutine assign_usage_tracker_analyzer(lhs, rhs)
        use semantic_analyzer_base, only: semantic_analyzer_t
        class(usage_tracker_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (usage_tracker_analyzer_t)
            ! Deep copy the result
            lhs%result = rhs%result
            lhs%analysis_complete = rhs%analysis_complete
        class default
            error stop "Type mismatch in usage_tracker_analyzer assignment"
        end select
    end subroutine

    ! Analysis methods for fluff rules
    function find_unused_variables(this) result(unused_vars)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(:), allocatable :: unused_vars(:)
        
        if (.not. this%analysis_complete) then
            allocate(character(0) :: unused_vars(0))
            return
        end if
        
        unused_vars = this%result%unused_variables
    end function

    function find_undefined_variables(this) result(undefined_vars)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(:), allocatable :: undefined_vars(:)
        
        if (.not. this%analysis_complete) then
            allocate(character(0) :: undefined_vars(0))
            return
        end if
        
        undefined_vars = this%result%undefined_variables
    end function

    function get_variable_usage_info(this) result(usage_info)
        class(usage_tracker_analyzer_t), intent(in) :: this
        type(variable_usage_info_t) :: usage_info
        
        if (this%analysis_complete) then
            usage_info = this%result%usage_info
        else
            usage_info = create_variable_usage_info()
        end if
    end function

    function is_variable_used(this, variable_name) result(used)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(*), intent(in) :: variable_name
        logical :: used
        
        integer :: i
        
        if (.not. this%analysis_complete) then
            used = .false.
            return
        end if
        
        used = .false.
        if (allocated(this%result%usage_info%variable_names)) then
            do i = 1, size(this%result%usage_info%variable_names)
                if (this%result%usage_info%variable_names(i) == variable_name) then
                    used = this%result%usage_info%usage_counts(i) > 0
                    exit
                end if
            end do
        end if
    end function

    function get_usage_locations(this, variable_name) result(locations)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(*), intent(in) :: variable_name
        integer, allocatable :: locations(:)
        
        integer :: i
        
        if (.not. this%analysis_complete) then
            allocate(locations(0))
            return
        end if
        
        ! For simplicity, return single location per variable
        ! In full implementation, would track all usage locations
        allocate(locations(0))
        if (allocated(this%result%usage_info%variable_names)) then
            do i = 1, size(this%result%usage_info%variable_names)
                if (this%result%usage_info%variable_names(i) == variable_name) then
                    allocate(locations(1))
                    locations(1) = this%result%usage_info%node_indices(i)
                    exit
                end if
            end do
        end if
    end function

    ! Assignment operator for usage_analysis_result_t
    subroutine assign_usage_result(lhs, rhs)
        class(usage_analysis_result_t), intent(inout) :: lhs
        type(usage_analysis_result_t), intent(in) :: rhs
        
        ! Clear any existing allocations
        if (allocated(lhs%unused_variables)) deallocate(lhs%unused_variables)
        if (allocated(lhs%undefined_variables)) deallocate(lhs%undefined_variables)
        if (allocated(lhs%unused_node_indices)) deallocate(lhs%unused_node_indices)
        if (allocated(lhs%undefined_node_indices)) deallocate(lhs%undefined_node_indices)
        
        ! Deep copy allocatable arrays
        if (allocated(rhs%unused_variables)) then
            allocate(character(len(rhs%unused_variables)) :: lhs%unused_variables(size(rhs%unused_variables)))
            lhs%unused_variables = rhs%unused_variables
        end if
        
        if (allocated(rhs%undefined_variables)) then
            allocate(character(len(rhs%undefined_variables)) :: lhs%undefined_variables(size(rhs%undefined_variables)))
            lhs%undefined_variables = rhs%undefined_variables
        end if
        
        if (allocated(rhs%unused_node_indices)) then
            allocate(lhs%unused_node_indices(size(rhs%unused_node_indices)))
            lhs%unused_node_indices = rhs%unused_node_indices
        end if
        
        if (allocated(rhs%undefined_node_indices)) then
            allocate(lhs%undefined_node_indices(size(rhs%undefined_node_indices)))
            lhs%undefined_node_indices = rhs%undefined_node_indices
        end if
        
        ! Copy usage_info (this has its own assignment operator)
        lhs%usage_info = rhs%usage_info
    end subroutine

    ! Reset analyzer state for repeated use
    subroutine reset_usage_tracker_state(this)
        class(usage_tracker_analyzer_t), intent(inout) :: this
        
        ! Clear allocatable arrays in result
        if (allocated(this%result%unused_variables)) deallocate(this%result%unused_variables)
        if (allocated(this%result%undefined_variables)) deallocate(this%result%undefined_variables)
        if (allocated(this%result%unused_node_indices)) deallocate(this%result%unused_node_indices)
        if (allocated(this%result%undefined_node_indices)) deallocate(this%result%undefined_node_indices)
        
        ! Reset usage info
        this%result%usage_info = create_variable_usage_info()
        
        ! Reset completion flag
        this%analysis_complete = .false.
    end subroutine

    ! Helper subroutines for analysis
    subroutine collect_variable_usage(result, arena, root_index)
        type(usage_analysis_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        character(:), allocatable :: identifiers(:)
        integer :: i, j, count
        integer, allocatable :: temp_counts(:)
        character(:), allocatable :: temp_names(:)
        
        ! Get all identifiers in the subtree
        identifiers = get_identifiers_in_subtree(arena, root_index)
        
        if (size(identifiers) == 0) return
        
        ! Count unique variables
        count = 1
        allocate(character(len(identifiers(1))) :: temp_names(size(identifiers)))
        allocate(temp_counts(size(identifiers)))
        
        temp_names(1) = identifiers(1)
        temp_counts(1) = 1
        
        do i = 2, size(identifiers)
            ! Check if this identifier already exists
            do j = 1, count
                if (temp_names(j) == identifiers(i)) then
                    temp_counts(j) = temp_counts(j) + 1
                    exit
                end if
            end do
            
            ! If we didn't find it, add new variable
            if (j > count) then
                count = count + 1
                temp_names(count) = identifiers(i)
                temp_counts(count) = 1
            end if
        end do
        
        ! Store results
        allocate(character(len(temp_names(1))) :: &
            result%usage_info%variable_names(count))
        allocate(result%usage_info%usage_counts(count))
        allocate(result%usage_info%node_indices(count))
        
        result%usage_info%variable_names(1:count) = temp_names(1:count)
        result%usage_info%usage_counts(1:count) = temp_counts(1:count)
        result%usage_info%total_count = count
        
        ! For simplicity, set node indices to root (would need more &
        ! sophisticated tracking)
        do i = 1, count
            result%usage_info%node_indices(i) = root_index
        end do
    end subroutine

    subroutine identify_unused_variables(result, arena, root_index)
        type(usage_analysis_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        integer :: i, unused_count
        character(:), allocatable :: temp_unused(:)
        
        ! Find variables with zero usage (declared but not used)
        unused_count = 0
        if (allocated(result%usage_info%variable_names)) then
            allocate(character(len(result%usage_info%variable_names(1))) :: &
                temp_unused(size(result%usage_info%variable_names)))
            
            do i = 1, size(result%usage_info%usage_counts)
                if (result%usage_info%usage_counts(i) == 0) then
                    unused_count = unused_count + 1
                    temp_unused(unused_count) = result%usage_info%variable_names(i)
                end if
            end do
            
            if (unused_count > 0) then
                allocate(character(len(temp_unused(1))) :: &
                    result%unused_variables(unused_count))
                result%unused_variables(1:unused_count) = temp_unused(1:unused_count)
                
                allocate(result%unused_node_indices(unused_count))
                ! Set to corresponding node indices
                do i = 1, unused_count
                    result%unused_node_indices(i) = root_index ! Simplified
                end do
            else
                allocate(character(0) :: result%unused_variables(0))
                allocate(result%unused_node_indices(0))
            end if
        else
            allocate(character(0) :: result%unused_variables(0))
            allocate(result%unused_node_indices(0))
        end if
    end subroutine

    subroutine identify_undefined_variables(result, arena, root_index)
        type(usage_analysis_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        ! For now, assume all variables are defined somewhere
        ! Full implementation would check against declarations
        allocate(character(0) :: result%undefined_variables(0))
        allocate(result%undefined_node_indices(0))
        
        ! Real implementation would:
        ! 1. Collect all variable declarations
        ! 2. Compare usage against declarations  
        ! 3. Flag variables used but not declared
    end subroutine

    function get_usage_dependencies(this) result(deps)
        class(usage_tracker_analyzer_t), intent(in) :: this
        character(len=32), allocatable :: deps(:)
        
        ! Usage tracker analyzer has no dependencies
        allocate(deps(0))
        
        associate(dummy => this)
        end associate
    end function

end module usage_tracker_analyzer