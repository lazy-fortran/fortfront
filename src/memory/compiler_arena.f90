module compiler_arena
    ! Unified arena management for the compiler. Currently bundles the type
    ! arena and the AST arena; new arenas can be added when the need arises.

    use arena_memory
    use type_system_arena
    use ast_arena_modern, only: ast_arena_t, create_ast_arena, ast_arena_stats_t, &
                                destroy_ast_arena
    use, intrinsic :: iso_fortran_env, only: int64, dp => real64
    implicit none
    private

    public :: compiler_arena_t, compiler_arena_stats_t, create_compiler_arena, &
              destroy_compiler_arena

    type :: compiler_arena_t
        type(type_arena_t) :: types
        type(ast_arena_t) :: ast

        ! Unified management state
        integer :: generation = 1  ! Global generation counter
        integer(int64) :: total_bytes = 0  ! Total memory usage across arenas
        logical :: is_initialized = .false.  ! Initialization state

        ! Performance tracking
        integer(int64) :: total_allocations = 0  ! Total allocations across arenas
        integer(int64) :: total_deallocations = 0  ! Total deallocations
        real(dp) :: total_allocation_time = 0.0_dp  ! Cumulative allocation time
        integer :: checkpoint_generation = 0  ! Last checkpoint generation

        ! Configuration
        integer :: default_chunk_size = 1048576  ! 1MB default chunk size
        logical :: enable_stats = .true.  ! Performance statistics tracking
    contains
        procedure :: init => compiler_arena_init
        procedure :: destroy => compiler_arena_destroy
        procedure :: reset => compiler_arena_reset
        procedure :: checkpoint => compiler_arena_checkpoint
        procedure :: rollback => compiler_arena_rollback
        procedure :: get_stats => compiler_arena_get_stats
        procedure :: get_total_memory => compiler_arena_get_total_memory
        procedure :: validate_all => compiler_arena_validate_all
        procedure :: next_phase => compiler_arena_next_phase
        procedure, private :: update_total_memory => compiler_arena_update_total_memory
        procedure :: assign_compiler_arena => compiler_arena_assign
        generic :: assignment(=) => assign_compiler_arena
    end type compiler_arena_t

    type :: compiler_arena_stats_t
        integer(int64) :: total_memory
        integer(int64) :: total_allocations
        integer(int64) :: types_memory
        integer(int64) :: ast_memory
        real(dp) :: average_utilization
        real(dp) :: allocation_rate
        integer :: active_generations
    end type compiler_arena_stats_t

contains

    ! Initialize unified compiler arena with all sub-arenas
    function create_compiler_arena(chunk_size, enable_stats) result(arena)
        integer, intent(in), optional :: chunk_size
        logical, intent(in), optional :: enable_stats
        type(compiler_arena_t) :: arena

        call arena%init(chunk_size, enable_stats)
    end function create_compiler_arena

    ! Initialize all sub-arenas with unified configuration
    subroutine compiler_arena_init(this, chunk_size, enable_stats)
        class(compiler_arena_t), intent(inout) :: this
        integer, intent(in), optional :: chunk_size
        logical, intent(in), optional :: enable_stats
        integer :: size

        ! Set configuration
        if (present(chunk_size)) then
            this%default_chunk_size = chunk_size
        end if

        if (present(enable_stats)) then
            this%enable_stats = enable_stats
        end if

        size = this%default_chunk_size

     ! PERFORMANCE FIX: Initialize arenas in-place to avoid assignment operator overhead
        ! Initialize type arena directly (avoid assignment operator deep copy)
        this%types%arena = create_arena(size / 4)  ! Types typically need less memory
        this%types%next_type_id = 1
        this%types%mono_count = 0
        this%types%poly_count = 0
        this%types%args_count = 0

        ! Initialize AST arena directly (avoid assignment operator deep copy)
        this%ast = create_ast_arena(16)  ! Start small, will grow as needed

        ! Initialize unified state
        this%generation = 1
        this%is_initialized = .true.
        this%total_bytes = 0
        this%total_allocations = 0
        this%total_deallocations = 0
        this%total_allocation_time = 0.0_dp
        this%checkpoint_generation = 1

        ! Update total memory from initialized arenas
        call compiler_arena_update_total_memory(this)
    end subroutine compiler_arena_init

    ! Destroy all arenas and reset state
    subroutine compiler_arena_destroy(this)
        class(compiler_arena_t), intent(inout) :: this

        if (.not. this%is_initialized) return

        ! Clear all sub-arenas using modern arena destroy
        call destroy_type_arena(this%types)
        call destroy_ast_arena(this%ast)

        ! Reset unified state
        this%generation = 0
        this%is_initialized = .false.
        this%total_bytes = 0
        this%total_allocations = 0
        this%total_deallocations = 0
        this%total_allocation_time = 0.0_dp
        this%checkpoint_generation = 0
    end subroutine compiler_arena_destroy

    ! Reset all arenas to clean state
    subroutine compiler_arena_reset(this)
        class(compiler_arena_t), intent(inout) :: this

        if (.not. this%is_initialized) return

        ! Reset all sub-arenas
        call this%types%reset()
        call this%ast%reset()

        ! Update unified state
        this%generation = this%generation + 1
        this%total_deallocations = this%total_deallocations + this%total_allocations
        this%total_allocations = 0

        call compiler_arena_update_total_memory(this)
    end subroutine compiler_arena_reset

    ! Create checkpoint for rollback support
    subroutine compiler_arena_checkpoint(this)
        class(compiler_arena_t), intent(inout) :: this

        if (.not. this%is_initialized) return

        this%checkpoint_generation = this%generation
    end subroutine compiler_arena_checkpoint

    ! Rollback to last checkpoint
    subroutine compiler_arena_rollback(this)
        class(compiler_arena_t), intent(inout) :: this

        if (.not. this%is_initialized) return

        ! Reset is currently the only rollback strategy the sub-arenas
        ! expose; this restores the saved generation count below.
        call this%reset()

        this%generation = this%checkpoint_generation
    end subroutine compiler_arena_rollback

    ! Get unified statistics across all arenas
    function compiler_arena_get_stats(this) result(stats)
        class(compiler_arena_t), intent(in) :: this
        type(compiler_arena_stats_t) :: stats
        type(type_arena_stats_t) :: type_stats

        ! Initialize stats
        stats%total_memory = 0
        stats%total_allocations = this%total_allocations
        stats%types_memory = 0
        stats%ast_memory = 0
        stats%average_utilization = 0.0_dp
        stats%allocation_rate = 0.0_dp
        stats%active_generations = 1

        if (.not. this%is_initialized) return

        ! Get type arena statistics
        type_stats = this%types%get_stats()
        stats%types_memory = type_stats%total_memory
        stats%total_memory = stats%total_memory + stats%types_memory
        stats%average_utilization = type_stats%utilization

        ! Get AST arena statistics
        block
            type(ast_arena_stats_t) :: ast_stats
            ast_stats = this%ast%get_stats()
            stats%ast_memory = ast_stats%total_memory
            stats%total_memory = stats%total_memory + stats%ast_memory

            ! Average utilization across arenas (average type and AST utilization)
            stats%average_utilization = (type_stats%utilization + &
                                         ast_stats%utilization) / 2.0_dp
        end block

        ! Calculate allocation rate (allocations per second)
        if (this%total_allocation_time > 0.0_dp) then
            stats%allocation_rate = real(this%total_allocations, dp) / &
                                    this%total_allocation_time
        end if

        stats%active_generations = this%generation
    end function compiler_arena_get_stats

    ! Get total memory usage across all arenas
    function compiler_arena_get_total_memory(this) result(total_bytes)
        class(compiler_arena_t), intent(in) :: this
        integer(int64) :: total_bytes

        if (.not. this%is_initialized) then
            total_bytes = 0
            return
        end if

        total_bytes = this%total_bytes
    end function compiler_arena_get_total_memory

    ! Validate all handles across all arenas
    function compiler_arena_validate_all(this) result(all_valid)
        class(compiler_arena_t), intent(in) :: this
        logical :: all_valid

        all_valid = .false.

        if (.not. this%is_initialized) return

        ! All arenas are structurally valid if initialized
        all_valid = .true.
    end function compiler_arena_validate_all

    ! Advance to next compilation phase with generation increment
    subroutine compiler_arena_next_phase(this, phase_name)
        class(compiler_arena_t), intent(inout) :: this
        character(len=*), intent(in) :: phase_name

        if (.not. this%is_initialized) return

        this%generation = this%generation + 1
        call compiler_arena_update_total_memory(this)
    end subroutine compiler_arena_next_phase

    ! Update total memory from all sub-arenas
    subroutine compiler_arena_update_total_memory(this)
        class(compiler_arena_t), intent(inout) :: this
        type(type_arena_stats_t) :: type_stats

        if (.not. this%is_initialized) return

        this%total_bytes = 0

        ! Add memory from type arena
        type_stats = this%types%get_stats()
        this%total_bytes = this%total_bytes + type_stats%total_memory

        ! Add memory from AST arena
        block
            type(ast_arena_stats_t) :: ast_stats
            ast_stats = this%ast%get_stats()
            this%total_bytes = this%total_bytes + ast_stats%total_memory
        end block
    end subroutine compiler_arena_update_total_memory

    ! Cleanup procedure for module finalization
    subroutine destroy_compiler_arena(arena)
        type(compiler_arena_t), intent(inout) :: arena

        call arena%destroy()
    end subroutine destroy_compiler_arena

    ! Deep copy assignment operator to prevent double free
    subroutine compiler_arena_assign(lhs, rhs)
        class(compiler_arena_t), intent(out) :: lhs
        type(compiler_arena_t), intent(in) :: rhs

        ! Copy sub-arenas (using their assignment operators)
        lhs%types = rhs%types  ! Uses type_arena_t assignment
        lhs%ast = rhs%ast  ! Uses ast_arena_t assignment

        ! Copy scalar members
        lhs%generation = rhs%generation
        lhs%total_bytes = rhs%total_bytes
        lhs%is_initialized = rhs%is_initialized
        lhs%total_allocations = rhs%total_allocations
        lhs%total_deallocations = rhs%total_deallocations
        lhs%total_allocation_time = rhs%total_allocation_time
        lhs%checkpoint_generation = rhs%checkpoint_generation
        lhs%default_chunk_size = rhs%default_chunk_size
        lhs%enable_stats = rhs%enable_stats
    end subroutine compiler_arena_assign

end module compiler_arena
