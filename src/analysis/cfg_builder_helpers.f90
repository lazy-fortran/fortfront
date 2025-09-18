module cfg_builder_helpers
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena_modern
    use control_flow_graph_module
    use cfg_builder_types
    implicit none
    private

    ! Public interface
    public :: add_statement_to_buffer, flush_statement_buffer
    public :: init_frame_stack, push_frame, pop_frame, frame_stack_is_empty

contains

    ! Add statement to buffer
    subroutine add_statement_to_buffer(builder, stmt_index)
        type(cfg_builder_t), intent(inout) :: builder
        integer, intent(in) :: stmt_index
        
        integer, allocatable :: temp_buffer(:)
        
        if (stmt_index <= 0) return
        
        ! If no current block (after return/stop), create or reuse unreachable block
        if (builder%current_block_id == 0) then
            ! Look for existing unreachable block first to avoid creating multiple ones
            block
                integer :: i
                do i = 1, builder%cfg%block_count
                    if (.not. builder%cfg%blocks(i)%is_reachable .and. &
                        .not. builder%cfg%blocks(i)%is_entry .and. &
                        .not. builder%cfg%blocks(i)%is_exit) then
                        builder%current_block_id = i
                        return
                    end if
                end do
            end block
            
            ! No existing unreachable block found, create new one
            builder%current_block_id = add_basic_block(builder%cfg, "unreachable")
            ! Note: is_reachable is false by default, so this creates an unreachable block
        end if
        
        ! Expand buffer if needed
        if (builder%buffer_size >= builder%buffer_capacity) then
            builder%buffer_capacity = builder%buffer_capacity * 2
            allocate(temp_buffer(builder%buffer_capacity))
            temp_buffer(1:builder%buffer_size) = builder%statement_buffer(1:builder%buffer_size)
            
            ! Use move_alloc for O(1) performance instead of O(n) copying
            call move_alloc(temp_buffer, builder%statement_buffer)
        end if
        
        builder%buffer_size = builder%buffer_size + 1
        builder%statement_buffer(builder%buffer_size) = stmt_index
    end subroutine add_statement_to_buffer

    ! Flush statement buffer to current block
    subroutine flush_statement_buffer(builder)
        type(cfg_builder_t), intent(inout) :: builder
        
        integer :: i
        
        if (builder%buffer_size == 0) return
        if (builder%current_block_id == 0) return
        
        ! Add buffered statements to current block
        do i = 1, builder%buffer_size
            builder%cfg%blocks(builder%current_block_id)%statement_indices = &
                [builder%cfg%blocks(builder%current_block_id)%statement_indices, &
                 builder%statement_buffer(i)]
        end do
        
        ! Update entry/exit statements
        if (size(builder%cfg%blocks(builder%current_block_id)%statement_indices) > 0) then
            builder%cfg%blocks(builder%current_block_id)%entry_statement = &
                builder%cfg%blocks(builder%current_block_id)%statement_indices(1)
            builder%cfg%blocks(builder%current_block_id)%exit_statement = &
                builder%cfg%blocks(builder%current_block_id)%statement_indices( &
                    size(builder%cfg%blocks(builder%current_block_id)%statement_indices))
        end if
        
        ! Clear buffer
        builder%buffer_size = 0
    end subroutine flush_statement_buffer

    subroutine init_frame_stack(stack, initial_capacity)
        type(cfg_frame_stack_t), intent(inout) :: stack
        integer, intent(in), optional :: initial_capacity
        integer :: capacity

        if (allocated(stack%items)) deallocate(stack%items)
        capacity = 64
        if (present(initial_capacity)) capacity = max(32, initial_capacity)
        allocate(stack%items(capacity))
        stack%top = 0
    end subroutine init_frame_stack

    subroutine ensure_stack_capacity(stack, required)
        type(cfg_frame_stack_t), intent(inout) :: stack
        integer, intent(in) :: required
        type(cfg_frame_t), allocatable :: temp(:)

        if (.not. allocated(stack%items)) then
            call init_frame_stack(stack, max(required, 32))
            return
        end if

        if (required <= size(stack%items)) return

        allocate(temp(max(size(stack%items) * 2, required)))
        if (stack%top > 0) temp(1:stack%top) = stack%items(1:stack%top)
        call move_alloc(temp, stack%items)
    end subroutine ensure_stack_capacity

    subroutine push_frame(stack, frame)
        type(cfg_frame_stack_t), intent(inout) :: stack
        type(cfg_frame_t), intent(in) :: frame

        if (.not. allocated(stack%items)) then
            call init_frame_stack(stack)
        end if
        call ensure_stack_capacity(stack, stack%top + 1)
        stack%top = stack%top + 1
        stack%items(stack%top) = frame
    end subroutine push_frame

    function pop_frame(stack) result(frame)
        type(cfg_frame_stack_t), intent(inout) :: stack
        type(cfg_frame_t) :: frame

        if (stack%top <= 0) then
            frame = cfg_frame_t()
            return
        end if

        frame = stack%items(stack%top)
        stack%top = stack%top - 1
    end function pop_frame

    logical function frame_stack_is_empty(stack)
        type(cfg_frame_stack_t), intent(in) :: stack
        frame_stack_is_empty = (stack%top <= 0)
    end function frame_stack_is_empty

end module cfg_builder_helpers
