
    module function infer_type(this, arena, expr_index) result(typ)
        class(semantic_context_t), intent(inout), target :: this
        type(ast_arena_t), intent(inout), target :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ
        type(infer_frame_t), allocatable :: stack(:)
        type(infer_frame_t) :: frame
        integer :: stack_size
        integer :: stack_capacity
        integer, parameter :: STATE_PRE = 0
        integer, parameter :: STATE_POST = 1
        integer, parameter :: STATE_ASSOC_DEFINE = 2

        typ = create_mono_type(TREAL)
        if (.not. arena%has_node_at(expr_index)) return

        ! Set global context for module_get_node_type_wrapper (workaround for gfortran bug)
        g_current_context => this
        g_current_arena => arena

        stack_capacity = max(16, arena%size / 4 + 1)
        allocate (stack(stack_capacity))
        stack_size = 0

        call push_pre(expr_index)

        do while (stack_size > 0)
            frame = stack(stack_size)
            stack_size = stack_size - 1

            select case (frame%state)
            case (STATE_PRE)
                call handle_previsit(frame)
            case (STATE_POST)
                call handle_postvisit(frame)
            case (STATE_ASSOC_DEFINE)
                call handle_association(frame)
            end select
        end do

        typ = get_node_type(expr_index)

        ! Clear global pointers
        g_current_context => null()
        g_current_arena => null()

    contains
        include 'semantic_analyzer_infer_type_locals_part1.inc'
        include 'semantic_analyzer_infer_type_locals_part2.inc'
        include 'semantic_analyzer_infer_type_locals_part3.inc'
    end function infer_type
