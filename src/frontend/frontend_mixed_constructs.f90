module frontend_mixed_constructs
    ! Mixed construct detection and handling functionality
    ! Supports Issue #511 - mixed module/program constructs

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                           TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: mixed_construct_container_node, create_mixed_construct_container
    use mixed_construct_detector, only: detect_mixed_constructs, mixed_construct_result_t

    implicit none
    private

    ! Public mixed constructs interface
    public :: parse_mixed_constructs, create_mixed_construct_container_arena
    public :: parse_declaration_range, parse_program_range

contains

    ! Parse mixed constructs (Issue #511 support)
    subroutine parse_mixed_constructs(tokens, arena, mixed_result, prog_index, error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(mixed_construct_result_t), intent(in) :: mixed_result
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg
        
        integer, allocatable :: implicit_indices(:), explicit_indices(:)
        integer :: i, stmt_index, range_start, range_end
        type(token_t), allocatable, target :: range_tokens(:)
        character(len=:), allocatable :: module_name
        
        error_msg = ""
        allocate(implicit_indices(0))
        allocate(explicit_indices(0))
        
        ! Generate module name (for now, use "implicit_module")
        module_name = "implicit_module"
        
        ! Parse implicit declarations
        do i = 1, mixed_result%num_implicit_ranges
            range_start = mixed_result%implicit_ranges(i, 1)
            range_end = mixed_result%implicit_ranges(i, 2)
            
            ! Extract tokens for this range
            if (range_end >= range_start .and. range_end <= size(tokens)) then
                range_tokens = tokens(range_start:range_end)
                
                ! Parse this declaration range
                call parse_declaration_range(range_tokens, arena, stmt_index, error_msg)
                
                if (len_trim(error_msg) > 0) then
                    return
                end if
                
                if (stmt_index > 0) then
                    implicit_indices = [implicit_indices, stmt_index]
                end if
            end if
        end do
        
        ! Parse explicit programs
        do i = 1, mixed_result%num_explicit_ranges
            range_start = mixed_result%explicit_ranges(i, 1)
            range_end = mixed_result%explicit_ranges(i, 2)
            
            ! Extract tokens for this range
            if (range_end >= range_start .and. range_end <= size(tokens)) then
                range_tokens = tokens(range_start:range_end)
                
                ! Parse this program range
                call parse_program_range(range_tokens, arena, stmt_index, error_msg)
                
                if (len_trim(error_msg) > 0) then
                    return
                end if
                
                if (stmt_index > 0) then
                    explicit_indices = [explicit_indices, stmt_index]
                end if
            end if
        end do
        
        ! Create the mixed construct container
        prog_index = create_mixed_construct_container_arena(arena, module_name, &
                                                          implicit_indices, explicit_indices)
    end subroutine parse_mixed_constructs

    ! Parse declaration range
    subroutine parse_declaration_range(tokens, arena, stmt_index, error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        character(len=*), intent(out) :: error_msg
        
        type(token_t), allocatable, target :: stmt_tokens(:)
        
        error_msg = ""
        
        if (size(tokens) == 0) then
            stmt_index = 0
            return
        end if
        
        ! Add EOF token if not present
        if (tokens(size(tokens))%kind /= TK_EOF) then
            allocate(stmt_tokens(size(tokens) + 1))
            stmt_tokens(1:size(tokens)) = tokens
            stmt_tokens(size(tokens) + 1)%kind = TK_EOF
            stmt_tokens(size(tokens) + 1)%text = ""
            stmt_tokens(size(tokens) + 1)%line = tokens(size(tokens))%line
            stmt_tokens(size(tokens) + 1)%column = tokens(size(tokens))%column + 1
        else
            allocate(stmt_tokens(size(tokens)))
            stmt_tokens = tokens
        end if
        
        ! Parse the declaration using statement dispatcher
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
        
        deallocate(stmt_tokens)
    end subroutine parse_declaration_range

    ! Parse program range  
    subroutine parse_program_range(tokens, arena, stmt_index, error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        character(len=*), intent(out) :: error_msg
        
        type(token_t), allocatable, target :: stmt_tokens(:)
        
        error_msg = ""
        
        if (size(tokens) == 0) then
            stmt_index = 0
            return
        end if
        
        ! Add EOF token if not present
        if (tokens(size(tokens))%kind /= TK_EOF) then
            allocate(stmt_tokens(size(tokens) + 1))
            stmt_tokens(1:size(tokens)) = tokens
            stmt_tokens(size(tokens) + 1)%kind = TK_EOF
            stmt_tokens(size(tokens) + 1)%text = ""
            stmt_tokens(size(tokens) + 1)%line = tokens(size(tokens))%line
            stmt_tokens(size(tokens) + 1)%column = tokens(size(tokens))%column + 1
        else
            allocate(stmt_tokens(size(tokens)))
            stmt_tokens = tokens
        end if
        
        ! Parse the program using statement dispatcher
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
        
        deallocate(stmt_tokens)
    end subroutine parse_program_range

    ! Create mixed construct container in arena
    function create_mixed_construct_container_arena(arena, module_name, &
                implicit_indices, explicit_indices) result(container_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        integer, intent(in) :: implicit_indices(:)
        integer, intent(in) :: explicit_indices(:)
        integer :: container_index
        
        type(mixed_construct_container_node) :: container_node
        
        ! Create the mixed construct container node
        container_node = create_mixed_construct_container(module_name, &
                                                        implicit_indices, explicit_indices)
        
        ! Add to arena using standard push method
        call arena%push(container_node, "mixed_construct_container", 0)
        container_index = arena%size
    end function create_mixed_construct_container_arena

end module frontend_mixed_constructs