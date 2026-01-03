module codegen_program_header
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, literal_node
    use ast_nodes_misc, only: comment_node, directive_node, blank_line_node, &
                              implicit_statement_node, use_statement_node, &
                              intrinsic_statement_node, interface_block_node, &
                              namelist_statement_node
    use ast_nodes_data, only: derived_type_node, declaration_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_declarations_inference, only: collect_program_variable_decls
    use codegen_indent, only: indent_lines
    use codegen_grouped_body, only: generate_grouped_body
    use string_utils_mod, only: to_lower
    use standardizer_parameter, only: get_standardizer_input_mode
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none
    private
    public :: assemble_program_header

contains

    subroutine assemble_program_header(arena, node, code, non_use_indices, &
                                       non_use_count, extra_decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        character(len=:), allocatable, intent(out) :: extra_decl_code
        logical :: has_implicit
        character(len=:), allocatable :: use_statements_code
        character(len=:), allocatable :: implicit_statements_code
        character(len=:), allocatable :: intrinsic_statements_code
        character(len=:), allocatable :: interface_blocks_code
        character(len=:), allocatable :: declaration_statements_code
        character(len=:), allocatable :: legacy_storage_code
        character(len=:), allocatable :: extra_decls

        call gather_program_header_entries(arena, node, has_implicit, &
                                           use_statements_code, &
                                           intrinsic_statements_code, &
                                           implicit_statements_code, &
                                           declaration_statements_code, &
                                           legacy_storage_code, &
                                           interface_blocks_code, non_use_indices, &
                                           non_use_count)

        if (len(use_statements_code) > 0) code = code // use_statements_code

        if (len(implicit_statements_code) > 0) then
            code = code // implicit_statements_code
        else if (.not. has_implicit) then
            ! Only add implicit none for lazy Fortran mode
            ! For standard Fortran, preserve original implicit typing rules
            if (get_standardizer_input_mode() /= INPUT_MODE_STANDARD) then
                code = code // "    implicit none" // new_line('A')
            end if
        end if

        if (len(intrinsic_statements_code) > 0) then
            code = code // intrinsic_statements_code
        end if

        if (len(declaration_statements_code) > 0) then
            code = code // declaration_statements_code
        end if

        extra_decl_code = ""
        ! Pass both declaration and legacy storage code (includes namelist) so that
        ! namelist groups can be identified before variable declarations are generated
        extra_decls = collect_program_variable_decls(arena, node, &
                                                     declaration_statements_code// &
                                                     legacy_storage_code)
        if (len_trim(extra_decls) > 0) then
            code = code // extra_decls
        end if

        if (len(legacy_storage_code) > 0) code = code // legacy_storage_code

        if (len(interface_blocks_code) > 0) code = code // interface_blocks_code
    end subroutine assemble_program_header

    subroutine gather_program_header_entries(arena, node, has_implicit, &
                                             use_statements_code, &
                                             intrinsic_statements_code, &
                                             implicit_statements_code, &
                                             declaration_statements_code, &
                                             legacy_storage_code, &
                                             interface_blocks_code, non_use_indices, &
                                             non_use_count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        logical, intent(out) :: has_implicit
        character(len=:), allocatable, intent(out) :: use_statements_code
        character(len=:), allocatable, intent(out) :: intrinsic_statements_code
        character(len=:), allocatable, intent(out) :: implicit_statements_code
        character(len=:), allocatable, intent(out) :: declaration_statements_code
        character(len=:), allocatable, intent(out) :: legacy_storage_code
        character(len=:), allocatable, intent(out) :: interface_blocks_code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        integer :: i
        integer, allocatable :: header_decl_indices(:)
        integer :: header_decl_count

        has_implicit = .false.
        use_statements_code = ""
        intrinsic_statements_code = ""
        implicit_statements_code = ""
        declaration_statements_code = ""
        legacy_storage_code = ""
        interface_blocks_code = ""
        non_use_count = 0

        if (.not. allocated(node%body_indices)) then
            allocate (non_use_indices(0))
            return
        end if

        allocate (non_use_indices(size(node%body_indices)))
        allocate (header_decl_indices(size(node%body_indices)))
        header_decl_count = 0

        do i = 1, size(node%body_indices)
            call categorize_header_entry(arena, node, node%body_indices(i), &
                                         has_implicit, use_statements_code, &
                                         intrinsic_statements_code, &
                                         implicit_statements_code, &
                                         legacy_storage_code, &
                                         interface_blocks_code, non_use_indices, &
                                         non_use_count, header_decl_indices, &
                                         header_decl_count)
        end do

        if (header_decl_count > 0) then
            associate (decl_indices => header_decl_indices(:header_decl_count))
                declaration_statements_code = generate_grouped_body( &
                                              arena, decl_indices, 1)
            end associate
        end if

        if (allocated(header_decl_indices)) then
            deallocate (header_decl_indices)
        end if
    end subroutine gather_program_header_entries

    subroutine categorize_header_entry(arena, prog_node, body_index, has_implicit, &
                                       use_statements_code, &
                                       intrinsic_statements_code, &
                                       implicit_statements_code, &
                                       legacy_storage_code, &
                                       interface_blocks_code, &
                                       non_use_indices, non_use_count, &
                                       header_decl_indices, &
                                       header_decl_count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog_node
        integer, intent(in) :: body_index
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        integer, intent(inout) :: non_use_indices(:)
        integer, intent(inout) :: non_use_count
        integer, intent(inout) :: header_decl_indices(:)
        integer, intent(inout) :: header_decl_count
        logical :: is_header_stmt

        if (.not. arena%has_node_at(body_index)) return

        is_header_stmt = classify_and_process_header_node(arena, body_index, &
                                                          has_implicit, &
                                                          use_statements_code, &
                                                          intrinsic_statements_code, &
                                                          implicit_statements_code, &
                                                          legacy_storage_code, &
                                                          interface_blocks_code, &
                                                          non_use_count, &
                                                          header_decl_count, &
                                                          header_decl_indices)

        if (is_header_stmt) return

        non_use_count = non_use_count + 1
        if (non_use_count <= size(non_use_indices)) then
            non_use_indices(non_use_count) = body_index
        end if
    end subroutine categorize_header_entry

    logical function classify_and_process_header_node(arena, body_index, &
                                                      has_implicit, &
                                                      use_statements_code, &
                                                      intrinsic_statements_code, &
                                                      implicit_statements_code, &
                                                      legacy_storage_code, &
                                                      interface_blocks_code, &
                                                      non_use_count, &
                                                      header_decl_count, &
                                                      header_decl_indices) &
        result(is_header)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        integer, intent(in) :: non_use_count
        integer, intent(inout) :: header_decl_count
        integer, intent(inout) :: header_decl_indices(:)
        character(len=:), allocatable :: stmt_code

        is_header = .false.

        select type (ib => arena%entries(body_index)%node)
        type is (use_statement_node)
            call process_use_statement(arena, body_index, use_statements_code)
            is_header = .true.
        type is (intrinsic_statement_node)
            call process_intrinsic_statement(arena, body_index, &
                                             intrinsic_statements_code)
            is_header = .true.
        type is (comment_node)
            is_header = process_comment_node(arena, body_index, ib, non_use_count, &
                                             header_decl_count, use_statements_code, &
                                             intrinsic_statements_code, &
                                             implicit_statements_code, &
                                             legacy_storage_code, &
                                             interface_blocks_code)
        type is (directive_node)
            is_header = process_directive_node(arena, body_index, ib, &
                                               non_use_count, header_decl_count, &
                                               use_statements_code, &
                                               intrinsic_statements_code, &
                                               implicit_statements_code, &
                                               legacy_storage_code, &
                                               interface_blocks_code)
        type is (blank_line_node)
            is_header = process_blank_line(arena, body_index, non_use_count, &
                                           header_decl_count, use_statements_code, &
                                           intrinsic_statements_code, &
                                           implicit_statements_code, &
                                           interface_blocks_code)
        type is (implicit_statement_node)
            call process_implicit_statement(arena, body_index, ib, has_implicit, &
                                            implicit_statements_code)
            is_header = .true.
        type is (derived_type_node)
            call process_derived_type(arena, body_index, implicit_statements_code)
            is_header = .true.
        type is (declaration_node)
            if (.not. ib%has_intent) then
                call process_declaration(body_index, header_decl_count, &
                                         header_decl_indices)
                is_header = .true.
            end if
        type is (interface_block_node)
            call process_interface_block(arena, body_index, interface_blocks_code)
            is_header = .true.
        type is (namelist_statement_node)
            call process_namelist_statement(arena, body_index, legacy_storage_code)
            is_header = .true.
        type is (literal_node)
            is_header = process_literal_node(ib, has_implicit, &
                                             implicit_statements_code)
        end select
    end function classify_and_process_header_node

    subroutine process_use_statement(arena, body_index, use_statements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, body_index)
        use_statements_code = use_statements_code // "    " // stmt_code // &
                              new_line('A')
    end subroutine process_use_statement

    subroutine process_intrinsic_statement(arena, body_index, &
                                           intrinsic_statements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, body_index)
        intrinsic_statements_code = intrinsic_statements_code // "    " // &
                                    stmt_code // new_line('A')
    end subroutine process_intrinsic_statement

    logical function process_comment_node(arena, body_index, ib, non_use_count, &
                                          header_decl_count, use_statements_code, &
                                          intrinsic_statements_code, &
                                          implicit_statements_code, &
                                          legacy_storage_code, &
                                          interface_blocks_code) result(is_header)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        type(comment_node), intent(in) :: ib
        integer, intent(in) :: non_use_count
        integer, intent(in) :: header_decl_count
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        character(len=:), allocatable :: comment_text

        if (allocated(ib%text)) then
            comment_text = ib%text
        else
            comment_text = ""
        end if

        is_header = process_comment_like_node(arena, body_index, &
                                              is_legacy_storage_text(comment_text), &
                                              non_use_count, &
                                              header_decl_count, use_statements_code, &
                                              intrinsic_statements_code, &
                                              implicit_statements_code, &
                                              legacy_storage_code, &
                                              interface_blocks_code)
    end function process_comment_node

    logical function process_directive_node(arena, body_index, ib, non_use_count, &
                                            header_decl_count, use_statements_code, &
                                            intrinsic_statements_code, &
                                            implicit_statements_code, &
                                            legacy_storage_code, &
                                            interface_blocks_code) result(is_header)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        type(directive_node), intent(in) :: ib
        integer, intent(in) :: non_use_count
        integer, intent(in) :: header_decl_count
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        character(len=:), allocatable :: directive_text

        if (allocated(ib%text)) then
            directive_text = ib%text
        else
            directive_text = ""
        end if

        is_header = process_comment_like_node(arena, body_index, &
                                              is_legacy_storage_text(directive_text), &
                                              non_use_count, &
                                              header_decl_count, use_statements_code, &
                                              intrinsic_statements_code, &
                                              implicit_statements_code, &
                                              legacy_storage_code, &
                                              interface_blocks_code)
    end function process_directive_node

    logical function process_comment_like_node(arena, body_index, is_legacy_storage, &
                                               non_use_count, header_decl_count, &
                                               use_statements_code, &
                                               intrinsic_statements_code, &
                                               implicit_statements_code, &
                                               legacy_storage_code, &
                                               interface_blocks_code) result(is_header)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        logical, intent(in) :: is_legacy_storage
        integer, intent(in) :: non_use_count
        integer, intent(in) :: header_decl_count
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        character(len=:), allocatable :: stmt_code

        is_header = .false.
        if (is_legacy_storage .or. ((non_use_count == 0) .and. &
                                    header_decl_count == 0)) then
            is_header = .true.
            stmt_code = generate_code_from_arena(arena, body_index)
            if (is_legacy_storage) then
                legacy_storage_code = legacy_storage_code // "    " // &
                                      stmt_code // new_line('A')
            else
                call append_header_trivia(stmt_code, use_statements_code, &
                                          intrinsic_statements_code, &
                                          implicit_statements_code, &
                                          interface_blocks_code)
            end if
        end if
    end function process_comment_like_node

    logical function process_blank_line(arena, body_index, non_use_count, &
                                        header_decl_count, use_statements_code, &
                                        intrinsic_statements_code, &
                                        implicit_statements_code, &
                                        interface_blocks_code) result(is_header)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        integer, intent(in) :: non_use_count
        integer, intent(in) :: header_decl_count
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: intrinsic_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        character(len=:), allocatable :: stmt_code

        is_header = .false.
        if (non_use_count == 0 .and. header_decl_count == 0) then
            is_header = .true.
            stmt_code = generate_code_from_arena(arena, body_index)
            call append_header_trivia(stmt_code, use_statements_code, &
                                      intrinsic_statements_code, &
                                      implicit_statements_code, &
                                      interface_blocks_code)
        end if
    end function process_blank_line

    subroutine process_implicit_statement(arena, body_index, ib, has_implicit, &
                                          implicit_statements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        type(implicit_statement_node), intent(in) :: ib
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable :: stmt_code

        if (ib%is_none) has_implicit = .true.
        stmt_code = generate_code_from_arena(arena, body_index)
        if (len_trim(stmt_code) > 0) then
            implicit_statements_code = implicit_statements_code // "    " // &
                                       trim(stmt_code) // new_line('A')
        end if
    end subroutine process_implicit_statement

    subroutine process_derived_type(arena, body_index, implicit_statements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, body_index)
        if (len_trim(stmt_code) > 0) then
            implicit_statements_code = implicit_statements_code // &
                                       indent_lines(stmt_code, 1)
            if (stmt_code(len(stmt_code):len(stmt_code)) /= new_line('A')) then
                implicit_statements_code = implicit_statements_code // new_line('A')
            end if
        end if
    end subroutine process_derived_type

    subroutine process_declaration(body_index, header_decl_count, header_decl_indices)
        integer, intent(in) :: body_index
        integer, intent(inout) :: header_decl_count
        integer, intent(inout) :: header_decl_indices(:)

        if (header_decl_count < size(header_decl_indices)) then
            header_decl_count = header_decl_count + 1
            header_decl_indices(header_decl_count) = body_index
        end if
    end subroutine process_declaration

    subroutine process_interface_block(arena, body_index, interface_blocks_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, body_index)
        if (len_trim(stmt_code) > 0) then
            interface_blocks_code = interface_blocks_code // &
                                    indent_lines(stmt_code, 1) // new_line('A')
        end if
    end subroutine process_interface_block

    subroutine process_namelist_statement(arena, body_index, legacy_storage_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=:), allocatable, intent(inout) :: legacy_storage_code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, body_index)
        if (len_trim(stmt_code) > 0) then
            legacy_storage_code = legacy_storage_code // "    " // stmt_code // &
                                  new_line('A')
        end if
    end subroutine process_namelist_statement

    logical function process_literal_node(ib, has_implicit, implicit_statements_code) &
        result(is_header)
        type(literal_node), intent(in) :: ib
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable :: lowered_value

        is_header = .false.
        if (allocated(ib%value)) then
            lowered_value = to_lower(ib%value)
            if (index(lowered_value, 'implicit none') > 0) then
                has_implicit = .true.
                is_header = .true.
                if (len_trim(ib%value) > 0) then
                    implicit_statements_code = implicit_statements_code // &
                                               "    " // trim(ib%value) // &
                                               new_line('A')
                end if
            end if
        end if
    end function process_literal_node

    subroutine append_header_trivia(fragment, use_code, intrinsic_code, &
                                    implicit_code, &
                                    interface_code)
        character(len=*), intent(in) :: fragment
        character(len=:), allocatable, intent(inout) :: use_code
        character(len=:), allocatable, intent(inout) :: intrinsic_code
        character(len=:), allocatable, intent(inout) :: implicit_code
        character(len=:), allocatable, intent(inout) :: interface_code
        character(len=:), allocatable :: trimmed_fragment
        logical :: is_blank

        if (len(fragment) == 0) return

        trimmed_fragment = fragment
        is_blank = len_trim(trimmed_fragment) == 0

        if (len(interface_code) > 0) then
            if (is_blank) then
                interface_code = interface_code // trimmed_fragment
            else
                interface_code = interface_code // "    " // &
                                 trim(trimmed_fragment) // new_line('A')
            end if
        else if (len(intrinsic_code) > 0) then
            if (is_blank) then
                intrinsic_code = intrinsic_code // trimmed_fragment
            else
                intrinsic_code = intrinsic_code // "    " // &
                                 trim(trimmed_fragment) // new_line('A')
            end if
        else if (len(implicit_code) > 0) then
            if (is_blank) then
                implicit_code = implicit_code // trimmed_fragment
            else
                implicit_code = implicit_code // "    " // trim(trimmed_fragment) // &
                                new_line('A')
            end if
        else
            if (is_blank) then
                use_code = use_code // trimmed_fragment
            else
                use_code = use_code // "    " // trim(trimmed_fragment) // &
                           new_line('A')
            end if
        end if
    end subroutine append_header_trivia

    logical function is_legacy_storage_text(text)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered_text

        is_legacy_storage_text = .false.
        if (len(text) == 0) return

        lowered_text = to_lower(adjustl(trim(text)))
        if (len_trim(lowered_text) >= 11) then
            if (index(lowered_text, "equivalence") == 1) then
                is_legacy_storage_text = .true.
                return
            end if
        end if
        if (len_trim(lowered_text) >= 6) then
            if (index(lowered_text, "common") == 1) then
                is_legacy_storage_text = .true.
                return
            end if
        end if
    end function is_legacy_storage_text

end module codegen_program_header
