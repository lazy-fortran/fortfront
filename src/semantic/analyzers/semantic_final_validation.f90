module semantic_final_validation
    ! Validates FINAL subroutines of a derived type against F2018 C789/C790
    ! (F2003 C476/C477): a final subroutine has exactly one dummy argument, and
    ! that argument is a nonpointer, nonallocatable, nonpolymorphic variable of
    ! the type being finalized. An alternate return indicator is not a data
    ! object and therefore cannot be that argument.
    !
    ! Only the two narrow rules above are enforced. Rank, kind and length type
    ! parameter agreement between distinct final subroutines of one type is not
    ! checked here.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: derived_type_node, type_binding_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use semantic_procedure_signature, only: procedure_signature_t, &
        build_procedure_signature, find_procedure_definition
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_final_procedures

contains

    ! Walk every derived type in the arena and check its FINAL bindings.
    subroutine validate_final_procedures(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i, b

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (derived_type_node)
                if (.not. allocated(node%binding_indices)) cycle
                if (.not. allocated(node%name)) cycle
                do b = 1, size(node%binding_indices)
                    call check_binding(arena, node%binding_indices(b), &
                        node%name, errors)
                end do
            end select
        end do
    end subroutine validate_final_procedures

    ! Check one type binding; non-FINAL bindings are ignored.
    subroutine check_binding(arena, binding_index, type_name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        character(len=*), intent(in) :: type_name
        type(error_collection_t), intent(inout) :: errors
        type(procedure_signature_t) :: sig
        integer :: proc_index

        if (binding_index <= 0) return
        if (.not. arena%has_node_at(binding_index)) return

        select type (binding => arena%entries(binding_index)%node)
            type is (type_binding_node)
            if (.not. binding%is_final) return
            if (.not. allocated(binding%binding_name)) return
            proc_index = find_procedure_definition(arena, binding%binding_name)
            if (proc_index <= 0) return
            sig = build_procedure_signature(arena, proc_index)
            if (.not. sig%found) return
            if (sig%is_function) return
            if (.not. allocated(sig%dummies)) return
            call check_signature(sig, binding%binding_name, type_name, &
                binding%line, binding%column, errors)
        end select
    end subroutine check_binding

    ! Apply the two enforced FINAL rules to a resolved signature.
    subroutine check_signature(sig, proc_name, type_name, line, column, errors)
        type(procedure_signature_t), intent(in) :: sig
        character(len=*), intent(in) :: proc_name, type_name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, size(sig%dummies)
            if (.not. allocated(sig%dummies(i)%name)) cycle
            if (sig%dummies(i)%name /= '*') cycle
            call report(errors, 'Argument of FINAL procedure '''// &
                trim(proc_name)//''' must be a data object, not an '// &
                'alternate return indicator', &
                'give the final subroutine a single nonpolymorphic dummy '// &
                'argument of type '''//trim(type_name)//'''', line, column)
            return
        end do

        if (size(sig%dummies) /= 1) then
            call report(errors, 'FINAL procedure '''//trim(proc_name)// &
                ''' must have exactly one dummy argument but has '// &
                int_to_string(size(sig%dummies)), &
                'declare the final subroutine with a single dummy argument '// &
                'of type '''//trim(type_name)//'''', line, column)
            return
        end if

        if (.not. sig%dummies(1)%category_known) return
        if (.not. is_polymorphic(sig%dummies(1)%category)) return
        call report(errors, 'Argument of FINAL procedure '''//trim(proc_name)// &
            ''' must be of type '''//trim(type_name)//''' and nonpolymorphic', &
            'declare the dummy argument as TYPE('//trim(type_name)// &
            ') instead of CLASS('//trim(type_name)//')', line, column)
    end subroutine check_signature

    ! Whether a type category produced by type_category names a CLASS entity.
    function is_polymorphic(category) result(polymorphic)
        character(len=*), intent(in) :: category
        logical :: polymorphic

        polymorphic = .false.
        if (len(category) < 6) return
        polymorphic = category(1:6) == 'class:'
    end function is_polymorphic

    subroutine report(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message, suggestion
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=message, &
            code=ERROR_SEMANTIC, &
            component='semantic_final_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion=suggestion, line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine report

end module semantic_final_validation
