module semantic_tbp_override_result
    ! Function result half of the F2003 type-bound procedure override rules
    ! (F2018 7.5.7.3): the result of an overriding function must have the same
    ! rank as the result of the overridden function, and when the overridden
    ! result has a constant character length the overriding result must have
    ! the same constant character length.
    !
    ! A nonconstant character length on the overridden result is left alone.
    ! Deciding whether two specification expressions such as `2*x` and `1+x*2`
    ! denote the same length needs symbolic evaluation the frontend does not
    ! have, and gfortran only warns there, so rejecting would be over-eager.
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use semantic_procedure_signature, only: procedure_signature_t
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: compare_function_results

contains

    ! Compare the results of an overriding and an overridden function binding.
    subroutine compare_function_results(child_sig, parent_sig, binding_name, &
            line, column, errors)
        type(procedure_signature_t), intent(in) :: child_sig, parent_sig
        character(len=*), intent(in) :: binding_name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors

        if (.not. child_sig%is_function) return
        if (.not. parent_sig%is_function) return
        if (.not. child_sig%result_known) return
        if (.not. parent_sig%result_known) return

        if (child_sig%result_is_array .neqv. parent_sig%result_is_array) then
            call report(errors, 'Rank mismatch in function result of '// &
                'overriding type-bound procedure '''//trim(binding_name)// &
                ''': overridden result is '// &
                rank_word(parent_sig%result_is_array)//' but the overriding '// &
                'result is '//rank_word(child_sig%result_is_array), &
                'give the overriding function result the same rank as the '// &
                'overridden function result', line, column)
            return
        end if

        if (child_sig%result_category /= 'character') return
        if (parent_sig%result_category /= 'character') return
        call compare_character_length(child_sig, parent_sig, binding_name, &
            line, column, errors)
    end subroutine compare_function_results

    ! Character length half of the result comparison.
    subroutine compare_character_length(child_sig, parent_sig, binding_name, &
            line, column, errors)
        type(procedure_signature_t), intent(in) :: child_sig, parent_sig
        character(len=*), intent(in) :: binding_name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors
        integer :: parent_length, child_length
        logical :: parent_constant, child_constant

        if (.not. parent_sig%result_has_char_len) return
        if (.not. child_sig%result_has_char_len) return
        if (.not. allocated(parent_sig%result_char_len)) return
        if (.not. allocated(child_sig%result_char_len)) return

        call constant_length(parent_sig%result_char_len, parent_constant, &
            parent_length)
        if (.not. parent_constant) return

        call constant_length(child_sig%result_char_len, child_constant, &
            child_length)
        if (.not. child_constant) then
            call report(errors, 'Overridden type-bound function '''// &
                trim(binding_name)//''' is declared with a constant '// &
                'character length of '//int_to_string(parent_length)// &
                ', so the overriding function result must be too', &
                'declare the overriding function result as character(len='// &
                int_to_string(parent_length)//')', line, column)
            return
        end if

        if (child_length == parent_length) return
        call report(errors, 'Character length mismatch in function result of '// &
            'overriding type-bound procedure '''//trim(binding_name)// &
            ''': overridden result has length '//int_to_string(parent_length)// &
            ' but the overriding result has length '// &
            int_to_string(child_length), &
            'declare the overriding function result as character(len='// &
            int_to_string(parent_length)//')', line, column)
    end subroutine compare_character_length

    ! Decide whether a character length specification is a plain nonnegative
    ! integer literal, and if so return its value. Anything else, including an
    ! assumed length `*` and any specification expression, is not constant for
    ! the purposes of this rule.
    subroutine constant_length(text, is_constant, length)
        character(len=*), intent(in) :: text
        logical, intent(out) :: is_constant
        integer, intent(out) :: length
        character(len=:), allocatable :: trimmed
        integer :: i, status

        is_constant = .false.
        length = 0
        trimmed = trim(adjustl(text))
        if (len(trimmed) == 0) return

        do i = 1, len(trimmed)
            if (trimmed(i:i) < '0') return
            if (trimmed(i:i) > '9') return
        end do

        read (trimmed, *, iostat=status) length
        if (status /= 0) then
            length = 0
            return
        end if
        is_constant = .true.
    end subroutine constant_length

    function rank_word(is_array) result(word)
        logical, intent(in) :: is_array
        character(len=:), allocatable :: word

        if (is_array) then
            word = 'an array'
        else
            word = 'a scalar'
        end if
    end function rank_word

    subroutine report(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message, suggestion
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=message, &
            code=ERROR_SEMANTIC, &
            component='semantic_tbp_override_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion=suggestion, line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine report

end module semantic_tbp_override_result
