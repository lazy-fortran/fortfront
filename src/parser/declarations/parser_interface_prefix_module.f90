module parser_interface_prefix_module
    use string_utils_mod, only: to_lower
    use parser_state_module, only: parser_state_t
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_procedure_shared_module, only: consume_optional_kind_spec
    use lexer_core, only: token_t
    implicit none
    private
    public :: is_procedure_prefix
    public :: is_interface_return_type_keyword
    public :: is_interface_return_type_with_parens
    public :: collect_interface_return_type
    public :: append_interface_prefix
contains

    logical function is_procedure_prefix(lowered_text) result(is_prefix)
        character(len=*), intent(in) :: lowered_text

        is_prefix = trim(lowered_text) == "pure" .or. &
                    trim(lowered_text) == "elemental" .or. &
                    trim(lowered_text) == "recursive" .or. &
                    trim(lowered_text) == "impure" .or. &
                    trim(lowered_text) == "nonrecursive" .or. &
                    trim(lowered_text) == "non_recursive" .or. &
                    trim(lowered_text) == "module"
    end function is_procedure_prefix

    logical function is_interface_return_type_keyword(lowered_text) result(is_type)
        character(len=*), intent(in) :: lowered_text

        is_type = trim(lowered_text) == "integer" .or. &
                  trim(lowered_text) == "real" .or. &
                  trim(lowered_text) == "logical" .or. &
                  trim(lowered_text) == "character" .or. &
                  trim(lowered_text) == "complex" .or. &
                  trim(lowered_text) == "double"
    end function is_interface_return_type_keyword

    ! type(...) and class(...) are function return-type prefixes inside an
    ! interface body, but a bare "type ::" / "type," begins a derived-type
    ! definition. Distinguish by requiring an opening parenthesis to follow.
    logical function is_interface_return_type_with_parens(parser, lowered_text) &
        result(is_type)
        type(parser_state_t), intent(in) :: parser
        character(len=*), intent(in) :: lowered_text

        type(token_t) :: next_token

        is_type = .false.
        if (trim(lowered_text) /= "type" .and. trim(lowered_text) /= "class") return

        next_token = parser%get_token_at_index(parser%current_token + 1)
        if (trim(next_token%text) == "(") is_type = .true.
    end function is_interface_return_type_with_parens

    subroutine collect_interface_return_type(parser, prefix_buffer, lowered_text)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: lowered_text

        type(token_t) :: type_token, lookahead_token
        character(len=:), allocatable :: type_with_kind
        character(len=:), allocatable :: lookahead_lower
        character(len=16), allocatable :: prefix_array(:)

        if (trim(lowered_text) == "double") then
            lookahead_token = parser%get_token_at_index(parser%current_token + 1)
            lookahead_lower = to_lower(trim(lookahead_token%text))
            if (trim(lookahead_lower) == "precision" .or. &
                trim(lookahead_lower) == "complex") then
                type_token = parser%consume()
                lookahead_token = parser%consume()
                type_with_kind = trim(type_token%text) // " " // &
                    trim(lookahead_token%text)
                call consume_optional_kind_spec(parser, type_with_kind)
                allocate (character(len=16) :: prefix_array(1))
                prefix_array(1) = trim(type_with_kind)
                call prefix_buffer%append_all(prefix_array)
                return
            end if
        end if

        type_token = parser%consume()
        type_with_kind = trim(type_token%text)
        call consume_optional_kind_spec(parser, type_with_kind)
        allocate (character(len=16) :: prefix_array(1))
        prefix_array(1) = trim(type_with_kind)
        call prefix_buffer%append_all(prefix_array)
    end subroutine collect_interface_return_type

    subroutine append_interface_prefix(prefix_buffer, keyword_text)
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: keyword_text

        character(len=16), allocatable :: prefix_array(:)

        allocate (character(len=16) :: prefix_array(1))
        prefix_array(1) = trim(keyword_text)
        call prefix_buffer%append_all(prefix_array)
    end subroutine append_interface_prefix

end module parser_interface_prefix_module
