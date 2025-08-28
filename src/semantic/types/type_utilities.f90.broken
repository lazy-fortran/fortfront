module type_utilities
    ! Type system utilities and intrinsic function support
    ! Provides utilities for working with type information and intrinsics
    
    use ast_core, only: ast_arena_t, literal_node, identifier_node, &
                       LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING
    use type_system_unified, only: mono_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                                  TFUN, TARRAY, TVAR
    use fortfront_types, only: type_info_t, function_signature_t
    
    implicit none
    private
    
    ! Public type utility functions
    public :: mono_type_to_type_info, get_type_info_for_base_type, &
              get_integer_literal_value, get_real_literal_value, &
              get_string_literal_value, is_intrinsic_function, &
              get_intrinsic_signature, get_type_string
    
contains

    ! Convert mono_type_t to type_info_t
    function mono_type_to_type_info(mono_type) result(info)
        type(mono_type_t), intent(in) :: mono_type
        type(type_info_t) :: info
        
        info%kind_name = get_mono_type_name(mono_type)
        info%is_array = (mono_type%kind == TARRAY)
        info%is_allocatable = mono_type%alloc_info%is_allocatable
        info%is_pointer = mono_type%alloc_info%is_pointer
        info%character_length = 0
        info%array_rank = 0
        
        if (mono_type%kind == TCHAR) then
            info%character_length = mono_type%size
        else if (info%is_array) then
            info%array_rank = get_array_rank_from_type(mono_type)
            if (mono_type%has_args() .and. mono_type%get_args_count() > 0) then
                info%element_type_name = get_mono_type_name(mono_type%get_arg(1))
            else
                info%element_type_name = "unknown"
            end if
        end if
    end function mono_type_to_type_info

    ! Create type info for basic Fortran types
    function get_type_info_for_base_type(base_type) result(info)
        character(len=*), intent(in) :: base_type
        type(type_info_t) :: info
        
        info%kind_name = base_type
        info%is_array = .false.
        info%is_allocatable = .false.
        info%is_pointer = .false.
        info%character_length = 0
        info%array_rank = 0
        info%element_type_name = ""
        
        select case (trim(base_type))
        case ("character")
            info%character_length = 1  ! Default character length
        case ("real", "double precision")
            ! Keep defaults
        case ("integer", "logical")
            ! Keep defaults
        case default
            ! Keep defaults for unknown types
        end select
    end function get_type_info_for_base_type

    ! Extract integer literal value
    function get_integer_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: value
        integer :: iostat
        
        value = 0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (node%literal_type == LITERAL_INTEGER .and. allocated(node%value)) then
                        read(node%value, *, iostat=iostat) value
                        if (iostat /= 0) value = 0
                    end if
                end select
            end if
        end if
    end function get_integer_literal_value

    ! Extract real literal value
    function get_real_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        real :: value
        integer :: iostat
        
        value = 0.0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (node%literal_type == LITERAL_REAL .and. allocated(node%value)) then
                        read(node%value, *, iostat=iostat) value
                        if (iostat /= 0) value = 0.0
                    end if
                end select
            end if
        end if
    end function get_real_literal_value

    ! Extract string literal value
    function get_string_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: value
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (node%literal_type == LITERAL_STRING .and. allocated(node%value)) then
                        ! Remove quotes if present
                        if (len(node%value) >= 2) then
                            if ((node%value(1:1) == '"' .and. node%value(len(node%value):len(node%value)) == '"') .or. &
                                (node%value(1:1) == "'" .and. node%value(len(node%value):len(node%value)) == "'")) then
                                value = node%value(2:len(node%value)-1)
                            else
                                value = node%value
                            end if
                        else
                            value = node%value
                        end if
                    else
                        value = ""
                    end if
                end select
            end if
        end if
        
        if (.not. allocated(value)) then
            value = ""
        end if
    end function get_string_literal_value

    ! Check if a function name is intrinsic
    function is_intrinsic_function(name) result(is_intrinsic)
        character(len=*), intent(in) :: name
        logical :: is_intrinsic
        
        select case (trim(name))
        case ("abs", "acos", "asin", "atan", "atan2", "cos", "sin", "tan", &
              "exp", "log", "log10", "sqrt", "ceiling", "floor", &
              "max", "min", "mod", "nint", "real", "int", "char", &
              "ichar", "len", "trim", "adjustl", "adjustr", "index", &
              "scan", "verify", "repeat", "len_trim", "present", &
              "size", "shape", "lbound", "ubound", "allocated", &
              "associated", "kind", "precision", "range", "huge", &
              "tiny", "epsilon", "digits", "radix", "maxexponent", &
              "minexponent")
            is_intrinsic = .true.
        case default
            is_intrinsic = .false.
        end select
    end function is_intrinsic_function

    ! Get signature for intrinsic function
    function get_intrinsic_signature(name) result(signature)
        character(len=*), intent(in) :: name
        type(function_signature_t) :: signature
        
        signature%name = name
        signature%is_intrinsic = .true.
        signature%return_type = ""
        
        ! Allocate empty parameter lists (will be filled based on function)
        allocate(character(len=16) :: signature%parameter_names(0))
        allocate(character(len=16) :: signature%parameter_types(0))
        allocate(character(len=8) :: signature%parameter_intents(0))
        allocate(signature%parameter_optional(0))
        
        select case (trim(name))
        case ("abs")
            signature%return_type = "numeric"
            call set_single_param_signature(signature, "x", "numeric", "in")
        case ("sqrt", "exp", "log", "log10")
            signature%return_type = "real"
            call set_single_param_signature(signature, "x", "real", "in")
        case ("cos", "sin", "tan", "acos", "asin", "atan")
            signature%return_type = "real"
            call set_single_param_signature(signature, "x", "real", "in")
        case ("atan2")
            signature%return_type = "real"
            call set_two_param_signature(signature, "y", "real", "x", "real")
        case ("max", "min")
            signature%return_type = "numeric"
            call set_variadic_signature(signature, "numeric")
        case ("mod")
            signature%return_type = "numeric"
            call set_two_param_signature(signature, "a", "numeric", "p", "numeric")
        case ("int")
            signature%return_type = "integer"
            call set_single_param_signature(signature, "a", "numeric", "in")
        case ("real")
            signature%return_type = "real"
            call set_single_param_signature(signature, "a", "numeric", "in")
        case ("nint", "ceiling", "floor")
            signature%return_type = "integer"
            call set_single_param_signature(signature, "a", "real", "in")
        case ("char")
            signature%return_type = "character"
            call set_single_param_signature(signature, "i", "integer", "in")
        case ("ichar")
            signature%return_type = "integer"
            call set_single_param_signature(signature, "c", "character", "in")
        case ("len", "len_trim")
            signature%return_type = "integer"
            call set_single_param_signature(signature, "string", "character", "in")
        case ("trim", "adjustl", "adjustr")
            signature%return_type = "character"
            call set_single_param_signature(signature, "string", "character", "in")
        case ("index", "scan", "verify")
            signature%return_type = "integer"
            call set_two_param_signature(signature, "string", "character", "substring", "character")
        case ("repeat")
            signature%return_type = "character"
            call set_two_param_signature(signature, "string", "character", "ncopies", "integer")
        case ("present")
            signature%return_type = "logical"
            call set_single_param_signature(signature, "a", "any", "in")
        case ("size")
            signature%return_type = "integer"
            call set_single_param_signature(signature, "array", "array", "in")
        case ("allocated", "associated")
            signature%return_type = "logical"
            call set_single_param_signature(signature, "array", "allocatable", "in")
        case default
            signature%return_type = "unknown"
        end select
    end function get_intrinsic_signature

    ! Convert type_info_t to string representation
    function get_type_string(type_info) result(type_str)
        type(type_info_t), intent(in) :: type_info
        character(len=:), allocatable :: type_str
        character(len=256) :: temp_str
        
        temp_str = type_info%kind_name
        
        if (type_info%is_array) then
            if (type_info%array_rank > 0) then
                write(temp_str, '(A,A,I0,A)') trim(temp_str), "(:", type_info%array_rank, ")"
            else
                temp_str = trim(temp_str) // "(:)"
            end if
        end if
        
        if (type_info%character_length > 0 .and. type_info%kind_name == "character") then
            write(temp_str, '(A,A,I0,A)') trim(temp_str), "(len=", type_info%character_length, ")"
        end if
        
        if (type_info%is_allocatable) then
            temp_str = trim(temp_str) // ", allocatable"
        end if
        
        if (type_info%is_pointer) then
            temp_str = trim(temp_str) // ", pointer"
        end if
        
        type_str = trim(temp_str)
    end function get_type_string

    ! Helper functions for setting up function signatures
    
    subroutine set_single_param_signature(signature, param_name, param_type, intent)
        type(function_signature_t), intent(inout) :: signature
        character(len=*), intent(in) :: param_name, param_type, intent
        
        deallocate(signature%parameter_names)
        deallocate(signature%parameter_types)
        deallocate(signature%parameter_intents)
        deallocate(signature%parameter_optional)
        
        allocate(character(len=16) :: signature%parameter_names(1))
        allocate(character(len=16) :: signature%parameter_types(1))
        allocate(character(len=8) :: signature%parameter_intents(1))
        allocate(signature%parameter_optional(1))
        
        signature%parameter_names(1) = param_name
        signature%parameter_types(1) = param_type
        signature%parameter_intents(1) = intent
        signature%parameter_optional(1) = .false.
    end subroutine set_single_param_signature

    subroutine set_two_param_signature(signature, param1_name, param1_type, param2_name, param2_type)
        type(function_signature_t), intent(inout) :: signature
        character(len=*), intent(in) :: param1_name, param1_type, param2_name, param2_type
        
        deallocate(signature%parameter_names)
        deallocate(signature%parameter_types)
        deallocate(signature%parameter_intents)
        deallocate(signature%parameter_optional)
        
        allocate(character(len=16) :: signature%parameter_names(2))
        allocate(character(len=16) :: signature%parameter_types(2))
        allocate(character(len=8) :: signature%parameter_intents(2))
        allocate(signature%parameter_optional(2))
        
        signature%parameter_names(1) = param1_name
        signature%parameter_types(1) = param1_type
        signature%parameter_intents(1) = "in"
        signature%parameter_optional(1) = .false.
        
        signature%parameter_names(2) = param2_name
        signature%parameter_types(2) = param2_type
        signature%parameter_intents(2) = "in"
        signature%parameter_optional(2) = .false.
    end subroutine set_two_param_signature

    subroutine set_variadic_signature(signature, param_type)
        type(function_signature_t), intent(inout) :: signature
        character(len=*), intent(in) :: param_type
        
        ! For variadic functions, set up single parameter representing the pattern
        call set_single_param_signature(signature, "args", param_type, "in")
    end subroutine set_variadic_signature

    ! Helper functions
    
    function get_mono_type_name(mono_type) result(name)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: name
        
        select case (mono_type%kind)
        case (TINT)
            name = "integer"
        case (TREAL)
            name = "real"
        case (TCHAR)
            name = "character"
        case (TLOGICAL)
            name = "logical"
        case (TARRAY)
            name = "array"
        case (TFUN)
            name = "function"
        case (TVAR)
            name = "type_variable"
        case default
            name = "unknown"
        end select
    end function get_mono_type_name

    function get_array_rank_from_type(mono_type) result(rank)
        type(mono_type_t), intent(in) :: mono_type
        integer :: rank
        
        rank = 1  ! Default rank for arrays
        
        if (mono_type%kind == TARRAY) then
            ! Use size field as a heuristic for rank
            if (mono_type%size > 1) then
                rank = mono_type%size
            end if
        end if
    end function get_array_rank_from_type

end module type_utilities