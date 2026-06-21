program test_type_bound_procedures_codegen
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use codegen_api, only: emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    call test_simple_procedure_binding()
    call test_procedure_with_implementation()
    call test_multiple_bindings()
    call test_final_procedure()
    call test_accessibility_modifiers()
    call test_generic_binding()
    call test_deferred_binding()
    call test_nopass_binding()

    print *, "PASS: all type-bound procedure codegen tests passed"

contains

    subroutine test_simple_procedure_binding()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                                       "   procedure :: method"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: simple binding lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: simple binding parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "contains") == 0) then
            print *, "FAIL: contains keyword not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
        if (index(code, "procedure :: method") == 0) then
            print *, "FAIL: procedure binding not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_simple_procedure_binding

    subroutine test_procedure_with_implementation()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                              "   procedure :: method => impl_method"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: implementation lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: implementation parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "procedure :: method => impl_method") == 0) then
            print *, "FAIL: procedure with implementation not in code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_procedure_with_implementation

    subroutine test_multiple_bindings()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                                   "   procedure :: method1 => impl1"//new_line('A')// &
                                   "   procedure :: method2 => impl2"//new_line('A')// &
                                       "   procedure :: method3"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: multiple bindings lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: multiple bindings parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "procedure :: method1 => impl1") == 0) then
            print *, "FAIL: method1 not found in generated code"
            stop 1
        end if
        if (index(code, "procedure :: method2 => impl2") == 0) then
            print *, "FAIL: method2 not found in generated code"
            stop 1
        end if
        if (index(code, "procedure :: method3") == 0) then
            print *, "FAIL: method3 not found in generated code"
            stop 1
        end if
    end subroutine test_multiple_bindings

    subroutine test_final_procedure()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                              "   procedure :: method => impl_method"//new_line('A')// &
                                       "   final :: destroy"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: final procedure lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: final procedure parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "final :: destroy") == 0) then
            print *, "FAIL: final procedure not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_final_procedure

    subroutine test_accessibility_modifiers()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                                 "   procedure, public :: pub_method"//new_line('A')// &
                               "   procedure, private :: priv_method"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: accessibility lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: accessibility parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "procedure, public :: pub_method") == 0) then
            print *, "FAIL: public procedure not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
        if (index(code, "procedure, private :: priv_method") == 0) then
            print *, "FAIL: private procedure not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_accessibility_modifiers

    subroutine test_generic_binding()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "   integer :: x"//new_line('A')// &
                                       "contains"//new_line('A')// &
                    "   generic, public :: assign_all => assign_impl"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: generic binding lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: generic binding parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "generic, public :: assign_all => assign_impl") == 0) then
            print *, "FAIL: generic binding not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_generic_binding

    subroutine test_deferred_binding()
        character(len=*), parameter :: source = &
                                       "type, abstract :: atype"//new_line('A')// &
                                       "contains"//new_line('A')// &
                          "   procedure, deferred :: abstract_method"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: deferred binding lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: deferred binding parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "procedure, deferred :: abstract_method") == 0) then
            print *, "FAIL: deferred binding not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_deferred_binding

    subroutine test_nopass_binding()
        character(len=*), parameter :: source = &
                                       "type :: atype"//new_line('A')// &
                                       "contains"//new_line('A')// &
                   "   procedure, private, nopass :: act => act_impl"//new_line('A')// &
                                       "end type atype"
        character(len=:), allocatable :: code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: nopass binding lexer error:", trim(error_msg)
            stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: nopass binding parse error:", trim(error_msg)
            stop 1
        end if

        call emit_fortran(arena, mod_index, code)
        if (index(code, "procedure, private, nopass :: act => act_impl") == 0) then
            print *, "FAIL: nopass binding not in generated code"
            print *, "Generated code:", code
            stop 1
        end if
    end subroutine test_nopass_binding

end program test_type_bound_procedures_codegen
