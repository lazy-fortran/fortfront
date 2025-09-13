module json_module
    ! Lightweight stub replacing json-fortran for build compatibility.
    ! Provides minimal types and no-op methods used by AST to_json code paths.
    implicit none
    private

    type, public :: json_value
        integer :: placeholder = 0
    end type json_value

    type, public :: json_core
    contains
        procedure, public :: initialize => jm_initialize
        procedure, public :: destroy => jm_destroy
        procedure, public :: create_object => jm_create_object
        procedure, public :: create_array  => jm_create_array
        procedure, public :: jm_add_int_kv
        procedure, public :: jm_add_real_kv
        procedure, public :: jm_add_logical_kv
        procedure, public :: jm_add_char_kv
        procedure, public :: jm_add_child
        procedure, public :: jm_add_int_arr_kv
        generic,   public :: add => jm_add_int_kv, jm_add_real_kv, jm_add_logical_kv, &
                                   jm_add_char_kv, jm_add_child, jm_add_int_arr_kv
        procedure, public :: print => jm_print
        procedure, public :: print_to_string => jm_print_to_string
    end type json_core

    ! No explicit interface block needed for module procedures

contains

    subroutine jm_initialize(this)
        class(json_core), intent(inout) :: this
    end subroutine jm_initialize

    subroutine jm_destroy(this, value)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: value
    end subroutine jm_destroy

    subroutine jm_create_object(this, value, key)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: value
        character(len=*), intent(in) :: key
        ! No-op
    end subroutine jm_create_object

    subroutine jm_create_array(this, value, key)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: value
        character(len=*), intent(in) :: key
        ! No-op
    end subroutine jm_create_array

    subroutine jm_add_int_kv(this, obj, key, val)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: obj
        character(len=*), intent(in) :: key
        integer, intent(in) :: val
    end subroutine jm_add_int_kv

    subroutine jm_add_real_kv(this, obj, key, val)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: obj
        character(len=*), intent(in) :: key
        real, intent(in) :: val
    end subroutine jm_add_real_kv

    subroutine jm_add_logical_kv(this, obj, key, val)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: obj
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
    end subroutine jm_add_logical_kv

    subroutine jm_add_char_kv(this, obj, key, val)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: obj
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
    end subroutine jm_add_char_kv

    subroutine jm_add_int_arr_kv(this, obj, key, val)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: obj
        character(len=*), intent(in) :: key
        integer, intent(in) :: val(:)
    end subroutine jm_add_int_arr_kv

    subroutine jm_add_child(this, parent, child)
        class(json_core), intent(inout) :: this
        type(json_value), intent(inout) :: parent
        type(json_value), intent(inout) :: child
    end subroutine jm_add_child

    

    subroutine jm_print(this, root, filename)
        class(json_core), intent(inout) :: this
        type(json_value), intent(in) :: root
        character(len=*), intent(in) :: filename
        integer :: u
        open(newunit=u, file=filename, status='replace', action='write')
        write(u,'(A)') '{}'
        close(u)
    end subroutine jm_print

    subroutine jm_print_to_string(this, root, out)
        class(json_core), intent(inout) :: this
        type(json_value), intent(in) :: root
        character(len=:), allocatable, intent(out) :: out
        out = '{}'
    end subroutine jm_print_to_string

end module json_module
