module test_codegen_comprehensive_coverage
    use json_module
    use test_drive, only : new_unittest, unittest_type, error_type, check, test_failed
    use frontend
    use ast_core
    implicit none
    private
    public :: collect_codegen_comprehensive_coverage_tests

contains

    subroutine collect_codegen_comprehensive_coverage_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_codegen_program_structure", test_codegen_program_structure), &
            new_unittest("test_codegen_declarations", test_codegen_declarations), &
            new_unittest("test_codegen_expressions", test_codegen_expressions), &
            new_unittest("test_codegen_statements", test_codegen_statements), &
            new_unittest("test_codegen_control_flow", test_codegen_control_flow), &
            new_unittest("test_codegen_procedures", test_codegen_procedures), &
            new_unittest("test_codegen_modules", test_codegen_modules), &
            new_unittest("test_codegen_derived_types", test_codegen_derived_types), &
            new_unittest("test_codegen_interfaces", test_codegen_interfaces), &
            new_unittest("test_codegen_formatting", test_codegen_formatting) &
        ]
    end subroutine

    subroutine test_codegen_program_structure(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test basic program structure generation
        code = "program test_prog" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "integer :: i" // new_line('a') // &
               "i = 42" // new_line('a') // &
               "print *, 'i =', i" // new_line('a') // &
               "end program test_prog"
        
        generated = compile_to_string(code)
        
        ! Check key elements are present
        call check(error, index(generated, "program test_prog") > 0, &
                  "Missing program statement")
        if (allocated(error)) return
        
        call check(error, index(generated, "implicit none") > 0, &
                  "Missing implicit none")
        if (allocated(error)) return
        
        call check(error, index(generated, "integer :: i") > 0, &
                  "Missing variable declaration")
        if (allocated(error)) return
        
        call check(error, index(generated, "end program") > 0, &
                  "Missing end program")
    end subroutine

    subroutine test_codegen_declarations(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test various declaration patterns
        code = "program test" // new_line('a') // &
               ! Basic types
               "integer :: i, j, k" // new_line('a') // &
               "real :: x, y = 3.14" // new_line('a') // &
               "complex :: c = (1.0, 2.0)" // new_line('a') // &
               "logical :: flag = .true." // new_line('a') // &
               "character(len=20) :: name = 'Fortran'" // new_line('a') // &
               ! Arrays
               "real, dimension(10) :: arr1" // new_line('a') // &
               "integer, dimension(5,5) :: matrix" // new_line('a') // &
               "real, dimension(:), allocatable :: dyn_arr" // new_line('a') // &
               ! Attributes
               "real, parameter :: pi = 3.14159" // new_line('a') // &
               "integer, save :: counter" // new_line('a') // &
               "real, pointer :: ptr => null()" // new_line('a') // &
               "real, target :: target_var" // new_line('a') // &
               ! Kind parameters
               "integer(kind=8) :: big_int" // new_line('a') // &
               "real(kind=8) :: double_prec" // new_line('a') // &
               "end program"
        
        generated = compile_to_string(code)
        
        ! Check various declarations are generated correctly
        call check(error, index(generated, "integer :: i, j, k") > 0, &
                  "Missing integer declaration")
        if (allocated(error)) return
        
        call check(error, index(generated, "real :: x") > 0 .or. &
                         index(generated, "real :: y = 3.14") > 0, &
                  "Missing real declaration")
        if (allocated(error)) return
        
        call check(error, index(generated, "dimension(10)") > 0 .or. &
                         index(generated, "dimension(:)") > 0, &
                  "Missing array dimension")
        if (allocated(error)) return
        
        call check(error, index(generated, "parameter") > 0, &
                  "Missing parameter attribute")
        if (allocated(error)) return
        
        call check(error, index(generated, "allocatable") > 0, &
                  "Missing allocatable attribute")
    end subroutine

    subroutine test_codegen_expressions(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test expression code generation
        code = "program test" // new_line('a') // &
               ! Arithmetic expressions
               "a = b + c" // new_line('a') // &
               "d = e - f * g" // new_line('a') // &
               "h = i / j ** k" // new_line('a') // &
               "l = (m + n) * (o - p)" // new_line('a') // &
               ! Logical expressions
               "flag1 = a < b .and. c > d" // new_line('a') // &
               "flag2 = e <= f .or. g >= h" // new_line('a') // &
               "flag3 = .not. (i == j)" // new_line('a') // &
               "flag4 = k /= l .eqv. m .ne. n" // new_line('a') // &
               ! Function calls
               "x = sin(angle)" // new_line('a') // &
               "y = sqrt(a**2 + b**2)" // new_line('a') // &
               "z = max(min(x, upper), lower)" // new_line('a') // &
               ! Array expressions
               "arr1 = arr2 + arr3" // new_line('a') // &
               "vec(1:5) = mat(i, :)" // new_line('a') // &
               "result = sum(array * weights)" // new_line('a') // &
               ! String expressions
               "full_name = first_name // ' ' // last_name" // new_line('a') // &
               "end program"
        
        generated = compile_to_string(code)
        
        ! Check arithmetic operators
        call check(error, index(generated, "+") > 0 .and. &
                         index(generated, "-") > 0 .and. &
                         index(generated, "*") > 0 .and. &
                         index(generated, "/") > 0 .and. &
                         index(generated, "**") > 0, &
                  "Missing arithmetic operators")
        if (allocated(error)) return
        
        ! Check logical operators
        call check(error, index(generated, ".and.") > 0 .or. &
                         index(generated, ".or.") > 0 .or. &
                         index(generated, ".not.") > 0, &
                  "Missing logical operators")
        if (allocated(error)) return
        
        ! Check comparison operators
        call check(error, index(generated, "<") > 0 .or. &
                         index(generated, ">") > 0 .or. &
                         index(generated, "==") > 0 .or. &
                         index(generated, "/=") > 0, &
                  "Missing comparison operators")
        if (allocated(error)) return
        
        ! Check function calls
        call check(error, index(generated, "sin(") > 0 .or. &
                         index(generated, "sqrt(") > 0, &
                  "Missing function calls")
    end subroutine

    subroutine test_codegen_statements(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test various statement types
        code = "program test" // new_line('a') // &
               ! Assignment statements
               "x = 42" // new_line('a') // &
               "y = x + 1" // new_line('a') // &
               ! I/O statements
               "print *, 'Hello, World!'" // new_line('a') // &
               "write(*,*) 'x =', x" // new_line('a') // &
               "read(*,*) input_value" // new_line('a') // &
               ! Allocate/deallocate
               "allocate(array(n))" // new_line('a') // &
               "allocate(matrix(m,n), stat=ierr)" // new_line('a') // &
               "deallocate(array)" // new_line('a') // &
               ! Call statements
               "call process_data(x, y)" // new_line('a') // &
               "call system_routine()" // new_line('a') // &
               ! Other statements
               "stop 'Error occurred'" // new_line('a') // &
               "return" // new_line('a') // &
               "cycle" // new_line('a') // &
               "exit" // new_line('a') // &
               "end program"
        
        generated = compile_to_string(code)
        
        ! Check I/O statements
        call check(error, index(generated, "print") > 0 .or. &
                         index(generated, "write") > 0, &
                  "Missing I/O statements")
        if (allocated(error)) return
        
        ! Check memory management
        call check(error, index(generated, "allocate") > 0, &
                  "Missing allocate statement")
        if (allocated(error)) return
        
        call check(error, index(generated, "deallocate") > 0, &
                  "Missing deallocate statement")
        if (allocated(error)) return
        
        ! Check call statements
        call check(error, index(generated, "call") > 0, &
                  "Missing call statement")
    end subroutine

    subroutine test_codegen_control_flow(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test control flow structures
        code = "program test" // new_line('a') // &
               ! IF constructs
               "if (x > 0) y = 1" // new_line('a') // &
               "if (a < b) then" // new_line('a') // &
               "    c = a" // new_line('a') // &
               "else if (a > b) then" // new_line('a') // &
               "    c = b" // new_line('a') // &
               "else" // new_line('a') // &
               "    c = 0" // new_line('a') // &
               "end if" // new_line('a') // &
               ! DO loops
               "do i = 1, 10" // new_line('a') // &
               "    sum = sum + i" // new_line('a') // &
               "end do" // new_line('a') // &
               "do j = 10, 1, -1" // new_line('a') // &
               "    print *, j" // new_line('a') // &
               "end do" // new_line('a') // &
               ! DO WHILE
               "do while (error > tolerance)" // new_line('a') // &
               "    call iterate()" // new_line('a') // &
               "    error = compute_error()" // new_line('a') // &
               "end do" // new_line('a') // &
               ! SELECT CASE
               "select case (option)" // new_line('a') // &
               "case (1)" // new_line('a') // &
               "    call option_one()" // new_line('a') // &
               "case (2:5)" // new_line('a') // &
               "    call option_range()" // new_line('a') // &
               "case default" // new_line('a') // &
               "    call default_option()" // new_line('a') // &
               "end select" // new_line('a') // &
               ! WHERE construct
               "where (array > 0)" // new_line('a') // &
               "    positive_array = array" // new_line('a') // &
               "elsewhere" // new_line('a') // &
               "    positive_array = 0" // new_line('a') // &
               "end where" // new_line('a') // &
               "end program"
        
        generated = compile_to_string(code)
        
        ! Check IF constructs
        call check(error, index(generated, "if") > 0 .and. &
                         index(generated, "then") > 0 .and. &
                         index(generated, "else") > 0, &
                  "Missing IF construct elements")
        if (allocated(error)) return
        
        ! Check DO loops
        call check(error, index(generated, "do") > 0 .and. &
                         index(generated, "end do") > 0, &
                  "Missing DO loop elements")
        if (allocated(error)) return
        
        ! Check SELECT CASE
        call check(error, index(generated, "select case") > 0 .and. &
                         index(generated, "case") > 0, &
                  "Missing SELECT CASE elements")
        if (allocated(error)) return
        
        ! Check WHERE construct
        call check(error, index(generated, "where") > 0 .and. &
                         index(generated, "elsewhere") > 0, &
                  "Missing WHERE construct elements")
    end subroutine

    subroutine test_codegen_procedures(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test procedure generation
        code = "module test_procs" // new_line('a') // &
               "contains" // new_line('a') // &
               ! Simple function
               "function add(a, b) result(c)" // new_line('a') // &
               "    real, intent(in) :: a, b" // new_line('a') // &
               "    real :: c" // new_line('a') // &
               "    c = a + b" // new_line('a') // &
               "end function add" // new_line('a') // &
               ! Function with explicit type
               "real function multiply(x, y)" // new_line('a') // &
               "    real, intent(in) :: x, y" // new_line('a') // &
               "    multiply = x * y" // new_line('a') // &
               "end function multiply" // new_line('a') // &
               ! Subroutine
               "subroutine swap(a, b)" // new_line('a') // &
               "    real, intent(inout) :: a, b" // new_line('a') // &
               "    real :: temp" // new_line('a') // &
               "    temp = a" // new_line('a') // &
               "    a = b" // new_line('a') // &
               "    b = temp" // new_line('a') // &
               "end subroutine swap" // new_line('a') // &
               ! Pure elemental function
               "pure elemental function square(x) result(y)" // new_line('a') // &
               "    real, intent(in) :: x" // new_line('a') // &
               "    real :: y" // new_line('a') // &
               "    y = x * x" // new_line('a') // &
               "end function square" // new_line('a') // &
               ! Recursive function
               "recursive function factorial(n) result(fact)" // new_line('a') // &
               "    integer, intent(in) :: n" // new_line('a') // &
               "    integer :: fact" // new_line('a') // &
               "    if (n <= 1) then" // new_line('a') // &
               "        fact = 1" // new_line('a') // &
               "    else" // new_line('a') // &
               "        fact = n * factorial(n-1)" // new_line('a') // &
               "    end if" // new_line('a') // &
               "end function factorial" // new_line('a') // &
               "end module"
        
        generated = compile_to_string(code)
        
        ! Check function declarations
        call check(error, index(generated, "function") > 0 .and. &
                         index(generated, "end function") > 0, &
                  "Missing function elements")
        if (allocated(error)) return
        
        ! Check subroutine declarations
        call check(error, index(generated, "subroutine") > 0 .and. &
                         index(generated, "end subroutine") > 0, &
                  "Missing subroutine elements")
        if (allocated(error)) return
        
        ! Check procedure attributes
        call check(error, index(generated, "pure") > 0 .or. &
                         index(generated, "elemental") > 0 .or. &
                         index(generated, "recursive") > 0, &
                  "Missing procedure attributes")
        if (allocated(error)) return
        
        ! Check intent specifications
        call check(error, index(generated, "intent(in)") > 0 .and. &
                         index(generated, "intent(inout)") > 0, &
                  "Missing intent specifications")
    end subroutine

    subroutine test_codegen_modules(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test module structure generation
        code = "module math_utils" // new_line('a') // &
               "    use iso_fortran_env, only: real64, int32" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    private" // new_line('a') // &
               "    public :: pi, calculate_area, point" // new_line('a') // &
               "    " // new_line('a') // &
               "    real(real64), parameter :: pi = 3.14159265358979323846_real64" // new_line('a') // &
               "    real(real64), parameter :: e = 2.71828182845904523536_real64" // new_line('a') // &
               "    " // new_line('a') // &
               "    type :: point" // new_line('a') // &
               "        real(real64) :: x, y" // new_line('a') // &
               "    end type point" // new_line('a') // &
               "    " // new_line('a') // &
               "contains" // new_line('a') // &
               "    " // new_line('a') // &
               "    function calculate_area(radius) result(area)" // new_line('a') // &
               "        real(real64), intent(in) :: radius" // new_line('a') // &
               "        real(real64) :: area" // new_line('a') // &
               "        area = pi * radius**2" // new_line('a') // &
               "    end function calculate_area" // new_line('a') // &
               "    " // new_line('a') // &
               "end module math_utils"
        
        generated = compile_to_string(code)
        
        ! Check module structure
        call check(error, index(generated, "module math_utils") > 0, &
                  "Missing module declaration")
        if (allocated(error)) return
        
        call check(error, index(generated, "end module") > 0, &
                  "Missing end module")
        if (allocated(error)) return
        
        ! Check use statements
        call check(error, index(generated, "use") > 0, &
                  "Missing use statement")
        if (allocated(error)) return
        
        ! Check visibility declarations
        call check(error, index(generated, "private") > 0 .and. &
                         index(generated, "public") > 0, &
                  "Missing visibility declarations")
        if (allocated(error)) return
        
        ! Check contains section
        call check(error, index(generated, "contains") > 0, &
                  "Missing contains section")
    end subroutine

    subroutine test_codegen_derived_types(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test derived type generation
        code = "module types_mod" // new_line('a') // &
               ! Simple type
               "type :: vector3d" // new_line('a') // &
               "    real :: x, y, z" // new_line('a') // &
               "end type vector3d" // new_line('a') // &
               ! Type with different component types
               "type :: person" // new_line('a') // &
               "    character(len=50) :: name" // new_line('a') // &
               "    integer :: age" // new_line('a') // &
               "    real :: height, weight" // new_line('a') // &
               "    logical :: active = .true." // new_line('a') // &
               "end type person" // new_line('a') // &
               ! Type with allocatable components
               "type :: matrix_type" // new_line('a') // &
               "    real, dimension(:,:), allocatable :: data" // new_line('a') // &
               "    integer :: rows = 0, cols = 0" // new_line('a') // &
               "end type matrix_type" // new_line('a') // &
               ! Type with type components
               "type :: line" // new_line('a') // &
               "    type(point) :: start, end" // new_line('a') // &
               "    real :: length" // new_line('a') // &
               "end type line" // new_line('a') // &
               ! Type with procedures
               "type :: shape" // new_line('a') // &
               "    real :: area" // new_line('a') // &
               "contains" // new_line('a') // &
               "    procedure :: calculate => shape_calculate_area" // new_line('a') // &
               "    procedure :: display => shape_display" // new_line('a') // &
               "end type shape" // new_line('a') // &
               ! Extended type
               "type, extends(shape) :: circle" // new_line('a') // &
               "    real :: radius" // new_line('a') // &
               "contains" // new_line('a') // &
               "    procedure :: calculate => circle_calculate_area" // new_line('a') // &
               "end type circle" // new_line('a') // &
               "end module"
        
        generated = compile_to_string(code)
        
        ! Check type declarations
        call check(error, index(generated, "type ::") > 0 .or. &
                         index(generated, "type::") > 0, &
                  "Missing type declarations")
        if (allocated(error)) return
        
        call check(error, index(generated, "end type") > 0, &
                  "Missing end type statements")
        if (allocated(error)) return
        
        ! Check type components
        call check(error, index(generated, "real ::") > 0 .and. &
                         index(generated, "integer ::") > 0 .and. &
                         index(generated, "character") > 0, &
                  "Missing type component declarations")
        if (allocated(error)) return
        
        ! Check advanced features
        call check(error, index(generated, "allocatable") > 0, &
                  "Missing allocatable component")
        if (allocated(error)) return
        
        call check(error, index(generated, "extends") > 0 .or. &
                         index(generated, "procedure") > 0, &
                  "Missing type extension or procedures")
    end subroutine

    subroutine test_codegen_interfaces(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        
        ! Test interface generation
        code = "module interfaces_mod" // new_line('a') // &
               ! Generic interface
               "interface process" // new_line('a') // &
               "    module procedure process_int, process_real" // new_line('a') // &
               "end interface process" // new_line('a') // &
               ! Operator overloading
               "interface operator(+)" // new_line('a') // &
               "    module procedure add_vectors" // new_line('a') // &
               "end interface operator(+)" // new_line('a') // &
               ! User-defined operator
               "interface operator(.cross.)" // new_line('a') // &
               "    module procedure cross_product" // new_line('a') // &
               "end interface operator(.cross.)" // new_line('a') // &
               ! Assignment overloading
               "interface assignment(=)" // new_line('a') // &
               "    module procedure assign_vector_array" // new_line('a') // &
               "end interface assignment(=)" // new_line('a') // &
               ! Abstract interface
               "abstract interface" // new_line('a') // &
               "    function func_r_r(x) result(y)" // new_line('a') // &
               "        real, intent(in) :: x" // new_line('a') // &
               "        real :: y" // new_line('a') // &
               "    end function func_r_r" // new_line('a') // &
               "end interface" // new_line('a') // &
               "end module"
        
        generated = compile_to_string(code)
        
        ! Check interface blocks
        call check(error, index(generated, "interface") > 0 .and. &
                         index(generated, "end interface") > 0, &
                  "Missing interface blocks")
        if (allocated(error)) return
        
        ! Check operator interfaces
        call check(error, index(generated, "operator") > 0, &
                  "Missing operator interfaces")
        if (allocated(error)) return
        
        ! Check assignment interface
        call check(error, index(generated, "assignment") > 0, &
                  "Missing assignment interface")
        if (allocated(error)) return
        
        ! Check abstract interface
        call check(error, index(generated, "abstract") > 0, &
                  "Missing abstract interface")
    end subroutine

    subroutine test_codegen_formatting(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: code, generated
        integer :: indent_count, i
        
        ! Test proper indentation and formatting
        code = "program test_format" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "integer :: i, j" // new_line('a') // &
               "do i = 1, 10" // new_line('a') // &
               "    if (i > 5) then" // new_line('a') // &
               "        do j = 1, i" // new_line('a') // &
               "            print *, i, j" // new_line('a') // &
               "        end do" // new_line('a') // &
               "    else" // new_line('a') // &
               "        print *, 'i <= 5'" // new_line('a') // &
               "    end if" // new_line('a') // &
               "end do" // new_line('a') // &
               "contains" // new_line('a') // &
               "    subroutine nested_sub()" // new_line('a') // &
               "        print *, 'In nested subroutine'" // new_line('a') // &
               "    end subroutine nested_sub" // new_line('a') // &
               "end program test_format"
        
        generated = compile_to_string(code)
        
        ! Check that output has proper structure
        call check(error, len(generated) > 0, &
                  "Generated code is empty")
        if (allocated(error)) return
        
        ! Check line breaks are present
        indent_count = 0
        do i = 1, len(generated)
            if (generated(i:i) == new_line('a')) indent_count = indent_count + 1
        end do
        
        call check(error, indent_count > 5, &
                  "Generated code lacks proper line breaks")
        if (allocated(error)) return
        
        ! Check nested structures
        call check(error, index(generated, "do i") > 0 .and. &
                         index(generated, "if") > 0 .and. &
                         index(generated, "do j") > 0, &
                  "Missing nested control structures")
    end subroutine

    ! Helper function to compile and return generated code
    function compile_to_string(code) result(generated)
        character(len=*), intent(in) :: code
        character(:), allocatable :: generated
        type(json_file) :: json
        character(:), allocatable :: json_str
        integer :: start_pos, end_pos
        
        call compile_to_json_string(code, json)
        call json%print_to_string(json_str)
        
        ! Extract generated code from JSON
        ! Look for "code" field in JSON output
        start_pos = index(json_str, '"code"')
        if (start_pos > 0) then
            start_pos = index(json_str(start_pos:), ':"') + start_pos + 1
            end_pos = index(json_str(start_pos:), '",') + start_pos - 2
            if (end_pos > start_pos) then
                generated = json_str(start_pos:end_pos)
            else
                generated = "// Code generation failed"
            end if
        else
            generated = "// No code field in JSON"
        end if
        
        call json%destroy()
    end function compile_to_string

end module test_codegen_comprehensive_coverage