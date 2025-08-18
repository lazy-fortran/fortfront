# Automatic Variable Declaration Generation

fortfront automatically generates proper Fortran variable declarations for functions and subroutines, eliminating the need to manually write parameter and result variable declarations.

## How It Works

When you write lazy Fortran functions without explicit variable declarations, fortfront automatically:

1. **Analyzes semantic types** from the AST and type inference system
2. **Applies Fortran implicit typing rules** for variables without explicit types
3. **Generates parameter declarations** with appropriate intent attributes
4. **Creates result variable declarations** for functions

## Basic Function Example

**Input (Lazy Fortran):**
```fortran
function twice(x) result(y)
y = 2*x
end function
```

**Output (Standard Fortran):**
```fortran
program main
    implicit none
contains
    function twice(x) result(y)
        implicit none
        real(8), intent(in) :: x
        real(8) :: y
        y = 2*x
    end function twice
end program main
```

## Type Inference Rules

fortfront follows standard Fortran implicit typing rules:

### Integer Variables
Variables starting with letters **i, j, k, l, m, n** are typed as `integer`:
```fortran
function increment(i) result(n)
n = i + 1
end function
```

Generates:
```fortran
integer, intent(in) :: i
integer :: n
```

### Real Variables  
All other variables default to `real(8)`:
```fortran
function calculate(a, b) result(sum)
sum = a + b  
end function
```

Generates:
```fortran
real(8), intent(in) :: a, b
real(8) :: sum
```

## Multiple Parameters

Parameters of the same type are grouped together:

**Input:**
```fortran
function add_three(a, b, c) result(total)
total = a + b + c
end function
```

**Output:**
```fortran
function add_three(a, b, c) result(total)
    implicit none
    real(8), intent(in) :: a, b, c
    real(8) :: total
    total = a + b + c
end function add_three
```

## Subroutines

Subroutines also get automatic parameter declarations:

**Input:**
```fortran
subroutine process_data(input, output)
output = input * 2
end subroutine
```

**Output:**
```fortran
subroutine process_data(input, output)
    implicit none
    real(8), intent(in) :: input
    real(8), intent(in) :: output
    output = input * 2
end subroutine process_data
```

## Intent Attributes

Function parameters automatically get `intent(in)` by default. For subroutines, intent can be specified explicitly in the original code or defaults to `intent(in)`.

## Mixed Explicit/Implicit Declarations

If you provide some explicit declarations, fortfront will:
- Respect your explicit declarations
- Generate missing declarations automatically
- Avoid duplicating existing declarations

**Input:**
```fortran
function compute(x, y) result(z)
integer :: y  ! Explicit declaration
z = x + y
end function
```

**Output:**
```fortran
function compute(x, y) result(z)
    implicit none
    integer :: y
    real(8), intent(in) :: x  ! Auto-generated
    real(8) :: z              ! Auto-generated
    z = x + y
end function compute
```

## Result Variable Handling

For functions with result clauses:
- If the result variable is not explicitly declared, fortfront infers its type
- Type inference uses the assigned expression or applies implicit typing rules
- Result variables do not get intent attributes (they are local to the function)

## Semantic Type Integration

The automatic declaration generation leverages fortfront's semantic analysis:
- **Type inference results** are used when available
- **Expression analysis** helps determine result variable types
- **Fallback to implicit rules** when semantic types are unavailable

## Benefits

1. **Write Less Code**: Focus on algorithm logic, not boilerplate declarations
2. **Prevent Errors**: Automatic declarations follow Fortran standards correctly
3. **Maintain Readability**: Generated code is clean and properly formatted
4. **Standard Compliance**: Output works with any Fortran compiler

## Current Implementation Status

✅ **Working Features:**
- Parameter declaration generation
- Result variable declaration generation  
- Intent(in) attributes for function parameters
- Fortran implicit typing rules application
- Multiple parameter grouping

🔧 **Current Behavior Notes:**
- Implicit typing follows Fortran rules but type inference may override in some cases
- All generated parameters currently default to `intent(in)`
- Result variable types are inferred from usage or implicit rules
- Complex type scenarios may require explicit declarations for optimal results

## Command Line Testing

Test the functionality with simple commands:

```bash
# Test basic function
echo "function twice(x) result(y)
y = 2*x
end function" | fortfront

# Test integer variables
echo "function count(i) result(n)
n = i + 1  
end function" | fortfront

# Test multiple parameters
echo "function add_numbers(a, b) result(sum)
sum = a + b
end function" | fortfront
```

## Integration Notes

This feature is automatically enabled in fortfront and requires no configuration. It works seamlessly with:
- Type inference system
- AST-based semantic analysis
- Standard code generation pipeline
- All other fortfront features