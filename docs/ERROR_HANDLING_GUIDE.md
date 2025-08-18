# Error Handling Guide

## Overview

fortfront uses a structured error handling system that provides graceful failure management instead of program termination. This guide explains how to use the error handling system effectively.

## Core Types

### result_t Type

The `result_t` type represents the outcome of an operation:

```fortran
use error_handling

type(result_t) :: result

! Check if operation succeeded
if (result%is_success()) then
    ! Handle success
else
    ! Handle error
    print *, result%get_full_message()
end if
```

### Error Severity Levels

```fortran
! Available severity levels
ERROR_INFO      ! Informational messages
ERROR_WARNING   ! Issues that don't prevent operation  
ERROR_ERROR     ! Failures that prevent operation
ERROR_CRITICAL  ! Severe failures requiring immediate attention
```

### Error Categories

```fortran
! Error category codes
ERROR_VALIDATION   ! Input validation failures
ERROR_TYPE_SYSTEM  ! Type checking and inference errors
ERROR_MEMORY       ! Memory allocation issues
ERROR_IO           ! Input/output operation failures
ERROR_PARSER       ! Parsing and syntax errors
ERROR_SEMANTIC     ! Semantic analysis errors
ERROR_INTERNAL     ! Internal consistency errors
```

## Creating Error Results

### Success Results

```fortran
use error_handling

type(result_t) :: result

! Create a success result
result = success_result()
```

### Error Results

```fortran
! Basic error
result = create_error_result("Something went wrong")

! Error with details
result = create_error_result( &
    message="Invalid input value", &
    code=ERROR_VALIDATION, &
    component="parser", &
    context="parse_expression", &
    suggestion="Check input syntax" &
)
```

### Warning and Critical Results

```fortran
! Warning (operation succeeds but with issues)
result = warning_result( &
    message="Deprecated syntax used", &
    suggestion="Use modern Fortran syntax" &
)

! Critical error
result = critical_result( &
    message="Memory allocation failed", &
    code=ERROR_MEMORY &
)
```

## Error Collection

For operations that can have multiple errors:

```fortran
use error_handling

type(error_collection_t) :: errors
integer :: i

! Create error collection
errors = create_error_collection()

! Add errors during processing
do i = 1, some_count
    if (some_error_condition) then
        call errors%add_error( &
            message="Error in item " // trim(i), &
            code=ERROR_VALIDATION &
        )
    end if
end do

! Check results
if (errors%has_errors()) then
    print *, "Found errors: ", errors%get_summary()
    
    ! Handle each error
    do i = 1, errors%get_error_count()
        print *, errors%errors(i)%get_full_message()
    end do
end if
```

## Integration Patterns

### Function Result Pattern

```fortran
function safe_operation(input) result(operation_result)
    character(len=*), intent(in) :: input
    type(result_t) :: operation_result
    
    ! Validate input
    if (len_trim(input) == 0) then
        operation_result = create_error_result( &
            "Input cannot be empty", &
            ERROR_VALIDATION, &
            suggestion="Provide a non-empty input value" &
        )
        return
    end if
    
    ! Perform operation
    ! ... 
    
    ! Return success
    operation_result = success_result()
end function
```

### Factory Result Pattern

For operations that create objects:

```fortran
type :: factory_result_t
    type(result_t) :: result
    integer :: node_index = 0
end type

function safe_create_node(...) result(factory_result)
    type(factory_result_t) :: factory_result
    
    ! Validate inputs
    if (invalid_input) then
        factory_result%result = create_error_result("Invalid input")
        return
    end if
    
    ! Create object
    call create_actual_node(...)
    factory_result%node_index = new_node_index
    factory_result%result = success_result()
end function
```

### Error Propagation

```fortran
function complex_operation(...) result(final_result)
    type(result_t) :: final_result, temp_result
    
    ! Step 1
    temp_result = first_operation(...)
    if (temp_result%is_failure()) then
        final_result = temp_result  ! Propagate error
        return
    end if
    
    ! Step 2  
    temp_result = second_operation(...)
    if (temp_result%is_failure()) then
        final_result = temp_result  ! Propagate error
        return
    end if
    
    final_result = success_result()
end function
```

## Error Message Best Practices

### Clear and Specific Messages

```fortran
! Bad: Generic message
result = create_error_result("Error occurred")

! Good: Specific message
result = create_error_result( &
    "Assignment target must be an identifier", &
    suggestion="Use variable name as assignment target" &
)
```

### Provide Context

```fortran
result = create_error_result( &
    message="Unknown binary operator: " // trim(operator), &
    code=ERROR_TYPE_SYSTEM, &
    component="semantic_analyzer", &
    context="analyze_binary_expression", &
    suggestion="Use valid Fortran operators (+, -, *, /, ==, etc.)" &
)
```

### Actionable Suggestions

```fortran
! Include helpful suggestions
result = create_error_result( &
    "Array index out of bounds", &
    suggestion="Check array bounds or use allocatable arrays" &
)
```

## Testing Error Conditions

### Testing Success Cases

```fortran
! Test successful operation
result = safe_operation("valid_input")
if (.not. result%is_success()) then
    error stop "Expected success but got error: " // result%get_message()
end if
```

### Testing Error Cases

```fortran
! Test error condition
result = safe_operation("")  ! Empty input should fail
if (result%is_success()) then
    error stop "Expected error for empty input"
end if

! Verify specific error
if (result%error_code /= ERROR_VALIDATION) then
    error stop "Expected validation error"
end if
```

### Testing Error Collections

```fortran
type(error_collection_t) :: errors

! ... perform operations that may generate errors

! Test error collection
if (.not. errors%has_errors()) then
    error stop "Expected errors but none found"
end if

if (errors%get_error_count() /= expected_count) then
    error stop "Wrong number of errors"
end if
```

## Migration from error_stop

### Before (error_stop pattern)

```fortran
subroutine validate_input(input)
    character(len=*), intent(in) :: input
    
    if (len_trim(input) == 0) then
        error stop "Input cannot be empty"
    end if
    
    if (invalid_format(input)) then
        error stop "Invalid input format"
    end if
end subroutine
```

### After (structured error handling)

```fortran
function validate_input(input) result(validation_result)
    character(len=*), intent(in) :: input
    type(result_t) :: validation_result
    
    if (len_trim(input) == 0) then
        validation_result = create_error_result( &
            "Input cannot be empty", &
            ERROR_VALIDATION, &
            suggestion="Provide a non-empty input value" &
        )
        return
    end if
    
    if (invalid_format(input)) then
        validation_result = create_error_result( &
            "Invalid input format", &
            ERROR_VALIDATION, &
            suggestion="Check input syntax and format" &
        )
        return
    end if
    
    validation_result = success_result()
end function
```

## Performance Considerations

- Error handling has minimal overhead in success cases
- Error results are only allocated when needed
- Use early returns to fail fast
- Combine related errors into collections rather than individual results

## Advanced Usage

### Combining Results

```fortran
type(result_t) :: results(3), combined

! ... populate individual results

! Combine multiple results
combined = combine_results(results)

! The combined result:
! - Fails if any individual result fails
! - Uses the worst severity level
! - Contains error information from the most severe failure
```

### Custom Error Types

For specialized error handling, extend the basic patterns:

```fortran
type, extends(result_t) :: semantic_result_t
    type(mono_type_t), allocatable :: inferred_type
    integer :: node_index = 0
end type
```

This structured approach to error handling makes fortfront components more robust, testable, and suitable for library integration while providing better user experience through meaningful error messages.