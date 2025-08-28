cmake_minimum_required(VERSION 3.18)
project(fortfront VERSION 0.1.0 LANGUAGES Fortran)

# Enable Fortran preprocessing
set(CMAKE_Fortran_PREPROCESS ON)

# Set compiler flags equivalent to FPM's flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fmax-stack-var-size=524288")

# Find dependencies
find_package(PkgConfig REQUIRED)

# Fetch json-fortran dependency
include(FetchContent)
FetchContent_Declare(
    json-fortran
    GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
    GIT_TAG 8.3.0
)
FetchContent_MakeAvailable(json-fortran)

# Set Fortran module directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Include directories
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# Source files - organized by dependency order
set(CORE_SOURCES
    src/error_handling.f90
    src/string_types.f90
    src/path_validation.f90
    src/intrinsic_registry.f90
    src/error_reporting.f90
    src/input_validation.f90
    src/common/uid_generator.f90
    src/memory/arena_memory.f90
    src/frontend_utilities.f90
)

set(LEXER_SOURCES
    src/lexer/lexer_token_types.f90
    src/lexer/lexer_scanners.f90
    src/lexer/lexer_core.f90
)

set(AST_SOURCES
    src/ast/ast_base.f90
    src/ast/ast_error_nodes.f90
    src/ast/ast_visitor.f90
    src/ast/ast_traversal.f90
    src/ast/ast_nodes_core.f90
    src/ast/ast_nodes_procedure.f90
    src/ast/ast_nodes_control.f90
    src/ast/ast_nodes_data.f90
    src/ast/ast_nodes_io.f90
    src/ast/ast_nodes_misc.f90
    src/ast/ast_nodes_bounds.f90
    src/ast/ast_arena_core.f90
    src/ast/ast_arena_compat.f90
    src/ast/ast_arena_modern.f90
    src/ast/ast_core.f90
    src/ast/ast_types.f90
    src/ast/ast_factory.f90
    src/ast/ast_introspection.f90
)

set(TYPE_SYSTEM_SOURCES
    src/semantic/type_constants.f90
    src/semantic/type_system_arena.f90
    src/semantic/type_system_unified.f90
)

set(PARSER_SOURCES
    src/parser/parser_state.f90
    src/parser/parser_result_types.f90
    src/parser/parser_utils.f90
    src/parser/parser_core.f90
    src/parser/parser_expressions.f90
    src/parser/parser_declarations.f90
    src/parser/parser_control_statements.f90
    src/parser/parser_dispatcher.f90
)

set(SEMANTIC_SOURCES
    src/semantic/semantic_context_types.f90
    src/semantic/scope_manager.f90
    src/semantic/type_checker.f90
    src/semantic/semantic_analyzer.f90
)

set(CODEGEN_SOURCES
    src/codegen/codegen_indent.f90
    src/codegen/codegen_utilities.f90
    src/codegen/codegen_expressions.f90
    src/codegen/codegen_statements.f90
    src/codegen/codegen_declarations.f90
    src/codegen/codegen_control_flow.f90
    src/codegen/codegen_core.f90
)

set(FRONTEND_SOURCES
    src/json_reader.f90
    src/frontend_core.f90
    src/frontend_parsing.f90
    src/fortfront.f90
)

# Combine all sources
set(ALL_SOURCES
    ${CORE_SOURCES}
    ${LEXER_SOURCES}
    ${AST_SOURCES}
    ${TYPE_SYSTEM_SOURCES}
    ${PARSER_SOURCES}
    ${SEMANTIC_SOURCES}
    ${CODEGEN_SOURCES}
    ${FRONTEND_SOURCES}
)

# Create main executable
add_executable(fortfront ${ALL_SOURCES})

# Link json-fortran
target_link_libraries(fortfront jsonfortran)

# Set module output directory
target_include_directories(fortfront PUBLIC 
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${CMAKE_BINARY_DIR}/_deps/json-fortran-build
)

# Installation
install(TARGETS fortfront RUNTIME DESTINATION bin)

# Enable testing
enable_testing()

# Add test discovery (simplified version)
file(GLOB TEST_SOURCES "test/*.f90")
foreach(test_file ${TEST_SOURCES})
    get_filename_component(test_name ${test_file} NAME_WE)
    add_executable(${test_name} ${test_file} ${ALL_SOURCES})
    target_link_libraries(${test_name} jsonfortran)
    target_include_directories(${test_name} PUBLIC 
        ${CMAKE_Fortran_MODULE_DIRECTORY}
        ${CMAKE_BINARY_DIR}/_deps/json-fortran-build
    )
    add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()

# Print configuration info
message(STATUS "fortfront CMake configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  Fortran flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "  Module directory: ${CMAKE_Fortran_MODULE_DIRECTORY}")