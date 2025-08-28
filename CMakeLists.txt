cmake_minimum_required(VERSION 3.18)

# Set policy version to handle json-fortran compatibility
if(POLICY CMP0002)
    cmake_policy(SET CMP0002 NEW)
endif()
set(CMAKE_POLICY_VERSION_MINIMUM "3.5")

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
    src/utilities/string_types.f90
    src/utilities/path_validation.f90
    src/utilities/intrinsic_registry.f90
    src/error_reporting.f90
    src/utilities/input_validation.f90
    src/common/uid_generator.f90
    src/memory/arena_memory.f90
    src/utilities/frontend_utilities.f90
)

set(LEXER_SOURCES
    src/lexer/lexer_token_types.f90
    src/lexer/lexer_scanners.f90
    src/lexer/lexer_core.f90
)

set(AST_SOURCES
    src/ast/ast_base.f90
    src/ast/ast_error_nodes.f90
    src/ast/traversal/ast_visitor.f90
    src/ast/traversal/ast_traversal.f90
    src/ast/nodes/ast_nodes_core.f90
    src/ast/nodes/ast_nodes_procedure.f90
    src/ast/nodes/ast_nodes_control.f90
    src/ast/nodes/ast_nodes_data.f90
    src/ast/nodes/ast_nodes_io.f90
    src/ast/nodes/ast_nodes_misc.f90
    src/ast/nodes/ast_nodes_bounds.f90
    src/ast/arena/ast_arena_core.f90
    src/ast/arena/ast_arena_compat.f90
    src/ast/arena/ast_arena_modern.f90
    src/ast/ast_core.f90
    src/ast/ast_types.f90
    src/ast/factory/ast_factory_core.f90
    src/ast/factory/ast_factory_expressions.f90
    src/ast/factory/ast_factory_declarations.f90
    src/ast/factory/ast_factory_control.f90
    src/ast/factory/ast_factory_io.f90
    src/ast/factory/ast_factory_procedures.f90
    src/ast/factory/ast_factory_arrays.f90
    src/ast/factory/ast_factory_statements.f90
    src/ast/factory/ast_factory.f90
    src/ast/ast_introspection.f90
)

set(TYPE_SYSTEM_SOURCES
    src/semantic/types/type_constants.f90
    src/semantic/types/type_system_arena.f90
    src/semantic/types/type_system_unified.f90
)

set(PARSER_SOURCES
    src/parser/parser_state.f90
    src/parser/parser_result_types.f90
    src/parser/parser_utils.f90
    src/parser/parser_core.f90
    src/parser/parser_expressions.f90
    src/parser/parser_declarations.f90
    src/parser/parser_control_statements.f90
    src/parser/parser_array_constructs.f90
    src/parser/parser_basic_statement_module.f90
    src/parser/parser_control_flow.f90
    src/parser/parser_definition_statements.f90
    src/parser/parser_do_constructs.f90
    src/parser/parser_execution_statements.f90
    src/parser/parser_if_constructs.f90
    src/parser/parser_memory_statements.f90
    src/parser/parser_select_constructs.f90
    src/parser/mixed_construct_detector.f90
    src/parser/url_utilities.f90
    src/parser/parser_io_statements.f90
    src/parser/parser_import_statements.f90
    src/parser/parser_dispatcher.f90
)

set(SEMANTIC_SOURCES
    src/semantic/types/semantic_context_types.f90
    src/semantic/scope_manager.f90
    src/semantic/types/type_checker.f90
    src/semantic/analyzers/parameter_tracker.f90
    src/semantic/analyzers/expression_temporary_tracker.f90
    src/semantic/constant_transformation.f90
    src/semantic/semantic_inference_helpers.f90
    src/semantic/semantic_query_api.f90
    src/semantic/analyzers/semantic_analyzer.f90
)

set(ANALYSIS_SOURCES
    src/analysis/control_flow_graph.f90
    src/analysis/conditional_evaluation.f90
    src/analysis/cfg_builder.f90
    src/analysis/control_flow_analysis.f90
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
    src/utilities/json_reader.f90
    src/frontend_core.f90
    src/frontend_parsing.f90
    src/frontend.f90
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
    ${ANALYSIS_SOURCES}
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

# Add test discovery - use specific patterns to avoid duplicates
# Instead of using GLOB_RECURSE which finds duplicates, use specific subdirectories
set(TEST_DIRECTORIES
    "test/analysis"
    "test/api"
    "test/ast"
    "test/benchmark"
    "test/build"
    "test/codegen"
    "test/common"
    "test/cst"
    "test/debug"
    "test/error_handling"
    "test/error_reporting"
    "test/frontend"
    "test/integration/array_tests"
    "test/integration/core_features"
    "test/integration/issue_tests"
    "test/integration/type_tests"
    "test/intrinsic"
    "test/json"
    "test/lexer"
    "test/memory"
    "test/parser"
    "test/performance"
    "test/semantic"
    "test/standardizer"
    "test/system"
    "test/utils"
    "test/validation"
)

set(FILTERED_TEST_SOURCES)
foreach(test_dir ${TEST_DIRECTORIES})
    if(EXISTS "${CMAKE_SOURCE_DIR}/${test_dir}")
        file(GLOB test_files "${test_dir}/test_*.f90")
        foreach(test_file ${test_files})
            get_filename_component(test_name ${test_file} NAME_WE)
            # Skip utility files that aren't actual tests
            if(NOT test_name STREQUAL "readme_file_utils")
                list(APPEND FILTERED_TEST_SOURCES ${test_file})
            endif()
        endforeach()
    endif()
endforeach()

foreach(test_file ${FILTERED_TEST_SOURCES})
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