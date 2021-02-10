cmake_minimum_required(VERSION 3.5)
set (CMAKE_CXX_STANDARD 20)
project(trans_gen_test)

# OS and compiler checks.
if(WIN32)
    add_definitions(-DWIN32)
    SET(PROJECT_LIBS Ws2_32.lib)
endif()

set(HEADERS
    "Enumeration.hpp"
    "Example.hpp"
    "OneOf.hpp"
    "Stream.hpp"
    "Structure.hpp"
    "TcpStream.hpp"
)
set (SRC
    "Enumeration.cpp"
    "Example.cpp"
    "OneOf.cpp"
    "Stream.cpp"
    "Structure.cpp"
    "TcpStream.cpp"
    "main.cpp"
)
SET_SOURCE_FILES_PROPERTIES(${HEADERS} PROPERTIES HEADER_FILE_ONLY TRUE)
include_directories(".")
add_executable(trans_gen_test ${HEADERS} ${SRC})
TARGET_LINK_LIBRARIES(trans_gen_test ${PROJECT_LIBS})