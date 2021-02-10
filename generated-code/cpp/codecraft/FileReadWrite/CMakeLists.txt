cmake_minimum_required(VERSION 3.5)
set (CMAKE_CXX_STANDARD 20)
project(trans_gen_test)

# OS and compiler checks.
if(WIN32)
    add_definitions(-DWIN32)
    SET(PROJECT_LIBS Ws2_32.lib)
endif()

set(HEADERS
    "Color.hpp"
    "Stream.hpp"
    "Vec2Float.hpp"
    "Vec2Int.hpp"
    "codegame/ClientMessage.hpp"
    "codegame/DebugCommand.hpp"
    "codegame/MessageGameModel.hpp"
    "codegame/ServerMessage.hpp"
    "model/Action.hpp"
    "model/AttackAction.hpp"
    "model/AttackProperties.hpp"
    "model/AutoAttack.hpp"
    "model/BuildAction.hpp"
    "model/BuildProperties.hpp"
    "model/Entity.hpp"
    "model/EntityAction.hpp"
    "model/EntityProperties.hpp"
    "model/EntityType.hpp"
    "model/MoveAction.hpp"
    "model/Player.hpp"
    "model/PlayerView.hpp"
    "model/RepairAction.hpp"
    "model/RepairProperties.hpp"
    "model/debug_interface/ColoredVertex.hpp"
    "model/debug_interface/DebugData.hpp"
    "model/debug_interface/PrimitiveType.hpp"
)
set (SRC
    "Color.cpp"
    "Stream.cpp"
    "Vec2Float.cpp"
    "Vec2Int.cpp"
    "codegame/ClientMessage.cpp"
    "codegame/DebugCommand.cpp"
    "codegame/MessageGameModel.cpp"
    "codegame/ServerMessage.cpp"
    "main.cpp"
    "model/Action.cpp"
    "model/AttackAction.cpp"
    "model/AttackProperties.cpp"
    "model/AutoAttack.cpp"
    "model/BuildAction.cpp"
    "model/BuildProperties.cpp"
    "model/Entity.cpp"
    "model/EntityAction.cpp"
    "model/EntityProperties.cpp"
    "model/EntityType.cpp"
    "model/MoveAction.cpp"
    "model/Player.cpp"
    "model/PlayerView.cpp"
    "model/RepairAction.cpp"
    "model/RepairProperties.cpp"
    "model/debug_interface/ColoredVertex.cpp"
    "model/debug_interface/DebugData.cpp"
    "model/debug_interface/PrimitiveType.cpp"
)
SET_SOURCE_FILES_PROPERTIES(${HEADERS} PROPERTIES HEADER_FILE_ONLY TRUE)
include_directories(".")
add_executable(trans_gen_test ${HEADERS} ${SRC})
TARGET_LINK_LIBRARIES(trans_gen_test ${PROJECT_LIBS})