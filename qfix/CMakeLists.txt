cmake_minimum_required(VERSION 2.6)

project(qfix)

add_definitions(-O2)

include_directories(/opt/quickfix/include)
link_directories(/opt/quickfix/lib)

add_executable(qfix_client qfix_client.cc)
add_executable(qfix_server qfix_server.cc)

target_link_libraries(qfix_client quickfix)
target_link_libraries(qfix_server quickfix)
