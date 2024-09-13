cmake_minimum_required(VERSION 3.20.0)

find_package(Zephyr REQUIRED HINTS $ENV{ZEPHYR_BASE})
project(bearsink)

add_compile_options(-Wall -Werror)

file(GLOB_RECURSE SOURCE "*.c")
file(GLOB_RECURSE SOURCE "*.cpp")
target_sources(app PRIVATE ${SOURCE})
