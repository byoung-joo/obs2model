set(src_string
    stringut.F90)

add_library(basic ${src_string})
#add_library(basic ALIAS ${PROJECT_NAME})

# use make install here:
install (TARGETS basic
         LIBRARY DESTINATION  ${CMAKE_BINARY_DIR}/lib
	)
