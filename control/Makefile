# Control subsystem makefile

.SILENT:

########################################################################
# Subsystem specification
########################################################################

# Subsystem name is obtained from the CWD; hardcode it if prefered
SUBSYSTEM := $(notdir $(PWD))

# Where to look for sources (hardcoded names: directories must exist)
TST_DIR := ./tests
SRC_DIR := ./

# Output directories (created by make); feel free to use any names
OBJ_DIR := ./OBJECTS
BIN_DIR := ./BINARIES
LIB_DIR := ./LIBRARY

# Include switches database
include switches.make

# Define switches to compile library and tests
SUBSYSTEM_SW :=     \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_LINT)       \
   $(SW_OPTIMIZE)   \
   $(SW_STYLE)      \
   $(SW_QUIET)  \
   $(SW_VERSION)    \

TESTS_SW :=         \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_NO_GNATADC) \
   $(SW_OPTIMIZE)   \
   $(SW_QUIET)  \
   $(SW_VERSION)    \

# Include generic subsystem rules
include subsystem.make

########################################################################
# Tests shortcuts
########################################################################

.PHONY: semi coro spingpong fpingpong fibonacci iocopy

semi:      $(BIN_DIR)/test_semi; $<
coro:      $(BIN_DIR)/test_coro; $<
spingpong: $(BIN_DIR)/test_semi_pingpong; $<
fpingpong: $(BIN_DIR)/test_full_pingpong; $<
fibonacci: $(BIN_DIR)/test_fibonacci; $<
iocopy:    $(BIN_DIR)/test_io; ls -1 *.ad? | $<

# ¡ISO-8859-1!
# vim:fileformat=unix:fileencoding=latin1:syntax=make
