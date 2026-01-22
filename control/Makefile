# Subsystem makefile

.SILENT:

########################################################################
# Subsystem parameters
########################################################################

SUBSYSTEM := control

TST_DIR := ./tests
SRC_DIR := ./

SW_ASSERTIONS   := -gnata
SW_CHECK        := -gnatc -q
SW_INLINING     := -gnatn
SW_LINT         := -gnatwaU
SW_NO_GNATADC   := -gnatA
SW_OPTIMIZE     := -O2
SW_STYLE        := -gnaty3abefhipux
SW_VERBOSITY    := -q # -q -d -vl
SW_VERSION      := -gnat2012

SUBSYSTEM_SW :=     \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_LINT)       \
   $(SW_OPTIMIZE)   \
   $(SW_STYLE)      \
   $(SW_VERBOSITY)  \
   $(SW_VERSION)    \

TESTS_SW :=         \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_NO_GNATADC) \
   $(SW_OPTIMIZE)   \
   $(SW_VERBOSITY)  \
   $(SW_VERSION)    \

########################################################################
# Common subsystem's rules
########################################################################

.PHONY: help build clean clobber check tests

OBJ_DIR := ./OBJECTS
BIN_DIR := ./BINARIES
LIB_DIR := ./LIBRARY

DIRECTORIES := $(OBJ_DIR) $(BIN_DIR) $(LIB_DIR)

BUILD_TIMESTAMP := $(BIN_DIR)/zilch_$(SUBSYSTEM)

ARCHIVE := $(LIB_DIR)/lib$(SUBSYSTEM).a

help:
	echo 'Usage: make {target}'
	echo 'Targets:'
	echo '    build   - build the subsystem library'
	echo '    tests   - compile all tests'
	echo '    check   - check syntax and semantics'
	echo '    clean   - remove generated files'
	echo '    clobber - remove also generated directories'

build: $(DIRECTORIES) $(ARCHIVE)

tests: $(patsubst $(TST_DIR)/%.adb, \
                  $(BIN_DIR)/%,     \
                  $(wildcard $(TST_DIR)/test_*.adb))

$(DIRECTORIES):
	@test -d $@ || mkdir -p $@

$(ARCHIVE): $(BUILD_TIMESTAMP)
	@ar rc $@ $(OBJ_DIR)/*.o
	@cp --force --update $(OBJ_DIR)/*.ali $(LIB_DIR)
	@chmod -w $(LIB_DIR)/*.ali

$(BUILD_TIMESTAMP): zilch_$(SUBSYSTEM).adb *.ad?
	gnatmake $(SUBSYSTEM_SW) \
		 -aO$(OBJ_DIR)   \
		 -D $(OBJ_DIR)   \
		 -o $@ $<
	@rm $(OBJ_DIR)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(BUILD_TIMESTAMP)

$(BIN_DIR)/test_%: $(TST_DIR)/test_%.adb
	gnatmake $(TESTS_SW)  \
		-aI$(SRC_DIR) \
		-aO$(LIB_DIR) \
		-D $(BIN_DIR) \
		-o $@ $<      \
		-largs -L$(LIB_DIR) -l$(SUBSYSTEM)

check: $(OBJ_DIR) $(BIN_DIR) 
	@gnatmake $(SW_CHECK) -D $(OBJ_DIR) *.ad?
	@gnatmake $(SW_CHECK) -aI$(SRC_DIR) -D $(BIN_DIR) $(TST_DIR)/*.ad?

clean:
	@rm -f $(OBJ_DIR)/* $(BIN_DIR)/*

clobber:
	@rm -rf $(DIRECTORIES)

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
