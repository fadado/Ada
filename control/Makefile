# Subsystem makefile
#   * Generates subsystem library in OBJ_DIR directory
#   * Generates subsystem tests in BIN_DIR directory

.SILENT:

########################################################################
# Subsystem parameters
########################################################################

SUBSYSTEM := control

SW_ASSERTIONS   := -gnata
SW_INLINING     := -gnatn
SW_LINT         := -gnatwaU
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
   $(SW_VERSION)

TESTS_SW :=        \
   $(SW_ASSERTIONS)\
   $(SW_INLINING)  \
   $(SW_OPTIMIZE)  \
   $(SW_VERBOSITY) \
   $(SW_VERSION)

########################################################################
# Common subsystem's rules
########################################################################

OBJ_DIR := ./OBJECTS
BIN_DIR := ./BINARIES

TST_DIR := ./tests

TIMESTAMP := $(BIN_DIR)/zilch_$(SUBSYSTEM)

.PHONY: all build clean clobber tests

all: build tests

build: $(OBJ_DIR) $(BIN_DIR) $(TIMESTAMP)

clean:
	@rm -f $(OBJ_DIR)/* $(BIN_DIR)/*

clobber:
	@rm -rf $(OBJ_DIR) $(BIN_DIR)

tests: build \
   $(patsubst $(TST_DIR)/%.adb,$(BIN_DIR)/%,$(wildcard $(TST_DIR)/test_*.adb))

$(OBJ_DIR):
	@test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	@test -d $(BIN_DIR) || mkdir -p $(BIN_DIR)

$(TIMESTAMP): zilch_$(SUBSYSTEM).adb *.ad?
	gnatmake $(SUBSYSTEM_SW) -aO$(OBJ_DIR) -D $(OBJ_DIR) -o $@ $<
	@rm $(OBJ_DIR)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(TIMESTAMP)

$(BIN_DIR)/test_%: $(TST_DIR)/test_%.adb ${TIMESTAMP}
	gnatmake $(TESTS_SW) -aI. -aO$(OBJ_DIR) -D $(BIN_DIR) -o $@ $<

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

# vim:fileformat=unix:fileencoding=latin1:syntax=make
