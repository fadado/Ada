# Subsystem makefile
# 	* Generates subsystem library in OBJ directory
# 	* Generates subsystem tests in BIN directory

#.SILENT:

########################################################################

SUBSYSTEM=control

OBJ=./OBJECTS
BIN=./BINARIES

VERBOSITY=-q # -q -d -vl

GNATFLAGS=$(VERBOSITY) -gnatn

LINT=-gnatwa

########################################################################

all: build testers

$(OBJ):
	@test -d $(OBJ) || mkdir -p $(OBJ)

$(BIN):
	@test -d $(BIN) || mkdir -p $(BIN)

TIMESTAMP=$(OBJ)/zilch_$(SUBSYSTEM)

$(TIMESTAMP): zilch_$(SUBSYSTEM).adb [^z][^i][^l][^c][^h]*.ad?
	gnatmake $(GNATFLAGS) $(LINT) --subdirs=$(OBJ) $(@F).adb
	@rm -f $(OBJ)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(TIMESTAMP)

build: $(OBJ) $(BIN) $(TIMESTAMP)

clean:
	rm -rf $(OBJ) $(BIN)

########################################################################

testers: $(patsubst tests/%.adb,$(BIN)/%,$(wildcard tests/test_*.adb))

$(BIN)/test_%: tests/test_%.adb ${TIMESTAMP}
	gnatmake $(GNATFLAGS) --subdirs=$(BIN) -aI. tests/$(@F).adb

semi:      $(BIN)/test_semi; $<
coro:      $(BIN)/test_coro; $<
spingpong: $(BIN)/test_semi_pingpong; $<
fpingpong: $(BIN)/test_full_pingpong; $<
fibonacci: $(BIN)/test_fibonacci; $<
io:        $(BIN)/test_io; ls -1 *.ad? | $<

