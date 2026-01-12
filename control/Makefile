#

.SILENT:

########################################################################

SUBSYSTEM=control

OBJ=./LIBRARY
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

$(TIMESTAMP): zilch_$(SUBSYSTEM).adb
	@gnatmake $(GNATFLAGS) $(LINT) --subdirs=$(OBJ) $(@F)
	@rm -f $(OBJ)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(TIMESTAMP)

build: $(OBJ) $(BIN) $(TIMESTAMP)

clean:
	rm -rf $(OBJ) $(BIN)

########################################################################

testers: $(patsubst %.adb,$(BIN)/%,$(wildcard test_*.adb))

$(BIN)/test_%: test_%.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(BIN) $(@F)

semi:      $(BIN)/test_semi; $<
coro:      $(BIN)/test_coro; $<
spingpong: $(BIN)/test_semi_pingpong; $<
fpingpong: $(BIN)/test_full_pingpong; $<
fibonacci: $(BIN)/test_fibonacci; $<
io:        $(BIN)/test_io; ls -1 *.ads | $<

