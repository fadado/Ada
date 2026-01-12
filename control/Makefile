#

.SILENT:

########################################################################

SUBSYSTEM=control

LIB=./LIBRARY

GNATFLAGS= # -q -d -vl

TIMESTAMP=$(LIB)/zilch_$(SUBSYSTEM)

all: build tests

########################################################################

$(LIB):
	@test -d $(LIB) || mkdir -p $(LIB)

$(LIB)/zilch_$(SUBSYSTEM): zilch_$(SUBSYSTEM).adb
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

build: $(LIB) $(TIMESTAMP)

clean:
	rm -rf $(LIB)

########################################################################

tests: \
     $(LIB)/test_semi \
     $(LIB)/test_coro \
     $(LIB)/test_semi_pingpong \
     $(LIB)/test_full_pingpong \
     $(LIB)/test_fibonacci \
     $(LIB)/test_io

# Test using the package Controllers
$(LIB)/test_semi: test_semi.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using the package CoRoutines
$(LIB)/test_coro: test_coro.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using symmetric Controllers
$(LIB)/test_semi_pingpong: test_semi_pingpong.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using asymmetric Controllers (TODO!)
$(LIB)/test_full_pingpong: test_full_pingpong.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using the package Generators
$(LIB)/test_fibonacci: test_fibonacci.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Testing IO
$(LIB)/test_io: test_io.adb ${TIMESTAMP}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

semi:      $(LIB)/test_semi; $<
coro:      $(LIB)/test_coro; $<
spingpong: $(LIB)/test_semi_pingpong; $<
fpingpong: $(LIB)/test_full_pingpong; $<
fibonacci: $(LIB)/test_fibonacci; $<
io:        $(LIB)/test_io; ls -1 *.ads | $<

