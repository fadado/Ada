#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

SEMILIB := \
	control.ad[sb] \
	control-spin_until.ad[sb] \
	gotcha.ad[sb]

RUTLIB := \
	control-coroutines.ad[s] \
	control-coroutines-implement.ad[sb] \
	control-coroutines-semi.ad[s] \
	control-coroutines-full.ad[s]

GENLIB := \
	control-generators.ad[sb]

COLLIB := \
	control-collectors.ad[sb]

FUNLIB := \
	control-functors.ad[sb]

JUNLIB := \
	control-junctions.ad[sb]

all: $(LIB) \
     $(LIB)/test_semi \
     $(LIB)/test_coro \
     $(LIB)/test_semi_pingpong \
     $(LIB)/test_full_pingpong \
     $(LIB)/test_fibonacci \
     $(LIB)/test_io
	@echo Done!

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

# Test using the package Controllers
$(LIB)/test_semi: test_semi.adb ${SEMILIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using the package CoRoutines
$(LIB)/test_coro: test_coro.adb ${SEMILIB} ${RUTLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using symmetric Controllers
$(LIB)/test_semi_pingpong: test_semi_pingpong.adb ${SEMILIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using asymmetric Controllers (TODO!)
$(LIB)/test_full_pingpong: test_full_pingpong.adb ${SEMILIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using the package Generators
$(LIB)/test_fibonacci: test_fibonacci.adb ${SEMILIB} ${GENLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB)/test_io: test_io.adb ${SEMILIB} ${GENLIB} ${COLLIB} ${JUNLIB} ${FUNLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

semi:      $(LIB)/test_semi; $<
coro:      $(LIB)/test_coro; $<
spingpong: $(LIB)/test_semi_pingpong; $<
fpingpong: $(LIB)/test_full_pingpong; $<
fibonacci: $(LIB)/test_fibonacci; $<
io:        $(LIB)/test_io; ls -1 | $<

tests: semi coro spingpong fpingpong fibonacci
	@echo Done!

clean:
	rm -rf $(LIB)
