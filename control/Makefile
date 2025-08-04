#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

CTRLIB := \
	control.ad[sb] \
	control-spin_until.ad[sb] \
	gotcha.ad[sb]

RUTLIB := \
	control-coroutines.ad[sb]

GENLIB := \
	control-generators.ad[sb]

all: $(LIB) \
     $(LIB)/test_hello \
     $(LIB)/test_pingpong \
     $(LIB)/test_fibonacci
	@echo Done!

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

# Test using the package Controllers and CoRoutines
$(LIB)/test_hello: test_hello.adb ${CTRLIB} ${RUTLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using symmetric Controllers
$(LIB)/test_pingpong: test_pingpong.adb ${CTRLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

# Test using the package Generators
$(LIB)/test_fibonacci: test_fibonacci.adb ${CTRLIB} ${GENLIB}
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

hello:     $(LIB)/test_hello;     $<
pingpong:  $(LIB)/test_pingpong;  $<
fibonacci: $(LIB)/test_fibonacci; $<

clean:
	rm -rf $(LIB)
