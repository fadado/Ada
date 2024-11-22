#

.SILENT:

all: test_hello test_pingpong test_fibonacci

LIBRARY := \
	control.ad[sb] \
	control-spin_until.ad[sb] \
	gotcha.ad[sb]

RUTLIB := \
	control-routines.ad[sb]

GENLIB := \
	control-generators.ad[sb]

# Test using the package Controllers and Routines
test_hello: test_hello.adb ${LIBRARY} ${RUTLIB}
	@gnatmake test_hello

# Test using symmetric Controllers
test_pingpong: test_pingpong.adb ${LIBRARY}
	@gnatmake test_pingpong

# Test using the package Generators
test_fibonacci: test_fibonacci.adb ${LIBRARY} ${GENLIB}
	@gnatmake test_fibonacci

clean:
	@rm -f *.o *.ali test_pingpong test_hello test_fibonacci *.ads.* *.adb.*
