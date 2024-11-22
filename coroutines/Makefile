#

.SILENT:

all: test_hello_1 test_hello_2 test_pingpong test_fibonacci

LIBRARY := \
	control.ad[sb] \
	control-spin_until.ad[sb] \
	gotcha.ad[sb]

RUTLIB := \
	control-routines.ad[sb]

GENLIB := \
	control-generators.ad[sb]

# Test using only the package Controllers
test_hello_1: test_hello_1.adb ${LIBRARY}
	@gnatmake test_hello_1

# Test using the package Routines
test_hello_2: test_hello_2.adb ${LIBRARY} ${RUTLIB}
	@gnatmake test_hello_2

# Test using symmetric Controllers
test_pingpong: test_pingpong.adb ${LIBRARY}
	@gnatmake test_pingpong

# Test using the package Generators
test_fibonacci: test_fibonacci.adb ${LIBRARY} ${GENLIB}
	@gnatmake test_fibonacci

clean:
	@rm -f *.o *.ali test_pingpong test_hello_[0-9] test_fibonacci *.ads.* *.adb.*
