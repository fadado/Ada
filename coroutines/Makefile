#

.SILENT:

all: test_hello_1 test_hello_2 test_pingpong test_fibonacci

LIBRARY := \
	signals.ad[s] \
	control.ad[sb] \
	gotcha.ad[sb]

RUTLIB := \
	control-routines.ad[sb]

GENLIB := \
	control-generators.ad[sb]

test_hello_1: test_hello_1.adb ${LIBRARY}
	@gnatmake test_hello_1

test_hello_2: test_hello_2.adb ${LIBRARY} ${RUTLIB}
	@gnatmake test_hello_2

test_pingpong: test_pingpong.adb ${LIBRARY}
	@gnatmake test_pingpong

test_fibonacci: test_fibonacci.adb ${LIBRARY} ${GENLIB}
	@gnatmake test_fibonacci

clean:
	@rm -f *.o *.ali test_pingpong test_hello_[0-9] test_fibonacci *.ads.* *.adb.*
