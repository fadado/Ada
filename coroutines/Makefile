#

.SILENT:

all: test_hello test_pingpong

LIBRARY := \
	signals.ads \
	control.ad[sb] \
	gotcha.ad[sb]

test_hello: test_hello.adb ${LIBRARY}
	@gnatmake test_hello

test_pingpong: test_pingpong.adb ${LIBRARY}
	@gnatmake test_pingpong

clean:
	@rm -f *.o *.ali test_pingpong test_hello *.ads.* *.adb.*
