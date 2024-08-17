#

.SILENT:

all: hello test_pingpong

test_hello: test_hello.adb signals.ad[s] control.ad[sb]
	@gnatmake test_hello

test_pingpong: test_pingpong.adb signals.ad[s] control.ad[sb]
	@gnatmake test_pingpong

clean:
	@rm -f *.o *.ali test_pingpong test_hello *.ads.* *.adb.*
