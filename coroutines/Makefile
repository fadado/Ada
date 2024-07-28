#

.SILENT:

all: hello pingpong

hello: hello.adb signals.ad[s] control.ad[sb]
	@gnatmake hello

pingpong: pingpong.adb signals.ad[s] control.ad[sb]
	@gnatmake pingpong

clean:
	@rm -f *.o *.ali pingpong hello *.ads.* *.adb.*
