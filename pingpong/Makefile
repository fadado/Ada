#

.SILENT:

all: pingpong

pingpong: pingpong.adb signals.ad[s] control.ad[sb]
	@gnatmake pingpong

clean:
	@rm -f *.o *.ali pingpong *.ads.* *.adb.*
