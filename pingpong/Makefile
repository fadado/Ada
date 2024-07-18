#

.SILENT:

all: pingpong

pingpong: pingpong.adb signals.ad? conveyors.ad?
	@gnatmake pingpong

clean:
	@rm -f *.o *.ali pingpong *.ads.* *.adb.*
