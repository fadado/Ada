#

.SILENT:

all: pingpong

pingpong: pingpong.adb signals.ad? control.ad?
	@gnatmake pingpong

clean:
	@rm -f *.o *.ali pingpong *.ads.* *.adb.*
