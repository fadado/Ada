#

.SILENT:

all: pingpong

pingpong: pingpong.adb
	gnatmake -q pingpong

clean:
	rm -f *.o *.ali pingpong *.ads.* *.adb.*
