#

.SILENT:

all: demo

demo: kiss.ad[sb] kiss-*.ad[sb] demo.adb
	gnatmake demo

clean:
	rm -f *.o *.ali demo *.ads.* *.adb.*
