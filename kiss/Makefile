#

.SILENT:

all: tester

tester: kiss.ad[sb] kiss-*.ad[sb] tests.ad[sb] tests-*.ad[sb] tester.adb
	gnatmake tester

clean:
	rm -f *.o *.ali tester *.ads.* *.adb.*
