#

.SILENT:

all: dummy tester

dummy: kiss.ad[sb] kiss-*.ad[sb] dummy.adb
	gnatmake -q dummy
	@echo KISS library built!

tester: dummy tests.ad[sb] tester.adb
	gnatmake -q tester
	@echo KISS tests compiled!

clean:
	rm -f *.o *.ali tester *.ads.* *.adb.* dummy
