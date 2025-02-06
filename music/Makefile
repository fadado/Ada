#

.SILENT:

all: tests

tests: music.ad[bs] music-*.ad[bs] database.ad[bs] tests.adb
	gnatmake -gnatf tests

clean:
	rm -f *.o *.ali tests *.ads.* *.adb.*
