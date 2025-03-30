#

.SILENT:

all: tests_music tests_generics

tests_music: music.ad[bs] music-*.ad[bs] database.ad[bs] tests_music.adb
	gnatmake tests_music

tests_generics: generics.ad[bs] generics-*.ad[bs] tests_generics.adb
	gnatmake tests_generics

clean:
	rm -f *.o *.ali  *.ads.* *.adb.* \
		tests_music tests_generics
