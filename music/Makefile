#

.SILENT:

GENERICS=generics.ad[bs] generics-*.ad[bs]
MUSIC=music.ad[bs] music-*.ad[bs] database.ad[bs]

all: tests_music tests_generics

tests_music: $(MUSIC) $(GENERICS) tests_music.adb
	gnatmake tests_music

tests_generics: $(GENERICS) tests_generics.adb
	gnatmake tests_generics

TESTS=tests_music tests_generics

clean:
	rm -f *.o *.ali  *.ads.* *.adb.* $(TESTS)
