#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

GENERICS=generics.ad[bs] generics-*.ad[bs]

MUSIC=music.ad[bs] music-*.ad[bs]

all: $(LIB) $(LIB)/tests_music $(LIB)/tests_generics
	$(LIB)/tests_generics
	$(LIB)/tests_music

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

$(LIB)/tests_music: $(MUSIC) $(GENERICS) tests_music.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) tests_music.adb

$(LIB)/tests_generics: $(GENERICS) tests_generics.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) tests_generics.adb

clean:
	rm -rf $(LIB)
