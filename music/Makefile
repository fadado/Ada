#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

GENLIB=generics.ad[bs] generics-*.ad[bs]

MUSLIB=music.ad[bs] music-*.ad[bs]

all: $(LIB) $(LIB)/tests_music $(LIB)/tests_generics
	@echo Done!

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

$(LIB)/tests_music: $(MUSLIB) $(GENLIB) tests_music.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB)/tests_generics: $(GENLIB) tests_generics.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

music:    $(LIB)/tests_music;    $<
generics: $(LIB)/tests_generics; $<

clean:
	rm -rf $(LIB)
