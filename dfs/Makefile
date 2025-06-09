#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

DFSLIB := dfs.ad[sb]

all: $(LIB) $(LIB)/dodeca
	@echo Done!

$(LIB)/dodeca: dodeca.adb dodeca-*.ad[sb] $(DFSLIB)
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

dodeca: $(LIB)/dodeca; $<

clean:
	rm -rf $(LIB)
