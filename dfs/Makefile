#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

DFSLIB := backtracker.ad[sb]

all: $(LIB) $(LIB)/test_dodeca $(LIB)/test_queens
	@echo Done!

$(LIB)/test_dodeca: test_dodeca.adb test_dodeca-*.ad[sb] $(DFSLIB)
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB)/test_queens: test_queens.adb $(DFSLIB)
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

test_dodeca: $(LIB)/test_dodeca; $<
test_queens: $(LIB)/test_queens; $<

clean:
	rm -rf $(LIB)
