#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

DFSLIB := depth_first_search.ad[sb]

all: $(LIB) $(LIB)/test_series $(LIB)/test_queens
	@echo Done!

$(LIB)/test_series: test_series.adb $(DFSLIB)
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB)/test_queens: test_queens.adb $(DFSLIB)
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

queens:      $(LIB)/test_queens; $<
opci-full:   $(LIB)/test_series; $<
opci-normal: $(LIB)/test_series; $< | sed -n '/^ 1/q;p'
opci-half:   $(LIB)/test_series; $< | sed -n '/^ 0 7/q;p'

clean:
	rm -f $(LIB)/*

clobber:
	rm -rf $(LIB)
