#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-q

all: $(LIB) $(LIB)/sieve

$(LIB)/sieve: sieve.adb 
	@gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

sieve: $(LIB)/sieve; $<

clean:
	rm -rf $(LIB)
