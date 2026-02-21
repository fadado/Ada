#

.SILENT:

OUT=./OUTPUT
GNATFLAGS=-q

all: $(OUT) $(OUT)/sieve

$(OUT)/sieve: sieve.adb 
	@gnatmake $(GNATFLAGS) --subdirs=$(OUT) $(@F)

$(OUT):
	@test -d $(OUT) || mkdir $(OUT)

sieve: $(OUT)/sieve; $<

clean:
	rm -f $(OUT)/*

clobber:
	rm -rf $(OUT)
