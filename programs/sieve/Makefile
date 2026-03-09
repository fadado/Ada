# Sieve program standalone makefile

.SILENT:

PROGRAM := sieve

OUT=./OUTPUT

.PHONY: help build clean clobber run

help:
	echo 'Usage: make {target}'
	echo 'Targets:'
	echo '    build   - build the subsystem library'
	echo '    clean   - remove generated files'
	echo '    clobber - remove also generated directories'
	echo '    help    - show this message'
	echo '    run     - execute the compiled program'

build: $(OUT) $(OUT)/$(PROGRAM)

$(OUT)/$(PROGRAM): $(PROGRAM).adb 
	@gnatmake -q -gnatW8 -D $(OUT) -o $@ $<

$(OUT):
	@test -d $(OUT) || mkdir $(OUT)

clean:
	rm -f $(OUT)/*

clobber:
	rm -rf $(OUT)

run: $(OUT) $(OUT)/$(PROGRAM)
	$(OUT)/$(PROGRAM)

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
