########################################################################
# Common program's rules
########################################################################

OUT := ./OUTPUT

SUBSYS_DIR := ../../subsystems
SUBSYS_LIB := $(SUBSYS_DIR)/LIBRARY

GNATFLAGS := \
   -gnatW8 \
   -aO$(SUBSYS_LIB) \
   -aI$(SUBSYS_DIR)/INCLUDE \
   -D $(OUT) \
   -largs -L$(SUBSYS_LIB) -lallinone -margs

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

clean:
	@rm -f $(OUT)/*

clobber:
	@rm -rf $(OUT)

run: $(OUT) $(OUT)/$(PROGRAM)
	$(OUT)/$(PROGRAM)

$(OUT):
	@test -d $(OUT) || mkdir $(OUT)

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
