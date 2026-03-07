# Series program makefile
#
# Require generics

.SILENT:

PROGRAM := permute

include ../program.make

$(OUT)/$(PROGRAM): $(PROGRAM).adb 
	@gnatmake -q $(GNATFLAGS) -o $@ $<

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
