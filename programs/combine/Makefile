.SILENT:

PROGRAM := $(notdir $(PWD))

include ../program.make

$(OUT)/$(PROGRAM): $(PROGRAM).adb 
	@gnatmake -q $(GNATFLAGS) -o $@ $<

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
