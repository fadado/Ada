# Series program makefile
#
# Require generics

.SILENT:

PROGRAM := series

include ../program.make

$(OUT)/$(PROGRAM): $(PROGRAM).adb 
	@gnatmake -q $(GNATFLAGS) -o $@ $<

opci-full:   $(OUT)/$(PROGRAM); $<
opci-normal: $(OUT)/$(PROGRAM); $< | sed -n '/^ 1/q;p'
opci-half:   $(OUT)/$(PROGRAM); $< | sed -n '/^ 0 7/q;p'

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
