#

.SILENT:

LIB=./LIBRARY
GNATFLAGS=-O2 -q -gnatW8

GENLIB=generics.ad[bs] generics-*.ad[bs]

MUSLIB=music.ad[bs] music-*.ad[bs]

all: $(LIB) $(LIB)/tests_music $(LIB)/tests_generics
	@echo Done!

$(LIB):
	@test -d $(LIB) || mkdir $(LIB)

$(LIB)/tests_music: $(MUSLIB) $(GENLIB) tests_music.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

$(LIB)/tests_generics: $(GENLIB) tests_generics.adb
	gnatmake $(GNATFLAGS) --subdirs=$(LIB) $(@F)

music:    $(LIB)/tests_music;    $<
generics: $(LIB)/tests_generics; $<

tests: music generics
	@echo Tests finished!

clean:
	rm -rf $(LIB)

# Α α  Β β  Γ γ  Δ δ  Ε ε  Ζ ζ  Η η  Θ θ  Ι ι  Κ κ  Λ λ  Μ μ 
# Ν ν  Ξ ξ  Ο ο  Π π  Ρ ρ  Σ ς  Τ τ  Υ υ  Φ φ  Χ χ  Ψ ψ  Ω ω

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
