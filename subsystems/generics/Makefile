# Generics subsystem makefile

.SILENT:

########################################################################
# Subsystem specification
########################################################################

# Subsystem name is obtained from the CWD; hardcode it if prefered
SUBSYSTEM := $(notdir $(PWD))

# Where to look for sources (hardcoded names: directories must exist)
TST_DIR := ./tests
SRC_DIR := ./

# Output directories (created by make); feel free to use any names
OBJ_DIR := ./OBJECTS
BIN_DIR := ./BINARIES
LIB_DIR := ./LIBRARY

# Include switches database
include ../switches.make

# Define switches to compile library and tests
SUBSYSTEM_SW :=     \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_LINT)       \
   $(SW_OPTIMIZE)   \
   $(SW_STYLE)      \
   $(SW_QUIET)      \
   $(SW_UTF8)       \
   $(SW_VERSION)    \

TESTS_SW :=         \
   $(SW_ASSERTIONS) \
   $(SW_INLINING)   \
   $(SW_NO_GNATADC) \
   $(SW_OPTIMIZE)   \
   $(SW_QUIET)      \
   $(SW_UTF8)       \
   $(SW_VERSION)    \

# Include generic subsystem rules
include ../subsystem.make

########################################################################
# Tests shortcuts
########################################################################

.PHONY: generics

generics: $(BIN_DIR)/test_generics; $<

# Α α  Β β  Γ γ  Δ δ  Ε ε  Ζ ζ  Η η  Θ θ  Ι ι  Κ κ  Λ λ  Μ μ 
# Ν ν  Ξ ξ  Ο ο  Π π  Ρ ρ  Σ ς  Τ τ  Υ υ  Φ φ  Χ χ  Ψ ψ  Ω ω

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
