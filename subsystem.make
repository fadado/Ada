########################################################################
# Common subsystem's rules
########################################################################

.PHONY: help build clean clobber check tests

OUTPUT_DIRECTORIES := $(OBJ_DIR) $(BIN_DIR) $(LIB_DIR)

BUILD_TIMESTAMP    := $(BIN_DIR)/zilch_$(SUBSYSTEM)

ARCHIVE_LIBRARY    := $(LIB_DIR)/lib$(SUBSYSTEM).a

help:
	echo 'Usage: make {target}'
	echo 'Targets:'
	echo '    build   - build the subsystem library'
	echo '    tests   - compile all tests'
	echo '    check   - check syntax and semantics'
	echo '    clean   - remove generated files'
	echo '    clobber - remove also generated directories'

build: $(OUTPUT_DIRECTORIES) $(ARCHIVE_LIBRARY)

# "test_" is the prefix for all executable tests

tests: $(patsubst $(TST_DIR)/%.adb, \
                  $(BIN_DIR)/%,     \
                  $(wildcard $(TST_DIR)/test_*.adb))

$(OUTPUT_DIRECTORIES):
	@test -d $@ || mkdir -p $@

$(ARCHIVE_LIBRARY): $(BUILD_TIMESTAMP)
	@ar rc $@ $(OBJ_DIR)/*.o
	@cp --force --update $(OBJ_DIR)/*.ali $(LIB_DIR)
	@chmod -w $(LIB_DIR)/*.ali

$(BUILD_TIMESTAMP): zilch_$(SUBSYSTEM).adb *.ad?
	gnatmake $(SUBSYSTEM_SW) \
		 -aO$(OBJ_DIR)   \
		 -D $(OBJ_DIR)   \
		 -o $@ $<
	@rm $(OBJ_DIR)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(BUILD_TIMESTAMP)

$(BIN_DIR)/test_%: $(TST_DIR)/test_%.adb
	gnatmake $(TESTS_SW)  \
		-aI$(SRC_DIR) \
		-aO$(LIB_DIR) \
		-D $(BIN_DIR) \
		-o $@ $<      \
		-largs -L$(LIB_DIR) -l$(SUBSYSTEM) $(EXTRA_LARGS)

check: $(OBJ_DIR) $(BIN_DIR) 
	@gnatmake $(SW_CHECK) -D $(OBJ_DIR) *.ad?
	@gnatmake $(SW_CHECK) -aI$(SRC_DIR) -D $(BIN_DIR) $(TST_DIR)/*.ad?

clean:
	@rm -f $(OBJ_DIR)/* $(BIN_DIR)/*

clobber:
	@rm -rf $(OUTPUT_DIRECTORIES)

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
