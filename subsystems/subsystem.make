########################################################################
# Common subsystem's rules
########################################################################

.PHONY: help build clean clobber tests silent

OUTPUT_DIRECTORIES := $(OBJ_DIR) $(BIN_DIR) $(LIB_DIR)

BUILD_TIMESTAMP    := $(BIN_DIR)/zilch_$(SUBSYSTEM)

ARCHIVE_LIBRARY    := $(LIB_DIR)/lib$(SUBSYSTEM).a

help:
	echo 'Usage: make {target}'
	echo 'Targets:'
	echo '    build   - build the subsystem library'
	echo '    clean   - remove generated files'
	echo '    clobber - remove also generated directories'
	echo '    help    - show this message'
	echo '    tests   - compile all tests'
	echo '    silent  - run silent tests if available'

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

$(BUILD_TIMESTAMP): zilch_$(SUBSYSTEM).adb $(SUBSYSTEM)*.ad?
	@gnatmake $(SUBSYSTEM_SW) \
		 -aO$(OBJ_DIR)   \
		 $(EXTRA_SUBSYS) \
		 -D $(OBJ_DIR)   \
		 -o $@ $<        \
		 -largs $(EXTRA_LARGS)
	@rm $(OBJ_DIR)/zilch_$(SUBSYSTEM).{o,ali}
	@chmod -x $(BUILD_TIMESTAMP)

$(BIN_DIR)/test_%: $(TST_DIR)/test_%.adb $(ARCHIVE_LIBRARY) 
	@gnatmake $(TESTS_SW)     \
		-aI$(SRC_DIR)    \
		-aO$(LIB_DIR)    \
		 $(EXTRA_SUBSYS) \
		-D $(BIN_DIR)    \
		-o $@ $<         \
		-largs -L$(LIB_DIR) -l$(SUBSYSTEM) $(EXTRA_LARGS)

$(BIN_DIR)/test_silent: tests/silent_*.adb

clean:
	@rm -f $(OBJ_DIR)/* $(BIN_DIR)/*

clobber:
	@rm -rf $(OUTPUT_DIRECTORIES)

silent: $(BIN_DIR)/test_silent; $<

# vim:fileformat=unix:fileencoding=UTF8:syntax=make
