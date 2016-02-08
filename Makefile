REPO            ?= riakshell
# packagers need hyphens not underscores
APP              = $(shell echo "$(REPO)" | sed -e 's/_/-/g')
PKG_REVISION    ?= $(shell git describe --tags)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean: testclean
	./rebar clean

##
## Lock Targets
##
##  see https://github.com/seth/rebar_lock_deps_plugin
lock: deps compile
	./rebar lock-deps

locked-all: locked-deps compile

locked-deps:
	@echo "Using rebar.config.lock file to fetch dependencies"
	./rebar -C rebar.config.lock get-deps

##
## Test targets
##
TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

# Test each dependency individually in its own VM
test: deps compile testclean
	@$(foreach dep, \
            $(wildcard deps/*), \
               (cd $(dep) && ../../rebar eunit deps_dir=.. skip_deps=true)  \
               || echo "Eunit: $(notdir $(dep)) FAILED" >> $(TEST_LOG_FILE);)
	./rebar eunit skip_deps=true
	@if test -s $(TEST_LOG_FILE) ; then \
             cat $(TEST_LOG_FILE) && \
             exit `wc -l < $(TEST_LOG_FILE)`; \
        fi
