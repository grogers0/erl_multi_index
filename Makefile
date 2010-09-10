ERL = erl
LIBDIR = $(shell $(ERL) -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION = 1.0

SRCS = ["src/multi_index.erl"]
TEST_SRCS = $(SRCS) ++ ["test/ordered_eqc.erl", "test/multi_index_eqc.erl"]
TEST_MODULES = [ordered_eqc, multi_index_eqc]

all: compile
docs: edoc

compile: ebin
	@$(ERL) -noinput +B -eval 'case make:files($(SRCS), [{outdir, "ebin"}]) of up_to_date -> halt(0); error -> halt(1) end.'

edoc:		
	@echo "Generating documentation from srcs"
	@$(ERL) -noinput +B -eval 'edoc:files($(SRCS)).' -s erlang halt	

ebin:
	@mkdir ebin

install: all
	@install -v -d $(LIBDIR)/multi_index-$(VERSION)/ebin
	@install -v ebin/*.beam $(LIBDIR)/multi_index-$(VERSION)/ebin

uninstall:
	rm -rf $(LIBDIR)/multi_index-$(VERSION)

clean:
	rm -rf ebin/*.beam edoc-info *.html erlang.png *.css current_counterexample.eqc

test: test_ebin
	@$(ERL) -noinput +B -eval 'case code:ensure_loaded(eqc) of {module, _} -> ok; {error, _} -> io:format("You must install erlang quickcheck to run these tests. See http://www.quviq.com/downloads~n", []), halt(1) end, case make:files($(TEST_SRCS), [export_all, {outdir, "test/ebin"}]) of up_to_date -> ok; error -> halt(1) end, code:add_patha("test/ebin"), lists:foreach(fun code:purge/1, $(TEST_MODULES)), lists:foreach(fun code:load_file/1, $(TEST_MODULES)), lists:foreach(fun(M) -> case eqc:module(M) of [] -> ok; Failing -> io:format("error: ~s: ~p~n", [M, Failing]), halt(1) end end, $(TEST_MODULES)), halt(0).'

test_ebin:
	@mkdir -p test/ebin
