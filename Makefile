ERL = erl
LIBDIR = $(shell $(ERL) -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION = 1.0

SRCS = ["src/multi_index.erl"]

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
	rm -rf ebin/*.beam edoc-info *.html erlang.png *.css
