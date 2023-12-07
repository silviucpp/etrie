REBAR=rebar3

NCPUS:=1
OS:=$(shell uname -s)

ifeq ($(OS),Linux)
	NCPUS:=$(shell grep -c ^processor /proc/cpuinfo)
endif
ifeq ($(OS),Darwin) # Assume Mac OS X
	NCPUS:=$(shell sysctl -n hw.ncpu)
endif

VM_ARGS=-env ERL_FULLSWEEP_AFTER 10 -mode interactive -noinput -noshell +K true +A 10 -IOt $(NCPUS) -IOp $(NCPUS)
BENCH_PROFILE_ARGS=-pa _build/bench/lib/etrie/benchmarks -pa _build/bench/lib/*/ebin $(VM_ARGS)

get_deps:
	@./build_deps.sh

compile_nif: get_deps
	@make V=0 -C c_src -j 8

clean_nif:
	@make -C c_src clean

compile:
	${REBAR} compile

clean:
	${REBAR} clean

benchmark:
	${REBAR} as bench compile
	erl $(BENCH_PROFILE_ARGS) -eval "benchmark:run(<<\"$(DATASET)\">>, $(MODULE), $(LIMIT))" -eval "init:stop()."
