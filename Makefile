PROGS:=env.ml env_proc.ml lc_exp.ml let.ml proc.ml
BINS:=$(patsubst %.ml,%.native,$(PROGS))

.PHONY: all
all: $(BINS)
	@:

%.native: %.ml
	if ocamlbuild  $@; then ./$@ ; fi

.PHONY: clean
clean:
	rm -rvf _build $(BINS) *~

