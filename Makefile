PROGS:=env.ml env_proc.ml
BINS:=$(patsubst %.ml,%.native,$(PROGS))

.PHONY: all
all: $(BINS)
	@:

%.native: %.ml
	ocamlbuild -classic-display $@

.PHONY: clean
clean:
	rm -rvf _build $(BINS) *~

