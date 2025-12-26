.PHONY: all clean quick-test sandbox

EMACS ?= emacs

EFLAGS :=
EFLAGS += --no-init-file
EFLAGS += --directory $(abspath $(CURDIR))
EFLAGS += --load gpr-ts-mode.el

quick-test:
	eldev test

all: clean
	eldev -p -dtT test
	eldev compile --keep-going --set all --warnings-as-errors
	eldev -p lint
	eldev -p doctor --all-tests

clean:
	eldev clean

sandbox:
	$(EMACS) $(EFLAGS)
