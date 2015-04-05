MMC = mmc
PARALLEL =
files = $(wildcard *.m)

../plugsink: plugsink
	@ln -L -f plugsink ../plugsink

plugsink: $(files) Mercury.options Mercury.params
	@$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.params:

tags: $(files)
	@mtags $(files)
