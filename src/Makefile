MMC = mmc
PARALLEL =
files = $(wildcard *.m)

../pushpull: pushpull
	@ln -L -f pushpull ../pushpull

pushpull: $(files) Mercury.options Mercury.params
	@$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.params:

tags: $(files)
	@mtags $(files)
