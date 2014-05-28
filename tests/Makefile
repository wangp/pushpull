MMC = mmc
PARALLEL =
DIFF = diff -u

files = $(wildcard *.m ../src/*.m)

TESTS = test_crc16

.PHONY: default
default: $(addsuffix .runtest,$(TESTS))

$(TESTS): $(files) Mercury.modules
	$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.modules: $(files)
	@$(MMC) -f $(files)

.PHONY: %.runtest
%.runtest: %
	./$(<) > $(<).out && \
		$(DIFF) $(<).exp $(<).out
