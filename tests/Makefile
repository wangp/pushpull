MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)
DIFF = diff -u

files = $(wildcard *.m ../src/*.m)

TESTS = test_crc8 test_crc16 test_imap_time test_openssl test_oauth2

.PHONY: default
default: $(addsuffix .runtest,$(TESTS))

$(TESTS): $(files) Mercury.modules
	$(MMC) --make $(PARALLEL) $@ && touch $@

Mercury.modules: $(files)
	@$(MMC) -f $(files)

.PHONY: %.runtest
%.runtest: %
	TZ='UTC-10' ./$(<) > $(<).out && \
		$(DIFF) $(<).exp $(<).out
