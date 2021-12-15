.PHONY: run
run:
	gosh csv2tsv.scm

.PHONY: test
test:
	gosh test/csv2tsv.scm
