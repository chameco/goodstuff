.PHONY: all goodstuff saturnal install backend-repl frontend-repl

all: goodstuff saturnal
goodstuff:
	stack build --copy-bins --local-bin-path=.
saturnal:
	mkdir -p assets/saturnal/js
	bower install
	pulp build -O --build-path .pulp-work --src-path frontend --test-path frontend-test --to assets/saturnal/js/main.js
install: all
	stack build --copy-bins
backend-repl: goodstuff
	stack repl
frontend-repl: saturnal
	pulp repl --src-path frontend --test-path frontend-test
