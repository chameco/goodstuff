.PHONY: all goodstuff frontend saturnal coal-snapshot install backend-repl frontend-repl

all: goodstuff saturnal coal-snapshot
goodstuff:
	stack build --copy-bins --local-bin-path=.
	mkdir -p db
frontend:
	bower install
saturnal: frontend
	mkdir -p assets/saturnal/js
	pulp build -O --build-path .pulp-work --src-path frontend --test-path frontend-test --main Saturnal.Main --to assets/saturnal/js/main.js
coal-snapshot: frontend
	mkdir -p assets/coal/snapshot/js
	pulp build -O --build-path .pulp-work --src-path frontend --test-path frontend-test --main Coal.Snapshot.Main --to assets/coal/snapshot/js/main.js
install: all
	stack build --copy-bins
backend-repl: goodstuff
	stack repl
frontend-repl: saturnal
	pulp repl --src-path frontend --test-path frontend-test
