.PHONY: all goodstuff saturnal install

all: goodstuff saturnal
goodstuff:
	stack build --copy-bins --local-bin-path=.
saturnal:
	mkdir -p assets/saturnal/js
	(cd saturnal; bower install; pulp build -O --to ../assets/js/saturnal.js)
test: all
	PORT=3000 stack exec goodstuff
install: all
	stack build --copy-bins
