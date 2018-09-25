.PHONY: all goodstuff saturnal install

all: server saturnal
goodstuff:
	stack build
saturnal:
	(cd saturnal; bower install; pulp build -O --to ../assets/js/saturnal.js)
test: all
	PORT=3000 stack exec goodstuff
install: all
	stack build --copy-bins
