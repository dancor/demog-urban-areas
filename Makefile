everything: all nation-by-count un1 output/un-nations-with-uas output/by-nation/Zimbabwe by-regions


clean:
	rm -f output/all output/nation-by-count output/un1 output/un-nations-without-uas output/un-nations-with-uas output/by-nation/* output/by-region/*

all: demog-urban-areas
	./demog-urban-areas all

nation-by-count: demog-urban-areas
	./demog-urban-areas nation-by-count

un1: demog-urban-areas
	./demog-urban-areas un1

output/un-nations-with-uas: src/UnNationsWithoutUAs.hs
	runghc src/UnNationsWithoutUAs.hs

output/by-nation/Zimbabwe: demog-urban-areas output/un-nations-with-uas
	cat output/un-nations-with-uas | xargs -n1 ./demog-urban-areas

by-regions: demog-urban-areas output/by-nation/Zimbabwe
	(cd output/by-nation; grep -l , *) | xargs -n1 -i{} ./demog-urban-areas R:{}

demog-urban-areas: src/Main.hs src/SigDigs.hs
	cabal configure && cabal build
