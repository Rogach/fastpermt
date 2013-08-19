GHC_OPTS := -O3 -Wall -o fastpermt -hidir target/ -odir target/ -fno-warn-name-shadowing -main-is Fastpermt

run: compile
	bash -c '/usr/bin/time -f "\nelapsed: %es\nrss: %Mk" ./fastpermt run --method maxt --graph-file aux/graph -c 100 --ignore-label aux/ft_ignore-lh.label $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) -o aux/result.stc'

compile: target target/ttest.o
	ghc ${GHC_OPTS} *.hs target/ttest.o

compile-static: clean target target/ttest.o
	ghc ${GHC_OPTS} -optl-static -optl-pthread *.hs target/ttest.o

send: compile-static
	install -m a+rwx fastpermt /media/meg/data/programs/platon/prj/permt/build/fastpermt

clean:
	rm -fr target/* fastpermt fastpermt.prof

target/ttest.o: ttest.c
	gcc -O3 -lm -std=gnu99 -c ttest.c -o target/ttest.o

target:
	mkdir -p target

.PHONY: run compile compile-static send clean
