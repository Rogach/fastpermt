GHC_OPTS := -O3 -Wall -o fastpermt -hidir target/ -odir target/ -fno-warn-name-shadowing -main-is Fastpermt

run: compile
	bash -c 'time ./fastpermt run --tmin 40 --tmax 120 --method maxt --tfce --no-thin-clusters --graph-file aux/graph -c 300 --ignore-label aux/ft_ignore-lh.label $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_90_control-lh.stc data/{}_90_kanizsa-lh.stc) -o aux/result.stc'

compile: target
	ghc ${GHC_OPTS} *.hs

compile-static: clean target
	ghc ${GHC_OPTS} -optl-static -optl-pthread *.hs

send: compile-static
	install -m a+rwx fastpermt /media/meg/data/programs/platon/prj/permt/build/fastpermt

clean:
	rm -fr target/* fastpermt fastpermt.prof

target:
	mkdir -p target

.PHONY: run compile compile-static send clean
