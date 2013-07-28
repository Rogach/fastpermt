run: compile
	bash -c 'time ./fastpermt run --method maxt --tfce --no-thin-clusters --graph-file aux/graph -c 30 --ignore-label aux/ft_ignore-lh.label $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) -o aux/result.stc'

compile:
	ghc -O3 -Wall -o fastpermt -fno-warn-name-shadowing -main-is Fastpermt *.hs

compile-static: clean
	ghc -optl-static -optl-pthread -O3 -Wall -o fastpermt -fno-warn-name-shadowing -main-is Fastpermt *.hs

profile: clean
	ghc -prof -auto-all -caf-all -O3 -Wall -o fastpermt -main-is Fastpermt *.hs
	bash -c 'time ./fastpermt maxt 50 $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) +RTS -p > aux/result.stc'
	cat fastpermt.prof

send: compile-static
	install -m a+rwx fastpermt /media/meg/data/programs/platon/prj/permt/build/fastpermt

clean:
	rm -f fastpermt *.o *.hi fastpermt.prof

.PHONY: run compile send clean
