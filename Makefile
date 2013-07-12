run: compile
	bash -c 'time ./fastpermt maxt 50 $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) > aux/result'

compile:
	ghc -O3 -Wall -o fastpermt -main-is Fastpermt *.hs

compile-static:
	ghc -optl-static -optl-pthread -O3 -Wall -o fastpermt -main-is Fastpermt *.hs

profile: clean
	ghc -prof -auto-all -caf-all -O3 -Wall -o fastpermt -main-is Fastpermt *.hs
	bash -c 'time ./fastpermt maxt 50 $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) +RTS -p > aux/result.stc'
	cat fastpermt.prof

send: clean compile
	scp -oPort=2222 -oGSSAPIAuthentication=no fastpermt meg.mutokukai.ru:/data/programs/platon/prj/permt/target/tor/

clean:
	rm -f fastpermt *.o *.hi fastpermt.prof

.PHONY: run compile send clean
