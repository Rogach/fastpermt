run: compile
	bash -c 'time ./fastpermt maxt 50 $$(ls data/ | cut -f1,2 -d_ | uniq | xargs -I{} echo data/{}_45_control-lh.stc data/{}_45_kanizsa-lh.stc) > aux/result'

compile:
	# -optl-static -optl-pthread 2>&1 | grep -vE "(addDLL|dlopen)"
	ghc -O3 -Wall -o fastpermt -main-is Fastpermt *.hs

send: clean compile
	scp -oPort=2222 -oGSSAPIAuthentication=no fastpermt meg.mutokukai.ru:/data/programs/platon/prj/permt/target/tor/
clean:
	rm -f fastpermt *.o *.hi
