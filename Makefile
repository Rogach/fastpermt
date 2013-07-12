run: clean compile
	bash -c "cd data/; time ../fastpermt"

compile:
	# -optl-static -optl-pthread 2>&1 | grep -vE "(addDLL|dlopen)"
	ghc -O3 -Wall -o fastpermt -main-is Fastpermt *.hs

send: clean compile
	scp -oPort=2222 -oGSSAPIAuthentication=no fastpermt meg.mutokukai.ru:/data/programs/platon/prj/permt/target/tor/
clean:
	rm -f fastpermt *.o *.hi
