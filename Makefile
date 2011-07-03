build:
	ghc -O -outputdir obj/ -o Main -isrc/ Main

clean:
	rm -rf obj/ Main

rebuild: clean build
