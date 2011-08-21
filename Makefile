HC_FLAGS = -O -fexcess-precision
HC_PROF_FLAGS = -rtsopts -prof -auto-all
OBJ_DIR = obj/
SRC_DIR = src/
OUT = mylisp

build:
	ghc ${HC_FLAGS} -outputdir ${OBJ_DIR} -i${SRC_DIR} Main -o ${OUT}

profile: clean
	ghc ${HC_FLAGS} ${HC_PROF_FLAGS} -outputdir ${OBJ_DIR} -i${SRC_DIR} \
		Main -o ${OUT}
	
clean:
	rm -rf ${OBJ_DIR} ${OUT}

rebuild: clean build
