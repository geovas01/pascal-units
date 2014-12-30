FLAGS := -Mobjfpc  -g -O3# -g -gh -gl # 

All: 
	fpc $(FLAGS) FactorUsingSAT.pp

all: libMiniSatManager.so 
	fpc $(FLAGS) FactorUsingSAT.pp

libMiniSatManager.so:
	cd ../SAT-Solver-Units/MiniSAT/ && make
	rm -f libMiniSatManager.so && ln -s ../SAT-Solver-Units/MiniSAT/libMiniSatManager.so .
	export MROOT="`pwd`/MiniSAT/MiniSat-2.2.0/" && cd ../SAT-Solver-Units/MiniSAT/MiniSat-2.2.0/core && gmake rs

clean:
	rm -Rf *.ppu *bak *.o libMiniSatManager.so FactorUsingSAT lib/ 
	cd ../SAT-Solver-Units/MiniSAT/ && make clean

