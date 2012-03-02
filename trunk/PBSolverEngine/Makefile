FLAGS := -Mobjfpc  -O3  -g#-g -gh -gl # 

All: 
	fpc $(FLAGS) PBPASSolver.lpr

all: libMiniSatManager.so 
	fpc $(FLAGS) PBPASSolver.lpr

commit:
	svn commit -m" "
	cd My-Units && svn commit -m" " && cd ..

libMiniSatManager.so:
	cd My-Units/SAT-Solver-Units/MiniSAT/ && make
	rm -f libMiniSatManager.so && ln -s My-Units/SAT-Solver-Units/MiniSAT/libMiniSatManager.so .
	export MROOT=`pwd`/My-Units/SAT-Solver-Units/MiniSAT/MiniSat-2.2.0/ && cd My-Units/SAT-Solver-Units/MiniSAT/MiniSat-2.2.0/core && gmake rs

clean:
	rm -f *.ppu *bak *.o libMiniSatManager.so PBPASSolver
	cd Parser/ && rm -f *.ppu *.o lib*.a *.bak
	cd PBSolverEngine/ && rm -f *.ppu *.o lib*.a *.bak
	cd Specification/ &&  rm -f *.ppu *.o lib*.a *.bak
	cd My-Units/ && make clean





