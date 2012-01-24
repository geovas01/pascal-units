all: LibMiniSatInterface
CFLAGS :=  -O3
#CFLAGS    += -I$(MROOT) -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS
CXXFLAGS    := $(CFLAGS)

MiniSatInterface: Solver.or Interface.cc cInterface_c.or cInterface_cc.or
	gcc $(CFLAGS) -IMiniSat-2.2.0/ -c Interface.cc -fPIC -o Interface.or

cInterface_c.or: cInterface.c
	gcc $(CFLAGS) -fPIC -IMiniSat-2.2.0/  -c cInterface.c -o cInterface_c.o

cInterface_cc.or: cInterface.cc
	g++ $(CFLAGS) -fPIC -IMiniSat-2.2.0/ -c cInterface.cc -o cInterface_cc.o

Solver.or:
#	cd MiniSat-2.2.0 && cd core && make r 
	cd MiniSat-2.2.0 && export MROOT=`pwd` && cd core && make rs
	cp MiniSat-2.2.0/core/Solver.or .

LibMiniSatInterface: MiniSatInterface
	gcc $(CFLAGS) -fPIC -shared -o libMiniSatManager.so Interface.or Solver.or cInterface_c.o cInterface_cc.o && cd ../../ && rm -f libMiniSatManager.so && ln -s SatSolver/MiniSat/libMiniSatManager.so .
#	gcc -g -shared -o libMiniSatManager.so Interface.o Solver.o cInterface_cc.o && cd ../../../ && rm -f libMiniSatManager.so && ln -s SatSolver/MiniSat/Core/libMiniSatManager.so .

clean:
	rm -f *.{o,bak} a.out *.or *.ppu *.o
	cd MiniSat-2.2.0 && export MROOT=`pwd` && cd core && make clean 
