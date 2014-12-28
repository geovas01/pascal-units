all:
	cd MiniSAT/ && make
	export MROOT=`pwd`/MiniSAT/MiniSat-2.2.0/ && cd MiniSAT/MiniSat-2.2.0/core && gmake rs

clean:
	rm -f *.o *.{ppu,bak} General/*.{o,ppu,bak} RandomWalk-SATSolver/*.{o,ppu,bak} RWalk-SATSolver/*.{o,ppu,bak} Circuits/*.{o,ppu,bak} Circuits/*/*.{o,ppu,bak}
	cd MiniSAT/  && make clean
