clean:
	rm -f General-Purpose-Units/*.ppu General-Purpose-Units/*.o SAT-Solver-Units/*.ppu  General-Purpose-Units/*.bak SAT-Solver-Units/*.bak 
	cd SAT-Solver-Units/  && make clean
	cd PBSolverEngine && make clean
