clean:
	rm -f General-Purpose-Units/*.ppu General-Purpose-Units/*.o SAT-Solver-Units/*.ppu 
	cd SAT-Solver-Units/  && make clean
