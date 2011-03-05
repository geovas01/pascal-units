UseWrapper: Wrapper4GLPK.o Interfaces.o GLPKFunctions.o UseWrapper.pp 
	fpc UseWrapper.pp 
Wrapper4GLPK.o: Interfaces.o Wrapper4GLPK.pp
	fpc Wrapper4GLPK.pp
Interfaces.o: GLPKFunctions.o
	fpc Interfaces.pp
GLPKFunctions.o:  GLPKFunctions.h GLPKFunctions.c
	gcc -o GLPKFunctions.o -c GLPKFunctions.c -lglpk
clean:
	rm *o UseWrapper *ppu -f
