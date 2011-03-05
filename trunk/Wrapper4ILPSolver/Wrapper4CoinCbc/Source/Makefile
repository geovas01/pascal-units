UseWrapper: Wrapper4CoinCbc.o CoinCbcInterfaces.o CoinCbcFunctions.o UseWrapper.pp 
	fpc UseWrapper.pp 
Wrapper4GLPK.o: CoinCbcInterfaces.o Wrapper4CoinCbc.pp
	fpc Wrapper4CoinCbc.pp
Interfaces.o: CoinCbcFunctions.o
	fpc CoinCbcInterfaces.pp
GLPKFunctions.o:  CoinCbcFunctions.h CoinCbcFunctions.c
	gcc -o CoinCbcFunctions.o -c CoinCbcFunctions.c -lglpk
clean:
	rm *o UseWrapper *ppu -f
