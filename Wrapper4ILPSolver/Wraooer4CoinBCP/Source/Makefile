UseWrapper: Wrapper4CoinBcp.o CoinBcpInterfaces.o CoinBcpFunctions.o UseWrapper.pp 
	fpc UseWrapper.pp 
Wrapper4CoinBcp.o: CoinBcpInterfaces.o Wrapper4CoinBcp.pp
	fpc Wrapper4CoinBcp.pp
CoinBcpInterfaces.o: CoinBcpFunctions.o
	fpc CoinBcpInterfaces.pp
CoinBcpFunctions.o:  CoinBcpFunctions.h CoinBcpFunctions.c
	gcc -o CoinBcpFunctions.o -c CoinBcpFunctions.c 
clean:
	rm *o UseWrapper *ppu -f
