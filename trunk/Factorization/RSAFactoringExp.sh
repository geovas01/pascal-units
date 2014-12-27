# for m in {2..10}; 
for n in {2..100}; 
do   
#    n=`expr $m \* $m \* 1000 + 1`
    #echo $n; 
    #./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.PrimeBin.cnf --FactorizerMode Modulo.Prime.Binary --Verbosity 15 > $n.PrimeBin.out;   
    #./minisat_static $n.PrimeBin.cnf $n.PrimeBin.ans >$n.sat.PrimeBin.out 2>$n.sat.PrimeBin.err ; grep UNSAT $n.sat.PrimeBin.out>/dev/null; PrimeBinRes=$?

    ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.cnf --FactorizerMode BinaryRep --Verbosity 15 > $n.BinRep.out;   
    ./minisat_static $n.BinRep.cnf $n.BinRep.ans >$n.sat.BinRep.out 2>$n.sat.BinRep.err ; grep UNSAT $n.sat.BinRep.out >/dev/null; BinRes=$?
    if [[ $BinRes -eq 0 ]];
    then
      #echo $PrimeBinRes, $BinRes
      #echo $n
      echo $n, $BinRes
    fi
done
