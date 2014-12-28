# for m in {2..10}; 
for n in {999997..1000000}; 
do   
#    n=`expr $m \* $m \* 1000 + 1`
    #echo $n; 
    #./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.PrimeBin.cnf --FactorizerMode Modulo.Prime.Binary --Verbosity 15 > $n.PrimeBin.out;   
    #./minisat_static $n.PrimeBin.cnf $n.PrimeBin.ans >$n.sat.PrimeBin.out 2>$n.sat.PrimeBin.err ; grep UNSAT $n.sat.PrimeBin.out>/dev/null; PrimeBinRes=$?

    ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.noaLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb False > $n.BinRep.noaLEb.out;   
    ./minisat_static $n.BinRep.noaLEb.cnf $n.BinRep.noaLEb.ans >$n.sat.BinRep.noaLEb.out 2>$n.sat.BinRep.noaLEb.err ; grep UNSAT $n.sat.BinRep.noaLEb.out >/dev/null; noaLEb_BinRes=$?
    ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb True > $n.BinRep.aLEb.out;   
    ./minisat_static $n.BinRep.aLEb.cnf $n.BinRep.aLEb.ans >$n.sat.BinRep.aLEb.out 2>$n.sat.BinRep.aLEb.err ; grep UNSAT $n.sat.BinRep.aLEb.out >/dev/null; aLEb_BinRes=$?
    if [[ $noaLEb_BinRes -eq 0 ]];
    then
      #echo $PrimeBinRes, $BinRes
      #echo $n
      echo $n, $noaLEb_BinRes, $aLEb_BinRes

    fi
done
