for n in {1000..1000}; 
 do
    ./FactorUsingSAT --InputSize $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryRep --Verbosity 0 --AddaLEb True --AddExtraClausesForEq False >  /dev/null 
     echo $n
    ./FactorUsingSAT --InputSize $n --SatSolverType CNFCollection --OutputFileName $n.ModuloRep.aLEb.cnf --FactorizerMode ModuloRep --Verbosity 0 --AddaLEb True  --ModuloMode Normal --AddExtraClausesForEq False  > /dev/null
  exit
done 
for n in {10..20}; 
 do
     echo $n
    ./FactorUsingSAT --InputSize $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryRep --Verbosity 0 --AddaLEb True --AddExtraClausesForEq False >  /dev/null 
     echo $n
    ./FactorUsingSAT --InputSize $n --SatSolverType CNFCollection --OutputFileName $n.ModuloRep.aLEb.cnf --FactorizerMode ModuloRep --Verbosity 0 --AddaLEb True  --ModuloMode Normal --AddExtraClausesForEq False  > /dev/null
done
exit;
#n=104395303   
n=5915587277
while [[ true ]];
do   
#    n=`expr $m \* $m \* 1000 + 1`
    #echo $n; 
    #./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.PrimeBin.cnf --FactorizerMode Modulo.Prime.Binary --Verbosity 15 > $n.PrimeBin.out;   
    #./minisat_static $n.PrimeBin.cnf $n.PrimeBin.ans >$n.sat.PrimeBin.out 2>$n.sat.PrimeBin.err ; grep UNSAT $n.sat.PrimeBin.out>/dev/null; PrimeBinRes=$?

  #  ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.noaLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb False > $n.BinRep.noaLEb.out;   
  #  ./minisat_static $n.BinRep.noaLEb.cnf $n.BinRep.noaLEb.ans >$n.BinRep.sat.noaLEb.out 2>$n.BinRep.sat.noaLEb.err ; grep UNSAT $n.BinRep.sat.noaLEb.out >/dev/null; noaLEb_BinRes=$?
    ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb True --AddExtraClausesForEq False > $n.BinRep.aLEb.out;   
    ./minisat_static $n.BinRep.aLEb.cnf $n.BinRep.aLEb.ans >$n.BinRep.sat.aLEb.out 2>$n.BinRep.sat.aLEb.err ; grep UNSAT $n.BinRep.sat.aLEb.out >/dev/null; aLEb_BinRes=$?

    ./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryMod --Verbosity 15 --AddaLEb True --AddExtraClausesForEq False > $n.BinRep.aLEb.out;   
    ./minisat_static $n.BinRep.aLEb.cnf $n.BinRep.aLEb.ans >$n.BinRep.sat.aLEb.out 2>$n.BinRep.sat.aLEb.err ; grep UNSAT $n.BinRep.sat.aLEb.out >/dev/null; aLEb_BinRes=$?


    if [[ $aLEb_BinRes_Addeq -eq 0 ]];
    then
      #echo $PrimeBinRes, $BinRes
      #echo $n
      echo $n, $aLEb_BinRes, $aLEb_BinRes_Addeq
      mv $n.BinRep.* /tmp/
      rm -f $n.BinRep.*
    else
      rm -f $n.BinRep.*
    fi
    n=`expr $n + 1`
    break
    if [[ n -eq 10 ]];
    then
        break
    fi
done
