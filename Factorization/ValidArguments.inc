  ValidArguments: array [1..10] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--FactorizerMode',
     '--ExtraClausesLevel',
     '--Verbosity',
     '--ModuloMode',
     '--AddaLeb',
     '--AddExtraClausesForEq',
     '--InputNumber',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..10] of AnsiString=
    (
//--ModularEncoder:
   'DP:DCDirect:DCTseitin:CARD.DP:CARD.DC:CARD.SN:Less.Variables:'
     + 'SingleSorter.DP:SingleSorter.DC:SingleSorter.SN:BIN:END',
//     '--SATSolverType',
     'CNFCollection:InternalMiniSAT:CNFStream:END',
//     '--FactorizerMode',
     'BinaryRep:ModuloRep:END',
//     '--ExtraClausesLevel',
     'None:END',
//    Verbosity
     'None:END',
//     '--ModuloMode',
     'normal:UsingDP:END',
//     '--AddaLeb'
     'True:False:END',
//     '--AddExtraClausesForEq',
     'True:False:END',
    //     '--InputNumber',
     'None:END',
//     '--OutputFilename'
     'None:END'
    );



