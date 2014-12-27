  ValidArguments: array [1..8] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--FactorizerMode',
     '--ExtraClausesLevel',
     '--Verbosity',
     '--ModuloMode',
     '--InputNumber',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..8] of AnsiString=
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
     '0:1:2:3:4:5:6:7:8:9:10:11:12:13:14:15:END',
//     '--ModuloMode',
     'Prime:PrimePower:LogN:END',
    //     '--InputNumber',
     'None:END',
//     '--OutputFilename'
     'None:END'
    );



