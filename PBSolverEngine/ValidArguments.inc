  ValidArguments: array [1..13] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--Verbosity',
     '--ExtraClausesLevel',
     '--UseRandomPermutation',
     '--ModuloMode',
     '--ForceLessThanForEquality',
     '--EncodeAsOneConstraint',
     '--BreakSymmetry',
     '--Parser',
     '--InputFilename',
     '--OutputFilename',
     '--Assignment'
    );
  ValidArgumentsValues: array [1..13] of AnsiString=
    (
//--ModularEncoder:
     'DP:DCDirect:DCTseitin:CARD.DP:CARD.DC:CARD.SN:Less.Variables:'
     + 'SingleSorter.DP:SingleSorter.DC:SingleSorter.SN:BIN:END',
//     '--SATSolverType',
     'CNFCollection:InternalMiniSAT:CNFStream:END',
//     '--Verbosity',
     '0:1:2:3:4:5:6:7:END',
//     '--ExtraClausesLevel',
     'Off:Medium:High:END',
//     '--UseRandomPermutation',
     '0:1:END',
//     '--ModuloMode',
     'Prime:MinimalPrime:PrimePower:LogS:END',
//     '--ForceLessThanForEquality',
     'UsingClauses:UsingCardinality:Disabled:END',
///     '--EncodeAsOneConstraint',
      'ENabled:Disabled:END',
//     '--BreakSymmerty',
      'Disabled:Level1:END',
//     '--Parser',
      'NOrmalParser:LazyParser:END',
//     '--InputFilename',
     'None:END',
//     '--OutputFilename'
     'None:END',
//     '--Assignment'
     'None:END'
);



