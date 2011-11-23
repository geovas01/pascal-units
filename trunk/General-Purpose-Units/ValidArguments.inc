  ValidArguments: array [1..7] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--Verbosity',
     '--ExtraClausesLevel',
     '--UseRandomPermutation',
     '--InputFilename',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..7] of array [1..6] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DC', 'CARD.DP', 'CARD.DC', 'CARD.SN', 'Less.Variables'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('0', '1', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--ExtraClausesLevel',
     ('Off', 'Medium', 'High', 'NONE', 'NONE', 'NONE'),
//     '--UseRandomPermutation',
     ('0', '1', 'None', 'NONE', 'NONE', 'NONE'),
//     '--InputFilename',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE'),
//     '--OutputFilename'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE')
    );



