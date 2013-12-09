  ValidArguments: array [1..9] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--Verbosity',
     '--ExtraClausesLevel',
     '--UseRandomPermutation',
     '--ModuloMode',
     '--ForceLessThanConstraints',
     '--InputNumber',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..9] of array [1..9] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DC', 'CARD.DP', 'CARD.DC', 'CARD.SN', 'Less.Variables', 'SingleSorter.DP',
      'SingleSorter.DC', 'SingleSorter.SN'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'CNFStream', 'NONE', 'NONE', 'NONE',
        'NONE', 'NONE', 'NONE'),
//     '--ExtraClausesLevel',
     ('Off', 'Medium', 'High', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--UseRandomPermutation',
     ('0', '1', 'None', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--ModuloMode',
     ('Prime', 'MinimalPrime', 'PrimePower', 'NONE', 'NONE', 'NONE',
                 'NONE', 'NONE', 'NONE'),
//     '--ForceLessThanConstraints',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
                   'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
         'NONE', 'NONE', 'NONE'),
//     '--InputFilename',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE'),
//     '--OutputFilename'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE')
    );



