  ValidArguments: array [1..10] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--Verbosity',
     '--ExtraClausesLevel',
     '--UseRandomPermutation',
     '--ModuloMode',
     '--ForceLessThanConstraints',
     '--Parser',
     '--InputFilename',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..10] of array [1..9] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DC', 'CARD.DP', 'CARD.DC', 'CARD.SN', 'Less.Variables', 'SingleSorter.DP',
      'SingleSorter.DC', 'SingleSorter.SN'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'CNFStream', 'NONE', 'NONE', 'NONE',
        'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('0', '1', '2', '3', '4', '5', '6', '7', 'NONE'),
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
//     '--Parser',
      ('NOrmalParser', 'LazyParser', 'None', 'NONE', 'NONE', 'NONE',
              'NONE', 'NONE', 'NONE'),

//     '--InputFilename',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE'),
//     '--OutputFilename'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE')
    );



