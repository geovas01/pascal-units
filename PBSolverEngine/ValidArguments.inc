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
  ValidArgumentsValues: array [1..13] of array [1..10] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DCDirect', 'DCTseitin', 'CARD.DP', 'CARD.DC', 'CARD.SN', 'Less.Variables', 'SingleSorter.DP',
      'SingleSorter.DC', 'SingleSorter.SN'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'CNFStream', 'NONE', 'NONE', 'NONE',
        'NONE', 'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('0', '1', '2', '3', '4', '5', '6', '7', 'NONE', 'NONE'),
//     '--ExtraClausesLevel',
     ('Off', 'Medium', 'High', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--UseRandomPermutation',
     ('0', '1', 'None', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--ModuloMode',
     ('Prime', 'MinimalPrime', 'PrimePower', 'NONE', 'NONE', 'NONE',
                 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--ForceLessThanForEquality',
     ('UsingClauses', 'UsingCardinality', 'Disabled', 'NONE', 'NONE', 'NONE',
                   'NONE', 'NONE', 'NONE', 'NONE'),
///     '--EncodeAsOneConstraint',
      ('Disabled', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE',
              'NONE', 'NONE', 'NONE', 'NONE'),
//     '--BreakSymmerty',
      ('Disabled', 'Level1', 'None', 'NONE', 'NONE', 'NONE',
              'NONE', 'NONE', 'NONE', 'NONE'),


//     '--Parser',
      ('NOrmalParser', 'LazyParser', 'None', 'NONE', 'NONE', 'NONE',
              'NONE', 'NONE', 'NONE', 'NONE'),

//     '--InputFilename',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE', 'NONE'),
//     '--OutputFilename'
            ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
              'NONE', 'NONE', 'NONE', 'NONE'),
//     '--Assignment'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE', 'NONE')
    );



