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
  ValidArgumentsValues: array [1..8] of array [1..9] of AnsiString=
    (
//--ModularEncoder:
     ('DP', 'DC', 'CARD.DP', 'CARD.DC', 'CARD.SN', 'Less.Variables', 'SingleSorter.DP',
      'SingleSorter.DC', 'SingleSorter.SN'),
//     '--SATSolverType',
     ('CNFCollection', 'InternalMiniSAT', 'CNFStream', 'NONE', 'NONE', 'NONE',
        'NONE', 'NONE', 'NONE'),
//     '--FactorizerMode',
     ('BinaryRep', 'Modulo.Prime.Unary', 'None', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', 'NONE'),
//     '--UseRandomPermutation',
     ('Prime', 'MinimalPrime', 'PrimePower', 'NONE', 'NONE', 'NONE',
                 'NONE', 'NONE', 'NONE'),
//     '--ModuloMode',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
                   'NONE', 'NONE', 'NONE'),
//     '--Verbosity',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
         'NONE', 'NONE', 'NONE'),
//     '--InputNumber',
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE'),
//     '--OutputFilename'
     ('NONE', 'NONE', 'None', 'NONE', 'NONE', 'NONE',
       'NONE', 'NONE', 'NONE')
    );



