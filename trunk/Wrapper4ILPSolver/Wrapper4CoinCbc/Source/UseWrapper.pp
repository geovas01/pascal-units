program UseWrapper;
uses
  Wrapper4GLPK;

var
  AnInstance: TGLPK;
  NewConstraint: TConstraint;
  ObjectiveFunction: TLinearFunction;

begin
  WriteLn ('<Main>');
  Flush (Output);

  AnInstance:= TGLPK.CreateMaximizationProblem ('L');
  AnInstance.AddNewIntegerVariable ('x1');
  AnInstance.AddNewIntegerVariable ('x2');
  AnInstance.AddNewIntegerVariable ('x3');

  ObjectiveFunction:= AnInstance.ObjectiveFunction;
  ObjectiveFunction.AddComponent ('x1', 3);

  AnInstance.VariableByName ['x1'].SetUpperBound (12);
  AnInstance.VariableByName ['x1'].SetLowerBound (10);
  AnInstance.VariableByName ['x2'].SetLowerBound (13);

  NewConstraint:= AnInstance.InitializeConstraint;
  NewConstraint.AddComponent ('x1', 2);
  NewConstraint.AddComponent ('x3', 3);
  NewConstraint.SetLowerBound (5);
  NewConstraint.SetUpperBound (5);
  
  AnInstance.SolveByGLPK;
  Flush (Output);
  WriteLn (AnInstance.ToXML);
  AnInstance.Free;

  AnInstance:= TGLPK.CreateMaximizationProblem ('L1');
  AnInstance.AddNewIntegerVariable ('a');
  AnInstance.AddNewIntegerVariable ('b');

  ObjectiveFunction:= AnInstance.ObjectiveFunction;
  ObjectiveFunction.AddComponent ('a', 2);
  ObjectiveFunction.AddComponent ('b', 3);

  AnInstance.VariableByName ['a'].SetUpperBound (5);
  AnInstance.VariableByName ['a'].SetLowerBound (0);
  AnInstance.VariableByName ['b'].SetUpperBound (5);
  AnInstance.VariableByName ['b'].SetLowerBound (0);

  NewConstraint:= AnInstance.InitializeConstraint;
  NewConstraint.AddComponent ('a', 1);
  NewConstraint.AddComponent ('b', 1);
  NewConstraint.SetLowerBound (8);
  NewConstraint.SetUpperBound (8);

  AnInstance.SolveByGLPK;
  Flush (Output);

  WriteLn (AnInstance.ToXML);
  AnInstance.Free;

  WriteLn ('</Main>');
end.
