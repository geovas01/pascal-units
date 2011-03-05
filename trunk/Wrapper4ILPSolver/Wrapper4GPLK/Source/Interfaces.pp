unit Interfaces;

{$link GLPKFunctions.o}
{$linklib c}
{$linklib glpk}

interface
uses 
  CTypes;
type TGLPKProblemPointer= type Pointer;

function GLPKCreateMaximizationProblem (Name: PCChar): TGLPKProblemPointer; cdecl; external;
function GLPKCreateMinimizationProblem (Name: PCChar): TGLPKProblemPointer; cdecl; external;

procedure GLPKAddNewContinuesVariable (Name: PCChar; BoundStatus: cInt; LowerHandle: cDouble; UpperBound: cDouble; CoefInObjFunction: cDouble; Handle: TGLPKProblemPointer); cdecl; external;
procedure GLPKAddNewIntegerVariable (Name: PCChar; BoundStatus: cInt; LowerHandle: cDouble; UpperBound: cDouble; CoefInObjFunction: cDouble; Handle: TGLPKProblemPointer); cdecl; external;

procedure GLPKAddNewConstraint (Name: PCChar; BoundStatus: cInt; LowerHandle: cDouble; UpperBound: cDouble; Handle: TGLPKProblemPointer); cdecl; external;

procedure GLPKLoadMatrix (IA, IB, IC: PCInt32; ne: cInt; Handle: TGLPKProblemPointer); cdecl; external;

procedure GLPKSimplex (Handle: TGLPKProblemPointer); cdecl; external;
procedure GLPKIntOptimization (Handle: TGLPKProblemPointer); cdecl; external;

function GLPKGetLPVariablePrimeValue (Handle: TGLPKProblemPointer; VarIndex: cInt): cDouble; cdecl; external;
function GLPKGetMIPVariablePrimeValue (Handle: TGLPKProblemPointer; VarIndex: cInt): cDouble; cdecl; external;

function GLPKGetLPObjectiveFunctionValue (Handle: TGLPKProblemPointer): cDouble; cdecl; external;



implementation

end.
