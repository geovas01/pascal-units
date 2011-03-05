unit Wrapper4GLPK;
{$Mode objfpc}
interface
uses
  Classes, CTypes, SysUtils;

type
  TBoundStatus= (bsFree= 0, bsLB, bsUB, bsDB, bsFixed);//Unbounded , Bounded with a lower bound, Bounded with an upper bound, double-bounded and fixed .
  TVariableType= (vtContinues, vtInteger);

  TBaseVariableInfo= class (TObject)
  private
    FName: String;
    FBoundStatus: TBoundStatus;
    FUB, FLB: Integer;
    FDoubleValue: Double;
    FVariableType: TVariableType;

    function GetHasUpperBound: Boolean;
    function GetHasLowerBound: Boolean;

    function GetBoundStatus: TBoundStatus;

  protected
    property BoundStatus: TBoundStatus read GetBoundStatus;
  
    procedure SetValue (AValue: Double); 
    function GetValue: Double; 

  public
    property Name: String read FName;
    property HasLowerBound: Boolean read GetHasLowerBound;
    property HasUpperBound: Boolean read GetHasUpperBound;
    property LowerBound: Integer read FLB;
    property UpperBound: Integer read FUB;
    property Value: Double read GetValue;
    property VariableType: TVariableType read FVariableType;

    constructor Create (VarType: TVariableType; const VarName: String);

    procedure SetUpperBound (AValue: Integer);
    procedure SetLowerBound (AValue: Integer);
    procedure ReleaseUpperBound;
    procedure ReleaseLowerBound; 

    function ToXML: AnsiString; virtual;

  end;

  TContinuousVariableInfo= class (TBaseVariableInfo)
  protected

  public
    constructor Create (const VarName: String);

    function ToXML: AnsiString; override;

  end;


  TIntVariableInfo= class (TBaseVariableInfo)
  protected
    function GetIntValue: Integer;

  public
    constructor Create (const VarName: String);

    function ToXML: AnsiString; override;

  end;

  TVariablesInfo= class (TStringList)
  private
    function GetVariableByName (VarName: String): TBaseVariableInfo;
    function GetVariableByIndex (Index: Integer): TBaseVariableInfo;

  public
    property VariableByName [VarName: String]: TBaseVariableInfo read GetVariableByName;
    property VariableByIndex [Index: Integer]: TBaseVariableInfo read GetVariableByIndex;

    procedure AddVariable (NewVariable: TBaseVariableInfo);

    function ToXML: AnsiString;

    destructor Destroy; override;

  end;

  TPairIntVariable= class (TObject)
  private
    FCoef: Integer;
    FVar: TBaseVariableInfo;

  public
    property Coef: Integer read FCoef;
    property Variable: TBaseVariableInfo read FVar;

    constructor Create (AnInteger: Integer; AVariable: TBaseVariableInfo);
    function ToXML: String;

  end;

  TLinearFunction= class (TList)
  private
    AvailableVariables: TVariablesInfo;

    function GetIntVariable (Index: Integer): TPairIntVariable;
    function GetPair (Variable: TBaseVariableInfo): TPairIntVariable;

    property PairIntVariable [Index: Integer]: TPairIntVariable read GetIntVariable;

  public
    property Component [Variable: TBaseVariableInfo]: TPairIntVariable read GetPair;
    constructor Create (Vars: TVariablesInfo);
    destructor Destroy; override;

    procedure AddComponent (const VarName: String; Coef: Integer);
    procedure DeleteComponent (Index: Integer);

    function FindCoef (Variable: TBaseVariableInfo): Integer;

    function ToXML: AnsiString;

  end;

  TConstraint= class (TLinearFunction)
  private
    LowerBound, UpperBound: Integer;
    FBoundStatus: TBoundStatus;

    function GetBoundStatus: TBoundStatus;

  public
    property BoundStatus: TBoundStatus read GetBoundStatus;

    constructor Create (Variables: TVariablesInfo);
    destructor Destroy; override;

    procedure SetLowerBound (LBound: Integer);
    procedure SetUpperBound (UBound: Integer);
    function ToXML: AnsiString;

  end;

  TConstraints= class (TList)
  private
    function GetConstraint (Index: Integer): TConstraint;

  public
    property Constraint [Index: Integer]: TConstraint read GetConstraint;

    function ToXML: AnsiString;

  end;

  TGLPK= class (TObject)
  private
    StringToPCChar: TStringList;
    IsMaximizationProblem: Boolean;
    FProblemName: String;
    Variables: TVariablesInfo;
    Constraints: TConstraints;
    FObjectiveFunction: TLinearFunction;

    function ConvertStringToPCChar (const S: String): PCChar;
    function GetVariableByName (Name: String): TBaseVariableInfo;
    function GetVariableByIndex (Index: Integer): TBaseVariableInfo;
    function GetConstraint (Index: Integer): TConstraint;
    function GetHasIntegerVariable: Boolean;

  public
    property ProblemName: String read FProblemName;
    property Variable [Index: Integer]: TBaseVariableInfo read GetVariableByIndex;
    property VariableByName [Name: String]: TBaseVariableInfo read GetVariableByName;
    property VariablesInProblem: TVariablesInfo read Variables;
    property Constraint [Index: Integer]: TConstraint read GetConstraint;
    property ObjectiveFunction: TLinearFunction read FObjectiveFunction;
    property HasIntegerVariable: Boolean read GetHasIntegerVariable;

    constructor CreateMaximizationProblem (const ProbName: String);
    constructor CreateMinimizationProblem (const ProbName: String);

    procedure AddNewContinuousVariable (const VariableName: String);
    procedure AddNewIntegerVariable (const VariableName: String);
    procedure SetABoundForAVariable (const VarName: String; UpperBound: Boolean; Value: Integer);
    procedure ReleaseABoundFromAVariable (const VarName: String; UpperBound: Boolean);

    function InitializeConstraint: TConstraint;
    function ToXML: AnsiString;

    procedure SolveByGLPK;

  end;

  { EVarAlreadyExists }

  EVarAlreadyExists= class (Exception)
  public
    constructor Create (const VariableName: String);

  end;

  { EVarDoesNotExist}
  EVarDoesNotExist= class (Exception)
  public
    constructor Create (const VariableName: String);

  end;

implementation
uses
  Interfaces;

//--------------TContinuousVariableInfo------------------

constructor TContinuousVariableInfo.Create (const VarName: String);
begin
  inherited Create (vtContinues, VarName);

end;

function TContinuousVariableInfo.ToXML: AnsiString;
begin
  Result:= '<ContinuousVariable >';
  Result+= inherited ToXML;
  
  Result+= '</ContinuousVariable>';

end;


//--------------TIntVariableInfo------------------

constructor TIntVariableInfo.Create (const VarName: String);
begin
  inherited Create (vtInteger, VarName);

end;

function TIntVariableInfo.GetIntValue: Integer;
begin
  Result:= Round (Value);
  if 1e-10< Abs (Result- Value) then
    raise Exception.Create ('Invalid Integer Value for an IntVariable');

end;

function TIntVariableInfo.ToXML: AnsiString;
begin
  Result:= '<IntegerVariable ';
  Result+= 'IntegerValue= "'+ IntToStr (GetIntValue)+ '">';
  Result+= inherited ToXML;
  
  Result+= '</IntegerVariable>';

end;

//--------------TBaseVariableInfo------------------

constructor TBaseVariableInfo.Create (VarType: TVariableType; const VarName: String);
begin
  inherited Create;

  FName:= VarName;
  FBoundStatus:= bsFree;
  FVariableType:= VarType;

end;

function TBaseVariableInfo.GetBoundStatus: TBoundStatus;
begin
  Result:= FBoundStatus;

  if Result= bsDB then
    if LowerBound= UpperBound then
      Result:= bsFixed;

end;

function TBaseVariableInfo.GetValue: Double;
begin
  Result:= FDoubleValue;

end;

procedure TBaseVariableInfo.SetValue (AValue: Double);
begin
  FDoubleValue:= AValue;
  WRiteLn (AValue);

end;

procedure TBaseVariableInfo.SetUpperBound (AValue: Integer);
begin
  FUB:= AValue;

  case FBoundStatus of
    bsFree: 
      FBoundStatus:= bsUB;

    bsLB:
      FBoundStatus:= bsDB;

  end; 

end;

procedure TBaseVariableInfo.SetLowerBound (AValue: Integer);
begin
  FLB:= AValue;

  case FBoundStatus of
    bsFree: 
      FBoundStatus:= bsLB;

    bsUB:
      FBoundStatus:= bsDB;

  end; 

end;

procedure TBaseVariableInfo.ReleaseUpperBound;
begin
  case FBoundStatus of
    bsUB:
      FBoundStatus:= bsFree; 

    bsDB:
      FBoundStatus:= bsLB;

  end; 

end;

procedure TBaseVariableInfo.ReleaseLowerBound;
begin
  case FBoundStatus of
    bsLB: 
      FBoundStatus:= bsFree;
    bsDB:
      FBoundStatus:= bsUB;
  end; 

end;

function TBaseVariableInfo.GetHasUpperBound: Boolean;
begin
  Result:= FBoundStatus in [bsUB, bsDB];

end;

function TBaseVariableInfo.GetHasLowerBound: Boolean;
begin
  Result:= FBoundStatus in [bsLB, bsDB];

end;

function TBaseVariableInfo.ToXML: AnsiString;
begin
  Result:= '<BaseVariableInfo Name= "'+ FName+ '" ';
  if HasUpperBound then
    Result+= 'UB= "'+ FloatToStr (UpperBound)+ '" ';
  if HasLowerBound then
    Result+= 'LB= "'+ FloatToStr (LowerBound)+ '" ';
  Result+= 'Value= "'+ FloatToStr (GetValue)+ '" ';
  Result+= '/>';

end;

{ TVariablesInfo }
function TVariablesInfo.GetVariableByName (VarName: String): TBaseVariableInfo;
var
  Index: Integer;

begin
  Index:= Self.IndexOf (UpperCase (VarName));

  if Index< 0 then
    raise EVarDoesNotExist.Create (VarName);

  Result:= VariableByIndex [Index];

end;

function TVariablesInfo.GetVariableByIndex (Index: Integer): TBaseVariableInfo;
begin
  Result:= Objects [Index] as TBaseVariableInfo;

end;

procedure TVariablesInfo.AddVariable (NewVariable: TBaseVariableInfo);
begin
  try
    VariableByName [NewVariable.Name];

  except
    on e: EVarDoesNotExist do
    begin
      AddObject (UpperCase (NewVariable.Name), NewVariable);
      Exit;

    end;

  end;

  raise EVarAlreadyExists.Create (NewVariable.Name);

end;

function TVariablesInfo.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<VariablesInfo>';

  for i:= 0 to Count- 1 do
    Result+= VariableByIndex [i].ToXML;
  Result+= '</VariablesInfo>';

end;

destructor TVariablesInfo.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    Objects [i].Free;

  inherited;

end;

{ TPairIntVariable }

function TPairIntVariable.ToXML: String;
begin
  Result:= '<Component Var="'+ FVar.Name+ '" Coef= "'+ IntToStr (FCoef)+ '" />';

end;

constructor TPairIntVariable.Create (AnInteger: Integer; AVariable: TBaseVariableInfo);
begin
  inherited Create;

  FCoef:= AnInteger;
  FVar:= AVariable;

end;

{ TLinearFunction }

procedure TLinearFunction.AddComponent (const VarName: String; Coef: Integer);
var
  Pair: TPairIntVariable;
  VarInfo: TBaseVariableInfo;

begin
  VarInfo:= AvailableVariables.VariableByName [VarName];
  Pair:= TPairIntVariable.Create (Coef, VarInfo);
  Add (Pair);

end;

constructor TLinearFunction.Create (Vars: TVariablesInfo);
begin
  inherited Create;

  AvailableVariables:= Vars;

end;

procedure TLinearFunction.DeleteComponent (Index: Integer);
var
  Pair: TPairIntVariable;

begin
  Dec (Index);
  Pair:= PairIntVariable [Index];
  Pair.Free;
  Delete (Index);

end;

destructor TLinearFunction.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    PairIntVariable [i].Free;

  inherited;

end;

function TLinearFunction.FindCoef (Variable: TBaseVariableInfo): Integer;
var
  TempPair: TPairIntVariable;

begin
  TempPair:= Component [Variable];
  Result:= TempPair.Coef;

  TempPair.Free;
  
end;

function TLinearFunction.GetIntVariable (Index: Integer): TPairIntVariable;
begin
  Result:= (TObject (Items [Index])) as TPairIntVariable;

end;

function TLinearFunction.GetPair (Variable: TBaseVariableInfo): TPairIntVariable;
var
  i: Integer;

begin
  Result:= TPairIntVariable.Create (0, Variable);

  for i:= 0 to Count- 1 do
    if PairIntVariable [i].Variable= Variable then
      Result.FCoef+= PairIntVariable [i].Coef;

end;

function TLinearFunction.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<LinearFunction>';

  for i:= 0 to Count- 1 do
    Result+= PairIntVariable [i].ToXML;

  Result+= '</LinearFunction>';

end;


{ TConstraint }

constructor TConstraint.Create (Variables: TVariablesInfo);
begin
  inherited;

  FBoundStatus:= bsFree;

end;

destructor TConstraint.Destroy;
begin
  inherited;

end;

function TConstraint.GetBoundStatus: TBoundStatus;
begin
  Result:= FBoundStatus;

  if Result= bsDB then
    if LowerBound= UpperBound then
      Result:= bsFixed;

end;

procedure TConstraint.SetLowerBound (LBound: Integer);
begin
  if FBoundStatus= bsFree then
    FBoundStatus:= bsLB
  else if FBoundStatus= bsUB then
    FBoundStatus:= bsDB;

  LowerBound:= LBound;
  
end;

procedure TConstraint.SetUpperBound (UBound: Integer);
begin
  if FBoundStatus= bsFree then
    FBoundStatus:= bsUB
  else if FBoundStatus= bsLB then
    FBoundStatus:= bsDB;

  UpperBound:= UBound;
  
end;

function TConstraint.ToXML: AnsiString;
begin
  Result:= '<Constraint BoundStatus= "'+ 
           IntToStr (Ord (BoundStatus))+ 
          '" LowerBound= "'+ 
          FloatToStr (LowerBound)+ 
          '" UpperBound= "'+
           FloatToStr (UpperBound)+ 
           '" >';
  Result+= inherited ToXML;
  Result+= '</Constraint>';

end;

{ TConstraints }

function TConstraints.ToXML: AnsiString;
var
  i: Integer;

begin
  Result:= '<Constraints>';

  for i:= 0 to Count- 1 do
    Result+= Constraint [i].ToXML;

  Result+= '</Constraints>';

end;

function TConstraints.GetConstraint (Index: Integer): TConstraint;
begin
  Result:= TObject (Items [Index]) as TConstraint;
end;

{ EVarAlreadyExists}

constructor EVarAlreadyExists.Create (const VariableName: String);
begin
  inherited Create ('A variable with the same name['+ VariableName+ '] exists!');

end;

{ EVarDoesNotExist}

constructor EVarDoesNotExist.Create (const VariableName: String);
begin
  inherited Create ('There is no variable with name['+ VariableName+ '] in the collection!');

end;

{ TGPLK }

procedure TGLPK.AddNewContinuousVariable (const VariableName: String);
var
  NewVar: TBaseVariableInfo;

begin
  if 0<= Variables.IndexOf (UpperCase (VariableName)) then
    raise EVarAlreadyExists.Create (VariableName);

  NewVar:= TContinuousVariableInfo.Create (VariableName);
  Variables.AddObject (UpperCase (VariableName), NewVar);

end;

procedure TGLPK.AddNewIntegerVariable (const VariableName: String);
var
  NewVar: TBaseVariableInfo;

begin
  if 0<= Variables.IndexOf (UpperCase (VariableName)) then
    raise EVarAlreadyExists.Create (VariableName);

  NewVar:= TIntVariableInfo.Create (VariableName);
  Variables.AddObject (UpperCase (VariableName), NewVar);

end;

constructor TGLPK.CreateMaximizationProblem (const ProbName: String);
begin
  inherited Create;

  Variables:= TVariablesInfo.Create;

  FProblemName:= ProbName;
  IsMaximizationProblem:= True;
  Constraints:= TConstraints.Create;
  FObjectiveFunction:= TLinearFunction.Create (Variables);

end;

constructor TGLPK.CreateMinimizationProblem (const ProbName: String);
begin
  inherited Create;

  Variables:= TVariablesInfo.Create;

  FProblemName:= ProbName;
  IsMaximizationProblem:= False;
  Constraints:= TConstraints.Create;
  FObjectiveFunction:= TLinearFunction.Create (Variables);

end;

function TGLPK.ConvertStringToPCChar (const S: String): PCChar;
var
  Index: Integer;
  i: Integer;

begin
  Index:= StringToPCChar.IndexOf (S);

  if 0<= Index then
    Result:= PCChar (StringToPCChar.Objects [Index])
  else
  begin
    GetMem (Result, Length (S)+ 1);
    for i:= 1 to Length (S) do
      (Result+ i- 1)^:= Ord (S [i]);
    (Result+ Length (S))^:= 0;

    StringToPCChar.AddObject (S, TObject (Result));
    StringToPCChar.Sort;

  end;

end;

function TGLPK.GetConstraint (Index: Integer): TConstraint;
begin
  Result:= Constraints.Constraint [Index];

end;

function TGLPK.GetHasIntegerVariable: Boolean;
var
  i: Integer;

begin
  Result:= True;

  for i:= 0 to Variables.Count- 1 do
    if Variables.VariableByIndex [i].VariableType= vtInteger then
      Exit;

  Result:= False;

end;

function TGLPK.GetVariableByName (Name: String): TBaseVariableInfo;
begin
  Result:= Variables.VariableByName [Name];

end;

function TGLPK.GetVariableByIndex (Index: Integer): TBaseVariableInfo;
begin
  Result:= Variables.VariableByIndex [Index];

end;

function TGLPK.InitializeConstraint: TConstraint;
begin
  Result:= TConstraint.Create (Variables);
  
  Constraints.Add (Result);
 
end;

procedure TGLPK.SetABoundForAVariable (const VarName: String; UpperBound: Boolean; Value: Integer);
var
  VarInfo: TBaseVariableInfo;

begin
  VarInfo:= Variables.VariableByName [VarName];

  if UpperBound then
    VarInfo.SetUpperBound (Value)
  else
    VarInfo.SetLowerBound (Value);

end;

procedure TGLPK.SolveByGLPK;

  procedure ExportVariables (ProblemObj: TGLPKProblemPointer; Variables: TVariablesInfo);
  var
    i: Integer;
    ActiveVariable: TBaseVariableInfo;

  begin
    for i:= 0 to Variables.Count- 1 do
    begin
      ActiveVariable:= Variables.VariableByIndex [i];

      case ActiveVariable.VariableType of
        vtInteger:
          GLPKAddNewIntegerVariable (ConvertStringToPCChar (Variables.VariableByIndex [i].Name), Ord (ActiveVariable.BoundStatus), ActiveVariable.LowerBound, ActiveVariable.UpperBound, ObjectiveFunction.FindCoef (ActiveVariable), ProblemObj);

        vtContinues:
          GLPKAddNewContinuesVariable (ConvertStringToPCChar (Variables.VariableByIndex [i].Name), Ord (ActiveVariable.BoundStatus), ActiveVariable.LowerBound, ActiveVariable.UpperBound, ObjectiveFunction.FindCoef (ActiveVariable), ProblemObj)

        else
          raise Exception.Create ('Invalid VariableType!');

      end;

    end;

   end;

  procedure ExportConstraints (ProblemObj: TGLPKProblemPointer; Constraints: TConstraints; Variables: TVariablesInfo);
  var
    c, v: Integer;
    ActiveConstraint: TConstraint;
    ActiveVariable: TBaseVariableInfo;
    IA, IB, IC: PCInt32;
    IAPtr, IBPtr, ICPtr: PCInt32;
    Size, MaxIndex: Integer;
    Coef: CInt16;

  begin
    Size:= Variables.Count* Constraints.Count+ 1;
    GetMem (IA, 2* Size);
    GetMem (IB, 2* Size);
    GetMem (IC, 2* Size);

    for c:= 0 to Constraints.Count- 1 do
    begin
      ActiveConstraint:= Constraints.Constraint [c];

      GLPKAddNewConstraint (ConvertStringToPCChar ('Row'+ IntToStr (c+ 1)), Ord (ActiveConstraint.BoundStatus), ActiveConstraint.LowerBound, ActiveConstraint.UpperBound, ProblemObj);
    
    end;

    IAPtr:= IA; IBPtr:= IB; ICPtr:= IC;
    IAPtr^:= 0; IBPtr^:= 0; ICPtr^:= 0;
    Inc (IAPtr); Inc (IBPtr); Inc (ICPtr);
    MaxIndex:= 1;
    for c:= 0 to Constraints.Count- 1 do
    begin
      ActiveConstraint:= Constraints.Constraint [c];
      for v:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [v];

        Coef:= ActiveConstraint.FindCoef (ActiveVariable);
        if Coef<> 0 then
        begin
          IAPtr^:= c+ 1;
          IBPtr^:= v+ 1;
          ICPtr^:= Coef;
       
          Inc (IAPtr, 1); Inc (IBPtr, 1); Inc (ICPtr, 1);
          Inc (MaxIndex);

        end;
          
      end;

    end;

    GLPKLoadMatrix (IA, IB, IC, MaxIndex- 1, ProblemObj);


  end;

  procedure RunSimplex (ProblemObj: TGLPKProblemPointer);
  begin
    GLPKSimplex (ProblemObj);

  end;

  procedure RunIntOptimization (ProblemObj: TGLPKProblemPointer);
  begin
    WriteLn ('In RunIntOptimization');
    GLPKIntOptimization (ProblemObj);

  end;

  procedure ImportVariables (ProblemObj: TGLPKProblemPointer; Variables: TVariablesInfo);
  var
    i: Integer;
    ActiveVariable: TBaseVariableInfo;
    PValue: double;

  begin
    if HasIntegerVariable then
    begin
      for i:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [i];

        PValue:= GLPKGetMIPVariablePrimeValue (ProblemObj, i+ 1);
        ActiveVariable.SetValue (PValue);

      end;
 

    end
    else
    begin
      for i:= 0 to Variables.Count- 1 do
      begin
        ActiveVariable:= Variables.VariableByIndex [i];

        PValue:= GLPKGetLPVariablePrimeValue (ProblemObj, i+ 1);
        ActiveVariable.SetValue (PValue);

      end;

    end;

   end;

var
  ProblemObj: TGLPKProblemPointer;

begin
  StringToPCChar:= TStringList.Create;
  if IsMaximizationProblem then
    ProblemObj:= GLPKCreateMaximizationProblem (ConvertStringToPCChar (ProblemName))
  else 
    ProblemObj:= GLPKCreateMinimizationProblem (ConvertStringToPCChar (ProblemName));

  ExportVariables (ProblemObj, Variables);
  ExportConstraints (ProblemObj, Constraints, Variables);

  if HasIntegerVariable then
    RunIntOptimization (ProblemObj)
  else
    RunSimplex (ProblemObj);

  ImportVariables (ProblemObj, Variables);

end;

procedure TGLPK.ReleaseABoundFromAVariable (const VarName: String; UpperBound: Boolean);
var
  VarInfo: TBaseVariableInfo;

begin
  VarInfo:= Variables.VariableByName [VarName];

  if UpperBound then
    VarInfo.ReleaseUpperBound
  else
    VarInfo.ReleaseLowerBound;

end;

function TGLPK.ToXML: AnsiString;
begin
  if IsMaximizationProblem then
    Result:= '<MaxProblem '
  else
    Result:= '<MinProblem ';

  Result+= 'Name= "'+ FProblemName+ '" >';
  Result+= Variables.ToXML;
  Result+= ObjectiveFunction.ToXML;
  Result+= Constraints.ToXML;

  if IsMaximizationProblem then
    Result+= '</MaxProblem> '
  else
    Result+= '</MinProblem>';

end;

initialization

end.
