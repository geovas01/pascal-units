unit RWalkSolverUnit;
{$mode objfpc}

interface
uses
  AbstractSolverUnit, ClauseUnit, SatSolverInterfaceUnit;

type
  TRWalkClause= array of Integer;

  TRWalkSolver= class (TAbstractSatSolver)
  private
    FVarCount: Integer;
    LastUsedVarIndex: Integer;
    FClauseCount: Integer;
    FLastUsedClauseIndex: Integer;

    FClauses: array of TRWalkClause;
    FClauseSize: array of Integer;

    VarValues: array of TGroundBool;
    Decide: array of Boolean;
    Polarity: array of TVariablePolarity;
    VarOccuredInClause: array of array of Integer;// VarOccuredInClause [i][j]= k => variable i occured in the k-th clause.
    VarOccuredInClauseCount: array of Integer;// VarOccuredInClauseCount [i]= k => variable i occured in k different clauses.
    VarOccuredPossitivelyInClause,
    VarOccuredNegativelyInClause: array of array of Integer;// VarOccuredInClause [i][j]= k => variable i occured in the k-th clause.
    VarOccuredPossitivelyInClauseCount,
    VarOccuredNegativelyInClauseCount: array of Integer;// VarOccuredInClauseCount [i]= k => variable i occured in k different clauses.
    ClauseStatus: array of TGroundBool;

    procedure SetVarCount (NewVCount: Integer);
    procedure SetClauseCount (NewCCount: Integer);

  protected
    property VarCount: Integer read FVarCount write SetVarCount;
    property ClauseCount: Integer read FClauseCount write SetClauseCount;
  
    function GetLiteralValue (Lit: TLiteral): TGroundBool; inline;
    procedure Propagate (NewVariable: TVariable);

  public
    {
      VCount: # of Variables
      CCount: # of Clauses 
    }
    constructor Create (VCount: Integer; CCount: Integer);
    destructor Destroy; override;

    function GetNewVar (VariablePolarity: TVariablePolarity; DecideForVar: Boolean): TVariable; override;
    function AddClause (AClause: TClause): Boolean; override;
    function Solve: Boolean; override;
    procedure SetDecisionVar (Variable: Integer; SetFlag: Boolean); override;
    function GetValue (x: Integer): TGroundBool; override;
    function GetValueInModel (x: Integer): TGroundBool; override; 
    function NoAssigns: Integer; override;
    function NoClauses: Integer; override;
    function NoVars: Integer; override; 


  end;

implementation

procedure TRWalkSolver.SetVarCount (NewVCount: Integer);
begin
  FVarCount:= NewVCount;
  SetLength (VarValues, VarCount+ 1);
  SetLength (Polarity, VarCount+ 1);
  SetLength (Decide, VarCount+ 1);
  SetLength (VarOccuredInClause, VarCount+ 1);
  SetLength (VarOccuredInClauseCount, VarCount+ 1);
  SetLength (VarOccuredPossitivelyInClause, VarCount+ 1);
  SetLength (VarOccuredPossitivelyInClauseCount, VarCount+ 1);
  SetLength (VarOccuredNegativelyInClause, VarCount+ 1);
  SetLength (VarOccuredNegativelyInClauseCount, VarCount+ 1);

end;

procedure TRWalkSolver.SetClauseCount (NewCCount: Integer);
begin
  FClauseCount:= NewCCount;
  SetLength (FClauses, ClauseCount);
  SetLength (FClauseSize, ClauseCount);
  SetLength (ClauseStatus, ClauseCount);

end;

function TRWalkSolver.GetLiteralValue (Lit: TLiteral): TGroundBool;
begin
  Result:= GetValue (GetVar (Lit));
  if IsNegated (Lit) then
    Result:= TGroundBool (2- Ord (Result));

end;

constructor TRWalkSolver.Create (VCount: Integer; CCount: Integer);
begin
  inherited Create;

  VarCount:= VCount;
  LastUsedVarIndex:= 0;
  ClauseCount:= CCount;

  FillChar (VarValues [0], SizeOf (VarValues), 1);
  FillChar (Decide [0], SizeOf (Decide), 1);
  FillChar (Polarity [0], SizeOf (Polarity), 0);
  FillChar (VarOccuredInClauseCount [0], SizeOf (VarOccuredInClauseCount), 0);
  FillChar (VarOccuredNegativelyInClauseCount [0], SizeOf (VarOccuredNegativelyInClauseCount), 0);
  FillChar (VarOccuredPossitivelyInClauseCount [0], SizeOf (VarOccuredPossitivelyInClauseCount), 0);

end;

destructor TRWalkSolver.Destroy; 
begin
  VarCount:= 0;
//Delete the clauses...

  ClauseCount:= 0;
  inherited;

end;

function TRWalkSolver.GetNewVar (VariablePolarity: TVariablePolarity; DecideForVar: Boolean): TVariable;
begin
  Inc (LastUsedVarIndex);
  Result:= LastUsedVarIndex;
 
  if VarCount< LastUsedVarIndex then
    VarCount:= 2* VarCount;

  VarValues [Result]:= gbUnknown;
  Polarity [Result]:= VariablePolarity;
  Self.Decide [Result]:= DecideForVar;
  VarOccuredInClauseCount [Result]:= 0;
  VarOccuredPossitivelyInClauseCount [Result]:= 0;
  VarOccuredNegativelyInClauseCount [Result]:= 0;
  SetLength (VarOccuredInClause [Result], 0);
  SetLength (VarOccuredPossitivelyInClause [Result], 0);
  SetLength (VarOccuredNegativelyInClause [Result], 0);
  
end;

function TRWalkSolver.AddClause (AClause: TClause): Boolean;

  procedure UpdateVariableInfo (var NewClause: TRWalkClause);
  var
    i: Integer;
    v: TVariable;
    Lit: TLiteral;

  begin
    for i:= 0 to High (NewClause) do
    begin
      Lit:= NewClause [i];
      v:= GetVar (Lit);

      Inc (VarOccuredInClauseCount [v]);
      SetLength (VarOccuredInClause [v], VarOccuredInClauseCount [v]);
      VarOccuredInClause [v][VarOccuredInClauseCount [v]- 1]:= FLastUsedClauseIndex;

      if IsNegated (Lit) then
      begin
        Inc (VarOccuredNegativelyInClauseCount [v]);
        SetLength (VarOccuredNegativelyInClause [v], VarOccuredNegativelyInClauseCount [v]);
        VarOccuredNegativelyInClause [v][VarOccuredNegativelyInClauseCount [v]- 1]:= FLastUsedClauseIndex;

      end
      else
      begin
        Inc (VarOccuredPossitivelyInClauseCount [v]);
        SetLength (VarOccuredPossitivelyInClause [v], VarOccuredPossitivelyInClauseCount [v]);
        VarOccuredPossitivelyInClause [v][VarOccuredPossitivelyInClauseCount [v]- 1]:= FLastUsedClauseIndex;

      end;

    end;
      
  end;

var
  i, Count: Integer;
  NewClause: TRWalkClause;
  Done: Boolean;

begin
  Assert (SolverState<> ssConflict);

  SetLength (NewClause, AClause.Count);
  Count:= 0;

  for i:= 0 to AClause.Count- 1 do
    case GetLiteralValue (AClause.Item [i]) of
      gbTrue:
      begin
        Done:= True;
        Break;

      end;
        
      gbUnknown:
      begin
        NewClause [Count]:= AClause.Item [i];
        Inc (Count);

      end;

    end;

  if Done then//Clause is trivially satisfiable
  begin
    SetLength (NewClause, 0);
    Exit (True);

  end
  else if Count= 0 then// Clause is not satisfiable
  begin
    SetLength (NewClause, 0);
    FSolverState:= ssConflict;

  end
  else if Count= 1 then
  begin
    case GetLiteralValue (NewClause [0]) of
      gbFalse:
        FSolverState:= ssConflict;
      gbTrue:
         ;
      gbUnknown:
      begin
        VarValues [GetVar (NewClause [0])]:= gbTrue;
        if IsNegated (NewClause [0]) then
          VarValues [GetVar (NewClause [0])]:= gbFalse;

        Propagate (NewClause [0]);

      end;
     end;

    SetLength (NewClause, 0);
  end;

  Inc (FLastUsedClauseIndex);
  if FLastUsedClauseIndex= ClauseCount then
    ClauseCount:= 2* ClauseCount;

  FClauseSize [FLastUsedClauseIndex]:= Count;
  FClauses [FLastUsedClauseIndex]:= NewClause;
  ClauseStatus [FLastUsedClauseIndex]:= gbUnknown;
  UpdateVariableInfo (NewClause);

   
end;

procedure TRWalkSolver.Propagate (NewVariable: TVariable);
begin

end;

function TRWalkSolver.Solve: Boolean; 
begin
end;

procedure TRWalkSolver.SetDecisionVar (Variable: Integer; SetFlag: Boolean); 
begin
end;

function TRWalkSolver.GetValue (x: Integer): TGroundBool; 
begin
end;

function TRWalkSolver.GetValueInModel (x: Integer): TGroundBool; 
begin
end;

function TRWalkSolver.NoAssigns: Integer; 
begin
end;

function TRWalkSolver.NoClauses: Integer;
begin
end;

function TRWalkSolver.NoVars: Integer; 
begin
end;


end.
