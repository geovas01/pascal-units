unit AbstractPBModEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSeitinVariableUnit, SatSolverInterfaceUnit, PBConstraintUnit,
  CollectionUnit, ClauseUnit;

type
  TBoolArray= array of Boolean;
//  TListOfLiteralCollection= specialize TGenericCollection<TLiteralCollection>;

  { TAbstractPBModEncoder }

  TAbstractPBModEncoder= class (TObject)
  private
    FConstraint: TPBConstraint;
    FVariableGenerator: TVariableManager;
    FCoefs: TInt64Collection;
    Fb: Int64;
    FOrigSum: TPBSum;
    FModulo: Integer;

    function GetCNFGenerator: TSATSolverInterface; inline;
    function GetVariableGenerator: TVariableManager; inline;


  protected
    property OriginalConstraint: TPBConstraint read FConstraint;
    property CNFGenerator: TSATSolverInterface read GetCNFGenerator;
    property VariableGenerator: TVariableManager read FVariableGenerator;
    property Coefs: TInt64Collection read FCoefs;
    property b: Int64 read Fb;
    property OrigSum: TPBSum read FOrigSum;
    property Modulo: Integer read FModulo;

    procedure AddExtraClauses_Medium; virtual;
    procedure AddExtraClauses_High; virtual;

    {
      Returns true if all values between 0 to modulo- 1 (mod Modulo) can be produced
      as the sum of subsets of elements in range SIndex and EIndex.
    }
    function GenerateAllPossible (SIndex, EIndex: Integer; Values: TBoolArray): Boolean;


  public
    {
      None of the input parameters will be deep copied, do not free them
      before freeing the encoder.
    }
    constructor Create (_OrigConstraint: TPBConstraint;
                        _VariableManager: TVariableManager;
                        _Coefs: TInt64Collection; _b: Int64;
                        _OrigSum: TPBSum; _Modulo: Integer);
    destructor Destroy; override;

    function EncodePBMod: TLiteral; virtual; abstract;

    procedure AddExtraClauses; virtual;

  end;

implementation
uses
  ParameterManagerUnit;

{ TAbstractPBModEncoder }

function TAbstractPBModEncoder.GetCNFGenerator: TSATSolverInterface;
begin
  Result:= SatSolverInterfaceUnit.GetSatSolver;

end;

function TAbstractPBModEncoder.GetVariableGenerator: TVariableManager;
begin
  Result:= FVariableGenerator;

end;

function TAbstractPBModEncoder.GenerateAllPossible(SIndex, EIndex: Integer;
  Values: TBoolArray): Boolean;
var
  i, j: Integer;
  v: Int64;
  ReachableValues: array [0..1] of TBoolArray;

begin
  SetLength (ReachableValues [0], Modulo);
  SetLength (ReachableValues [1], Modulo);
  FillChar (ReachableValues [0, 0], SizeOf (ReachableValues [0]), 0);
  FillChar (ReachableValues [1, 0], SizeOf (ReachableValues [1]), 0);
  ReachableValues [(SIndex mod 2) xor 1, 0]:= True;

  for i:= SIndex to EIndex do
  begin
    ReachableValues [i mod 2]:= ReachableValues [(i mod 2) xor 1];

    v:= Coefs.Item [i];
    for j:= 0 to Modulo- 1 do
      if not ReachableValues [i mod 2, j] then
        ReachableValues [i mod 2, j]:=
          ReachableValues [(i mod 2) xor 1, j] or
          ReachableValues [(i mod 2) xor 1, (j+ Modulo- v) mod Modulo];

  end;

  Result:= True;
  for i:= 0 to Modulo- 1 do
  begin
    Values [i]:= ReachableValues [EIndex mod 2, i];
    Result:= Result and Values [i];

  end;
  SetLength (ReachableValues [0], 0);
  SetLength (ReachableValues [1], 0);

end;

procedure TAbstractPBModEncoder.AddExtraClauses_Medium;
begin
  // Do nothing
end;

procedure TAbstractPBModEncoder.AddExtraClauses_High;
begin
  // Do nothing

end;

constructor TAbstractPBModEncoder.Create (_OrigConstraint: TPBConstraint;
  _VariableManager: TVariableManager; _Coefs: TInt64Collection; _b: Int64;
  _OrigSum: TPBSum; _Modulo: Integer);
begin
  inherited Create;

  FConstraint:= _OrigConstraint;
  FVariableGenerator:= _VariableManager;
  FCoefs:= _Coefs;
  Fb:= _b;
  FOrigSum:= _OrigSum;
  FModulo:= _Modulo;

end;

destructor TAbstractPBModEncoder.Destroy;
begin
  FVariableGenerator:= nil;

  inherited Destroy;

end;

procedure TAbstractPBModEncoder.AddExtraClauses;
begin
  if VariableGenerator.SimulationMode then
    Exit;

  if (UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('Medium')) or
     (UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('High')) then
     AddExtraClauses_Medium;

  if UpperCase (GetRunTimeParameterManager.GetValueByName ('--ExtraClausesLevel'))= UpperCase ('High') then
     AddExtraClauses_High;

end;

end.

