unit DCBasedSorterEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit, ClauseUnit, TSeitinVariableUnit;

type

  { TDCBasedSorterEncoder }

  TDCBasedSorterEncoder= class (TAbstractSorterEncoder)
  private
    DC: array of TListOfLiteralCollection;

  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function Encode: TLiteralCollection; override;

    constructor Create (_VariableManager: TVariableManager;
                _Modulo: Integer;
                Inputs: TLiteralCollection);
    destructor Destroy; override;

  end;

implementation

{ TDCBasedSorterEncoder }

procedure TDCBasedSorterEncoder.AddExtraClauses_Medium;
var
  l, i, b1, b2: Integer;
  ActiveResult: TLiteralCollection;

begin
  for l:= 0 to InputLiterals.Count- 1 do
    for i:= 0 to DC [l].Count- 1 do
    begin
      ActiveResult:= DC [l].Item [i];

      for b1:= 0 to Modulo- 1 do
        for b2:= b1+ 1 to Modulo- 1 do
        begin
          VariableManager.SatSolver.BeginConstraint;

          VariableManager.SatSolver.AddLiteral (NegateLiteral (ActiveResult.Item [b1]));
          VariableManager.SatSolver.AddLiteral (NegateLiteral (ActiveResult.Item [b2]));

          VariableManager.SatSolver.SubmitClause;// Result[i]=> \lnot Result [j]

        end;

    end;


  for l:= 0 to InputLiterals.Count- 1 do
    for i:= 0 to DC [l].Count- 1 do
    begin
      ActiveResult:= DC [l].Item [i];
      VariableManager.SatSolver.BeginConstraint;

     for b1:= 0 to Modulo- 1 do
       if GetVar (ActiveResult.Item [b1])<> 0 then
         VariableManager.SatSolver.AddLiteral (ActiveResult.Item [b1]);

     VariableManager.SatSolver.SubmitClause;// Result [0] or  Result [1] or ... or Result [Modulo- 1]

    end;

end;

procedure TDCBasedSorterEncoder.AddExtraClauses_High;
begin
// Donothig
end;

function TDCBasedSorterEncoder.Encode: TLiteralCollection;

  function _CreateDCSorter (Index: Integer; Len: Integer; Level: Integer): TLiteralCollection;
   {
     Create a sorter for InputLiterals [Index, ..., Index+ Len- 1]
   }
   var
     Left, Right: TLiteralCollection;
     i, j: Integer;
     TempLiteralCollection: TLiteralCollection;

   begin
     Result:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
     DC [Len- 1].AddItem (Result);

     if Len= 1 then
     begin
       Result.Item [0]:= NegateLiteral (InputLiterals.Item [Index]);//0
       Result. Item [1]:= InputLiterals.Item [Index];//1
       for i:= 2 to Modulo- 1 do
         Result.Item [i]:= VariableManager.FalseLiteral;

       Exit;

     end;

     Left:= _CreateDCSorter (Index, Len div 2, Level+ 1);
     Right:= _CreateDCSorter (Index+ Len div 2, Len- Len div 2, Level+ 1);

     TempLiteralCollection:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
     for i:= 0 to Modulo- 1 do
     begin

       for j:= 0 to Modulo- 1 do
         TempLiteralCollection.Item [j]:= VariableManager.CreateVariableDescribingAND
                              (
                                Left.Item [(i+ j) mod Modulo],
                                Right.Item [(Modulo- j) mod Modulo]
                              );
       Result.Item [i]:= VariableManager.CreateVariableDescribingOR (TempLiteralCollection);

     end;

     TempLiteralCollection.Free;

   end;

begin
  Result:= _CreateDCSorter (0, InputLiterals.Count, 0).Copy;

end;

constructor TDCBasedSorterEncoder.Create (_VariableManager: TVariableManager;
  _Modulo: Integer; Inputs: TLiteralCollection);
var
  i: Integer;

begin
  inherited ;

  SetLength (DC, Inputs.Count);
  for i:= 0 to High (DC) do
    DC [i]:= TListOfLiteralCollection.Create;

end;

destructor TDCBasedSorterEncoder.Destroy;
var
  i: Integer;

begin
  for i:= 0 to High (DC) do
    DC [i].Free;

  inherited Destroy;

end;


end.

