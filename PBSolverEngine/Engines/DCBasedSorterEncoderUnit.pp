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
uses
  GenericFactoryUnit;

{ TDCBasedSorterEncoder }

procedure TDCBasedSorterEncoder.AddExtraClauses_Medium;
var
  l, i, b1, b2: Integer;
  ActiveResult: TLiteralCollection;
  Ignore: Boolean;

begin
  for l:= 0 to InputLiterals.Count- 1 do
    for i:= 0 to DC [l].Count- 1 do
    begin
      ActiveResult:= DC [l].Item [i];
      VariableManager.SatSolver.BeginConstraint;

      Ignore:= False;
      for b1:= 0 to Modulo- 1 do
        if GetVar (ActiveResult.Items [b1])<> 0 then
        begin
          if ActiveResult.Items [b1]= VariableManager.FalseLiteral then
          else if ActiveResult.Items [b1]= VariableManager.TrueLiteral then
          begin
              VariableManager.SatSolver.AbortConstraint;
              Ignore:= True;
              Break;

          end
          else
            VariableManager.SatSolver.AddLiteral (ActiveResult.Items [b1]);

        end;

     if not Ignore then
       VariableManager.SatSolver.SubmitClause;// Result [0] or  Result [1] or ... or Result [Modulo- 1]

    end;

    for l:= 0 to InputLiterals.Count- 1 do
      for i:= 0 to DC [l].Count- 1 do
      begin
        ActiveResult:= DC [l].Item [i];

        for b1:= 0 to Modulo- 1 do
          for b2:= b1+ 1 to Modulo- 1 do
            if (ActiveResult.Items [b1]<> VariableManager.FalseLiteral) and
               (ActiveResult.Items [b2]<> VariableManager.FalseLiteral) then
            begin

              VariableManager.SatSolver.BeginConstraint;

              VariableManager.SatSolver.AddLiteral (NegateLiteral (ActiveResult.Items [b1]));
              VariableManager.SatSolver.AddLiteral (NegateLiteral (ActiveResult.Items [b2]));

              VariableManager.SatSolver.SubmitClause;// Result[i]=> \lnot Result [j]

            end;

      end;

end;

procedure TDCBasedSorterEncoder.AddExtraClauses_High;
begin

end;

type
  TLiteralCollectionFactory= specialize TGenericFactoy<TLiteralCollection>;

function TDCBasedSorterEncoder.Encode: TLiteralCollection;
 var
   LiteralCollectionFactory: TLiteralCollectionFactory;

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
       Result.Items [0]:= NegateLiteral (InputLiterals.Items [Index]);//0
       Result. Items [1]:= InputLiterals.Items [Index];//1
       for i:= 2 to Modulo- 1 do
         Result.Items [i]:= VariableManager.FalseLiteral;

       Exit;

     end;

     Left:= _CreateDCSorter (Index, Len div 2, Level+ 1);
     Right:= _CreateDCSorter (Index+ Len div 2, Len- Len div 2, Level+ 1);

//     TempLiteralCollection:= TLiteralCollection.Create (Modulo, GetVariableManager.FalseLiteral);
     TempLiteralCollection:= LiteralCollectionFactory.GetNewMemeber;
     TempLiteralCollection.Count:= Modulo;

     for i:= 0 to Modulo- 1 do
     begin

       for j:= 0 to Modulo- 1 do
         TempLiteralCollection.Items [j]:= VariableManager.CreateVariableDescribingAND
                              (
                                Left.Items [(i+ j) mod Modulo],
                                Right.Items [(Modulo- j) mod Modulo]
                              );
       Result.Items [i]:= VariableManager.CreateVariableDescribingOR (TempLiteralCollection);

     end;

     LiteralCollectionFactory.ReleaseMemeber (TempLiteralCollection);

   end;

begin
  LiteralCollectionFactory:= TLiteralCollectionFactory.Create;

  Result:= _CreateDCSorter (0, InputLiterals.Count, 0).Copy;

  LiteralCollectionFactory.Free;

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

