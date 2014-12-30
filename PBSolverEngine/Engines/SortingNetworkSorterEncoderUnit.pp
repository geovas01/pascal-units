unit SortingNetworkSorterEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractSorterEncoderUnit, ClauseUnit;

type

  { TSortingNetworkSorterEncoder }

  TSortingNetworkSorterEncoder= class(TAbstractSorterEncoder)
  private
  protected
    procedure AddExtraClauses_Medium; override;
    procedure AddExtraClauses_High; override;

  public
    function Encode: TLiteralCollection; override;

  end;

implementation

{ TSortingNetworkSorterEncoder }

procedure TSortingNetworkSorterEncoder.AddExtraClauses_Medium;
begin
  // Do Nothing
end;

procedure TSortingNetworkSorterEncoder.AddExtraClauses_High;
begin
  // Do Nothing

end;

function TSortingNetworkSorterEncoder.Encode: TLiteralCollection;

  function CreateSorter(InputLiterals: TLiteralCollection): TLiteralCollection;
  var
    LastLayerLiterals: TLiteralCollection;

    function GreatestPowerOfTwoLessThan(n: Integer): Integer;
    begin
      Result:= 1;
      while Result< n do
        Result*= 2;

      Result:= Result div 2;

    end;

    procedure Compare(Index1, Index2: Integer; Dir: Boolean);
    var
      l1, l2: TLiteral;

    begin
      L1:= LastLayerLiterals.Items[Index1];
      L2:= LastLayerLiterals.Items[Index2];

      if Dir then
      begin
        LastLayerLiterals.Items[Index1]:= VariableManager.CreateVariableDescribingAND(l1, l2);
        LastLayerLiterals.Items[Index2]:= VariableManager.CreateVariableDescribingOr(l1, l2);

      end
      else
      begin
        LastLayerLiterals.Items[Index1]:= VariableManager.CreateVariableDescribingOr(l1, l2);
        LastLayerLiterals.Items[Index2]:= VariableManager.CreateVariableDescribingAnd(l1, l2);

      end;

    end;

    procedure Merge(lo, n: Integer; Dir: Boolean);
    var
      m, i: Integer;

    begin
      if 1< n then
      begin
        m:= GreatestPowerOfTwoLessThan(n);
        for i:= lo to lo+ n- m- 1 do
          Compare(i, i+ m, Dir);

        Merge(lo, m, Dir);
        Merge(lo+ m, n- m, Dir);

      end;

    end;

    procedure Sort(lo, n: Integer; Dir: Boolean);
    var
      m: Integer;

    begin
      if n<> 1 then
      begin
        m:= n div 2;
        Sort(lo, m, not Dir);
        Sort(lo+ m, n- m, Dir);
        Merge(lo, n, Dir);

      end;

    end;

  begin
    LastLayerLiterals:= InputLiterals.Copy;

    Sort(0, InputLiterals.Count, False);

    Result:= LastLayerLiterals;

  end;

var
 Sorter: TLiteralCollection;
 i, j: Integer;
 TempLitCollection: TLiteralCollection;

begin
  if 1< InputLiterals.Count then
    Sorter:= CreateSorter(InputLiterals)
  else {if InputLiterals.Count= 0 or 1  then}
    Sorter:= InputLiterals.Copy;

  Result:= TLiteralCollection.Create(Modulo, VariableManager.FalseLiteral);

  if Sorter.Count= 0 then
  begin
    Result.Items[0]:= VariableManager.TrueLiteral;
    for i:= 1 to Modulo- 1 do
      Result.Items[i]:= VariableManager.FalseLiteral;

  end
  else
  begin
    TempLitCollection:= TLiteralCollection.Create;

    for i:= 0 to Modulo- 1 do
    begin
      TempLitCollection.Count:= 0;

      j:= i;

      while j<= InputLiterals.Count do
      begin
        if j= InputLiterals.Count then
          TempLitCollection.PushBack(Sorter.Items[j- 1])
        else if j= 0 then
          TempLitCollection.PushBack(NegateLiteral(Sorter.Items[j]))
        else
          TempLitCollection.PushBack(
                  VariableManager.CreateVariableDescribingAND(
                                    Sorter.Items[j- 1],
                                    NegateLiteral(Sorter.Items[j])));
        Inc(j, Modulo);

      end;

      Result.Items[i]:=
                       VariableManager.CreateVariableDescribingOR(
                                     TempLitCollection, TempLitCollection.Count
                                                                  );

    end;

    TempLitCollection.Free

  end;

  Sorter.Free;

end;


end.

