unit PBPreProcessingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PreprocessorUnit, ClauseUnit, TSeitinVariableUnit;

type

  { TPBPrePorecessor }

  TPBPrePorecessor= class (TAbstractPreprocessor)
  private
  public
    function Process (CNF: TClauseCollection; Assignment: TAssignments): Boolean;
      override;


  end;

implementation
uses
  BigInt;

{ TPBPrePorecessor }

function TPBPrePorecessor.Process (CNF: TClauseCollection;
  Assignment: TAssignments): Boolean;

  function AllNeg (CNF: TClauseCollection;   Assignment: TAssignments): Boolean;
  var
    i, j: Integer;
    ActiveClause: TClause;
    ActiveLiteral: TLiteral;
    Satisfied: Boolean;

  begin
    for i:= 0 to CNF.Count- 1 do
    begin
      ActiveClause:= CNF.Item [i];
      Satisfied:= False;

      for j:= 0 to ActiveClause.Count- 1 do
        if IsNegated (ActiveClause.Item [j]) then
          case Assignment.GetValue (ActiveClause.Item [j]) of
            gbFalse:
            begin
              Satisfied:= True;
              Break;

            end;
            gbUnknown:
            begin
              Satisfied:= True;
              Break;

            end;

          end
        else if Assignment.GetValue (ActiveClause.Item [j])= gbTrue then
        begin
          Satisfied:= True;
          Break;

        end;

      if not Satisfied then
        Exit (False);

    end;

    Result:= True;

  end;

var
  n, m, l: Integer;// n: No of VAr; m: no of Clauses; l: No of Literals
  Ci: array of TBigInt;
  i, j: Integer;
  ActiveClause: TClause;
  Sum, PToBase, Base, Temp: TBigInt;

begin
  n:= CNF.MaxVar;
  m:= CNF.ClauseCount;
  l:= CNF.LiteralCount;

  Assignment.Count:= n+ 1;
  if AllNeg (CNF, Assignment) then
  begin
    for i:= 0 to Assignment.Count- 1 do
      if Assignment.Item [i]= gbUnknown then
        Assignment.Item [i]:= gbFalse;

    Exit (True);

  end;

  SetLength (Ci, n+ 2* m+ 1);

  Base:= BigIntFactory.GetNewMemeber.SetValue (7);
  PToBase:= Base.Pow (m);
  { Add a clause stating at least one of the variables should be true. }
  for i:= 0 to n do
    ci [i]:= PToBase.Copy;
  PToBase.SetValue (1);
  Sum:= BigIntFactory.GetNewMemeber.SetValue (0);

  for i:= 0 to m- 1 do
  begin
    ActiveClause:= CNF.Item [i];

    for j:= 0 to ActiveClause.Count- 1 do
      if not IsNegated (ActiveClause.Item [j]) then
        Ci [GetVar (ActiveClause.Item [j])].Add (PToBase)
      else
        Ci [GetVar (ActiveClause.Item [j])].Sub (PToBase);

    ci [n+ 1+ 2* i]:= PToBase.Copy;
    ci [n+ 1+ 2* i+ 1]:= PToBase.Copy;

    Sum.Add (PToBase);
    Temp:= PToBase.Mul (Base);
    BigIntFactory.ReleaseMemeber (PToBase.Free);
    PToBase:= Temp;

  end;

  Temp:= Sum.Copy;
  Sum.Mul2;
  Sum.Add (Temp);//Sum:= 3* Sum
  BigIntFactory.ReleaseMemeber (Temp);

  for i:= 0 to High (Ci)- 1 do
    Write (Ci [i].ToString, ' x', i+ 1, ' + ');
  i:= High (Ci);
  Write (Ci [i].ToString, ' x', i+ 1);
  Write (' = ');
  WriteLn (Sum.ToString);

  for i:= 0 to High (Ci) do
    BigIntFactory.ReleaseMemeber (Ci [i]);
  SetLength (Ci, 0);

end;

end.

