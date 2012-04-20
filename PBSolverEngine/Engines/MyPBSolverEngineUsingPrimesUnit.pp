unit MyPBSolverEngineUsingPrimesUnit;

{$mode objfpc}{$H+}

interface
uses
  AbstractMyPBSolverEngineUnit, BigInt, CollectionUnit;

type

  { TMyPBSolverEngineUsingPrimeModulos }

  {
    This solver uses the prime number P_1, P_2,\cdots, P_m as the modulos ...
  }
  TMyPBSolverEngineUsingPrimeModulos= class (TAbstractMyPBSolverEngine)
  private
    Primes: TIntegerCollection;

  protected

    function GenerateModulos (m: TBigInt): TIntegerCollection; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;

  {
    This solver uses the minimal set of prime numbers which is a subset of (P_1, P_2,\cdots, P_m) as the modulos ...
  }

  { TMyPBSolverEngineUsingMinimalPrimeModulos }

  TMyPBSolverEngineUsingMinimalPrimeModulos= class (TMyPBSolverEngineUsingPrimeModulos)
  protected

    function GenerateModulos (m: TBigInt): TIntegerCollection; override;

  public
    destructor Destroy; override;

  end;

  {
    This solver uses the set of the following set of numbers (P_1^n1, P_2^n2,\cdots, P_m^nm), such that P_i^ni~\log S
  }

  { TMyPBSolverEngineUsingPrimePowerModulos }

  TMyPBSolverEngineUsingPrimePowerModulos= class (TMyPBSolverEngineUsingPrimeModulos)
  protected

    function GenerateModulos (m: TBigInt): TIntegerCollection; override;

  public
    destructor Destroy; override;

  end;


implementation
uses
  Classes, SysUtils, ParameterManagerUnit;

{ TMyPBSolverEngineUsingPrimePowerModulos }

function TMyPBSolverEngineUsingPrimePowerModulos.GenerateModulos (
  m: TBigInt): TIntegerCollection;
var
  CandidatePrimes: TIntegerCollection;
  Logm, Prod, PrimePower: TBigInt;
  Temp, p: TBigInt;
  i: Integer;


begin
  CandidatePrimes:= inherited GenerateModulos (m);

  Logm:= m.Log.Incr;
  Prod:= BigIntFactory.GetNewMemeber.SetValue (1);

  Result:= TIntegerCollection.Create;

  for i:= 0 to CandidatePrimes.Count- 1 do
  begin
    PrimePower:= BigIntFactory.GetNewMemeber.SetValue (1);
    p:= BigIntFactory.GetNewMemeber.SetValue (CandidatePrimes.Item [i]);

    while PrimePower.CompareWith (Logm)<= 0 do
    begin
      Temp:= PrimePower.Mul (p);
      BigIntFactory.ReleaseMemeber (PrimePower);
      PrimePower:= Temp;

    end;
    BigIntFactory.ReleaseMemeber (p);

    Result.AddItem (PrimePower.GetValue);
    Temp:= Prod.Mul (PrimePower);
    BigIntFactory.ReleaseMemeber (Prod);
    Prod:= Temp;

    if m.CompareWith (Prod)< 0 then
      break;

  end;

  BigIntFactory.ReleaseMemeber (Logm);
  CandidatePrimes.Free;

end;

destructor TMyPBSolverEngineUsingPrimePowerModulos.Destroy;
begin
  inherited Destroy;
end;

{ TMyPBSolverEngineUsingMinimalPrimeModulos }

function TMyPBSolverEngineUsingMinimalPrimeModulos.GenerateModulos (m: TBigInt): TIntegerCollection;

  function FindMinimalPrimes (MinTargetProd: TBigInt; CandidatePrimeSet: TIntegerCollection): TIntegerCollection;

    function FindTheBestMinimalPrimes: TIntegerCollection;
    var
      IsThere: array of Boolean;
      MinProd: TBigInt;

      procedure Find (Index: Integer; CurrentProd: TBigInt);
      var
        i: Integer;
        b: Int64;
        Temp, p: TBigInt;

      begin
        if Index= -1 then
        begin
          if (CurrentProd.CompareWith (MinProd)<= 0) and (MinTargetProd.CompareWith (CurrentProd)<= 0) then
          begin
            Result.Free;
            Result:= TIntegerCollection.Create;
            BigIntFactory.ReleaseMemeber (MinProd);
            MinProd:= CurrentProd.Copy;

            for i:= 0 to High (IsThere) do
              if IsThere [i] then
              begin
                b:= CandidatePrimeSet.Item [i];
                Result.AddItem (b);

              end;

          end;

          Exit;

        end;

        IsThere [Index]:= False;
        Find (Index- 1, CurrentProd);
        p:= BigIntFactory.GetNewMemeber.SetValue (CandidatePrimeSet.Item [Index]);
        IsThere [Index]:= True;

        Temp:= CurrentProd.Mul (p);
        BigIntFactory.ReleaseMemeber (p);
        Find (Index- 1, Temp);
        BigIntFactory.ReleaseMemeber (Temp);

      end;

    var
      CurrentProd: TBigInt;

    begin
      Result:= nil;
      MinProd:= nil;

      SetLength (IsThere, CandidatePrimeSet.Count);
      FillChar (IsThere [0], SizeOf (IsThere), 0);

      MinProd:= BigIntFactory.GetNewMemeber.LoadFromString ('1000000000000000000000000000000000000000');
      CurrentProd:= BigIntFactory.GetNewMemeber.SetValue (1);
      Find  (CandidatePrimeSet.Count- 1, CurrentProd);
      BigIntFactory.ReleaseMemeber (CurrentProd);
      BigIntFactory.ReleaseMemeber (MinProd);
      SetLength (IsThere, 0);

     end;
{
    function FindMinimalPrimesFast: TIntegerCollection;
    var
      i: Integer;
      CurrentSum: Extended;

    begin
      CurrentSum:= 0;
      for i:= 0 to CandidatePrimeSet.Count- 1 do
        CurrentSum+= ln (CandidatePrimeSet.Item [i]);

      Result:= TIntegerCollection.Create;
      i:= 0;

      while i< CandidatePrimeSet.Count do
      begin
        if MinTargetProd< CurrentSum- ln (CandidatePrimeSet.Item [i]) then
          CurrentSum:= CurrentSum- ln (CandidatePrimeSet.Item [i])
        else
        begin
          Dec (i);
          Break;

        end;

        Inc (i);

      end;

      Inc (i);
      while i< CandidatePrimeSet.Count do
      begin
        Result.AddItem (i);
        Inc (i);

      end;

     end;
}
  begin
    {
    if GetRunTimeParameterManager.GetValueByName ('MinimalPrimesMethod')= 'G' then
      Result:= FindMinimalPrimesFast
   else
   }
      Result:= FindTheBestMinimalPrimes

  end;

var
  CandidatePrimes: TIntegerCollection;

begin
  CandidatePrimes:= inherited GenerateModulos (m);
  Result:= FindMinimalPrimes (m, CandidatePrimes);

  CandidatePrimes.Free;

end;

destructor TMyPBSolverEngineUsingMinimalPrimeModulos.Destroy;
begin
  inherited Destroy;
end;

{ TMyPBSolverEngineUsingPrimeModulos }

function TMyPBSolverEngineUsingPrimeModulos.GenerateModulos (m: TBigInt): TIntegerCollection;
  function IsPrime (p: Integer): Boolean;
  var
    i: Integer;
    SqrtP: Integer;

  begin
    SqrtP:= Round (Sqrt (p))+ 1;

    Result:= True;
    for i:= 0 to Primes.Count- 1 do
    begin
      if p mod Primes.Item [i]= 0 then
        Exit (False);

      if SqrtP< Primes.Item [i] then
        Break;

    end;

  end;

var
  i: Integer;
  Temp,
  ProdTemp,
  Prod: TBigInt;
  P: Integer;

begin
  Prod:= BigIntFactory.GetNewMemeber.SetValue (1);
  Temp:= BigIntFactory.GetNewMemeber;
  Result:= TIntegerCollection.Create;

  for i:= 0 to Primes.Count- 1 do
  begin
    Temp.SetValue (Primes.Item [i]);
    Result.AddItem (Primes.Item [i]);

    ProdTemp:= Prod.Mul (Temp);
    BigIntFactory.ReleaseMemeber (Prod);
    Prod:= ProdTemp;

    if m.CompareWith (Prod)< 0 then
    begin
      BigIntFactory.ReleaseMemeber (Temp);
      BigIntFactory.ReleaseMemeber (Prod);
      Exit;

    end;

  end;

  p:= Primes.Item [Primes.Count- 1];

  while m.CompareWith (Prod)>= 0 do
  begin
    while not IsPrime (p) do
      p+= 2;

    if GetRunTimeParameterManager.Verbosity and Ord (vbFull)<> 0 then
       ReportLn ('c NewPrime'+ IntToStr (p));

    Temp.SetValue (p);

    Primes.AddItem (p);
    Result.AddItem (p);

    ProdTemp:= Prod.Mul (Temp);
    BigIntFactory.ReleaseMemeber (Prod);
    Prod:= ProdTemp;

    p+= 2;

  end;
  BigIntFactory.ReleaseMemeber (Temp);
  BigIntFactory.ReleaseMemeber (Prod);

end;

constructor TMyPBSolverEngineUsingPrimeModulos.Create;
begin
  inherited Create;

  Primes:= TIntegerCollection.Create;
  Primes.AddItem (2);
  Primes.AddItem (3);

end;

destructor TMyPBSolverEngineUsingPrimeModulos.Destroy;
begin
  Primes.Free;

  inherited Destroy;
end;

end.

