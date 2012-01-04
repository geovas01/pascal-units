unit BigInt;
{$Mode objfpc}

interface
const
  MaxLen= 9999;
type

  { TBigInt }

  TBigInt= class
  private
    FDigits: array [0..MaxLen] of Byte;
    FLength: Integer;

    function GetDigit (Index: Integer): Byte;
    function GetIsZero: Boolean; inline;
    procedure SetDigit (Index: Integer; Value: Byte); inline;
    procedure SetLen (Value: Integer); inline;
    {
      Multiplies Self by n and returns itself
    }
    function MulByDigit (n: Integer): TBigInt;
    
  public
    property Length: Integer read FLength write SetLen;
    property Digits [Index: Integer]: Byte read GetDigit write SetDigit;
    property IsZero: Boolean read GetIsZero;

    {Adds m with itself and returns Self}
    function Add (m: TBigInt): TBigInt;
    {Subtracts m from itself and returns Self}
    function Sub (m: TBigInt): TBigInt;
    {Increases itself by one and returns Self}
    function Incr: TBigInt;
    {Decreases itself by one and returns Self}
    function Decr: TBigInt;
    {Multiplies itself by n and returns a new TBigInt}
    function Mul (n: TBigInt): TBigInt;
    {This is a faster implementation for Mul}
    function NewMul (n: TBigInt): TBigInt;
    function Divide (n: TBigInt): TBigInt;
    function SQRT: TBigInt;
    {Compares Self and n, returns 1 if Self is greater, 0 if they are equal and -1 otherwise}
    function CompareWith (n: TBigInt): Integer;
    {Returns a new BigInt which is equal to Self mod m}
    function Modulo (m: TBigInt): TBigInt;

    {Returns a new BigInt which is equal to log of Self in base 2}
    function Log: TBigInt;

    function SumOfDigits: Integer;
    function ShiftLeft (n: Integer): TBigInt;
    function Copy: TBigInt;
    function ToString: AnsiString;

    {Divides itself by two and returns a new TBigInt}
    function Div2: TBigInt;
    {Multiplies itself by two and returns Self}
    function Mul2: TBigInt;

    {Returns a new TBigInt which is equal to Self^n}
    function Pow (n: Integer): TBigInt;

    constructor Create; overload;
    constructor Create (S: PChar); overload;
    destructor Destroy; override;

    {
      Imports n and returns Self
    }
    function SetValue (n: Int64): TBigInt;
    function GetValue: Int64;

    {
      Result:= gcd (self, b).
    }
    function gcd (b: TBigInt): TBigInt;

  end;

implementation
uses
  Math, SysUtils;

procedure TBigInt.SetDigit (Index: Integer; Value: Byte);
begin
  if FLength<= Index  then
    Length:= Index+ 1;

  FDigits [Index]:= Value;

  while FLength> 0 do
  begin
    if FDigits [FLength- 1]= 0 then
      Dec (FLength)
    else
      Break;

  end;

end;

procedure TBigInt.SetLen (Value: Integer);
begin
  FLength:= Value;

end;

function TBigInt.CompareWith (n: TBigInt): Integer;
var
  i: Integer;

begin
  if Self.Length< n.Length then
  begin
    Result:= -1;
    Exit;

  end;

  if n.Length< Self.Length then
  begin
    Result:= +1;
    Exit;

  end;

  for i:= Self.FLength- 1 downto 0 do
  begin
    if Self.FDigits [i]< n.FDigits [i] then
    begin
      Result:= -1;
      Exit;

    end
    else if n.FDigits [i]< Self.FDigits [i] then
    begin
      Result:= 1;
      Exit;

    end;

  end;

  Result:= 0;

end;

function TBigInt.Modulo (m: TBigInt): TBigInt;
{
  Result:= 0;
  for i:= 0 to a.Length do
  begin
    Result:= 10* Result+ a [i];
    Result:= Result mod n;, i.e.,
    {
    while Result< m do
      Result-= m;

    }

  end;

}
var
  Divisor: TBigInt;
  i: Integer;

begin

  Result:= TBigInt.Create;
  Result.SetValue (0);

  for i:= Self.Length- 1 downto 0 do
  begin
    Result.ShiftLeft (1).SetDigit (0, FDigits [i]);

    while Result.CompareWith (m)>= 0 do
      Result.Sub (m);

  end;

end;

function TBigInt.Log: TBigInt;
var
  TwoPower: TBigInt;

begin
  Result:= TBigInt.Create.SetValue (0);
  TwoPower:= TBigInt.Create.SetValue (1);

  while TwoPower.CompareWith (Self)< 0 do
  begin
    TwoPower.Mul2;
    Result.Incr;

  end;

  TwoPower.Free;

end;

function TBigInt.SumOfDigits: Integer;
var
  i: Integer;

begin
  Result:= 0;

  for i:= 0 to Length- 1 do
    Inc (Result, FDigits [i]);

end;

function TBigInt.Sub (m: TBigInt): TBigInt;
var
  Len,
  i, Borrow, Digit: Integer;

begin
  Len:= Max (m.Length, Self.Length);

  Digit:= 0;
  Borrow:= 0;

  for i:= 0 to Len- 1 do
  begin
    Digit:= Self.FDigits [i]- m.FDigits [i]- Borrow;
    if Digit< 0 then
    begin
      Inc (Digit, 10);
      Borrow:= 1;

    end
    else
      Borrow:= 0;

    FDigits [i]:= Byte (Digit);

  end;

  Assert (Borrow<> 0, 'Sorry!! TBigInt can not handle Negative Numbers');
  while FLength> 0 do
  begin
    if FDigits [FLength- 1]= 0 then
      Dec (FLength)
    else
      Break;

  end;

  Result:= Self;

end;

function TBigInt.Add (m: TBigInt): TBigInt;
var
  Len,
  i, Carry,
  Digit: Integer;

begin
  Len:= Max (m.Length, Self.Length);

  Digit:= 0;
  Carry:= 0;

  for i:= 0 to Len- 1 do
  begin
    Digit:= Carry+ m.FDigits [i]+ Self.FDigits [i];
    Carry:= Digit div 10;
    Digit:= Digit mod 10;
    FDigits [i]:= Byte (Digit);
    
  end;

  Length:= Len;
  if Carry> 0 then
  begin
    Length:= Len+ 1;
    FDigits [Len]:= Byte (Carry);

  end;

  Result:= Self;

end;

function TBigInt.Incr: TBigInt;
var
  Carry,
  i: Integer;

begin
  Carry:= 1;

  for i:= 0 to FLength- 1 do
  begin
    Carry:= FDigits [i]+ Carry;

    if Carry= 10 then
    begin
      FDigits [i]:= 0;
      Carry:= 1;

    end
    else
    begin
      FDigits [i]:= Byte (Carry);
      Carry:= 0;
      Break;

    end;

  end;

  if Carry> 0 then
  begin
    Length:= FLength+ 1;
    FDigits [FLength- 1]:= Byte (Carry);

  end;

  Result:= Self;

end;

function TBigInt.Decr: TBigInt;
var
  Borrow,
  i: Integer;

begin
  Borrow:= 1;

  for i:= 0 to Length- 1 do
  begin
    Borrow:= FDigits [i]- Borrow;

    if Borrow< 0 then
    begin
      FDigits [i]:= 10+ Borrow;
      Borrow:= 1;

    end
    else
    begin
      FDigits [i]:= Byte (Borrow);
      Borrow:= 0;
      Break;

    end;

  end;

  while FDigits [FLength- 1]= 0 do
    Dec (FLength);

  Result:= Self;

end;

function TBigInt.ShiftLeft (n: Integer): TBigInt;
var
  TargetPtr, SourcePtr: ^Byte;
  j, i: Integer;

begin
  assert (n> 0);
  assert (n+ Length< MaxLen);

  if IsZero then
    Exit (Self);

  j:= FLength- 1+ n;

  TargetPtr:= @FDigits [0];
  Inc (TargetPtr, j);
  SourcePtr:= @FDigits [FLength- 1];

  for i:= FLength- 1 downto 0 do
  begin
    TargetPtr^:= SourcePtr^;
    Dec (TargetPtr);
    Dec (SourcePtr);

  end;

  TargetPtr:= @FDigits [0];
  for i:= 0 to n- 1 do
  begin
    TargetPtr^:= 0;
    Inc (TargetPtr);

  end;

  Length:= FLength+ n;

  Result:= Self;

end;

function TBigInt.Mul (n: TBigInt): TBigInt;
var
  i: Integer;
  Temp: TBigInt;

begin
  Result:= TBigInt.Create.SetValue (0);

  for i:= 0 to Self.FLength- 1 do
    if Self.FDigits [i]<> 0 then
    begin
      Temp:= n.MulByDigit (Self.FDigits [i]);
      Temp.ShiftLeft (i);
      Result.Add (Temp);
      Temp.Free;

    end;

end;

function TBigInt.NewMul (n: TBigInt): TBigInt;
var
  Temp1, Temp2, Temp3: TBigInt;

begin
  Result:= TBigInt.Create;
  Result.SetValue (0);
  Temp1:= Self.Copy;
  Temp2:= n.Copy;

  while (Temp1.Length>= 1) or (Temp1.FDigits [0]<> 0) do
  begin
    if Odd (Temp1.FDigits [0]) then
      Result.Add (Temp2);

    Temp2.Add (Temp2);
    Temp3:= Temp1.Div2;
    Temp1.Free;
    Temp1:= Temp3;

  end;

  Temp1.Free;
  Temp2.Free;

end;

function TBigInt.ToString: AnsiString;
const
  DigitChar: array [0..9] of char= ('0', '1', '2', '3', '4', '5', '6', '7',
                '8', '9');

var
  i: Integer;
  
begin
  Result:= '';

  for i:= FLength- 1 downto 0 do
    Result:= Result+ DigitChar [FDigits [i]];

  if Result= '' then
    Result:= '0';

end;

constructor TBigInt.Create;
begin
  FLength:= 0;
  FillChar (FDigits, SizeOf(FDigits), 0);

end;

constructor TBigInt.Create (S: PChar);
var
  i: Integer;

begin
  inherited Create;

  Assert (System.Length (S)<= MaxLen);

  Length:= System.Length (S);
  for i:= 0 to System.Length (S)- 1 do
    FDigits [Length- 1- i]:= Ord (S [i])- 48;

  while Length> 0 do
  begin
    if FDigits [FLength- 1]= 0 then
      Dec (FLength)
    else
      Break;

  end;

end;

function TBigInt.SetValue (n: Int64): TBigInt;
begin
  FillChar (FDigits, SizeOf(FDigits), 0);
  FLength:= 0;

  while n> 0 do
  begin
    FDigits [FLength]:= n mod 10;
    n:= n div 10;
    Inc (FLength);

  end;

  Result:= Self;

end;

function TBigInt.GetValue: Int64;
var
  i: Integer;

begin

  Result:= 0;

  for i:= FLength- 1 downto 0 do
  begin
    Result*= 10;
    Result+= FDigits [i];
    if Result< 0 then
    begin
      WriteLn ('Overflow in GetValue!');
      raise Exception.Create ('Overflow in GetValue!');

    end;

  end;

end;

function TBigInt.gcd (b: TBigInt): TBigInt;
var
  a, c: TBigInt;

begin
  a:= Self.Copy;
  b:= b.Copy;

  while not b.IsZero do
  begin
    c:= a.Modulo (b);
    a.Free;
    a:= b;
    b:= c;

  end;
  b.Free;
  Result:= a;

end;

destructor TBigInt.Destroy;
begin
  FLength:= 0;
  
  Inherited;
  
end;

function TBigInt.GetDigit (Index: Integer): Byte;
begin
  if Index< FLength then
    Result:= FDigits [Index]
  else
    Result:= 0;

end;

function TBigInt.GetIsZero: Boolean;
begin
  Result:= Length= 0;

end;

function TBigInt.Copy: TBigInt;
var
  i: Integer;

begin
  Result:= TBigInt.Create;
  Result.Length:= Self.FLength;

  System.Move (FDigits [0], Result.FDigits [0], Sizeof (FDigits [0])* (MaxLen+ 1));
{  for i:= 0 to FLength- 1 do
    Result.FDigits [i]:= FDigits [i];
}

end;

function TBigInt.Divide (n: TBigInt): TBigInt;
var
  i: Integer;
  CompareRes: Integer;
  Temp,
  Lower, Mid,
  Higher: TBigInt;

begin
  if Self.CompareWith (n)< 0 then
  begin
    Result:= TBigInt.Create.SetValue (0);
    Exit;

  end;

  Lower:= TBigInt.Create.SetValue (1);
  Temp:= Lower.NewMul (n);

  while Self.CompareWith (Temp)> 0 do
  begin
    Lower.Mul2;
    Temp.Free;
    Temp:= Lower.NewMul (n);

  end;

  Higher:= Lower;
  Lower:= Lower.Div2;

  Mid:= nil;
  Result:= nil;

  while 0<= Higher.CompareWith (Lower) do
  begin
    Temp.Free;
    Mid.Free;

    Temp:= Lower.Copy.Add (Higher);
    Mid:= Temp.Div2;
    Temp.Free;
    Temp:= n.NewMul (Mid);

    CompareRes:= Self.CompareWith (Temp);

    if 0< CompareRes then
    begin
      Result.Free;
      Result:= Lower.Copy;
      Lower.Free;
      Lower:= Mid.Copy.Incr;

    end
    else if CompareRes< 0 then
    begin
      Higher.Free;
      Higher:= Mid.Copy.Decr;

    end
    else
    begin
      Result.Free;
      Result:= Mid.Copy;
      Break;

    end;

  end;

  if Result= nil then
    Result:= Lower.Copy;

  Higher.Free;
  Lower.Free;
  Mid.Free;
  Temp.Free;

end;

function TBigInt.SQRT: TBigInt;
var
  Temp,
  Lower,
  Mid,
  Higher: TBigInt;

begin
  Lower:= TBigInt.Create;
  Lower.SetValue (1);
  Higher:= Lower.Copy;
  Temp:= Lower.Mul (Lower);

  while Self.CompareWith (Temp)> 0 do
  begin
    Lower.Free;
    Lower:= Higher.Copy;
    Higher.Free;
    Higher:= Lower.MulByDigit (2);

    Temp.Free;
    Temp:= Higher.Mul (Higher);

  end;

  Temp.Free;
  Temp:= Lower.Mul (Lower);

  while Higher.CompareWith (Lower)>= 0 do
  begin
    Temp.Free;
    Temp:= Lower.Copy;
    Temp.Add (Higher);
    Mid:= Temp.Div2;

    Temp.Free;
    Temp:= Mid.Mul (Mid);

    case Self.CompareWith (Temp) of
    +1:
      begin
        Lower.Free;
        Lower:= Mid.Incr;

      end;
     0:
        Break;

    -1:
      begin
        Higher.Free;
        Higher:= Mid.Decr;

      end;

    end;

  end;

  Temp:= Lower.Copy;
  Temp.Add (Higher);
  Mid:= Temp.Div2;

  Temp.Free;
  Temp:= Mid.Mul (Mid);

  Result:= Mid;
  if Self.CompareWith (Temp)< 0 then
    Result.Decr;

  if Higher<> Mid then
    Higher.Free;

  if Lower<> Mid then
    Lower.Free;

  Temp.Free;

end;

function TBigInt.MulByDigit (n: Integer): TBigInt;
var
  i, Carry: Integer;

begin
  Assert (n< 10);
  Result:= TBigInt.Create;

  Result.Length:= FLength;
  Carry:= 0;
  for i:= 0 to FLength- 1 do
  begin
    Inc (Carry, FDigits [i]* n);
    Result.FDigits [i]:= Carry mod 10;
    Carry:= Carry div 10;

  end;

  while Carry> 0 do
  begin
    Result.Length:= Result.Length+ 1;
    Result.FDigits [Result.FLength- 1]:=
            Carry mod 10;
    Carry:= Carry div 10;

  end;

end;

function TBigInt.Pow (n: Integer): TBigInt;
var
  Temp, Temp1: TBigInt;

begin
  Assert (0<= n);

  if n= 0 then
  begin
    Result:= TBigInt.Create;
    Result.SetValue (1);

  end
  else if n= 1 then
    Result:= Self.Copy

  else
  begin
    Temp:= Self.Pow (n div 2);
    Temp1:= Temp.Copy;
    Result:= Temp.Mul (Temp1);
    Temp1.Free;
    Temp.Free;

    if n mod 2= 1 then
    begin
      Temp:= Result.Mul (Self);
      Result.Free;
      Result:= Temp;

    end;

  end;

end;

function TBigInt.Div2: TBigInt;
var
  i, j, Borrow: Integer;

begin
  Result:= TBigInt.Create;

  Borrow:= 0;
  i:= FLength- 1;

  while i>= 0 do
  begin

    if i= FLength- 1 then
    begin

      if FDigits [i]< 2 then
      begin
        Result.Length:= FLength- 1;
        j:= FLength- 2;
        Borrow:= FDigits [i];
        if 1<= i then
          Borrow:= Borrow* 10+ FDigits [i- 1];

        if 0<= j then
          Result.FDigits [j]:= Borrow div 2;

        Borrow:= Borrow mod 2;
        Dec (i);

      end
      else
      begin
        Result.Length:= FLength;

        j:= Result.Length- 1;
        Borrow:= FDigits [i];
        Result.FDigits [j]:= Borrow div 2;
        Borrow:= Borrow mod 2;

      end;

    end
    else
    begin
      Borrow:= Borrow* 10+ FDigits [i];
      Result.FDigits [j]:= Borrow div 2;
      Borrow:= Borrow mod 2;

    end;

    Dec (j);
    Dec (i);

  end;

end;

function TBigInt.Mul2: TBigInt;
var
  i, Carry: Integer;
  n: Integer;

begin
  n:= 2;
  Result:= Self;

  Carry:= 0;
  for i:= 0 to FLength- 1 do
  begin
    Inc (Carry, FDigits [i]* n);
    Result.FDigits [i]:= Carry mod 10;
    Carry:= Carry div 10;

  end;

  while Carry> 0 do
  begin
    Result.Length:= Result.Length+ 1;
    Result.FDigits [Result.FLength- 1]:=
            Carry mod 10;
    Carry:= Carry div 10;

  end;

end;

end.
