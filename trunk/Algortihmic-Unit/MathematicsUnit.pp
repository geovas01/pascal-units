unit MathematicsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function Cnr (const n, m: Int64): Int64;
function gcd (a, b: Int64; var x, y: Int64): Int64;

implementation

function gcd (a, b: Int64; var x, y: Int64): Int64;
var
  x1, y1: Int64;

begin

  if a= 0 then
  begin
    x:=0;
    y:=1;
    Result:= b;
    Exit;

  end;

  Result:= gcd (b mod a, a, x1, y1);
  x:= y1- b div a* x1;
  y:= x1;

end;

function Cnr (const n, m: Int64): Int64;

  function gcd (a, b: Int64): Int64;
  begin

    if a< b then
    begin
      a:= a xor b;
      b:= a xor b;
      a:= a xor b;

    end;

    while a mod b<> 0 do
    begin
      Result:= a mod b;
      a:= b;
      b:= Result;

    end;
    Result:= b;

  end;

var
  Numerator, Demoninator: Int64;
  i, j: Int64;
  k, Num, Dem: Int64;

begin
  if n< m then
    Exit (0);

  Result:= 1;

  if n< 2* m then
  begin
    Result:= Cnr (n, n- m);
    Exit;

  end
  else
  begin
    Numerator:= 1;
    Demoninator:= 1;

    j:= 1;
    i:= n- m+ 1;

    while i<= n do
    begin
      k:= gcd (i, j);
      Num:= i div k;
      Dem:= j div k;

      k:= gcd (Numerator, Demoninator);
      Numerator:= Numerator div k;
      Demoninator:= Demoninator div k;

      k:= gcd (Numerator, Dem);
      Numerator:= Numerator div k;
      Dem:= Dem div k;

      k:= gcd (Num, Demoninator);
      Num:= Num div k;
      Demoninator:= Demoninator div k;

      Numerator:= Numerator* Num;
      Demoninator:= Demoninator* Dem;

      Inc (j);
      Inc (i);

    end;

    Result:= Numerator div Demoninator;

  end;

end;


end.

