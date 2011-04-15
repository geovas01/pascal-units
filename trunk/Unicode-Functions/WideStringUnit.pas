unit WideStringUnit;
{$mode objfpc}

interface

{
  The function does not change the content of CharPtr but it might increases its value.
}
function ReadWideStringFromACharArray (var CharPtr: PChar; Len: Integer): WideString;
function ReadWideStringFromString (const Source: String): WideString;
function ReadWideString (var FdFile: TextFile): WideString;
function WideStrPos (SubStr, Str: WideString): Integer; 
function WideStrCopy (var Str: WideString; Index, Len: Integer): WideString;
procedure WideStrDelete (var Str: WideString; Index, Len: Integer);
function WideStringCompare (var Str1, Str2: WideString): Integer;

implementation
uses
  Math;
  
function ReadWideStringFromACharArray (var CharPtr: PChar; Len: Integer): WideString;

  function ReadAWideChar: WideChar;
  var
    c1, c2, c3, c4: Char;
    b1, b2, b3, b4: Byte;
    Value: Integer;

  begin
    c1:= CharPtr^;
    Inc (CharPtr);
    Dec (Len);
    b1:= Ord (c1);

    if b1 and 128= 0 then
      Result:= WideChar (b1)
    else if b1 and 32= 0 then
    begin
      c2:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);
      b2:= Ord (c2);
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64);
      Value:= b2+ b1 shl 6; 
      Result:= WideChar (Value);

    end
    else if b1 and 16= 0 then
    begin
      c2:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);
      
      b2:= Ord (c2);
      c3:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);
      
      b3:= Ord (c3);
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32);
      Value:= b3+ b2 shl 6+ b1 shl 12;
      Result:= WideChar (Value);

    end
    else if b1 and 8= 0 then
    begin
      c2:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);

      b2:= Ord (c2);
      c3:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);

      b3:= Ord (c3);
      c4:= CharPtr^;
      Inc (CharPtr);
      Dec (Len);

      b4:= Ord (c4);
      b4:= b4 xor 128;
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32+ 16);
      Value:= b4+ b3 shl 6+ b2 shl 12+ (b1 shl 18);
      Result:= WideChar (Value);

    end
    else
      Result:= WideChar (' ');
  end;

begin
  Result:= '';
  while 0< Len do
    Result:= Result+ ReadAWideChar;
    
end;

function ReadWideStringFromString (const Source: String): WideString;
var
  ChPtr: PChar;

begin
  ChPtr:= @Source [1];
  Result:= ReadWideStringFromACharArray (ChPtr, Length (Source));

end;

function ReadWideString (var FdFile: TextFile): WideString;

  function ReadAWideChar: WideChar;
  var
    c1, c2, c3, c4: Char;
    b1, b2, b3, b4: Byte;
    Value: Integer;

  begin
    Read (FdFile, c1);
    b1:= Ord (c1);

    if b1 and 128= 0 then
      Result:= WideChar (b1)
    else if b1 and 32= 0 then
    begin
      Read (FdFile, c2);
      b2:= Ord (c2);
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64);
      Value:= b2+ b1 shl 6; 
      Result:= WideChar (Value);

    end
    else if b1 and 16= 0 then
    begin
      Read (FdFile, c2);
      b2:= Ord (c2);
      Read (FdFile, c3);
      b3:= Ord (c3);
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32);
      Value:= b3+ b2 shl 6+ b1 shl 12;
      Result:= WideChar (Value);

    end
    else if b1 and 8= 0 then
    begin
      Read (FdFile, c2);
      b2:= Ord (c2);
      Read (FdFile, c3);
      b3:= Ord (c3);
      Read (FdFile, c4);
      b4:= Ord (c4);
      b4:= b4 xor 128;
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32+ 16);
      Value:= b4+ b3 shl 6+ b2 shl 12+ (b1 shl 18);
      Result:= WideChar (Value);

    end
    else
      Result:= WideChar (' ');
      
  end;

var
  Temp: WideChar;

begin
  Result:= '';
  Temp:= ReadAWideChar;
  while Temp<> #$D do
  begin
    Result:= Result+ Temp;
    Temp:= ReadAWideChar;
    
  end;
  if ReadAWideChar<> #$A then
    ;

end;

function WideStrPos (SubStr, Str: WideString): Integer;
var
  i, j, SubStrLen: Integer;
  Flag: Boolean;

begin
  SubStrLen:= Length (SubStr);

  for i:= 1 to Length (Str)- SubStrLen+ 1 do
  begin
    Flag:= True;

    for j:= 1 to SubStrLen do
      if Str [i+ j- 1]<> SubStr [j] then
      begin
        Flag:= False;
        Break;

      end;

    if Flag then
    begin
      Result:= i;
      Exit;

    end;


  end;
   Result:= 0;
   
end;

function WideStrCopy (var Str: WideString; Index, Len: Integer): WideString;
var
  i: Integer;
  
begin
  Result:= '';

  for i:= Max (1, Index) to Min (Index+ Len- 1, Length (Str)) do
    Result:= Result+ Str [i];

end;

procedure WideStrDelete (var Str: WideString; Index, Len: Integer);
begin
  Str:= WideStrCopy (Str, 1, Index- 1)+
    WideStrCopy (Str, Index+ Len, Length (Str));
    
end;

function WideStringCompare (var Str1, Str2: WideString): Integer;
var
  i: Integer;

begin
  if Length (Str1)< Length (Str2) then
    Exit (-1)
  else if Length (Str2)< Length (Str1) then
    Exit (1)
  else
    for i:= 1 to Length (Str1) do
      if Str1 [i]< Str2 [i] then
        Exit (-1)
      else if Str2 [i]< Str1 [i] then
        Exit (1);

  Result:= 0;

end;

end.
