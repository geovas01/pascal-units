unit MyFileStreamUnit;

interface
uses
  Classes, SysUtils;

type
  ExpectFailed= class (Exception)
  end;

  { TMyFileStream }

  TMyFileStream= class (TFileStream)
  private
    function GetEoS: Boolean;
  public
    property EoS: Boolean read GetEoS;

    procedure WriteString (Str: String);
    function ReadString: String;
    function ReadLn: String;
    procedure WriteLn (Str: String);
    
    function ReadNextWord: String;
    function GetNextChar: Char;

    function ReadByte: Byte;
    
    procedure Expect (Str: String);
    
  end;
  

implementation

{ TMyFileStream }

function TMyFileStream.ReadString: String;
var
  Len: Integer;
  

begin
  Read (Len, 4);
  SetLength (Result, Len);
  Read (Result [1], Len);
  
end;

function TMyFileStream.ReadLn: String;
var
  Ch: Char;
  
begin
  Read (Ch, 1);
  Result:= '';
  
  while Ch<> #10 do
  begin
    Result:= Result+ Ch;
    Read (Ch, 1);

  end;
  
end;

procedure TMyFileStream.WriteLn (Str: String);
const
  NewLine: Char= #10;
var
  i: Integer;
  
begin
  for i:= 1 to Length (Str) do
    Write (Str [i], 1);
  Write (NewLine, 1);
  
end;

function TMyFileStream.ReadNextWord: String;
var
  Ch: Char;
  
begin
  Ch:= Char (ReadByte);
  while Ch in [#32, #9, #10, #13] do
    Ch:= Char (ReadByte);

  Result:= '';
  
  while not (Ch in [#32, #9, #10, #13]) do
  begin
    Result:= Result+ Ch;
    Ch:= Char (ReadByte);
    
  end;

end;

function TMyFileStream.GetNextChar: Char;
begin
  Result:= Char (ReadByte);
  while Result in [#32, #9, #10, #13] do
    Result:= Char (ReadByte);
    
end;

procedure TMyFileStream.Expect (Str: String);
begin
  if UpperCase (ReadNextWord)<> UpperCase (Str) then
    raise ExpectFailed.Create (Str);
    
end;

function TMyFileStream.GetEoS: Boolean;
begin
  Result:= Position< Size;

end;

procedure TMyFileStream.WriteString (Str: String);
var
  Len: Integer;

begin
  Len:= Length (Str);
  Write (Len, 4);
  Write (Str [1], Len);
  
end;

function TMyFileStream.ReadByte: Byte;
begin
  Read (Result, 1);
  
end;

end.
