unit MyFileStreamerTokenizerUnit;

interface
uses
  SysUtils, Classes;
  
type
  PString= String;
  TChunkID= String [4];

  EExpectFaild= class (Exception)
  public
    constructor Create (ExpectedString, VisitedString: String);

  end;

  EInvalidFile= class (Exception);

  EInvalidReadSize= class (Exception)
  public
    constructor Create (Size: Integer);

  end;

  TMyFileStream= class (TFileStream)
  public
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadChunkID: TChunkID;
    function ReadSignedBytes (NumberOfBytesToBeRead: Integer): Integer;
    function ReadUnsignedBytes (NumberOfBytesToBeRead: Integer): Cardinal;

    procedure Expect (S: String);

  end;


implementation

{ EExpectFaild }

constructor EExpectFaild.Create(ExpectedString, VisitedString: String);
begin
  inherited Create ('Expected= '+ ExpectedString+ ' but Visited= '+ VisitedString);
  
end;

{ TMyFileStream }

procedure TMyFileStream.Expect (S: String);
var
  i: Integer;
  b: Byte;
  
begin
  for i:= 1 to Length (S) do
  begin
    b:= ReadByte;
    if b<> Ord (S [i]) then
      raise EExpectFaild.Create (S [i], Chr (b));
  end;

end;

function TMyFileStream.ReadByte: Byte;
begin
  if Position= Size then
  begin
    Result:= 0;
    Exit;

  end;

  ReadBuffer (Result, 1);

end;

function TMyFileStream.ReadCardinal: Cardinal;
var
  ResPtr: PByte;

begin
  Result:= 0;
  ResPtr:= @Result;

  ResPtr^:= ReadByte;

  Inc (ResPtr);
  ResPtr^:= ReadByte;

  Inc (ResPtr);
  ResPtr^:= ReadByte;

  Inc (ResPtr);
  ResPtr^:= ReadByte;

end;

function TMyFileStream.ReadChunkID: TChunkID;
var
  i: Integer;

begin
  Result:= '';

  for i:= 1 to 4 do
    Result:= Result+ Chr (ReadByte);
    
end;

function TMyFileStream.ReadSignedBytes (
  NumberOfBytesToBeRead: Integer): Integer;
var
  ResPtr: PByte;
  i: Integer;

begin
  ResPtr:= @Result;
  if 4< NumberOfBytesToBeRead then
    raise EInvalidReadSize.Create (NumberOfBytesToBeRead);

  for i:= 1 to NumberOfBytesToBeRead do
  begin
    ResPtr^:= ReadByte;
    Inc (ResPtr);
     
  end;

  if NumberOfBytesToBeRead< 4 then
    Dec (Result, 1 shl (8* NumberOfBytesToBeRead- 1));

end;

function TMyFileStream.ReadUnSignedBytes(
  NumberOfBytesToBeRead: Integer): Cardinal;
var
  ResPtr: PByte;
  i: Integer;

begin
  ResPtr:= @Result;
  if 4< NumberOfBytesToBeRead then
    raise EInvalidReadSize.Create (NumberOfBytesToBeRead);

  for i:= 1 to NumberOfBytesToBeRead do
  begin
    ResPtr^:= ReadByte;
    Inc (ResPtr);
     
  end;

end;

function TMyFileStream.ReadWord: Word;
var
  ResPtr: PByte;

begin
  Result:= 0;
  ResPtr:= @Result;
  
  ResPtr^:= ReadByte;
  Inc (ResPtr);
  ResPtr^:= ReadByte;

end;

{ EInvalidReadSize }

constructor EInvalidReadSize.Create (Size: Integer);
begin
  inherited Create ('Size= '+ IntTostr (Size) + 'is not a valid argument'+
    ' for function ReadSignedBytes or ReadUnsignedBytes');
 
end;

end.
 