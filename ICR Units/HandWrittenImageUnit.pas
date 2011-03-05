unit HandWrittenImageUnit;

interface
uses
  FMLImage;
  
type
  THandwrittenImage= class (TFMLImage)
  private

  public

    constructor Create (AnImage: TFMLImage);
    procedure Free;

    function GetAsFMLImage: TFMLImage;

  end;
  
implementation

uses
  Math, GeometryUnit;

{ THandwrittenImage }

constructor THandwrittenImage.Create (AnImage: TFMLImage);
var
  r, c: Integer;
  SPtr, TPtr: PInteger;
  
begin
  inherited Create;
  
  IsBlank:= mbFalse;
  Row:= AnImage.Row;
  Column:= AnImage.Column;
  Pattern:= AnImage.Pattern;
  FImageKind:= AnImage.ImageKind;
  FImageType:= AnImage.ImageType;

  for r:= 0 to Row- 1 do
  begin
    SPtr:= AnImage.ScanLine [r];
    TPtr:= ScanLine [r];
    Move (SPtr^, TPtr^, SizeOf (Integer)* Column);

  end;

end;

procedure THandwrittenImage.Free;
begin
  inherited Free;
  
end;

function THandwrittenImage.GetAsFMLImage: TFMLImage;
var
  TL, BR: TPoint;
  
begin
  TL:= TPoint.Create (0, 0);
  BR:= TPoint.Create (Row- 1, Column- 1);
   
  Result:= Self.Copy (TL, BR);
  
end;

procedure AllocateArrayArrayInt (Row, Col: Integer; var Data: TArrayofArrayofInt);
var
  r: Integer;

begin
  if Data= nil then
    SetLength (Data, Row);

  if Length (Data)< Row then
  begin
    for r:= 0 to High (Data) do
      SetLength (Data [r], 0);

    SetLength (Data, Row);
    for r:= 0 to Row- 1 do
      SetLength (Data [r], Col);

  end;

  if Length (Data [0])< Col then
    for r:= 0 to High (Data) do
      SetLength (Data [r], Col);

end;

end.
