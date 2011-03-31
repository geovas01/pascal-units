unit ImageRuleUnit;

interface
uses
  StreamUnit, SysUtils, CollectionUnit, FMLImage;
  
type

  TRule= class (TObject)
  private
    FCount: Integer;
    FProcName: String;

    procedure Apply (Image: TFMLImage);

  public
    property Count: Integer read FCount;
    property ProcName: String read FProcName;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile (FileStream: TMyFileStream);

  end;

  TRuleCollection= class (TBaseCollection)
  private
    function GetRule(Index: Integer): TRule;
  public
    property Rule [Index: Integer]: TRule read GetRule;

    procedure LoadFromFile (FileStream: TMyFileStream); overload;
    procedure LoadFromFile (FileName: String); overload;
    function Apply (ImageCollection: TImageCollection): TImageCollection;
    
  end;

implementation

uses GeometryUnit;

{ TRule }

procedure TRule.Apply (Image: TFMLImage);

begin
{
  if FProcName= 'ERODE' then
  begin
    Image.Erode ()
  end
  else
  begin

  end;
}
end;

constructor TRule.Create;
begin
  inherited ;

end;

destructor TRule.Destroy;
begin
  inherited Free;
  
end;

procedure TRule.LoadFromFile (FileStream: TMyFileStream);
var
  S: String;
  
begin
  S:= FileStream.ReadLine;
  FProcName:= UpperCase (Copy (S, 1, Pos (':', S)- 1));
  FCount:= StrToInt (Copy (S, Pos (':', S)+ 1, Length (S)));

end;

{ TRuleCollection }

function TRuleCollection.Apply(
  ImageCollection: TImageCollection): TImageCollection;
var
  i, j: Integer;
  ActiveImage, ResultedImage: TFMLImage;
  TL, BR: TPoint;

begin
  Result:= TImageCollection.Create;

  TL:= TPoint.Create (0, 0);
  BR:= TPoint.Create (0, 0);

  for i:= 0 to ImageCollection.Size- 1 do
  begin
    ActiveImage:= ImageCollection.Image [i];
    BR.r:= ActiveImage.Row; BR.c:= ActiveImage.Column;
    
    ResultedImage:= ActiveImage.Copy (TL, BR);
    
    for j:= 0 to Size- 1 do
      Rule [j].Apply (ResultedImage);
    Result.AddImage (ResultedImage);

  end;
end;

function TRuleCollection.GetRule (Index: Integer): TRule;
begin
  Result:= Member [Index] as TRule;

end;

procedure TRuleCollection.LoadFromFile (FileStream: TMyFileStream);
var
  i: Integer;
  NewRule: TRule;
  
begin
  i:= StrToInt (FileStream.ReadLine);

  while 0< i do
  begin
    NewRule:= TRule.Create;
    NewRule.LoadFromFile (FileStream);
    Add (NewRule);
    
  end;
  
end;

procedure TRuleCollection.LoadFromFile(FileName: String);
var
  FileStream: TMyFileStream;

begin
  FileStream:= TMyFileStream.Create (FileName, fmOpenRead);
  try
    LoadFromFile (FileStream);
    
  finally
    FileStream.Free;
  end;
  
end;

end.
