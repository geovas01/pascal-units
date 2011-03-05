unit GeometryUnit;

interface
uses
  SysUtils, CollectionUnit;
  
type
  EPointIsNotInitialized= class (Exception);

  TPolarPoint= class;
  
  TPoint= class
  private
    Fr, Fc: Integer;
    procedure Setr (const Value: Integer);
    procedure Setc (const Value: Integer);
    function GetR: Integer;
    function GetC: Integer;

  public
    IsValid: Boolean;
{    property x: Integer read GetX write Setx;
    property y: Integer read GetY write Sety;
    }
    property r: Integer read Fr write Fr;//r
    property c: Integer read Fc write Fc;//c

    function Scale (Ratio: Extended): TPoint; overload;
    function Scale (RatioR, RationC: Extended): TPoint; overload;
    function Copy: TPoint;
    function ToString: String;
    function ToPolar: TPolarPoint;
    function StrToTPoint (Value: String): TPoint;
    function IsSame (Point: TPoint): Boolean; overload;
    function IsSame (PointR, PointC: Integer): Boolean; overload;

    function Rotate (Angle: Extended): TPoint; overload;
    function Rotate (Center: TPoint; Angle: Extended): TPoint; overload;
    function Move (Delta: TPoint): TPoint;overload;
    function Move (DeltaR, DeltaC: Integer): TPoint;overload;
    function Strach (Coef: Extended): TPoint;
    function CalcDistance (AnotherPoint: TPoint): Extended;
    function CalcDistance2 (AnotherPoint: TPoint): Integer;

    constructor Create; overload;
    constructor Create (Str: String);overload;
    constructor Create (r1, c1: Integer);overload;
    destructor Destroy; override;
    
  end;

  TPolarPoint= class
  private
    Fr, FTeta: Extended;
    procedure Setr (const Value: Extended);
    procedure SetTeta (const Value: Extended);
    function Getr: Extended;
    function GetTeta: Extended;
  public
    property r: Extended read Getr write Setr;
    property Teta: Extended read GetTeta write SetTeta;

    function Scale (Ratio: Extended): TPolarPoint; overload;
    function Scale (RatioX, RationY: Extended): TPolarPoint; overload;
    function Copy: TPolarPoint;
    function ToString: String;
    function StrToTPoint (Value: String): TPolarPoint;
    function ToPoint: TPoint;
    function IsSame (Point: TPolarPoint): Boolean; overload;
    function IsSame (Pointr, PointTeta: Integer): Boolean; overload;

    function Rotate (Angle: Extended): TPolarPoint;
    function Move (Delta: TPoint): TPolarPoint;overload;
    function Move (DeltaX, DeltaY: Integer): TPolarPoint;overload;
    function Strach (Coef: Extended): TPolarPoint;
    function CalcDistance (AnotherPoint: TPolarPoint): Extended;

    constructor Create;overload;
    constructor Create (Str: String);overload;
    constructor Create (R, Teta: Extended);overload;
    
  end;
  
  TPointCollection= class (TBaseCollection)
  private
    FMin: TPoint;
    FMax: TPoint;
    
    function GetPoint(Index: Integer): TPoint;

  public
    property Point [Index: Integer]: TPoint read GetPoint;
    property Max: TPoint read FMax;
    property Min: TPoint read FMin;
    
    constructor Create;
    destructor Destroy; override;

    procedure AddPoint (NewPoint: TPoint);
  end;
  
  TBox= class (TObject)
  private
    FTopLeft: TPoint;
    FBotRight: TPoint;
    
  public
    property TopLeft: TPoint read FTopLeft;
    property BotRight: TPoint read FBotRight;
    
    constructor Create (TL, BR: TPoint); overload;
    constructor Create (TIndex, LIndex, BIndex, RIndex: Integer); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Move (Delta: TPoint); overload;
    procedure Move (DeltaX, DeltaY: Integer); overload;

    procedure LoadFromFile (var InputFile: TextFile);
    
  end;
  
  TBoxCollection= class (TBaseCollection) 
  private
    function GetBox (Index: Integer): TBox;

  public
    property Box [Index: Integer]: TBox read GetBox;

  end;

  TVector= class (TBaseCollection)
  private
  public
  
  end;
  function CalcArcTan (x, y: Integer): Extended;

implementation

uses
  MyTypes;

function CalcArcTan (x, y: Integer): Extended;
begin
  if Abs (x)< 1e-10 then
  begin
  //??!!
    if x* y< 0 then
      Result:= 3* Pi/ 2
    else
      Result:= Pi/ 2; 
    Exit;
  end;

  Result:= ArcTan (y/ x);
  if x< 0 then
    Result:= Result+ Pi;

end;


{ TPoint }

constructor TPoint.Create;
begin
  inherited;
  IsValid:= False;
  
end;

function TPoint.Copy: TPoint;
begin
  Result:= TPoint.Create (Fr, Fc);
  
end;

constructor TPoint.Create (r1, c1: Integer);
begin
  inherited Create;

  r:= r1;
  c:= c1;
  IsValid:= True;
  
end;

function TPoint.GetR: Integer;
begin
  if IsValid then
    Result:= Fr
  else
    raise EPointIsNotInitialized.Create ('Point is not Initialized!');
    
end;

function TPoint.GetC: Integer;
begin
  if IsValid then
    Result:= Fc
  else
    raise EPointIsNotInitialized.Create ('Point is not Initialized!');
    
end;

procedure TPoint.SetR (const Value: Integer);
begin
  Fr:= Value;

end;

procedure TPoint.SetC (const Value: Integer);
begin
  Fc:= Value;

end;

function TPoint.ToString: String;
begin
  Result:= IntToStr (r)+ ': '+ IntToStr (c);

end;

function TPoint.StrToTPoint (Value: String): TPoint;
{
var
  XStr, YStr: String;
  }
begin
  raise Exception.Create ('Not Implemented!');
  {
  XStr:= Value.Substring (1, Pos (':', Value)- 1);
  Delete (Value, 1, Pos (':', Value));
  YStr:= Value;

  x:= StrToInt (XStr);
  y:= StrToInt (YStr);

  Result:= Self;
}
end;

function TPoint.IsSame (Point: TPoint): Boolean;
const
  SametinessThrShd: Integer= 1;

begin
  Result:= (Abs (Point.r- r)< SametinessThrShd) and (Abs (Point.c- c)< SametinessThrShd);
  
end;

function TPoint.IsSame (PointR, PointC: Integer): Boolean;
const
  SametinessThrShd: Integer= 1;
begin
  Result:= (Abs (PointR- Self.r)< SametinessThrShd) and (Abs (PointC- Self.c)< SametinessThrShd);
  
end;

constructor TPoint.Create (Str: String);
var
  RStr, CStr: String;

begin
  inherited Create;

  CStr:= System.Copy (Str, 0, Pos (':', Str)- 1);
  Delete (Str, 1, Pos (':', Str));
  RStr:= Str;

  r:= StrToInt (RStr);
  c:= StrToInt (CStr);
  IsValid:= True;
{
  XStr:= Str.Substring (0, Pos (':', Str)- 1);
  Delete (Str, 0, Pos (':', Str));
  YStr:= Str;

  x:= StrToInt (XStr);
  y:= StrToInt (YStr);
  IsValid:= True;
  }
end;

function TPoint.Rotate (Angle: Extended): TPoint;
var
  LocalR, LocalC: Integer;
  
begin
  Result:= Self;
  LocalR:= r;
  LocalC:= c;
  Result.Fr:= Round (LocalR* Cos (Angle)- LocalC* Sin (Angle));
  Result.Fc:= Round (LocalR* Sin (Angle)+ LocalC* Cos (Angle));

end;

function TPoint.Move (Delta: TPoint): TPoint;
begin
  Result:= Self;
  Result.Fr:= Result.Fr+ Delta.Fr;
  Result.Fc:= Result.Fc+ Delta.Fc;
  
end;

function TPoint.Move (DeltaR, DeltaC: Integer): TPoint;
begin
  if not IsValid then
    Result:= nil
  else
    Result:= Self;

  Inc (Fr, DeltaR);
  Inc (Fc, DeltaC);
    
end;

function TPoint.Strach (Coef: Extended): TPoint;
begin
  Result:= Self;
  Result.r:= Round (Coef* Result.r);
  Result.c:= Round (Coef* Result.c);
  
end;

function TPoint.Scale (Ratio: Extended): TPoint;
begin
  r:= Round (r* Ratio);
  c:= Round (c* Ratio);
  Result:= Self;
  
end;

function TPoint.Scale (RatioR, RationC: Extended): TPoint;
begin
  r:= Round (r* RatioR);
  c:= Round (c* RationC);
  Result:= Self;
  
end;

function TPoint.CalcDistance (AnotherPoint: TPoint): Extended;
begin
  Result:= Sqrt (Sqr (Self.r- AnotherPoint.r)+ Sqr (Self.c- AnotherPoint.c));
  
end;


destructor TPoint.Destroy;
begin
  IsValid:= False;

  inherited;
  
end;


function TPoint.ToPolar: TPolarPoint;
begin
  Result:= TPolarPoint.Create (Sqrt (Sqr (Fr)+ Sqr (Fc)),
      CalcArcTan (Fr, Fc));
end;

function TPoint.CalcDistance2(AnotherPoint: TPoint): Integer;
begin
  Result:= Sqr (Self.r- AnotherPoint.r)+ Sqr (Self.c- AnotherPoint.c);

end;

{ TPolarPoint }
function TPoint.Rotate (Center: TPoint; Angle: Extended): TPoint;
var
  x, y: Integer;

begin
  Result:= Self;
  r:= Fr- Center.Fr;
  c:= Fc- Center.Fc;

  Result.r:= Round (r* Cos (Angle)- c* Sin (Angle))+ Center.r;
  Result.c:= Round (r* Sin (Angle)+ c* Cos (Angle))+ Center.c;
  
end;

{ TPolarPoint }

function TPolarPoint.CalcDistance (AnotherPoint: TPolarPoint): Extended;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.Copy: TPolarPoint;
begin
  Result:= TPolarPoint.Create (Fr, FTeta);
end;

constructor TPolarPoint.Create(R, Teta: Extended);
begin
  Fr:= r;
  FTeta:= Teta;
end;

constructor TPolarPoint.Create (Str: String);
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

constructor TPolarPoint.Create;
begin
  inherited Create;
end;

function TPolarPoint.Getr: Extended;
begin
  Result:= Fr;
end;

function TPolarPoint.GetTeta: Extended;
begin
  Result:= FTeta;
end;

function TPolarPoint.IsSame(Point: TPolarPoint): Boolean;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.IsSame(Pointr, PointTeta: Integer): Boolean;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.Move (Delta: TPoint): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.Move (DeltaX, DeltaY: Integer): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.Rotate (Angle: Extended): TPolarPoint;
begin
  FTeta:= FTeta+ Angle;
  Result:= Self;
  
end;

function TPolarPoint.Scale (RatioX, RationY: Extended): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.Scale (Ratio: Extended): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

procedure TPolarPoint.Setr (const Value: Extended);
begin
  Fr:= Value;
end;

procedure TPolarPoint.SetTeta (const Value: Extended);
begin
  FTeta:= Value;
end;

function TPolarPoint.Strach (Coef: Extended): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.StrToTPoint(Value: String): TPolarPoint;
begin
  raise Exception.Create ('Not Implemented Yet!');
end;

function TPolarPoint.ToPoint: TPoint;
begin
  Result:= TPoint.Create (Round (Fr* Cos (FTeta)),
                           Round (Fr* Sin (FTeta)));  
end;

function TPolarPoint.ToString: String;
begin
  Result:= FloatToStr (Fr)+ ':'+ FloatToStr (FTeta);
  
end;


{ TPointCollection }

procedure TPointCollection.AddPoint (NewPoint: TPoint);

begin
  if Size= 0 then
  begin
    FMin.Fr:= NewPoint.Fr;
    FMin.Fc:= NewPoint.Fc;
    FMax.Fr:= NewPoint.Fr;
    FMax.Fc:= NewPoint.Fc;

  end
  else
  begin
    if FMax.r< NewPoint.r then
      FMax.r:= NewPoint.r
    else if NewPoint.r< FMin.r then
      FMin.r:= NewPoint.r;

    if FMax.c< NewPoint.c then
      FMax.c:= NewPoint.c
    else if NewPoint.c< FMin.c then
      FMin.c:= NewPoint.c;

  end;

  inherited Add (NewPoint);

end;

constructor TPointCollection.Create;
begin
  inherited Create;

  FMin:= TPoint.Create (MaxInt, MaxInt);
  FMax:= TPoint.Create (-MaxInt, -MaxInt);
  
end;

destructor TPointCollection.Destroy;
begin
  FMax.Free;
  FMin.Free;
  FSize:= 0;

  inherited;
  
end;

function TPointCollection.GetPoint (Index: Integer): TPoint;
begin
  Result:= Member [Index] as TPoint;
  
end;

{ TBox }

constructor TBox.Create (TL, BR: TPoint);
begin
  inherited Create;

  FTopLeft:= TL.Copy;
  FBotRight:= BR.Copy;
  
end;

destructor TBox.Destroy;
begin
  if FTopLeft<> nil then
    FTopLeft.Free;

  if FBotRight<> nil then
    FBotRight.Free;

  inherited;
    
end;

procedure TBox.Move(Delta: TPoint);
begin
  FTopLeft.Move (Delta);
  FBotRight.Move (Delta);
  
end;

procedure TBox.LoadFromFile(var InputFile: TextFile);
var
  S: String;

begin
  ReadLn (InputFile, S);
  FTopLeft:= TPoint.Create (S);
  Readln (InputFile, S);
  FBotRight:= TPoint.Create (S);
  
end;

procedure TBox.Move(DeltaX, DeltaY: Integer);
begin
  FTopLeft.Move (DeltaX, DeltaY);
  FBotRight.Move (DeltaX, DeltaY);
  
end;

constructor TBox.Create;
begin
  inherited;

  FTopLeft:= nil;
  FBotRight:= nil;
  
end;

constructor TBox.Create (TIndex, LIndex, BIndex, RIndex: Integer);
begin
  inherited Create;

  FTopLeft:= TPoint.Create (TIndex, LIndex);
  FBotRight:= TPoint.Create (BIndex, RIndex);
  
end;

{ TBoxCollection }

function TBoxCollection.GetBox (Index: Integer): TBox;
begin
  Result:= Member [Index] as TBox;

end;

end.
