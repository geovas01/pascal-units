unit VectorUnit;

interface
uses
  CollectionUnit;
  
type
  TVector = class (TObject)
  private
    FLength: Extended;
    FTeta: Extended;
    Fx, Fy: Extended;
    function Getx: Extended;
    function Gety: Extended;
    
  public
    property Len: Extended read FLength;
    property Teta: Extended read FTeta;
    property x: Extended read Getx;
    property y: Extended read Gety;
    
    constructor Create; overload;
    constructor Create (r, Teta: Extended); overload;//Teta is in Degree;
    constructor CreateXY (x, y: Extended); overload;//Teta is in Degree;
    procedure Free;

    function Add (AnotherVector: TVector): TVector;//Create a new Vector
    function Multiply (ASize: Extended): TVector;//Create a new Vector 
    
  end;
  
  TVectorCollection= class (TBaseCollection)
  private
    function GetVector (Index: Integer): TVector;
    
  public
    property Vector [Index: Integer]: TVector read GetVector;

    constructor Create;
    procedure Free (FreeObj: Boolean= True);

    procedure Add (NewVector: TVector);
  end;
  
  TVectorColCollection= class (TBaseCollection)
  private
    function GetVectorCollection (Index: Integer): TVectorCollection;
    function GetVector(Index1, Index2: Integer): TVector;
    
  public
    property VectorCollection [Index: Integer]: TVectorCollection read GetVectorCollection;
    property Vector [Index1, Index2: Integer]: TVector read GetVector;

    constructor Create;
    procedure Free (FreeObj: Boolean= True);

    procedure Add (NewVectorCollection: TVectorCollection);
    
  end;
  
implementation
uses
  Math;
  
function MyArcTan (x, y: Extended): Extended;//Result is in Degree.
begin
  if abs (x)< 1e-10 then
  begin
    if y< 0 then
      Result:= 3* Pi/ 2.0
    else
      Result:= Pi/ 2.0;

  end
  else
    Result:= ArcTan2 (x, y);

end;

{ TVector }

constructor TVector.Create;
begin
  inherited;

  Fx:= 0; Fy:= 0;
end;

function TVector.Add (AnotherVector: TVector): TVector;
begin
  Result:= TVector.Create (x+ AnotherVector.x,
        y+ AnotherVector.y);

end;

constructor TVector.Create (r, Teta: Extended);
begin
  inherited Create;

  FLength:= r;
  FTeta:= Teta;
  Fx:= 0; Fy:= 0;

end;

constructor TVector.CreateXY (x, y: Extended);
begin
  inherited Create;
  
  Fx:= x;
  Fy:= y;

  FLength:= Sqrt (Sqr (Fx)+ Sqr (Fy));
  FTeta:= MyArcTan (Fy, Fx);
    
end;

procedure TVector.Free;
begin
  inherited;
  
end;

function TVector.Getx: Extended;
begin
  if Abs (Fx)< 1e-10  then
    Fx:= FLength* Cos (FTeta/ 180* Pi);

  Result:= Fx;

end;

function TVector.Gety: Extended;
begin
  if Abs (Fy)< 1e-10  then
    Fy:= FLength* Sin (FTeta/ 180* Pi);
    
  Result:= Fx;

end;

function TVector.Multiply(ASize: Extended): TVector;
begin
  Result:= TVector.Create (x+ ASize, y+ ASize);

end;

{ TVectorCollection }

procedure TVectorCollection.Add (NewVector: TVector);
begin
  inherited Add (NewVector);
  
end;

constructor TVectorCollection.Create;
begin
  inherited;
  
end;

procedure TVectorCollection.Free (FreeObj: Boolean);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to Size- 1 do
      Vector [i].Free;

  inherited Free (False);

end;

function TVectorCollection.GetVector (Index: Integer): TVector;
begin
  Result:= Member [Index] as TVector
  
end;

{ TVectorColCollection }

procedure TVectorColCollection.Add (NewVectorCollection: TVectorCollection);
begin
  inherited Add (NewVectorCollection);
  
end;

constructor TVectorColCollection.Create;
begin
  inherited;
  
end;

procedure TVectorColCollection.Free(FreeObj: Boolean);
var
  i: Integer;
  
begin
  if FreeObj then
    for i:= 0 to Size- 1 do
      VectorCollection [i].Free (True);

  inherited Free (False);
  
end;

function TVectorColCollection.GetVector (Index1, Index2: Integer): TVector;
begin
  Result:= VectorCollection [Index1].Vector [Index2];
  
end;

function TVectorColCollection.GetVectorCollection (Index: Integer): TVectorCollection;
begin
  Result:= Member [Index] as TVectorCollection;
  
end;

end.
