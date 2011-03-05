unit VectorUnit;

interface
uses
  CollectionUnit, StreamUnit, SysUtils;
  
type
  EinvalidFile= class (Exception);
  
  TVector= class (TDoubleCollection)
  private
  public
    
    constructor Create; overload;
    constructor Create (Size: Integer); overload;
    constructor Create (ADoubleCollection: TDoubleCollection); overload;
    destructor Destroy; override;

    // Adds the members of AVector to the corresponding member in self and retuns self
    function Add (AVector: TDoubleCollection): TVector; overload;
    
    // Subs the members of AVector to the corresponding member in self and retuns self
    function Sub (AVector: TVector): TVector;
    // Multiplies the members of self with Value and returns self
    function Scale (Value: Extended): TVector;
    // Copy 
    function Copy: TVector;
    //returns the square of ocliducian distance between two vectors
    function FindDistanceWith (AVector: TDoubleCollection): Extended; 

    // Returns the vector in a string
    function ToString: String;
    // Loads the contents of the vector from the stream
    procedure LoadFromStream (AStream: TMyFileStream);
    // Saves the contents of the vector in the stream
    procedure SaveToStream (AStream: TMyFileStream);

  end;

  TVectorCollection= class (TBaseCollection)
  private
    function GetVector (Index: Integer): TVector;
    
  public
    property Vector [Index: Integer]: TVector read GetVector;

    constructor Create; overload;
    constructor Create (ADoubleColCollection: TBaseCollection); overload;
      //ADoubleCollection must be a collection whose all elements are TDoubleCollection. 

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

    procedure Add (NewVectorCollection: TVectorCollection);
    
  end;
  
implementation
uses
  Math, MyTypes;
  
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

{ TVectorCollection }

procedure TVectorCollection.Add (NewVector: TVector);
begin
  inherited Add (NewVector);

end;

constructor TVectorCollection.Create;
begin
  inherited;
  
end;

constructor TVectorCollection.Create (
  ADoubleColCollection: TBaseCollection);
var
  SrcPtr, TrgPtr: PObject;
  i: Integer;
  AVector: TVector;
  
begin
  inherited Create;

  Allocate (ADoubleColCollection.Size);
  SrcPtr:= ADoubleColCollection.GetPointerToFirst;
  TrgPtr:= Self.GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    AVector:= TVector.Create (TDoubleCollection (SrcPtr^));
    TrgPtr^:= AVector;

    Inc (TrgPtr);
    Inc (SrcPtr);
    
  end;

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

function TVectorColCollection.GetVector (Index1, Index2: Integer): TVector;
begin
  Result:= VectorCollection [Index1].Vector [Index2];
  
end;

function TVectorColCollection.GetVectorCollection (Index: Integer): TVectorCollection;
begin
  Result:= Member [Index] as TVectorCollection;
  
end;

{ TVector }

constructor TVector.Create;
begin
  inherited;
    
end;

function TVector.Copy: TVector;
var
  Ptr1, Ptr2: PExtended;
  i: Integer;

begin
  Result:= TVector.Create (Size);

  Ptr1:= GetPointerToTheFirst;
  Ptr2:= Result.GetPointerToTheFirst;
  
  for i:= 1 to Size do
  begin
    Ptr2^:= Ptr1^;
    Inc (Ptr1);
    Inc (Ptr2);
    
  end;

end;

constructor TVector.Create (Size: Integer);
var
  i: Integer;
  Ptr: PExtended;
  
begin
  inherited Create;

  Allocate (Size);
  Ptr:= GetPointerToTheFirst;
  for i:= 1 to Size do
  begin
    Ptr^:= 0;
    Inc (Ptr);
    
  end;

end;

destructor TVector.Destroy;
begin
  SetLength (FMembers, 0);
  
  inherited;
  
end;

function TVector.Scale (Value: Extended): TVector;
var
  Ptr1: PExtended;
  i: Integer;

begin
  Ptr1:= GetPointerToTheFirst;

  for i:= 1 to Size do
  begin
    Ptr1^:= Value* Ptr1^;
    Inc (Ptr1);
    
  end;

  Result:= Self;
  
end;

function TVector.Sub (AVector: TVector): TVector;
var
  Ptr1, Ptr2: PExtended;
  i: Integer;

begin
  Ptr1:= GetPointerToTheFirst;
  Ptr2:= AVector.GetPointerToTheFirst;
  
  for i:= 1 to Size do
  begin
    Ptr1^:= Ptr1^- Ptr2^;
    Inc (Ptr1);
    Inc (Ptr2);
    
  end;
  
  Result:= Self;

end;

procedure TVector.LoadFromStream (AStream: TMyFileStream);
var
  S, Temp: String;
  Index: Integer;
  Ptr: PExtended;  

begin
  S:= AStream.ReadLine;
  S:= Trim (S);
  if S [1]<> '(' then
    raise EinvalidFile.Create ('');
  System.Delete (S, 1, 1);
   
  Temp:= System.Copy (S, 1, Pos (',', S)- 1);
  System.Delete (S, 1, Pos (',', S));

  Allocate (StrToInt (Temp));

  Ptr:= GetPointerToTheFirst;
  for Index:= 2 to Size  do
  begin
    Temp:= Trim (System.Copy (S, 1, Pos (',', S)- 1));
    System.Delete (S, 1, Pos (',', S));
    
    S:= Trim (S);

    Ptr^:= StrToFloat (Temp);
    Inc (Ptr);
    
  end;
  Temp:= Trim (System.Copy (S, 1, Pos (')', S)- 1));
  System.Delete (S, 1, Pos (')', S)- 1);
  S:= Trim (S);

  Ptr^:= StrToFloat (Temp);
  Inc (Ptr);

  if S [1]<> ')' then
    raise EinvalidFile.Create ('');

end;

procedure TVector.SaveToStream (AStream: TMyFileStream);
begin
  AStream.WriteLine (ToString);
  
end;

function TVector.ToString: String;
var
  i: Integer;
  Ptr: PExtended;
  
begin
  Result:= '('+ IntToStr (Size)+ ', ';
  Ptr:= GetPointerToTheFirst;

  for i:= 2 to Size do
  begin
    Result:= Result+ FloatToStr (Ptr^)+ ', ';
    Inc (Ptr);
    
  end;
  Result:= Result+ FloatToStr (Ptr^)+ ')';

end;

constructor TVector.Create (ADoubleCollection: TDoubleCollection);
var
  Ptr1, Ptr2: PExtended;
  i: Integer;

begin
  inherited Create;
  ADoubleCollection.Member [0];
  
  FSize:= ADoubleCollection.Size;
  Allocate (fSize);

  Ptr2:= ADoubleCollection.GetPointerToTheFirst;
  Ptr1:= GetPointerToTheFirst;

  for i:= 1 to Size do
  begin
    Ptr1^:= Ptr2^;
    Inc (Ptr1);
    Inc (Ptr2);
    
  end;

end;

function TVector.Add (AVector: TDoubleCollection): TVector;
var
  Ptr1, Ptr2: PExtended;
  i: Integer;

begin
  Ptr1:= GetPointerToTheFirst;
  Ptr2:= AVector.GetPointerToTheFirst;

  for i:= 1 to Size do
  begin
    Ptr1^:= Ptr1^+ Ptr2^;
    Inc (Ptr1);
    Inc (Ptr2);
    
  end;
  
  Result:= Self;

end;

function TVector.FindDistanceWith(AVector: TDoubleCollection): Extended;
var
  i: Integer;
  Ptr1, Ptr2: PExtended;

begin
  Ptr1:= GetPointerToTheFirst;
  Ptr2:= AVector.GetPointerToTheFirst;

  Result:= 0;
  for i:= 1 to Size do
  begin
    Result:= Result+ Sqr (Ptr1^- Ptr2^);
    Inc (Ptr1);
    Inc (Ptr2);
    
  end;

end;

end.
