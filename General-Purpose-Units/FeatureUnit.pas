unit FeatureUnit;

interface
uses
  VectorUnit, CollectionUnit, SysUtils, ComponentsUnit,
   GeometryUnit, {HashUnit, }MyTypes;

type
  TTwoDimensionDoubleArray= class
  private
    FBody: array of array of Extended;
    FRow, FColumn: Integer;
    function GetBody (r, c: Integer): Extended;
    function GetScanLine (r: Integer): PExtended;

  public
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property Data [r, c: Integer]: Extended read GetBody;
    property ScanLine [r: Integer]: PExtended read GetScanLine;

    constructor Create (Row, Col: Integer);
    destructor Destroy; override;

    procedure SaveToFile (var OutputFileHandle: TextFile);
    
  end;

  TGradiantFeature= TTwoDimensionDoubleArray;
  TFeatureVectorBasedOnGradiant= class;


  TFeatureVectorBasedOnGradiantCollection= class (TBaseCollection)
  private
    function GetFeatureVectorBasedOnGradiant (Index: Integer):
        TFeatureVectorBasedOnGradiant;
  
  public
    property FeatureVectorBasedOnGradiant [Index: Integer]: TFeatureVectorBasedOnGradiant
        read GetFeatureVectorBasedOnGradiant;

    procedure Add (NewGradiantFeature: TFeatureVectorBasedOnGradiant);
    procedure SaveToFile (FileName: String);
    
  end;

  TFeatureVectorBasedOnGradiant= class (TDoubleCollection)
  public
    procedure SaveToFile (FileName: String); overload;
    procedure SaveToFile (var OutputFile: TextFile); overload;
    
  end;
   
  T8DirGradiantFeature= class (TBaseCollection)
  private
    FRow, FColumn: Integer;
    
    function GetGradiantFeature (Index: Integer): TGradiantFeature;
    procedure Add (NewGradiantFeature: TGradiantFeature);

    function GausianFilter (Center: TPoint;
    DistanceBetweenSamples: Extended; ThirdIndex: Integer): Extended;
    
  public
    property GradiantFeature [Index: Integer]: TGradiantFeature read GetGradiantFeature;

    constructor Create;
    procedure Prepare (Row, Column: Integer);

    procedure SaveToFile (FileName: String);
    function SampleGradiant (NumberOfMasks: Integer): TFeatureVectorBasedOnGradiant;
    
  end;

  TSampleGradiantIn8Dir= T8DirGradiantFeature;

  TMultiDimensionalArray= class (TObject)
  private
    FDimension: Integer;
    FSizes: array of Integer;
    FData: array of array of array of Extended;

    function GetSize (Index: Integer): Integer;
    function GetData (x, y, z: Integer): Extended;
    function GetScanLine (x: Integer): PExtended;
    procedure SetData (x, y, z: Integer; const Value: Extended);
  public
    property Dimenstion: Integer read FDimension;
    property Sizes [Index: Integer]: Integer read GetSize;
    property Data [x, y, z: Integer]: Extended read GetData write SetData;
    property ScanLine [x: Integer]: PExtended read GetScanLine;

    constructor Create (Dimension: Integer; Sizes: array of Integer);
    destructor Destroy; override;

  end;

  TGradiantIn8Dir= TMultiDimensionalArray;

implementation

uses
  ExceptionUnit, Math, Classes;

{ TMultiDimensionalArray }

constructor TMultiDimensionalArray.Create (Dimension: Integer;
  Sizes: array of Integer);
var
  i: Integer;

begin
  inherited Create;
  if Dimension<> 2 then
    raise ENotImplemented.Create ('Create (Dimenstion<> 2)');

  FDimension:= Dimension;
  SetLength (FSizes, FDimension);
  for i:= 0 to Dimension- 1 do
    FSizes [i]:= Sizes [i];

end;

destructor TMultiDimensionalArray.Destroy;
begin
  SetLength (FSizes, 0);

  inherited;
end;

{
function TMultiDimensionalArray.GetData (Indexes: TIntegerArray): Extended;
var
  i: Integer;
  Temp: PExtended;

begin
  if Length (Indexes)<> FDimension then
    raise ERangeCheckError.Create ('GetData');

  for i:= 0 to FDimension- 1 do
  begin
    PExtended:= FD
  end;

end;
}

function TMultiDimensionalArray.GetData (x, y, z: Integer): Extended;
begin
  if (x< Sizes [0]) and (y< Sizes [1]) and (z< Sizes [2]) then
    Result:= FData [x, y, z]
  else
    raise ERangeCheckError.Create ('Get Data');

end;

function TMultiDimensionalArray.GetScanLine (x: Integer): PExtended;
begin
  Result:= @FData [x, 0];
  
end;

function TMultiDimensionalArray.GetSize (Index: Integer): Integer;
begin
  Result:= FSizes [Index];

end;

procedure TMultiDimensionalArray.SetData (x, y, z: Integer;
  const Value: Extended);
begin
  if (x< Sizes [0]) and (y< Sizes [1]) and (z< Sizes [2]) and
    (0<= x) and (0<= y) and (0<= z) then
    FData [x, y, z]:= Value
  else
    raise ERangeCheckError.Create ('Get Data');

end;


{ TGradiantFeature }
{
constructor TGradiantFeature.Create (Row, Col: Integer);
var
  r, c: Integer;
  Ptr: PExtended;

begin
  inherited Create;

  FRow:= Row;
  FColumn:= Col;

  SetLength (FBody, FRow);

  for r:= 0 to FRow- 1 do
  begin
    SetLength (FBody [r], FColumn);
    Ptr:= @FBody [r, 0];

    for c:= 0 to FColumn- 1 do
    begin
      Ptr^:= 0;
      Inc (Ptr);

    end;

  end;

end;

procedure TGradiantFeature.Free;
var
  r: Integer;

begin
  for r:= 0 to FRow- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  inherited;

end;

function TGradiantFeature.GetBody (r, c: Integer): Extended;
begin
  if (r< 0) or (FRow<= r) or
    (c< 0)  or (FColumn<= c) then
    raise ERangeCheckError.Create ('GetBody!');

  Result:= FBody [r, c];

end;

function TGradiantFeature.GetScanLine (r: Integer): PExtended;
begin
  if (r< 0) or (FRow<= r) then
    raise ERangeCheckError.Create ('GetScanLine');

  Result:= @FBody [r, 0];

end;

procedure TGradiantFeature.SaveToFile (var OutputFileHandle: TextFile);
var
  r, c: Integer;

begin
  for r:= 0 to FRow- 1 do
  begin
    for c:= 0 to FColumn- 1 do
      Write (OutputFileHandle, FBody [r, c]:0:3, ' ');

    WriteLn (OutputFileHandle);

  end;

end;
}
{ TFeatureVectorBasedOnGradiant }
procedure TFeatureVectorBasedOnGradiant.SaveToFile (FileName: String);
var
  OutputFile: TextFile;
  
begin
  AssignFile (OutputFile, FileName);
  ReWrite (OutputFile);
  
  SaveToFile (OutputFile);
  
  CloseFile (OutputFile);

end;

procedure TFeatureVectorBasedOnGradiant.SaveToFile (var OutputFile: TextFile);
var
  i: Integer;
  Ptr: PExtended;
  
begin
  Ptr:= GetPointerToTheFirst;
   
  for i:= 0 to Size- 1 do
  begin
    Write (OutputFile, Ptr^:0:5, ' ');
    Inc (Ptr);
    
  end;
  WriteLn (OutputFile);

end;

{ T8DirGradiantFeature }

function T8DirGradiantFeature.GausianFilter (Center: TPoint;
           DistanceBetweenSamples: Extended; ThirdIndex: Integer): Extended;
var
  Ptr: PExtended;
  Coef,
  SumOfCof: Extended;
  r, c, r1, c1: Integer;
  StartC, EndC, StartR, EndR: Integer;
  HalfOfMaskSize: Integer;
  Sigma: Extended;
  ActiveGradiant: TGradiantFeature;

begin
  
  Result:= 0;
  SumOfCof:= 0;
  Sigma:= 2/ Pi* DistanceBetweenSamples;
  HalfOfMaskSize:= Round (DistanceBetweenSamples);
  ActiveGradiant:= GradiantFeature [ThirdIndex];

  StartR:= Math.Max (-HalfOfMaskSize, -Center.r);
  EndR:= Min (HalfOfMaskSize, FRow- Center.r- 1);
  r1:= Math.Max (-HalfOfMaskSize, -Center.r)+ Center.r ;

  for r:= StartR to EndR do
  begin
    c1:= Math.Max (-HalfOfMaskSize, -Center.c)+ Center.c;

    Ptr:= ActiveGradiant.ScanLine [r1];
    Inc (Ptr, c1);
                       
    for c:= Math.Max (-HalfOfMaskSize, -Center.c) to
          Min (HalfOfMaskSize, FColumn- Center.c- 1)do
    begin
      Coef:= Exp (- (Sqr (r)+ Sqr (c))/ Sqr (Sigma));
      SumOfCof:= SumOfCof+ Coef;

      Result:= Result+ Ptr^* Coef;
//      Inc (c1);
      Inc (Ptr);
      
    end;
    Inc (r1);

  end;

  Result:= Result/ SumOfCof;
  
end;

procedure T8DirGradiantFeature.Add (NewGradiantFeature: TGradiantFeature);
begin
  inherited Add (NewGradiantFeature);
  
end;

constructor T8DirGradiantFeature.Create;
begin
  inherited Create;

end;

function T8DirGradiantFeature.GetGradiantFeature (Index: Integer): TGradiantFeature;
begin
  Result:= Member [Index] as TGradiantFeature;
  
end;

procedure T8DirGradiantFeature.Prepare (Row, Column: Integer);
var
  i: Integer;
  NewGradiantFeature: TGradiantFeature;
  
begin
  FRow:= Row;
  FColumn:= Column;
  
  for i:= 1 to 8 do
  begin
    NewGradiantFeature:= TGradiantFeature.Create (Row, Column);
    Self.Add (NewGradiantFeature);
    
  end;
  
end;

function T8DirGradiantFeature.SampleGradiant (NumberOfMasks: Integer): TFeatureVectorBasedOnGradiant;
var
  Centers: array of array of TPoint;
  i, j, k: Integer;
  DistanceBetweenSamplesInRow,
  DistanceBetweenSamplesInCol: Extended;

begin
  Result:= TFeatureVectorBasedOnGradiant.Create;
  DistanceBetweenSamplesInRow:= FRow/ NumberOfMasks;
  DistanceBetweenSamplesInCol:= FColumn/ NumberOfMasks;

  SetLength (Centers, NumberOfMasks);

  for i:= 0 to NumberOfMasks- 1 do
  begin
    SetLength (Centers [i], NumberOfMasks);

    for j:= 0 to NumberOfMasks- 1 do
      Centers [i, j]:= TPoint.Create (
        Round ((i+ 0.5)* DistanceBetweenSamplesInRow),
        Round ((j+ 0.5)* DistanceBetweenSamplesInCol));

  end;

  for k:= 0 to 7 do
    for i:= 0 to NumberOfMasks- 1 do
      for j:= 0 to NumberOfMasks- 1 do
      begin
        Result.Add (Self.GausianFilter (Centers [i, j],
                DistanceBetweenSamplesInRow, k));

      end;

  for i:= 0 to NumberOfMasks- 1 do
  begin
     for j:= 0 to NumberOfMasks- 1 do
       Centers [i, j].Free;
       
     SetLength (Centers [i], 0);

  end;
  
  SetLength (Centers, 0);

end;

procedure T8DirGradiantFeature.SaveToFile (FileName: String);
var
  i, r, c: Integer;
  OutputFile: TextFile;

begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);

  for r:= 0 to GradiantFeature [0].Row- 1 do
    for c:= 0 to GradiantFeature [0].Column- 1 do
    begin
      Write (OutputFile, r, ',', c, ': (');
      for i:= 0 to 7 do
        Write (OutputFile, GradiantFeature [i].FBody [r, c]:0:3, ' ');
      WriteLn (OutputFile, ')');
    
    end;

  CloseFile (OutputFile);
  
end;

{ TTwoDimensionDoubleArray }

constructor TTwoDimensionDoubleArray.Create (Row, Col: Integer);
var
  r: Integer;

begin
  inherited Create;

  SetLength (FBody, Row);
  for r:= 0 to Row- 1 do
    SetLength (FBody [r], Col);

  FRow:= Row;
  FColumn:= Col;

end;

destructor TTwoDimensionDoubleArray.Destroy;
var
  r: Integer;
  
begin
  for r:= 0 to Row- 1 do
    SetLength (FBody [r], 0);
  SetLength (FBody, 0);

  inherited;
  
end;

function TTwoDimensionDoubleArray.GetBody (r, c: Integer): Extended;
begin
  if (r< 0) or (Row<= r) or (c< 0) or (Column<= c) then
    raise ERangeCheckError.Create ('Get Body');

  Result:= FBody [r, c];

end;

function TTwoDimensionDoubleArray.GetScanLine (r: Integer): PExtended;
begin
  if (r< 0) or (Row<= r) then
    raise ERangeCheckError.Create ('ScanLine');

  Result:= @FBody [r, 0];

end;

procedure TTwoDimensionDoubleArray.SaveToFile (
  var OutputFileHandle: TextFile);
var
  Ptr: PExtended;
  r, c: Integer;
  
begin
  for r:= 0 to Row- 1 do
  begin
    Ptr:= ScanLine [r];

    for c:= 0 to Column- 1 do
    begin
      Write (OutputFileHandle, Ptr^:0:4, ' ');
      Inc (Ptr);
      
    end;
    WriteLn (OutputFileHandle);

  end;


end;

{ TTFeatureVectorBasedOnGradiantCollection }

procedure TFeatureVectorBasedOnGradiantCollection.Add (
  NewGradiantFeature: TFeatureVectorBasedOnGradiant);
begin
  inherited Add (NewGradiantFeature);

end;

function TFeatureVectorBasedOnGradiantCollection.GetFeatureVectorBasedOnGradiant (
  Index: Integer): TFeatureVectorBasedOnGradiant;
begin
  Result:= Member [Index] as TFeatureVectorBasedOnGradiant;

end;

procedure TFeatureVectorBasedOnGradiantCollection.SaveToFile (
  FileName: String);
var
  OutputFile: TextFile;
  i: Integer;
  
begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);

  for i:= 0 to Size- 1 do
  begin
    FeatureVectorBasedOnGradiant [i].SaveToFile (OutputFile);
//    WriteLn (OutputFile);
    
  end;

  CloseFile (OutputFile);

end;

end.
