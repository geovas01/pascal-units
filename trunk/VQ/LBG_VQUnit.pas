unit LBG_VQUnit;
{

Reference: http://www.data-compression.com/vq.shtml

}

interface
uses
  VectorUnit, CollectionUnit, StreamUnit;

type
  TCodeVector= TVector;
  TCodeVectors= class (TBaseCollection)
  private
    function GetCodeVector (Index: Integer): TCodeVector;
  public
    property CodeVector [Index: Integer]: TCodeVector read GetCodeVector;

    function Copy: TCodeVectors;
    constructor Create; overload;
    constructor Create (Size, VectorSize: Integer); overload;

    procedure  LoadFromStream (AStream: TMyFileStream);
    procedure  SaveToStream (AStream: TMyFileStream);

  end;

  TTrainSet= class (TBaseCollection)
  private
    function GetTrainSample (Index: Integer): TDoubleCollection;
  public
    property TrainSample [Index: Integer]: TDoubleCollection
             read GetTrainSample;

    procedure Add (NewTrainSample: TDoubleCollection);
    procedure SaveToStream (OutputStream: TMyFileStream); 
    procedure LoadFromStream (InputStream: TMyFileStream); 

  end;

  TVQEngine= class (TObject)
  private
    FinalCodeVectors: TCodeVectors;
    function GetCodeBookCount: Integer;
    
  public
    property CodeBookCount: Integer read GetCodeBookCount;
    
    procedure Train (TrainSet: TTrainSet; Epsilon1, Epsilon2: Extended;
              FinalCodeBookCount: Integer);
    procedure SaveWeights (OutputStream: TMyFileStream);
    procedure Load (InputStream: TMyFileStream);

    constructor Create;
    destructor Destroy; override;

    function FindCluster (AQuery: TDoubleCollection): Integer;
              
  end;
  
implementation

uses
  MyTypes, SysUtils;

{ TCodeVectors }

function TCodeVectors.Copy: TCodeVectors;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Result:= TCodeVectors.Create;

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    Result.Add (TCodeVector (Ptr^).Copy);
    Inc (Ptr);
    
  end;

end;

constructor TCodeVectors.Create;
begin
  inherited;
  
end;

constructor TCodeVectors.Create (Size, VectorSize: Integer);
var
  i: Integer;
  Ptr: PObject;

begin
  inherited Create;

  Allocate (Size);
  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    Ptr^:= TCodeVector.Create (VectorSize);
    Inc (Ptr);
    
  end;

end;

function TCodeVectors.GetCodeVector (Index: Integer): TCodeVector;
begin
  Result:= Member [Index] as TCodeVector;
  
end;

procedure TCodeVectors.LoadFromStream (AStream: TMyFileStream);
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    TCodeVector (Ptr^).LoadFromStream (AStream);
    Inc (Ptr);
    
  end;

end;

procedure TCodeVectors.SaveToStream (AStream: TMyFileStream);
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    TCodeVector (Ptr^).SaveToStream (AStream);
    Inc (Ptr);
    
  end;

end;

{ TrainSet }

procedure TTrainSet.Add (NewTrainSample: TDoubleCollection);
begin
  inherited Add (NewTrainSample);
  
end;

function TTrainSet.GetTrainSample (Index: Integer): TDoubleCollection;
begin
  Result:= Member [Index] as TDoubleCollection;
  
end;

{ TLBGTrainer }

constructor TVQEngine.Create;
begin
  inherited ;

  FinalCodeVectors:= nil;
  
end;

destructor TVQEngine.Destroy;
begin
  FinalCodeVectors.Free;
  
  inherited;
  
end;

function TVQEngine.FindCluster (AQuery: TDoubleCollection): Integer;
var
  Vector: TVector;
  i: Integer;
  Min, Temp: Extended;
  Ptr: PObject;

begin
  Vector:= TVector.Create (AQuery);

  Ptr:= FinalCodeVectors.GetPointerToFirst;
  Min:= TCodeVector (Ptr^).FindDistanceWith (AQuery);
  Result:= 0;
  
  for i:= 2 to FinalCodeVectors.Size do
  begin
    Inc (Ptr);
    Temp:= TCodeVector (Ptr^).FindDistanceWith (AQuery);

    if Temp< Min then
    begin
      Min:= Temp;
      Result:= i- 1;

    end;

  end;

end;

function TVQEngine.GetCodeBookCount: Integer;
begin
  Result:= FinalCodeVectors.Size;
  
end;

procedure TVQEngine.Load (InputStream: TMyFileStream);
var
  S: String;
  CodeBookCount: Integer;
  i: Integer;
  Ptr: PObject;
  NewCodeVector: TCodeVector;

begin
  S:= InputStream.ReadLine;
  CodeBookCount:= StrToInt (S);

  if FinalCodeVectors<> nil then
    FinalCodeVectors.Free;
  FinalCodeVectors:= nil;

  FinalCodeVectors:= TCodeVectors.Create;
  FinalCodeVectors.Allocate (CodeBookCount);

  Ptr:= FinalCodeVectors.GetPointerToFirst;
  for i:= 1 to FinalCodeVectors.Size do
  begin
    NewCodeVector:= TCodeVector.Create;
    NewCodeVector.LoadFromStream (InputStream);
    Ptr^:= NewCodeVector;
    if i mod 100= 0 then
      WriteLn (i, ' out of ', FinalCodeVectors.Size, ' has been loaded.');
    Inc (Ptr);

  end;

end;

procedure TVQEngine.SaveWeights (OutputStream: TMyFileStream);
begin
  OutputStream.WriteLine (IntToStr (FinalCodeVectors.Size));
  
  FinalCodeVectors.SaveToStream (OutputStream);
  
end;

procedure TVQEngine.Train (TrainSet: TTrainSet; Epsilon1,
  Epsilon2: Extended; FinalCodeBookCount: Integer);
var
  VectorSize, TrainSetSize: Integer;
  ActiveCodeVectors: TCodeVectors;
  TrainSetVectors: TVectorCollection;
  Qs: TVectorCollection;

  function CalculateDAve (TrainSet: TTrainSet; AVector: TCodeVector): Extended;
  var
    i: Integer;
    Ptr: PObject;

  begin
    Ptr:= TrainSet.GetPointerToFirst;
    Result:= 0;
    for i:= 1 to TrainSetSize do
    begin
      Result:= Result+ AVector.FindDistanceWith (TDoubleCollection (Ptr^));
      Inc (Ptr);

    end;

    Result:= Result/ (TrainSetSize* VectorSize);
    
  end;

  function Splitting: TCodeVectors;
  var
    i: Integer;
    Ptr1, Ptr2: PObject;
    TempVector: TCodeVector;
    
  begin
    Result:= TCodeVectors.Create;
    Result.Allocate (2* ActiveCodeVectors.Size);

    Ptr1:= ActiveCodeVectors.GetPointerToFirst;
    Ptr2:= Result.GetPointerToFirst;

    for i:= 1 to ActiveCodeVectors.Size do
    begin
      TempVector:= TCodeVector (Ptr1^);
      Ptr2^:= TempVector.Copy.Scale (1+ Epsilon1);
      Inc (Ptr2);
      Ptr2^:= TempVector.Copy.Scale (1- Epsilon1);

      Inc (Ptr2);
      Inc (Ptr1);

    end;

    ActiveCodeVectors.Free;
    
  end;

  function Iteration (LastDAvg: Extended): Extended;
  var
    NewDAvg: Extended;
    Min, Temp: Extended;
    m, n, i, MinIndex: Integer;
    Ptr, TrainPtr, CodeBookPtr: PObject;
    SumVector: TVector;
    Count: Integer;
    NewCodeBooks: TCodeVectors;
    ActiveVector: TVector;

  begin

    while True do
    begin
      NewCodeBooks:= TCodeVectors.Create;
      NewCodeBooks.Allocate (ActiveCodeVectors.Size);

      TrainPtr:= TrainSetVectors.GetPointerToFirst;

      for m:= 1 to TrainSetSize do
      begin
        CodeBookPtr:= ActiveCodeVectors.GetPointerToFirst;
        Min:= TVector (TrainPtr^).FindDistanceWith (TVector (CodeBookPtr^));
        MinIndex:= 1;

        for n:= 2 to ActiveCodeVectors.Size do
        begin
          Inc (CodeBookPtr);
          Temp:= TVector (TrainPtr^).FindDistanceWith (TVector (CodeBookPtr^));
          if Temp< Min then
          begin
            MinIndex:= n;
            Min:= Temp;
            
          end;

        end;
        Dec (MinIndex);
        Qs.Member [m- 1]:= ActiveCodeVectors.CodeVector [MinIndex];

        Inc (TrainPtr);

      end;

      for n:= 0 to ActiveCodeVectors.Size- 1 do
      begin
        ActiveVector:= ActiveCodeVectors.CodeVector [n];
        Count:= 0;
        SumVector:= TVector.Create (VectorSize);

        for m:= 0 to TrainSetVectors.Size- 1 do
          if Qs.Vector [m]= ActiveVector then
          begin
            SumVector.Add (TrainSetVectors.Vector [m]);
            Inc (Count);

          end;

        if Count<> 0 then
          NewCodeBooks.Member [n]:= SumVector.Scale (1/ Count)
        else
        begin
          NewCodeBooks.Member [n]:= ActiveCodeVectors.CodeVector [n].Copy;
          SumVector.Free;
          
        end;
      
      end;

      NewDAvg:= 0;
      Ptr:= TrainSet.GetPointerToFirst;
      for i:= 0 to TrainSet.Size- 1 do
      begin
        NewDAvg:= NewDAvg+ Qs.Vector [i].FindDistanceWith (TVector (Ptr^));
        Inc (Ptr);

      end;

      NewDAvg:= NewDAvg/ (TrainSetSize* VectorSize);
      if NewDAvg< 1e-9 then
        Break;

      if (Abs (LastDAvg- NewDAvg)/ LastDAvg)< Epsilon2 then
        Break;
      LastDAvg:= NewDAvg;
      ActiveCodeVectors.Free;

      ActiveCodeVectors:= NewCodeBooks;

    end;

    ActiveCodeVectors.Free;
    ActiveCodeVectors:= NewCodeBooks;
    Result:= NewDAvg;

  end;

var
  CStarOne: TCodeVector;
  Ptr, TrgPtr: PObject;
  i, n: Integer;
  DAvg: Extended;
  AVector: TVector;

begin
  TrainSetVectors:= TVectorCollection.Create (TrainSet);
  TrainSetSize:= TrainSet.Size;
  VectorSize:= TrainSet.TrainSample [0].Size;

  Qs:= TVectorCollection.Create;
  Qs.Allocate (TrainSetSize);

  CStarOne:= TCodeVector.Create (VectorSize);

{Step 2}
  (*C*_1= \frac{1}{M} \Sigma x_m*)
  Ptr:= TrainSetVectors.GetPointerToFirst;
  for i:= 1 to TrainSet.Size do
  begin
    CStarOne.Add (TVector (Ptr^));
    Inc (Ptr);
    
  end;
  CStarOne.Scale (1/ TrainSetSize);

  DAvg:= 0;
  Ptr:= TrainSet.GetPointerToFirst;
  for i:= 1 to TrainSet.Size do
  begin
    DAvg:= DAvg+ CStarOne.FindDistanceWith (TVector (Ptr^));
    Inc (Ptr);
    
  end;
  DAvg:= DAvg/ (TrainSetSize* VectorSize);

  ActiveCodeVectors:= TCodeVectors.Create;
  ActiveCodeVectors.Add (CStarOne);
  n:= 1;

  while n< FinalCodeBookCount do
  begin
    ActiveCodeVectors:= Splitting;{Step3}
    n:= n shl 1;
    DAvg:= Iteration (DAvg);{Step4}

    Writeln (n* 100 div FinalCodeBookCount, '%');
    if DAvg< 1e-9 then
    begin
      WriteLn ('D^{*}_ave= 0, exiting ...');
      Break;
      
    end;

  end;

  Qs.Clear;
  QS.Free;
  FinalCodeVectors:= ActiveCodeVectors;
  TrainSetVectors.Free;

end;

procedure TTrainSet.SaveToStream (OutputStream: TMyFileStream);
var
  Ptr: PObject;
  dPtr: PExtended;
  i, j, Len: Integer;
  S: String;

begin
  if Size<> 0 then
  begin
    Len:= TrainSample [0].Size;
    OutputStream.WriteLine (IntToStr (Size)+ ' '+ IntToStr (Len));

    Ptr:= GetPointerToFirst;
    for i:= 1 to Size do
    begin
      dPtr:= TDoubleCollection (Ptr^).GetPointerToTheFirst;
      Inc (Ptr);

      for j:= 1 to Len do
      begin
        S:= FloatToStr (dPtr^);
        OutputStream.WriteStr (S+ ' ');
        Inc (dPtr);

      end;
      OutputStream.WriteLine ('');

    end;

  end;
  
end;

procedure TTrainSet.LoadFromStream (InputStream: TMyFileStream);
var
  Ptr: PObject;
  dPtr: PExtended;
  i, j, Len: Integer;
  S: String;
  NewDoubleCollection: TDoubleCollection;

begin
  S:=  InputStream.ReadLine;
  Allocate (StrToInt (Copy (S, 1, Pos (' ', S)- 1)));
  System.Delete (S, 1, Pos (' ', S));
  Len:=  StrToInt (S);

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    NewDoubleCollection:= TDoubleCollection.Create;
    NewDoubleCollection.Allocate (Len);
    Ptr^:= NewDoubleCollection;
    Inc (Ptr);
    dPtr:= NewDoubleCollection.GetPointerToTheFirst;
    S:= InputStream.ReadLine+ ' ';

    for j:= 1 to Len do
    begin
      dPtr^:= StrToFloat (Copy (S, 1, Pos (' ', S)- 1));
      System.Delete (S, 1, Pos (' ', S));
      Inc (dPtr);

    end;
    
  end;

end;

end.
