unit SvmClientUnit;

interface
uses
  IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, SysUtils, FeatureUnit, CollectionUnit,
  Classes, MyTypes, StreamUnit;
  
type
  ESVMNotFound= class (Exception)
  public
    constructor Create (KernelFileName, SvmFileName: String);
    
  end;
  EInvalidUsage= class (Exception);
  EInvalidFile= class (Exception);
  
  TSVMHandle= Integer;
  TSVMBatchModeResult= record
    MaxIndex: Integer;
    MaxValue: Extended;
    
  end;

  ENoSVMLoaded= class (Exception)
  public
    constructor Create;
    
  end;

  TSVMClient= class (TObject)
  private
    RecognitionEngineTCPConnection: TIdTCPClient;
    LoadedSVMHandles: TLongWordCollection;
    function GetSVMHandle (SVMName, Kernel: String): TSVMHandle;
    function GetLoadedSVMCount: Integer;

  public
    property SVMHandle [SVMName, KernelName: String]: TSVMHandle read GetSVMHandle;
    property LoadedSVMCount: Integer read GetLoadedSVMCount;

    constructor Create (ServerHost: String; ServerPort: Integer); overload;
    destructor Destroy; override;

    function OpenSVM (KernelFileName, SVMFileName: String): TSVMHandle;
    procedure WriteIntoSVM (SVMHandle: TSVMHandle;
      FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant);

    function AskQuery (SVMHandle: TSVMHandle; FeatureVectorBasedOnGradiant: TDoubleCollection): Extended;
    function AskBatchQuery (FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant;
    var SVMRating: TIntegerArray): TSVMBatchModeResult;
    function ListAllLoadedSVMS: TStringList;

    procedure LoadSVMs (ConfigFilename: String);

    function FindClass (FeatureVectorBasedOnGradiant: TDoubleCollection): String; virtual;
    
  end;

  TMultiLayerSVMClient= class;

  TMultiLayerSVMClientNode= class (TSVMClient)
  private
    Parent: TMultiLayerSVMClient;
    SVMFileName, KernelFileName, ChildsID: TStringList;
    Childs: TBaseCollection;
    ChildsSVMID: TIntegerCollection;
  
  public
    Name: String;
    ID: Integer;

    constructor Create (Parent: TMultiLayerSVMClient);
    destructor Destroy; override;

    procedure Load (InputStream: TMyFileStream); 
    function FindClass (FeatureVectorBasedOnGradiant: TDoubleCollection): String; 

  end;

  TMultiLayerSVMClient= class (TSVMClient)
  private
    FRoot: TMultiLayerSVMClientNode;

  private
    property Root: TMultiLayerSVMClientNode read FRoot;

  public
    destructor Destroy; override;

    procedure LoadSVMS (ConfigFilename: String);
    function FindClass (FeatureVectorBasedOnGradiant: TDoubleCollection): String; override;
      
  end;

  TSVMClients= class (TBaseCollection)
  private
    function GetSVMClient (Index: Integer): TSVMClient;

  public
    property SVMClient [Index: Integer]: TSVMClient read GetSVMClient;
    procedure AddNewSVMClient (SVMClient: TSVMClient);

  end;
  
  TByteArray= array of Byte;

  TKohonenClient= class (TObject)
  private
    RecognitionEngineTCPConnection: TIdTCPClient;
    
  public
    constructor Create (ServerHost: string; ServerPort: Integer;
      MyIP: String);
    destructor Destroy; override;


    function AskQuery (Query: TByteArray): Integer;

  end;

  procedure ReadTrainSet (FileStream: TMyFileStream;
         DataSet: TBaseCollection; Targets: TIntegerCollection);
  procedure CalculateApha_BetaForProb (Outs: TDoubleCollection;
     Targets: TIntegerCollection; var aOut, bOut: Extended);
     
implementation

uses
  ExceptionUnit, Dialogs, Math;

{ TSVMClient }
var
  Scores: array [0..1000] of Extended;// To make the code faster.
  {This variable should not be used anywhere except AskBatchQuery
  }
  
function TSVMClient.AskBatchQuery (
  FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant;
  var SVMRating: TIntegerArray): TSVMBatchModeResult;
var
  i, j: Integer;
  Temp: Extended;

begin
  if 0= LoadedSVMHandles.Size then
    raise EInvalidUsage.Create ('No BatchQuery with 0 SVMs');

  SetLength (SVMRating, LoadedSVMHandles.Size);
  Result.MaxValue:= AskQuery (LoadedSVMHandles.MemberAt [0], FeatureVectorBasedOnGradiant);
  Result.MaxIndex:= 0;
  SVMRating [0]:= 0;
  Scores [0]:= Result.MaxValue;
  
  for i:= 1 to LoadedSVMHandles.Size- 1 do
  begin
    Temp:= AskQuery (LoadedSVMHandles.MemberAt [i], FeatureVectorBasedOnGradiant);
    Scores [i]:= Temp;
    
    if Result.MaxValue< Temp then
    begin
      Result.MaxIndex:= i;
      Result.MaxValue:= Temp;

    end;

   SVMRating [i]:= i;
   
  end;

  for i:= 0 to LoadedSVMHandles.Size- 1 do
    for j:= 0 to i- 1 do
      if Scores [SVMRating [j]]< Scores [SVMRating [i]] then
      begin
        SVMRating [i]:= SVMRating [i] xor SVMRating [j];         
        SVMRating [j]:= SVMRating [i] xor SVMRating [j];         
        SVMRating [i]:= SVMRating [i] xor SVMRating [j];

      end;

end;

function TSVMClient.AskQuery (SVMHandle: TSVMHandle;
  FeatureVectorBasedOnGradiant: TDoubleCollection): Extended;
var
  i: Integer;

begin
  RecognitionEngineTCPConnection.WriteLn ('ExecQuery');
  RecognitionEngineTCPConnection.WriteLn (IntToStr (SVMHandle));
  RecognitionEngineTCPConnection.WriteLn (IntToStr (FeatureVectorBasedOnGradiant.Size));
  
  for i:= 0 to FeatureVectorBasedOnGradiant.Size- 1 do
    RecognitionEngineTCPConnection.WriteLn (
     FloatToStr(FeatureVectorBasedOnGradiant.Member [i]));

  Result:= StrToFloat (RecognitionEngineTCPConnection.ReadLn)
  
end;

constructor TSVMClient.Create (ServerHost: String; ServerPort: Integer);
begin
  inherited Create;
  
  LoadedSVMHandles:= TLongWordCollection.Create;
  
  RecognitionEngineTCPConnection:= TIdTCPClient.Create (nil);
  RecognitionEngineTCPConnection.Host:= ServerHost;
  RecognitionEngineTCPConnection.Port:= ServerPort;
  
  try
    RecognitionEngineTCPConnection.Connect;

    RecognitionEngineTCPConnection.WriteLn ('SRRF');
    RecognitionEngineTCPConnection.WriteLn ('OCR');

  except

  end;

end;

destructor TSVMClient.Destroy;
begin
  if RecognitionEngineTCPConnection.Connected then
    RecognitionEngineTCPConnection.Disconnect;
    
  RecognitionEngineTCPConnection.Free;

  LoadedSVMHandles.Free;

  inherited;
  
end;

function TSVMClient.FindClass (
  FeatureVectorBasedOnGradiant: TDoubleCollection): String;
var
  i: Integer;
  Max, Temp: Extended;
  MaxIndex: Integer;
  Ptr: PObject;

begin
  if LoadedSVMCount= 0 then
    raise ENoSVMLoaded.Create;

  Ptr:= LoadedSVMHandles.GetPointerToFirst;

  Max:= AskQuery ((PLongWord (Ptr^))^, FeatureVectorBasedOnGradiant);
  MaxIndex:= 0;

  for i:= 1 to LoadedSVMCount- 1 do
  begin
    Inc (Ptr);
    Temp:= AskQuery ((PLongWord (Ptr^))^, FeatureVectorBasedOnGradiant);

    if Max< Temp then
    begin
      Max:= Temp;
      MaxIndex:= i;

    end;

  end;

  Result:= IntToStr (MaxIndex);//Trainingset [i]
   
end;

function TSVMClient.GetLoadedSVMCount: Integer;
begin
  Result:= LoadedSVMHandles.Size;
end;

function TSVMClient.GetSVMHandle (SVMName, Kernel: String): TSVMHandle;
begin
  raise ENotImplemented.Create ('GetSVMHandle');
  
end;

function TSVMClient.ListAllLoadedSVMS: TStringList;
var
  SVMFileName,
  KernelFileName: String;
  
begin
  Result:= TStringList.Create;
  RecognitionEngineTCPConnection.WriteLn ('SVMsList');
  SVMFileName:= RecognitionEngineTCPConnection.ReadLn;

  while SVMFileName<> '****' do
  begin
    KernelFileName:= RecognitionEngineTCPConnection.ReadLn;
    Result.Add (ExtractFileName (SVMFileName)+ ':'+ ExtractFileName (KernelFileName));

    SVMFileName:= RecognitionEngineTCPConnection.ReadLn;

  end;
  
end;

procedure TSVMClient.LoadSVMs (ConfigFilename: String);
var
  InputFile: TextFile;
  KernelFileName, SVMFileName: String;

begin
  AssignFile (InputFile, ConfigFilename);
  Reset (InputFile);
  while not Eof (InputFile) do
  begin
    ReadLn (InputFile, KernelFileName);
    ReadLn (InputFile, SVMFileName);
    OpenSVM (KernelFileName, SVMFileName);

  end;

  CloseFile (InputFile);

end;

function TSVMClient.OpenSVM (KernelFileName,
  SVMFileName: String): TSVMHandle;
begin
  RecognitionEngineTCPConnection.WriteLn ('LoadSVM');
  RecognitionEngineTCPConnection.WriteLn (KernelFileName);
  RecognitionEngineTCPConnection.WriteLn (SVMFileName);

  Result:= RecognitionEngineTCPConnection.ReadInteger;
  
  if Result= -1 then
    raise ESVMNotFound.Create (KernelFileName, SVMFileName);
  LoadedSVMHandles.Add (Result);
  
end;

procedure TSVMClient.WriteIntoSVM (SVMHandle: TSVMHandle;
  FeatureVectorBasedOnGradiant: TFeatureVectorBasedOnGradiant);
var
  i: Integer;
//  DoublePtr: PDouble;
  
begin

  RecognitionEngineTCPConnection.WriteLn (IntToStr (SVMHandle));
  for i:= 0 to FeatureVectorBasedOnGradiant.Size- 1 do
    RecognitionEngineTCPConnection.WriteLn (
     FloatToStr(FeatureVectorBasedOnGradiant.Member [i]));
     
end;

{ ESVMNotFound }

constructor ESVMNotFound.Create(KernelFileName, SvmFileName: String);
begin
  inherited Create ('One or both of '+
    KernelFileName+ ' '+ SvmFileName+ ' not Found!');

end;

{ THopfieldClient }

function TKohonenClient.AskQuery (Query: TByteArray): Integer;
var
  SourcePtr, TargetPtr: PByte;
  Source, Target: Integer;
  i: Integer;
  Last18Ch, Ch: char;
  Last18Str: String;
                    
begin
  RecognitionEngineTCPConnection.WriteInteger ($0A000000);
  RecognitionEngineTCPConnection.WriteInteger ($04000000);
  RecognitionEngineTCPConnection.WriteInteger (0);

  Source:= Length (Query);
  Target:= 0;
  TargetPtr:= @Target;
  Inc (TargetPtr, 3);
  SourcePtr:= @Source;

  for i:= 1 to 4 do
  begin
    TargetPtr^:= SourcePtr^;
    Inc (SourcePtr);
    Dec (TargetPtr);

  end;
  RecognitionEngineTCPConnection.WriteInteger (Target);

  RecognitionEngineTCPConnection.WriteBuffer (Query [0], Length (Query), True);
  Last18Ch:= #0;
  Ch:= RecognitionEngineTCPConnection.ReadChar;
  Last18Str:= '';
  i:= 0;
  
  while Last18Str<> #0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!' do
  begin
    Last18Str:= Last18Str+ Ch;
    if 18< i then
    begin
      Last18Ch:= Last18Str [1];
      Delete (Last18Str, 1, 1);

    end;

    if Copy (Last18Str, 1, Length (#0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!'))=
       #0'F'#0'i'#0'n'#0'i'#0's'#0'h'#0'e'#0'd'#0'!' then
      Break;

    Ch:= RecognitionEngineTCPConnection.ReadChar;
    Inc (i);

  end;

  Result:= Integer (Last18Ch);

end;

constructor TKohonenClient.Create(ServerHost: string;
  ServerPort: Integer; MyIP: String);
var
  RevLen,
  Len: Integer;
  Source, Dest: PByte;
  i,
  IntTemp: Integer;

begin
  RecognitionEngineTCPConnection:= TIdTCPClient.Create (nil);
  RecognitionEngineTCPConnection.Host:= ServerHost;
  RecognitionEngineTCPConnection.Port:= ServerPort;

  try
    RecognitionEngineTCPConnection.Connect;

    RecognitionEngineTCPConnection.WriteInteger ($0B000000);
    RecognitionEngineTCPConnection.WriteInteger ($04000000);
    RecognitionEngineTCPConnection.WriteInteger (0);

    Len:= 2* Length (MyIP);
    RevLen:= 0;
    Source:= @Len;
    Dest:= @RevLen;
    Inc (Dest, 3);

    for i:= 0 to 3 do
    begin
      Dest^:= Source^;
      Dec (Dest);
      Inc (Source);

    end;

    RecognitionEngineTCPConnection.WriteInteger (RevLen);
    Len:= Len div 2;
    for i:= 1 to Len do
    begin
      IntTemp:= Ord (MyIp [i])* 256;
      IntTemp:= IntTemp mod 65536;
      RecognitionEngineTCPConnection.WriteSmallInt (IntTemp);
      
    end;

{    RecognitionEngineTCPConnection.WriteLn ('SRRF');
    RecognitionEngineTCPConnection.WriteLn ('LPR');
}
  except

  end;


end;

destructor TKohonenClient.Destroy;
begin
  RecognitionEngineTCPConnection.Disconnect;
  RecognitionEngineTCPConnection.Free;
  
  inherited;

end;

{ TSVMClients }

procedure TSVMClients.AddNewSVMClient (SVMClient: TSVMClient);
begin
  inherited Add (SVMClient);
  
end;

function TSVMClients.GetSVMClient (Index: Integer): TSVMClient;
begin
  Result:= Member [Index] as TSVMClient;
  
end;

procedure ReadTrainSet (FileStream: TMyFileStream;
         DataSet: TBaseCollection; Targets: TIntegerCollection);
var
  i, Size: Integer;
  Temp: Extended;
  S, STemp: String;
  NewDoubleCollection: TDoubleCollection;
  StreamPos: Int64;
  
begin
  S:= FileStream.ReadLine;
  if S= '@examples' then
  begin
    S:= FileStream.ReadLine;
    Delete (S, 1, Pos (' ', S));
    Size:= StrToInt (S);

    if Size= 0 then
    begin
      StreamPos:= FileStream.Position;
      S:= FileStream.ReadLine;
      S:= TrimLeft (TrimRight (S));
      if S<> 'format xy' then
        raise EInvalidFile.Create ('"format xy" was expected but '+ S+ ' found!');
      S:= FileStream.ReadLine;
      
      S:= TrimLeft (TrimRight (S));
      S:= S+ ' ';
      while Pos (' ', S)<> 0 do
      begin
        Inc (Size);
        Delete (S, 1, Pos (' ', S));
        S:= TrimLeft (S);
        
      end;
      Dec (Size);  

      FileStream.Position:=  StreamPos;
    end;
    S:= FileStream.ReadLine;
    S:= TrimLeft (TrimRight (S));
  
    if S= 'format xy' then
    begin
      while FileStream.Position< FileStream.Size do
      begin
        S:= FileStream.ReadLine;
        NewDoubleCollection:= TDoubleCollection.Create;

        for i:= 1 to Size do
        begin
          S:= TrimLeft (S);
          Temp:= StrToFloat (Copy (S, 1, Pos (' ', S)- 1));
          NewDoubleCollection.Add (Temp);
          Delete (S, 1, Pos (' ', S));
        
        end;
        S:= TrimLeft (S);
        Targets.Add (StrToInt (S));
        DataSet.Add (NewDoubleCollection);

      end;

    end

  end
  else
  begin
    S:= Trim (S);
    S:= S+ ' ';
    STemp:= S;
    Size:= 0;
    
    while S<> '' do
    begin
      S:= TrimLeft (S);
      
      Temp:= StrToFloat (Copy (S, 1, Pos (' ', S)- 1));
      Delete (S, 1, Pos (' ', S));
      Inc (Size);
      
    end;
    Dec (Size);
    S:= STemp;
    NewDoubleCollection:= TDoubleCollection.Create;

    for i:= 1 to Size do
    begin
      S:= TrimLeft (S);
      Temp:= StrToFloat (Copy (S, 1, Pos (' ', S)- 1));
      NewDoubleCollection.Add (Temp);
      Delete (S, 1, Pos (' ', S));

    end;

    S:= Trim (S);
    Targets.Add (StrToInt (S));
    DataSet.Add (NewDoubleCollection);

    while FileStream.Position< FileStream.Size do
    begin
      S:= FileStream.ReadLine;
      NewDoubleCollection:= TDoubleCollection.Create;

      for i:= 1 to Size do
      begin
        S:= TrimLeft (S);
        Temp:= StrToFloat (Copy (S, 1, Pos (' ', S)- 1));
        NewDoubleCollection.Add (Temp);
        Delete (S, 1, Pos (' ', S));
        
      end;
      S:= TrimLeft (S);
      Targets.Add (StrToInt (S));
      DataSet.Add (NewDoubleCollection);

    end;
  end;

end;

procedure CalculateApha_BetaForProb (Outs: TDoubleCollection;
   Targets: TIntegerCollection; var aOut, bOut: Extended);
var
  Prior1, Prior0: Integer;
  i: Integer;
  HiTarget, LoTarget, Lambda,
  Err, Temp, t: Extended;
  pp: array of Double;
  Count: Integer;
  it: Integer;
  a, b, c, d, e: Extended;
  OldA, OldB, Olderr,
  Det, p, Diff, Scale,
  d1, d2: Extended;
  FS: TMyFileStream;
begin
  FS:= TMyFileStream.Create ('C:\Output.txt', fmCreate);
  for i:= 0 to Outs.Size- 1 do
    FS.WriteLine (FloatToStr (Outs.Member [i]));

  FS.Free;

  Prior1:= 0; Prior0:= 0;
  for i:= 0 to Targets.Size- 1 do
  begin
    if 0< Targets.MemberAt [i] then
      Inc (Prior1)
    else
      Inc (Prior0);

  end;

  aOut:= 0;
  bOut:= ln ((Prior0+ 1)/(Prior1+ 1));

  HiTarget:= (Prior1+ 1)/ (Prior1+ 2);
  LoTarget:= 1.0/ (Prior0+ 2);
  Lambda:= 1e-3;
  Olderr:= 1e300;
  SetLength (pp, Targets.Size);
  Temp:= (Prior1+ 1)/ (Prior0+ Prior1+ 2); 
  for i:= 0 to Targets.Size- 1 do
    pp [i]:= Temp;
  
  for it:= 0 to 100 do
  begin
    a:= 0; b:= 0; c:= 0; d:= 0; e:= 0;

    for i:= 0 to Targets.Size- 1 do
    begin
      if 0< Targets.MemberAt [i] then
        t:= HiTarget
      else
        t:= LoTarget;

      d1:= pp [i]- t;
      d2:= pp [i]* (1- pp [i]);
      a:= a+ Outs.Member [i]* Outs.Member [i]* d2;
      b:= b+ d2;
      c:= c+ Outs.Member [i]* d2;
      d:= d+ Outs.Member [i]* d1;
      e:= e+ d1;
      
    end;

    if (abs (d)< 1e-9) and (Abs (e)< 1e-9) then
      Break;

    OldA:= aOut; OldB:= bOut;
    Err:= 0;

    while (True) do
    begin
      Det:= (a+ Lambda)* (b+ Lambda)- c* c;
      if Abs (Det)< 1e-10 then
      begin
        Lambda:= Lambda* 10;
        Continue;

      end;

      aOut:= OldA+ ((b+ Lambda)* d- c* e)/ Det;
      bOut:= OldB+ ((a+ Lambda)* e- c* d)/ Det;

      Err:= 0;
      for i:= 0 to Targets.Size- 1 do
      begin
        p:= 1/ (1+ Exp (Outs.Member [i]* aOut+ bOut));
        pp [i]:= p;
        if 1e-9< Abs (p)  then
          Err:= Err- (t* ln (p)+ (1- t)* ln (1- p))
        else
          Err:= Err- (-200* t);

        if Err< Olderr* (1+ 1e-7) then
        begin
          Lambda:= Lambda* 0.1;
          Break;
          
        end;

        Lambda:= Lambda* 10;
        if 1e6< Lambda then
          Break;

      end;

      Diff:= Err- Olderr;
      Scale:= 0.5* (Err++ Olderr+ 1);
      if (-1e-3* Scale< Diff) and  (Diff< 1e-7* Scale) then
        Inc (Count)
      else
        Count:= 0;

      OldErr:= 0;
      if Count= 3 then
        Break;
        
    end;


  end;


end;

{ TMultiLayerSVMClient }

destructor TMultiLayerSVMClient.Destroy;
begin
  Root.Free;
  
  inherited;
  
end;

function TMultiLayerSVMClient.FindClass (
  FeatureVectorBasedOnGradiant: TDoubleCollection): String;
  
begin
  Result:= Root.FindClass (FeatureVectorBasedOnGradiant);
  
end;

procedure TMultiLayerSVMClient.LoadSVMS (ConfigFilename: String);
var
  InputFileStream: TMyFileStream;
  KernelFileName, SVMFileName: String;
  Nodes: TBaseCollection;
  NewNode: TMultiLayerSVMClientNode;
  Count: Integer;
  i: Integer;
  VersionInfo: String;

  procedure LoadVersion1;
  var
    i, j: Integer;
    ActiveNode: TMultiLayerSVMClientNode;
    S: String;
    
  begin
    Count:= StrToInt (InputFileStream.ReadUnCommentedLine);
    Nodes.Allocate (Count);

    while 0< Count do
    begin
      NewNode:= TMultiLayerSVMClientNode.Create (Self);
      NewNode.Load (InputFileStream);
      Nodes.Member [NewNode.ID]:= NewNode;
      Dec (Count);

    end;

    for i:= 0 to Nodes.Size- 1 do
    begin
      ActiveNode:= (Nodes.Member [i] as TMultiLayerSVMClientNode);
      for j:= 0 to ActiveNode.ChildsID.Count- 1 do
      begin
        S:= ActiveNode.ChildsID.Strings [j];
        ActiveNode.Childs.Member [j]:= Nodes.Member [StrToInt (S)];
         
      end;

    end;

    FRoot:= Nodes.Member [0] as TMultiLayerSVMClientNode;
    Nodes.Clear;
    Nodes.Free;
    
  end;
   
begin
  InputFileStream:= TMyFileStream.Create (ConfigFilename, fmOpenRead);

  Nodes:= TBaseCollection.Create;
  VersionInfo:= InputFileStream.ReadUnCommentedLine;
  if UpperCase (VersionInfo)= 'VERSION 1' then
    LoadVersion1;
  
  InputFileStream.Free;
  

end;

{ TSVMClassifierInfo }

constructor TMultiLayerSVMClientNode.Create (Parent: TMultiLayerSVMClient);
begin
  inherited Create;

  Self.Parent:= Parent;
  SVMFileName:= TStringList.Create;
  KernelFileName:= TStringList.Create;
  ChildsID:= TStringList.Create;
  ChildsSVMID:= TIntegerCollection.Create;
  Childs:= TBaseCollection.Create;
  
end;

destructor TMultiLayerSVMClientNode.Destroy;
begin
  Childs.Free;
  SVMFileName.Free;
  KernelFileName.Free;
  ChildsID.Free;
  ChildsSVMID.Free;
  
  inherited;

end;

function TMultiLayerSVMClientNode.FindClass (
  FeatureVectorBasedOnGradiant: TDoubleCollection): String;
var
  i, MaxIndex: Integer;
  v, Max: Extended;
    
begin
  if 0< ChildsSVMID.Size then
  begin
    Max:= Parent.AskQuery (ChildsSVMID.MemberAt [0], FeatureVectorBasedOnGradiant);
    MaxIndex:= 0;
    
    for i:= 1 to ChildsSVMID.Size- 1 do
    begin
      v:= Parent.AskQuery (ChildsSVMID.MemberAt [i], FeatureVectorBasedOnGradiant);
      if Max< v then
      begin
        Max:= v;
        MaxIndex:= i;

      end;

    end;

    Result:= (Childs.Member [MaxIndex] as TMultiLayerSVMClientNode).FindClass (FeatureVectorBasedOnGradiant);

  end
  else
    Result:= Name;
  
end;

procedure TMultiLayerSVMClientNode.Load (InputStream: TMyFileStream);
var
  n: Integer;
  NewNode: TMultiLayerSVMClientNode;
  Kernel, SVMFile, S: String;

begin
  Name:= InputStream.ReadUnCommentedLine;
  ID:= StrToInt (InputStream.ReadUnCommentedLine);

  n:= StrToInt (InputStream.ReadUnCommentedLine);
  Childs.Allocate (n);
  
  while 0< n do
  begin
    Kernel:= InputStream.ReadUnCommentedLine;
    KernelFileName.Add (Kernel);
    SVMFile:= InputStream.ReadUnCommentedLine;
    SVMFileName.Add (SVMFile);
    S:= InputStream.ReadUnCommentedLine;
    ChildsID.Add (S);
    ChildsSVMID.Add (Parent.OpenSVM (Kernel, SVMFile));

    Dec (n);

  end;

end;

{ ENoSVMLoaded }

constructor ENoSVMLoaded.Create;
begin
  inherited Create ('No SVM is loaded!');
  
end;

end.

