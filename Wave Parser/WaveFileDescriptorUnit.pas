unit WaveFileDescriptorUnit;

interface
uses
  CollectionUnit, MyTypes, MyFileStreamerTokenizerUnit,
  SysUtils, VirtualDiskUnit;

type
  ENoFMTChunk= class (Exception)
  public
    constructor Create;
    
  end;

  TChunk= class (TObject)
  private
    procedure Parse (FileStream: TMyFileStream); virtual;
    
  protected
    FChunkName: String;
    FDataSize: Integer;


  public
    property ChunkName: String read FChunkName;
    property DataSize: Integer read FDataSize;

    constructor Create (ChunkName: String);
    procedure Free;


  end;

  TChunkCollection= class (TBaseCollection)
  private
    function GetChunk (Index: Integer): TChunk;
    procedure FreeAllChunk;
    
  public
    property Chunk [Index: Integer]: TChunk read GetChunk;

    procedure AddChunk (NewChunk: TChunk);

    procedure Free;
  end;

  TCompressionCode= (ccUnknown= 0, ccPCM_uncompressed= 1,
    ccMicrosoftADPCM= 2, ccITUG711a_law= 6,
    ccITUG_711= 7, ccIMAADPCM= 17, ccITUG_723= 20,
    ccGSM6_10= 49, ccITUG_721= 64, ccMPEG= 80,
    ccExperimental= 65536);
    
  TFMTChunk= class (TChunk)
  private
    FCompressionCode: TCompressionCode;
    FNumberOfChannel: Word;
    FSampleRate: Cardinal;
    FAverageSamplePerSecond: Cardinal;
    FBlockAlign: Word;
    FSignificantBitPerSample: Word;
    FExtraBytes: Word;

    procedure Parse (FileStream: TMyFileStream); override;
    
  public
    property CompressionCode: TCompressionCode read FCompressionCode;
    property SampleRate: Cardinal read FSampleRate;
    property NumberOfChannels: Word read FNumberOfChannel;
    property SignificantBitPerSample: Word read FSignificantBitPerSample;

    constructor Create;
    procedure Free;

  end;

  TFactChunk= class (TChunk)
  private
    FBytesInChunk: TByteCollection;
    procedure Parse (FileStream: TMyFileStream); override;

  public
    property BytesInChunk: TByteCollection read FBytesInChunk;
    
    constructor Create;
    procedure Free;
    

  end;

  TDataChunk= class (TChunk)
  private
    EachSampleSize, ChannelCount: Integer;
    FChannelsData: TBaseCollection;

    procedure Parse (FileStream: TMyFileStream); override;
    
    function GetChannelData (Index: Integer): TInt64Collection;

  public
    property ChannelData [Index: Integer]: TInt64Collection read GetChannelData;
    property NumberOfChannel: Integer read ChannelCount;
    
    constructor Create (EachSampleSize, ChannelCount: Integer);
    procedure Free;
    
  end;

  TWavlChunk= class (TChunk)
  private
    EachSampleSize, ChannelCount: Integer;
    DataChunk: TDataChunk;

    procedure Parse (FileStream: TMyFileStream); override;

  public
    constructor Create (EachSampleSize, ChannelCount: Integer);
   procedure Free;

  end;

  TSlntChunk= class (TChunk)
  private
    FLength: Integer;
    
    procedure Parse (FileStream: TMyFileStream); override;
    
  public
    property Length: Integer read FLength;
    
    constructor Create;
    
  end;

  TCuePoint= class (TObject)
  private
    FDataChunkID: Cardinal;
    FChunkStart: Cardinal;
    FPosition: Cardinal;
    FBlockStart: Cardinal;
    FID: Cardinal;
    FSampleOffset: Cardinal;

    procedure Parse (FileStream: TMyFileStream); 

  public
    property ID: Cardinal read FID;
    property Position: Cardinal read FPosition;
    property DataChunkID: Cardinal read FDataChunkID;
    property ChunkStart: Cardinal read FChunkStart;
    property BlockStart: Cardinal read FBlockStart;
    property SampleOffset: Cardinal read FSampleOffset;
        
    constructor Create;

  end;

  TCueCollection= class (TBaseCollection)
  private
    function GetCuePoint (Index: Integer): TCuePoint;

  public
    property CuePoint [Index: Integer]: TCuePoint read GetCuePoint;
    
    constructor Create;
    procedure Free;
    
  end;

  TCueChunk= class (TChunk)
  private
    FCuePointCollection: TCueCollection;

    procedure Parse (FileStream: TMyFileStream); override;
    
  public
    property CuePointCollection: TCueCollection read FCuePointCollection;
    
    constructor Create;
    procedure Free;
    
  end;

  TOtherChunks= class (TChunk)
  private
    procedure Parse (FileStream: TMyFileStream); override;

  public
    constructor Create (ChunkName: String);

  end;

  TWaveFileDescriptor= class (TChunkCollection)
  private
    FAllSignals: TLongWordCollection;
    FCuePoints: TCueCollection;
    function GetFMTChunk: TFMTChunk;
    function GetChunkByID (ChunkID: TChunkID): TChunkCollection;
    function GetDataChunk: TDataChunk;
    function GetDataSignals: TInt64Collection;

  public
    property ChunkByID [ChunkID: TChunkID]: TChunkCollection read GetChunkByID;
    property FMTChunk: TFMTChunk read GetFMTChunk;
    property DataChunk: TDataChunk read GetDataChunk;
    property CuePoints: TCueCollection read FCuePoints;
    property AllDataSignals: TInt64Collection read GetDataSignals;

    constructor Create (FileName: String);
    procedure Free;

    procedure SaveInVirtualDisk (FileWriter: TFileWriter);
    
  end;

implementation


{ TChunk }

constructor TChunk.Create (ChunkName: String);
begin
  inherited Create;

  FChunkName:= UpperCase (ChunkName);

end;

procedure TChunk.Free;
begin
  inherited Free;
  
end;

procedure TChunk.Parse (FileStream: TMyFileStream);
begin
  FDataSize:= FileStream.ReadCardinal;
  
end;

{ TFMTChunk }

constructor TFMTChunk.Create;
begin
  inherited Create ('FMT ');
  
end;

procedure TFMTChunk.Free;
begin

  inherited;

end;

procedure TFMTChunk.Parse (FileStream: TMyFileStream);
var
  i: Integer;

begin
  inherited;
  
  FCompressionCode:= TCompressionCode (FileStream.ReadWord);
  FNumberOfChannel:= FileStream.ReadWord;
  FSampleRate:= FileStream.ReadCardinal;
  FAverageSamplePerSecond:= FileStream.ReadCardinal;
  FBlockAlign:= FileStream.ReadWord;
  FSignificantBitPerSample:= FileStream.ReadWord;
  if not (FSignificantBitPerSample in [8, 16, 24, 32]) then
    raise EInvalidFile.Create ('Invalid SignificantBitPerSample!');
    
{  if CompressionCode<> ccPCM_uncompressed then
  begin
  }
    FExtraBytes:= FileStream.ReadWord;
    for i:= 1 to FExtraBytes do
      FileStream.ReadByte;

//  end;

end;

{ TChunkCollection }

procedure TChunkCollection.AddChunk (NewChunk: TChunk);
begin
  inherited Add (NewChunk);

end;

procedure TChunkCollection.Free;
begin
  inherited Free;
  
end;

procedure TChunkCollection.FreeAllChunk;
var
  Ptr: PObject;
  i: Integer;

begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
    TChunk (Ptr^).Free;

  inherited Free;
  
end;

function TChunkCollection.GetChunk (Index: Integer): TChunk;
begin
  Result:= Member [Index] as TChunk;
  
end;

{ TWaveFileDescriptor }

constructor TWaveFileDescriptor.Create (FileName: String);
var
  FileStream: TMyFileStream;
  FileSize: Cardinal;
  ChunkID: TChunkID;
  NewChunk: TChunk;
  Parsed: Boolean;
  
begin
  inherited Create;

  FileStream:= TMyFileStream.Create (FileName, fmOpenRead);
  FCuePoints:= TCueCollection.Create;
  
  FileStream.Expect ('RIFF');
  FileSize:= FileStream.ReadCardinal;
  if FileSize+ 8<> FileStream.Size then
    raise EInvalidFile.Create ('Invalid Size');

  FileStream.Expect ('WAVE');
  
  while FileStream.Position< FileStream.Size do
  begin
    ChunkID:= FileStream.ReadChunkID;
    Parsed:= False;
    
    case ChunkID [4] of
      ' '://fmt . or cue .
      begin
        if ChunkID [1]= 'f' then//fmt .
        begin
          NewChunk:= TFMTChunk.Create;
          NewChunk.Parse (FileStream);
          AddChunk (NewChunk);
          Parsed:= True;

        end
        else if ChunkID [1]= 'c' then//cue .
        begin
          NewChunk:= TCueChunk.Create;
          NewChunk.Parse (FileStream);
          AddChunk (NewChunk);
          FCuePoints.Free;
          FCuePoints:= TCueChunk (NewChunk).CuePointCollection;
          Parsed:= True;

        end;
        
      end;
        
      't', 'T'://fact.
      begin
        NewChunk:= TFactChunk.Create;
        NewChunk.Parse (FileStream);
        AddChunk (NewChunk);
        Parsed:= True;

      end;

      'a', 'A'://data.
      begin
        NewChunk:= TDataChunk.Create (FMTChunk.SignificantBitPerSample,
          FMTChunk.NumberOfChannels);
        NewChunk.Parse (FileStream);
        AddChunk (NewChunk);
        Parsed:= True;

      end; 
        
      'l', 'L'://Wavl.
      begin
        NewChunk:= TWavlChunk.Create (FMTChunk.FSignificantBitPerSample, FMTChunk.FNumberOfChannel);
        NewChunk.Parse (FileStream);
        AddChunk (NewChunk);
        Parsed:= True;

      end;

    end;

    if not Parsed then
    begin
      NewChunk:= TOtherChunks.Create (ChunkID);
      NewChunk.Parse (FileStream);
      AddChunk (NewChunk);

    end;

  end;
  
  FileStream.Free;
  
end;

procedure TWaveFileDescriptor.Free;
begin
  FCuePoints.Free;
  
end;

function TWaveFileDescriptor.GetChunkByID (ChunkID: TChunkID): TChunkCollection;
var
  i: Integer;
  Ptr: PObject;
  ActiveChunk: TChunk;

begin
  ChunkID:= UpperCase (ChunkID);
  Result:= TChunkCollection.Create;

  Ptr:= GetPointerToFirst;
  for i:= 1 to Size do
  begin
    ActiveChunk:= TChunk (Ptr^);
    if ActiveChunk.ChunkName= ChunkID then
      Result.AddChunk (ActiveChunk);

    Inc (Ptr);
    
  end;

end;

function TWaveFileDescriptor.GetDataChunk: TDataChunk;
begin
  Result:=  (ChunkByID ['data'].Chunk [0]) as TDataChunk;
  
end;

function TWaveFileDescriptor.GetDataSignals: TInt64Collection;
begin
   Result:= DataChunk.ChannelData [0];
   
end;

function TWaveFileDescriptor.GetFMTChunk: TFMTChunk;
var
  Temp: TChunkCollection;

begin
  Temp:= ChunkByID ['fmt '];
  if Temp.Size= 0 then
    raise ENoFMTChunk.Create;
    
  Result:= Temp.Chunk [0] as TFMTChunk;
  
end;

procedure TWaveFileDescriptor.SaveInVirtualDisk (FileWriter: TFileWriter);
var
  j: Integer;
  i: Int64;
  Ptr: PObject;
  Data: TInt64Collection;

begin
  Data:= AllDataSignals;
  Ptr:= Data.GetPointerToFirst;

  i:= 0;                     
  FileWriter.WriteByte (255- 1000000 div FMTChunk.FSampleRate);
  
  while i< AllDataSignals.Size- 1 do
  begin    
    FileWriter.WriteByte (Byte (PInt64 (Ptr^)^));
    Inc (Ptr);
    Inc (i);
    
    if i mod 512= 400 then
    begin
      for j:= 1 to 107 do
        FileWriter.WriteByte (0);
        
      FileWriter.WriteByte (255- 1000000 div FMTChunk.FSampleRate);
      Inc (i, 112);

    end;

  end;
  
  Data.Free;
  

end;

{ ENoFMTChunk }

constructor ENoFMTChunk.Create;
begin
  inherited Create ('No chunk file is in the wave file!');
  
end;

{ TFactChunk }

constructor TFactChunk.Create;
begin
  inherited Create ('Fact');

  FBytesInChunk:= TByteCollection.Create;
  
end;

procedure TFactChunk.Free;
begin
  FBytesInChunk.Free;
  
  inherited;
  
end;

procedure TFactChunk.Parse (FileStream: TMyFileStream);
var
  i: Integer;
  
begin
  inherited;

  for i:= 1 to FDataSize do
    FBytesInChunk.AddByte (FileStream.ReadByte);
  
end;

{ TDataChunk }

constructor TDataChunk.Create (EachSampleSize, ChannelCount: Integer);
var
  i: Integer;

begin
  inherited Create ('data');

  Self.EachSampleSize:= EachSampleSize div 8;
  Self.ChannelCount:= ChannelCount;
  FChannelsData:= TBaseCollection.Create;
  
  for i:= 1 to ChannelCount do
    FChannelsData.Add (TInt64Collection.Create);

end;

procedure TDataChunk.Free;
var
  i: Integer;
  
begin
  FChannelsData.Free;

  inherited Free;

end;

function TDataChunk.GetChannelData (Index: Integer): TInt64Collection;
begin
  Result:= FChannelsData.Member [Index] as TInt64Collection;
  
end;

procedure TDataChunk.Parse (FileStream: TMyFileStream);
var
  Bytes: array [1..10* 1024] of Byte;
  i, j, k: Integer;
  ChData: array of TInt64Collection;
  Pointers: array of PObject ;
  Ptr: PByte;
  n: Integer;

begin
  inherited;

  SetLength (ChData, ChannelCount+ 1);
  for i:= 0 to FChannelsData.Size- 1 do
    ChData [i+ 1]:= FChannelsData.Member [i] as TInt64Collection;

  if (FDataSize) mod (ChannelCount* EachSampleSize)<> 0 then
    raise EInvalidFile.Create ('Invalid Data Size in DataChunk!');

  n:= (FDataSize) div (ChannelCount* EachSampleSize);

  SetLength (Pointers, ChannelCount+ 1);
  for i:= 1 to ChannelCount do
  begin
    ChData [i].FillWithZero (n);
    Pointers [i]:= ChData [i].GetPointerToFirst;

  end;
  if 10< EachSampleSize* ChannelCount then
    raise Exception.Create ('10< EachSampleSize* ChannelCount!');

  for i:= 1 to n div 1024 do
  begin
    FileStream.Read (Bytes [1], 1024* EachSampleSize* ChannelCount);
    Ptr:= @Bytes [1];

    for j:= 1 to 1024 do
      for k:= 1 to ChannelCount do
      begin
        case EachSampleSize of
          1:
            PInt64 (Pointers [k]^)^:= PByte (Ptr)^;
          2:
            PInt64 (Pointers [k]^)^:= PSmallInt (Ptr)^;
          3:
            PInt64 (Pointers [k]^)^:= PInteger (Ptr)^;
          4:
            PInt64 (Pointers [k]^)^:= PInteger (Ptr)^;
              
        end;
        Inc (Pointers [k]);
        Inc (Ptr, EachSampleSize);

      end;

  end;

  if n mod 1024<> 0 then
  begin
    FileStream.Read (Bytes [1], (n mod 1024)* EachSampleSize* ChannelCount);
    Ptr:= @Bytes [1];

    for j:= 1 to (n mod 1024) do
      for k:= 1 to ChannelCount do
      begin
        case EachSampleSize of
          1:
            PInt64 (Pointers [k]^)^:= PByte (Ptr)^- 128;
          2:
            PInt64 (Pointers [k]^)^:= PSmallInt (Ptr)^;
          3:
            PInt64 (Pointers [k]^)^:= PInteger (Ptr)^;
          4:
            PInt64 (Pointers [k]^)^:= PInteger (Ptr)^;
              
        end;
        Inc (Pointers [k]);
        Inc (Ptr, EachSampleSize);

      end;

  end;
  SetLength (Pointers, 0);
  
end;

{ TWavlChunk }

constructor TWavlChunk.Create(EachSampleSize, ChannelCount: Integer);
begin
  inherited Create ('wavl');

  Self.EachSampleSize:= EachSampleSize;
  Self.ChannelCount:= ChannelCount;
  DataChunk:= TDataChunk.Create (EachSampleSize, ChannelCount);
  
end;

procedure TWavlChunk.Free;
begin
  DataChunk.Free;

  inherited;
  
end;

procedure TWavlChunk.Parse (FileStream: TMyFileStream);
var
  l, i, j: Integer;
  ChunkID: TChunkID;
  SLNTChunk: TSlntChunk;
  LocalDataChunk: TDataChunk;
  LastValue: Cardinal;

begin
  inherited;

  if EachSampleSize= 1 then
    LastValue:= 127
  else
    LastValue:= 0;

  l:= 0;
  while l< FDataSize do
  begin
    ChunkID:= FileStream.ReadChunkID;

    if ChunkID= 'slnt' then
    begin
      SLNTChunk:= TSlntChunk.Create;
      SLNTChunk.Parse (FileStream);

      for i:= 1 to SLNTChunk.Length do
        for j:= 0 to ChannelCount- 1 do
          DataChunk.ChannelData [j].Add (LastValue);

      WriteLn ('In Wavl! this method is not tested completely!');
      SLNTChunk.Free;
      
    end
    else if ChunkID= 'data' then
    begin
      LocalDataChunk:= TDataChunk.Create (EachSampleSize, ChannelCount);
      LocalDataChunk.Parse (FileStream);
      
      for j:= 0 to ChannelCount- 1 do
        for i:= 0 to LocalDataChunk.ChannelData [j].Size- 1 do
          DataChunk.ChannelData [j].Add (LocalDataChunk.ChannelData [j].MemberAt [i]);
          
    end;

  end;

end;

{ TSlntChunk }

constructor TSlntChunk.Create;
begin
  inherited Create ('slnt');
  
end;

procedure TSlntChunk.Parse (FileStream: TMyFileStream);
begin
  inherited;

  FLength:= FileStream.ReadCardinal;

end;

{ TCuePoint }

constructor TCuePoint.Create;
begin
  inherited;

  
end;

procedure TCuePoint.Parse (FileStream: TMyFileStream);
begin
  FID:= FileStream.ReadCardinal;
  FPosition:= FileStream.ReadCardinal;
  FDataChunkID:= FileStream.ReadCardinal;
  FChunkStart:= FileStream.ReadCardinal;
  FBlockStart:= FileStream.ReadCardinal;
  FSampleOffset:= FileStream.ReadCardinal;
  
end;

{ TCueCollection }

constructor TCueCollection.Create;
begin
  inherited;

end;

procedure TCueCollection.Free;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    TCuePoint (Ptr^).Free;
    Inc (Ptr);
    
  end;

  inherited Free; 
  
end;

function TCueCollection.GetCuePoint (Index: Integer): TCuePoint;
begin
  Result:= Member [Index] as TCuePoint;
  
end;

{ TCueChunk }

constructor TCueChunk.Create;
begin
  inherited Create ('cue');

end;

procedure TCueChunk.Free;
begin
//  FCuePointCollection.Free;
// There is no need to be freed!

  inherited;
  
end;

procedure TCueChunk.Parse (FileStream: TMyFileStream);
var
  n: Int64;
  CuePoint: TCuePoint;

begin
  inherited;

  n:= FileStream.ReadSignedBytes (4);

  while 0< n do
  begin
    CuePoint:= TCuePoint.Create;
    CuePoint.Parse (FileStream);

    FCuePointCollection.Add (CuePoint);
    Dec (n);

  end;

end;

{ TOtherChunks }

constructor TOtherChunks.Create(ChunkName: String);
begin
  inherited Create (ChunkName);
  
end;

procedure TOtherChunks.Parse (FileStream: TMyFileStream);
const
{$J+}
  Buffer: array of Integer= nil;
{$J-}

var
  i: Integer;

begin
  inherited;

  if Buffer= nil then
    SetLength (Buffer, FDataSize+ 1)
  else if Length (Buffer)< FDataSize then
    SetLength (Buffer, FDataSize+ 1);

//  for i:= 1 to FDataSize do
    FileStream.Read (Buffer, FDataSize);
    

end;

end.
