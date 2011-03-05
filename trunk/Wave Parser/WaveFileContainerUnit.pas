unit WaveFileContainerUnit;

interface
uses
  CollectionUnit, MyTypes;

type
  TChunk= class (TObject)
  private
    FChunkName: String;

  public
    property ChunkName: String read FChunkName;
    
    constructor Create (ChunkName: String);
    procedure Free;
    
  end;

  TChunkCollection= class (TBaseCollection)
  private
    function GetChunk(Index: Integer): TChunk;
  public
    property Chunk [Index: Integer]: TChunk read GetChunk;

    procedure AddChunk (NewChunk: TChunk);
    
  end;
  
  TWaveChunk= class (TChunk)
  private
  public
    constructor Create;
    procedure Free;

  end;

implementation


{ TChunk }

constructor TChunk.Create (ChunkName: String);
begin
  inherited Create;

  FChunkName:= ChunkName;

end;

procedure TChunk.Free;
begin
  inherited Free;
  
end;

{ TWaveChunk }

constructor TWaveChunk.Create;
begin
  inherited Create ('WAVE');
  
end;

procedure TWaveChunk.Free;
begin

  inherited;

end;

{ TChunkCollection }

procedure TChunkCollection.AddChunk (NewChunk: TChunk);
begin
  inherited Add (NewChunk);

end;

function TChunkCollection.GetChunk (Index: Integer): TChunk;
begin
  Result:= Member [Index] as TChunk;
  
end;

end.
