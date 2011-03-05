unit WaveFileUnitParser;

interface
uses
  MyFileStreamerTokenizerUnit, WaveFileDescriptorUnit,
  CollectionUnit, MyTypes, Classes, SysUtils;

type
  TWaveFileParser= class (TObject)
  private
    FileStream: TMyFileStream;

    function ParseHeader: Boolean;
    
  public
    constructor Create (FileName: String);
    procedure Free;

    function Parse: TWaveFileDescriptor;

  end;

implementation



{ TWaveFileParser }

constructor TWaveFileParser.Create (FileName: String);
begin
  inherited Create;

  FileStream:= TMyFileStream.Create (FileName, fmOpenRead);

end;

procedure TWaveFileParser.Free;
begin
  FileStream.Free;
  
  inherited;

end;

function TWaveFileParser.Parse: TWaveFileDescriptor;
var
  FileSize: Cardinal;
  ChunkID: TChunkID;
  NewChunk: TChunk;

begin

  FileStream.Expect ('RIFF');
  FileSize:= FileStream.ReadCardinal;
  if FileSize+ 8<> FileStream.Size then
    raise EInvalidFile.Create ('Invalid Size');

  FileStream.Expect ('WAVE');
  
  while FileStream.Position< FileStream.Size do
  begin
    ChunkID:= FileStream.ReadChunkID;

    case ChunkID [2] of
      'm', 'M'://fmt .
      begin
        NewChunk:= TFMTChunk.Create;
        NewChunk.Parse (FileStream);
        Result.AddChunk (NewChunk);
        
      end; 
        
      'a', 'A'://fact.
      begin
        NewChunk:= TFMTChunk.Create;
        NewChunk.Parse (FileStream);
        Result.AddChunk (NewChunk);
        
      end; 
        
    end;
    
  end;

end;

function TWaveFileParser.ParseHeader: Boolean;
begin
  
end;

end.
