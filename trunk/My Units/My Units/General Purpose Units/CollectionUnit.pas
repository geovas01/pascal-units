unit CollectionUnit;

interface
type
  TBaseCollection= class (TObject)
  protected
    FMembers: array of TObject;
  private
    FSize: Integer;
    function GetMember (Index: Integer): TObject;

  public
    property Size: Integer read FSize;
    property Member [Index: Integer]: TObject read GetMember;

    function GetPointerToFirst: Pointer;
    constructor Create;
    procedure Free (FreeObj: Boolean= False);

    procedure Add (Data: TObject);
    procedure Delete (Index: Integer);

  end;

  TDoubleCollection= class (TObject)
  private
    FMembers: array of Extended;
    FSize: Integer;
    function GetMember (Index: Integer): Extended;
    function GetPointerToMembers: PExtended;
    function GetMinIndex: Integer;
    function GetMaxIndex: Integer;

  public
    property Size: Integer read FSize;
    property Member [Index: Integer]: Extended read GetMember;
    property PointerToMembers: PExtended read GetPointerToMembers;
    property MinIndex: Integer read GetMinIndex;
    property MaxIndex: Integer read GetMaxIndex;

    constructor Create;
    procedure Free;

    procedure Add (Data: Extended);
    procedure Delete (Index: Integer);
    
  end;

  TLongWordCollection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): LongWord;

  public
    property Size: Integer read FSize;
    property MemberAt [Index: Integer]: LongWord read GetMember;

    constructor Create;
    procedure Free;

    procedure Add (Data: LongWord);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);

  end;
  
implementation

uses ExceptionUnit, Math;

{ TBaseCollection }

procedure TBaseCollection.Add (Data: TObject);
begin
  Inc (FSize);
  SetLength (FMembers, FSize);

  FMembers [FSize- 1]:= Data;  

end;

constructor TBaseCollection.Create;
begin
  inherited;

  FSize:= 0;
  
end;

procedure TBaseCollection.Delete (Index: Integer);
var
  i: Integer;

begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  for i:= Index+ 1 to FSize- 1 do
    FMembers [i- 1]:= FMembers [i];
    
  Dec (FSize);
  SetLength (FMembers, FSize);

end;

procedure TBaseCollection.Free;
var
  i: Integer;

begin
  if FreeObj then
    for i:= 0 to FSize- 1 do
     FMembers [i].Free;

  SetLength (FMembers, 0);
  FSize:= 0;
  
  inherited Free;

end;

function TBaseCollection.GetMember (Index: Integer): TObject;
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  Result:= FMembers [Index];

end;

function TBaseCollection.GetPointerToFirst: Pointer;
begin
  Result:= Pointer (@FMembers [0]);

end;

{ TDoubleCollection }

procedure TDoubleCollection.Add(Data: Extended);
begin
  Inc (FSize);
  SetLength (FMembers, FSize);

  FMembers [FSize- 1]:= Data;  

end;

constructor TDoubleCollection.Create;
begin
  inherited;

  FSize:= 0;
  
end;

procedure TDoubleCollection.Delete(Index: Integer);
var
  i: Integer;

begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  for i:= Index+ 1 to FSize- 1 do
    FMembers [i- 1]:= FMembers [i];
    
  Dec (FSize);
  SetLength (FMembers, FSize);
  
end;

procedure TDoubleCollection.Free;
begin

  SetLength (FMembers, 0);
  FSize:= 0;
  
  inherited Free;

end;

function TDoubleCollection.GetMaxIndex: Integer;
var
  i: Integer;
  MaxValue: Extended;
  PMember: PExtended;

begin
  Result:= 0;
  if Size= 0 then
    Result:= -1
  else
  begin
    MaxValue:= Member [0];
    PMember:= @FMembers [1];

    for i:= 1 to Size- 1 do
    begin
      if MaxValue< PMember^ then
      begin
        MaxValue:= PMember^;
        Result:= i;

      end;

      Inc (PMember);

    end;

  end;

end;

function TDoubleCollection.GetMember (Index: Integer): Extended;
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TBaseCollection.Delete');

  Result:= FMembers [Index];

end;

function TDoubleCollection.GetMinIndex: Integer;
var
  i: Integer;
  MinValue: Extended;
  PMember: PExtended;

begin
  Result:= 0;
  if Size= 0 then
    Result:= -1
  else
  begin
    MinValue:= Member [0];
    PMember:= @FMembers [0];

    for i:= 1 to Size- 1 do
      if PMember^< MinValue then
      begin
        MinValue:= PMember^;
        Result:= i;
                
      end;

  end;

end;

function TDoubleCollection.GetPointerToMembers: PExtended;
begin
  if Size= 0 then
    raise ERangeCheckError.Create ('GetPointerToMembers');
    
  Result:= @FMembers [0];
  
end;

{ TLongWordCollection }

procedure TLongWordCollection.Add (Data: LongWord);
var
  NewPtr: PLongWord;

begin
  NewPtr:= New (PLongWord);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

constructor TLongWordCollection.Create;
begin
  inherited;
  
end;

procedure TLongWordCollection.Delete (Index: Integer);
begin
  Dispose (PLongWord (Member [Index]));
  inherited;
  
end;

procedure TLongWordCollection.FillWithZero (Length: Integer);
var
  i: Integer;

begin
  for i:= 1 to Length do
    Self.Add (0);

end;

procedure TLongWordCollection.Free;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 0 to Size- 1 do
  begin
    Dispose (PLongWord (Ptr^));
    Inc (Ptr);
    
  end;

  inherited Free (False);

end;

function TLongWordCollection.GetMember(Index: Integer): LongWord;
begin
  Result:= PLongWord (Member [Index])^;
  
end;

end.
