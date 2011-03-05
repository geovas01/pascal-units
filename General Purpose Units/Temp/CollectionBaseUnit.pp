unit CollectionBaseUnit;

interface
type
  TBaseCollection= class (TObject)
  protected
    FMembers: array of TObject;

  private
    FSize: Integer;
    function GetMember (Index: Integer): TObject;

  protected
    procedure Allocate (Size: Integer);
    
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

  { TLongWordCollection }

  TLongWordCollection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): LongWord;
    procedure SetMemberAt(Index: Integer; const AValue: LongWord);

  public
    property MemberAt [Index: Integer]: LongWord read GetMember write
      SetMemberAt;

    constructor Create;
    procedure Free;

    procedure Add (Data: LongWord);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);
    function Min: LongWord;
    function Max: LongWord;
    function Avg: LongWord;

  end;

  TInt64Collection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): Int64;

  public
    property MemberAt [Index: Integer]: Int64 read GetMember;

    constructor Create;
    procedure Free;

    procedure Add (Data: Int64);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);
    function Min: Int64;
    function Max: Int64;
    function Avg: Int64;

  end;

  TIntegerCollection= class (TBaseCollection)
  private
    function GetMember (Index: Integer): Integer;

  public
    property MemberAt [Index: Integer]: Integer read GetMember;

    constructor Create;
    procedure Free;

    procedure Add (Data: Integer);
    procedure Delete (Index: Integer);

    procedure FillWithZero (Length: Integer);

  end;

  TByteCollection= class (TBaseCollection)
  private
    function GetByteAt(Index: Integer): Byte;
  public
    property ByteAr [Index: Integer]: Byte read GetByteAt;

    procedure AddByte (AByte: Byte);
    procedure Delete (Index: Integer);

    procedure Free;
    
  end;
  
implementation

uses ExceptionUnit, Math, MyTypes;

{ TBaseCollection }

procedure TBaseCollection.Add (Data: TObject);
begin
  Inc (FSize);
  SetLength (FMembers, FSize);

  FMembers [FSize- 1]:= Data;  

end;

procedure TBaseCollection.Allocate (Size: Integer);
begin
  SetLength (FMembers, Size);
  FSize:= Size;
    
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

procedure TBaseCollection.Free (FreeObj: Boolean= False);
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
    raise ERangeCheckError.Create ('TBaseCollection.GetMember');

  Result:= FMembers [Index];

end;

function TBaseCollection.GetPointerToFirst: Pointer;
begin
  if Size= 0 then
    Result:= nil
  else
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

function TLongWordCollection.Avg: LongWord;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  Result:= 0;

  for i:= 1 to Size do
  begin
    Inc (Result, PLongWord (Ptr^)^);
    Inc (Ptr);
    
  end;

  if Size<> 0 then
    Result:= Result div Size;

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
  Ptr: PObject;
  LongWordPtr: PLongWord;
  
begin
  Allocate (Length);
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Length do
  begin
    New (LongWordPtr);
    LongWordPtr^:= 0;
    Ptr^:= TObject (LongWordPtr);
    Inc (Ptr);

  end;

end;

procedure TLongWordCollection.Free;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PLongWord (Ptr^));
    Inc (Ptr);
    
  end;

  inherited Free (False);

end;

function TLongWordCollection.GetMember (Index: Integer): LongWord;
begin
  Result:= PLongWord (Member [Index])^;
  
end;

procedure TLongWordCollection.SetMemberAt (Index: Integer; const AValue: LongWord);
begin
  PLongWord (Member [Index])^:= AValue;;

end;

function TLongWordCollection.Max: LongWord;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    Inc (Ptr);
    if Result< PLongWord (Ptr^)^ then
      Result:= PLongWord (Ptr^)^

  end;

end;

function TLongWordCollection.Min: LongWord;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    if PLongWord (Ptr^)^< Result then
      Result:= PLongWord (Ptr^)^;
    Inc (Ptr);
    
  end;
  
end;

{ TByteCollection }

procedure TByteCollection.AddByte (AByte: Byte);
var
  Ptr: PByte;

begin
  New (Ptr);
  Ptr^:= AByte;

  inherited Add (TObject (Ptr));
  
end;

procedure TByteCollection.Delete(Index: Integer);
begin
  Dispose (PByte (Member [Index]));

  inherited;

end;

procedure TByteCollection.Free;
var
  i: Integer;
  Ptr: PObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PByte (Ptr^));
    Inc (Ptr);
    
  end;

  inherited Free;
  
end;

function TByteCollection.GetByteAt(Index: Integer): Byte;
begin
  Result:= PByte (Member [Index])^;
  
end;

{ TIntegerCollection }

procedure TIntegerCollection.Add (Data: Integer);
var
  NewPtr: PInteger;

begin
  New (NewPtr);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

constructor TIntegerCollection.Create;
begin
  inherited;
  
end;

procedure TIntegerCollection.Delete (Index: Integer);
begin
  Dispose (PInteger (Member [Index]));

  inherited;
  
end;

procedure TIntegerCollection.FillWithZero (Length: Integer);
var
  i: Integer;

begin
  for i:= 1 to Length do
    Self.Add (0);

end;

procedure TIntegerCollection.Free;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PInteger (Ptr^));
    Inc (Ptr);
    
  end;

  inherited Free (False);

end;

function TIntegerCollection.GetMember(Index: Integer): Integer;
begin
  Result:= PInteger (Member [Index])^;

end;

{ TInt64Collection }

procedure TInt64Collection.Add (Data: Int64);
var
  NewPtr: PInt64;

begin
  NewPtr:= New (PInt64);
  NewPtr^:= Data;
  
  inherited Add (TObject (NewPtr));
  
end;

function TInt64Collection.Avg: Int64;
var
  i: Integer;
  Ptr: PObject;

begin
  Ptr:= GetPointerToFirst;
  Result:= 0;

  for i:= 1 to Size do
  begin
    Inc (Result, PInt64 (Ptr^)^);
    Inc (Ptr);
    
  end;

  if Size<> 0 then
    Result:= Result div Size;

end;

constructor TInt64Collection.Create;
begin
  inherited;

end;

procedure TInt64Collection.Delete (Index: Integer);
begin
  Dispose (PInt64 (Member [Index]));

  inherited;
  
end;

procedure TInt64Collection.FillWithZero (Length: Integer);
var
  i: Integer;
  Ptr: PObject;
  Int64Ptr: PInt64;
  
begin
  Allocate (Length);
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Length do
  begin
    Int64Ptr:= New (PInt64);
    Int64Ptr^:= 0;
    Ptr^:= TObject (Int64Ptr);
    Inc (Ptr);

  end;

end;

procedure TInt64Collection.Free;
var
  i: Integer;
  Ptr: ^TObject;
  
begin
  Ptr:= GetPointerToFirst;
  
  for i:= 1 to Size do
  begin
    Dispose (PInt64 (Ptr^));
    Inc (Ptr);
    
  end;

  inherited Free (False);

end;

function TInt64Collection.GetMember (Index: Integer): Int64;
begin
  Result:= PInt64 (Member [Index])^;
  
end;

function TInt64Collection.Max: Int64;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    Inc (Ptr);
    if Result< PInt64 (Ptr^)^ then
      Result:= PInt64 (Ptr^)^

  end;

end;

function TInt64Collection.Min: Int64;
var
  Ptr: PObject;
  i: Integer;

begin
  Result:= 0;
  if 0< Size then
    Result:= MemberAt [0];
  Ptr:= GetPointerToFirst;

  for i:= 2 to Size do
  begin
    if PInt64 (Ptr^)^< Result then
      Result:= PInt64 (Ptr^)^;
    Inc (Ptr);
    
  end;

end;

end.
