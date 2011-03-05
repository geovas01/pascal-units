unit HashUnit;

interface
uses
  SysUtils, GeometryUnit;
const
  HashFunctionMod: integer= 10000;
  
type
  THashFunctionType= (hftMod);

  THash= class
    private
      FHashFunctionType: THashFunctionType;

      procedure SetHashFunctionType(const Value: THashFunctionType);

      function HashData (const Argument: TPoint): Int64;overload;
      function HashData (const x, y: Integer): Int64;overload;
    public
      Data: array of array of TPoint;
      
      property HashFunctionType: THashFunctionType read FHashFunctionType write SetHashFunctionType;

      function IsExist (const Argument: TPoint): Boolean; overload;
      function IsExist (const r, c: Integer): Boolean; overload;

      function Insert (const Argument: TPoint): Boolean; overload;
      function Insert (const r, c: Integer): Boolean; overload;
      procedure SaveToFile (FileName: String= 'Hashlog.txt');

      constructor Create;overload;
      constructor Create (HashFuctionType: THashFunctionType);overload;
      procedure Free;
      procedure Clear;
  end;

implementation

{ THash }

procedure THash.Clear;
var
  i, j: Integer;
begin        
  for i:= 0 to High(Data) do
  begin
    for j:= 0 to High(Data [i]) do
      Data [i][j].Free;

    SetLength (Data [i], 0);

  end;
  SetLength (Data, 0);
  
end;

constructor THash.Create;
begin
  inherited;
  
  FHashFunctionType:= hftMod;
  SetLength (Data, HashFunctionMod);
  
end;

constructor THash.Create (HashFuctionType: THashFunctionType);
begin
  inherited Create;
  
  FHashFunctionType:= HashFuctionType;
  SetLength (Data, HashFunctionMod);
end;

procedure THash.Free;
begin
  Clear;

  inherited;
  
end;

function THash.HashData (const Argument: TPoint): Int64;
begin
  Result:= 0;

  case HashFunctionType of
    hftMod:
      Result:= (Argument.r+ Argument.c) mod HashFunctionMod;
      
  end;
end;

function THash.Insert (const Argument: TPoint): Boolean;
var
  Pos: Integer;
begin
  Result:= IsExist (Argument);
  if Result then
    Exit;

  Pos:= HashData (Argument);
  SetLength (Data [Pos], Length (Data [Pos])+ 1);
  Data [Pos][High (Data [Pos])]:= Argument;
  
end;

function THash.IsExist (const Argument: TPoint): Boolean;
var
  Pos, i: Integer;
begin
   Pos:= HashData (Argument);
   Result:= False;

   for i:= 0 to High (Data [Pos]) do
   begin

     if Data [Pos][i].IsSame (Argument) then
     begin
       Result:= True;
       Exit;

     end;
     
   end;

end;

function THash.Insert (const r, c: Integer): Boolean;
var
  Pos: Integer;
  
begin
  Result:= IsExist (r, c);
  if Result then
    Exit;

  Pos:= HashData (r, c);
  SetLength (Data [Pos], Length (Data [Pos])+ 1);
  Data [Pos][High (Data [Pos])]:= TPoint.Create (r, c);
  
end;

function THash.IsExist (const r, c: Integer): Boolean;
var
  Pos, i: Integer;
  Ptr: ^TPoint;
   
begin
   Pos:= HashData (r, c);
   Ptr:= @Data [Pos, 0];
   Result:= False;

   for i:= 0 to High (Data [Pos]) do
   begin

     if Ptr^.IsSame (r, c) then
     begin
       Result:= True;
       Exit;

     end;

     Inc (Ptr);

   end;

end;

procedure THash.SetHashFunctionType (const Value: THashFunctionType);
begin
  Clear;

  FHashFunctionType:= Value;
end;

function THash.HashData (const x, y: Integer): Int64;
begin
  Result:= 0;

  case HashFunctionType of
    hftMod:
      Result:= (x+ y) mod HashFunctionMod;
  end;

end;

procedure THash.SaveToFile (FileName: String);
var
  i, j: Integer;
  Output: TextFile;
begin
  AssignFile (Output, FileName);
  ReWrite (Output);
  for i:= 0 to High (Data) do
  begin
    for j:= 0 to High (Data [i]) do
      WriteLn (Output, i, ':', Data [i][j].ToString);
      
  end;

  CloseFile (Output);
end;


end.
