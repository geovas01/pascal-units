unit MatrixUnit;
{$mode objfpc}{$H+}

interface
uses
  ExceptionUnit, SysUtils;
  
type
  TMatEntry= Extended;
  
  PMatEntry= ^TMatEntry;
  TArrayOfTMatEntry= array of TMatEntry;
  TRow= TArrayOfTMatEntry;
  TColumn= TArrayOfTMatEntry;
  EInvalidSize= class (Exception);
  EDeterminantIsZero= class (Exception);
  
  TMatrix= class;

  { TVector }

  TVector= class (TObject)
  private
    FCells: array of TMatEntry;
    FSize: Integer;
    function GetCells (Index: Integer): TMatEntry;
    procedure SetCells (Index: Integer; AValue: TMatEntry);
  public
    property Size: Integer read FSize;
    property Cells [Index: Integer]: TMatEntry read GetCells;
    
    constructor Create (VectorSize: Integer);
    procedure Free;
    
    function Copy: TVector;
    function Distance (AnotherVector: TVector): Extended;
    
    function Multiply (Mat: TMatrix): TVector;
    function ToString: String;

  end;
  
  { TMatrix }

  TMatrix= class (TObject)
  private
    FCells: array of array of TMatEntry;
    FColSize: Integer;
    FDeterminant: Extended;
    FRowSize: Integer;
    DeterminantCalced: Boolean;
    RandomID: Integer;
    
    function GetCells (RowIndex, ColumnIndex: Integer): TMatEntry;
    function GetColumns (ColumnIndex: Integer): TColumn;
    function GetDeterminant: Extended;
    function GetRows (RowIndex: Integer): TVector;
    procedure SetCells (RowIndex, ColumnIndex: Integer; const AValue: TMatEntry);

  public
    property Rows [RowIndex: Integer]: TVector read GetRows ;
    property Columns [ColumnIndex: Integer]: TColumn read GetColumns;
    property Cells [RowIndex, ColumnIndex: Integer]: TMatEntry read GetCells write SetCells;
    property Determinant: Extended read GetDeterminant;


    property RowSize: Integer read FRowSize;
    property ColSize: Integer read FColSize;
    
    constructor Create (Row, Column: Integer);
    procedure Free;
    function Copy: TMatrix;

    function ToString: String;
    
    function FixedPoint: TVector;
    function TaraNahade: TMatrix;
    procedure DeleteRowCol (RowIndex, ColIndex: Integer);
    procedure DeleteRow (RowIndex: Integer);
    procedure DeleteColumn (ColIndex: Integer);
    function Inverse: TMatrix;
    function Multiply (AnotherMat: TMatrix): TMatrix;
    procedure AddToRow (RowIndex: Integer; DeltaRow: TVector; Coef: Extended);
    procedure ChangeRows (Row1Index, Row2Index: Integer);
    
  end;
implementation

{ TMatrix }

function TMatrix.GetColumns (ColumnIndex: Integer): TColumn;
var
  r: Integer;

begin
  SetLength (Result, FRowSize);
  if (ColumnIndex< 0) or (FRowSize<= ColumnIndex)then
    raise ERangeCheckError.Create ('Get Columns');


  for r:= 0 to FRowSize- 1 do
    Result [r]:= FCells [r, ColumnIndex];

end;

function TMatrix.GetDeterminant: Extended;

  function CalulateDeterminant (Mat: TMatrix): Extended;
  var
    i: Integer;
    r, c: Integer;
    ActiveCell,
    Temp: Extended;
    TempVector: TVector;
    Flag: Boolean;

  begin
    Result:= 1;
    
    for c:= 0 to Mat.ColSize- 1 do
    begin
      Flag:= True;
      
      for r:= c to Mat.RowSize- 1 do
        if 1e-10< Abs (Mat.FCells [r, c]) then
        begin
          Flag:= False;
          ActiveCell:= Mat.FCells [r, c];

          if r<> c then
          begin
            Mat.ChangeRows (c, r);
            Result:= -Result;
              
          end;
          
          Break;
          
        end;

        if Flag then
        begin
          Result:= 0;
          Exit;
          
        end;
      
      TempVector:= Mat.Rows [c];
      for r:= c+ 1 to Mat.RowSize- 1 do
        if 1e-10< Abs (Mat.FCells [r, c]) then
          Mat.AddToRow (r, TempVector, -Mat.FCells [r, c]/ ActiveCell);

      TempVector.Free;
      
    end;

    for r:= 0 to RowSize- 1 do
      Result:= Result* Mat.FCells [r, r];

  end;
  
var
  Mat: TMatrix;
  
begin

  if ColSize<> RowSize then
    raise EInvalidSize.Create ('Determinant');
    
  if DeterminantCalced then
    Result:= Self.FDeterminant
  else
  begin
    Mat:= Self.Copy;
    Result:= CalulateDeterminant (Mat);
    Mat.Free;
    
    FDeterminant:= Result;
    DeterminantCalced:= True;
    
  end;

end;

function TMatrix.GetCells (RowIndex, ColumnIndex: Integer): TMatEntry;
begin
  if (RowIndex< 0) or (FRowSize<= RowIndex)then
    raise ERangeCheckError.Create ('Get Cells');
    
  if (ColumnIndex< 0) or (FColSize<= ColumnIndex)then
    raise ERangeCheckError.Create ('Get Cells');
    
  Result:= FCells [RowIndex, ColumnIndex];
  
end;

function TMatrix.GetRows (RowIndex: Integer): TVector;
var
  c: Integer;
  
begin
  if (RowIndex< 0) or (FRowSize<= RowIndex)then
    raise ERangeCheckError.Create ('Get Rows');
    
  Result:= TVector.Create (FColSize);

  for c:= 0 to FColSize- 1 do
    Result.FCells [c]:= FCells [RowIndex, c];

end;

procedure TMatrix.SetCells (RowIndex, ColumnIndex: Integer;
  const AValue: TMatEntry);
begin
  if (RowIndex< 0) or (RowSize<= RowIndex)then
    raise ERangeCheckError.Create ('Get Cells');

  if (ColumnIndex< 0) or (ColSize<= ColumnIndex)then
    raise ERangeCheckError.Create ('Get Cells');

  FCells [RowIndex, ColumnIndex]:= AValue;
  
end;

constructor TMatrix.Create (Row, Column: Integer);
var
  r, c: Integer;
//  RowPtr: PTMatEntry;
  
begin
  inherited Create;
  
  FRowSize:= Row;
  FColSize:= Column;

  SetLength (FCells, FRowSize);
  
  for r:= 0 to FRowSize- 1 do
  begin
    SetLength (FCells [r], FColSize);
//    RowPtr:= @FCells [r];
    
    for c:= 0 to FColSize- 1 do
    begin
//      RowPtr^:= 0;
      FCells [r, c]:= 0;
//      Inc (RowPtr);

    end;
    
  end;
  DeterminantCalced:= False;
  RandomID:= Round (100000* Random);
  
end;

procedure TMatrix.Free;
var
  r: Integer;
  
begin
  for r:= 0 to FRowSize- 1 do
    SetLength (FCells [r], 0);

  SetLength (FCells, 0);

  inherited;
  
end;

function TMatrix.Copy: TMatrix;
var
  r, c: Integer;
  
begin
  Result:= TMatrix.Create (RowSize, ColSize);

  for r:= 0 to RowSize- 1 do
    for c:= 0 to ColSize- 1 do
      Result.FCells [r, c]:= FCells [r, c];

end;

function TMatrix.ToString: String;
var
  r, c: Integer;
  Temp: Extended;
  
begin
  WriteLn;
  Result:= '';
  for r:= 0 to RowSize- 1 do
  begin
    for c:= 0 to ColSize- 1 do
    begin
      Temp:= FCells [r, c];
      Result:= Result+ FloatToStr (Temp)+ ' ';
      Write (Temp:0:3, ' ');
    end;

    Result:= Result+ #10;
    WriteLn;
  end;
  
end;

function TMatrix.FixedPoint: TVector;
var
  i, j: integer;
  LastResult,
  Temp: TVector;
  Sum: TMatEntry;

begin
  if ColSize<> RowSize then
   raise EInvalidSize.Create ('FixPoint');

  Result:= TVector.Create (RowSize);
  for i:= 0 to RowSize- 1 do
    Result.SetCells (i, 1.0/ RowSize);
  
  LastResult:= TVector.Create (RowSize);
  
  repeat
    LastResult.Free;
    LastResult:= Result.Copy;
    
    Result.Multiply (Self);
    Sum:= 0;
    for i:= 0 to Result.Size- 2 do
      Sum:= Sum+ Result.FCells [i];
    Result.SetCells (Result.FSize- 1, 1- Sum);

    Result.ToString;
    WriteLn (Result.Distance (LastResult):0:4);
  until Result.Distance (LastResult)< 1e-10;
  
end;

function TMatrix.TaraNahade: TMatrix;
var
  r, c: Integer;
  
begin
  Result:= TMatrix.Create (FColSize, FRowSize);
  
  for r:= 0 to FRowSize- 1 do
    for c:= 0 to FColSize- 1 do
      Result.FCells [c, r]:= FCells [r, c];

end;

procedure TMatrix.DeleteRowCol (RowIndex, ColIndex: Integer);
var
  r, c: Integer;
  
begin
  
  for r:= RowIndex+ 1 to RowSize- 1 do
    for c:= 0 to ColSize- 1 do
      FCells [r- 1, c]:= FCells [r, c];

  for r:= 0 to RowSize- 1 do
    for c:= ColIndex+ 1 to ColSize- 1 do
      FCells [r, c- 1]:= FCells [r, c];

  for r:= 0 to FRowSize- 1 do
    SetLength (FCells [r], FColSize- 1);
    
  SetLength (FCells [FRowSize- 1], 0);
  SetLength (FCells, FRowSize- 1);

  Dec (FRowSize);
  Dec (FColSize);

end;

procedure TMatrix.DeleteRow (RowIndex: Integer);
var
  r, c: Integer;

begin

  for r:= RowIndex+ 1 to RowSize- 1 do
    for c:= 0 to ColSize- 1 do
      FCells [r- 1, c]:= FCells [r, c];

  SetLength (FCells [FRowSize- 1], 0);
  SetLength (FCells, FRowSize- 1);

  Dec (FRowSize);
  
end;

procedure TMatrix.DeleteColumn (ColIndex: Integer);
var
  r, c: Integer;

begin

  for r:= 0 to RowSize- 1 do
  begin
    for c:= ColIndex+ 1 to ColSize- 1 do
      FCells [r, c- 1]:= FCells [r, c];

    SetLength (FCells [r], ColSize- 1);
    
  end;

  Dec (FColSize);

end;

function TMatrix.Inverse: TMatrix;
var
  r, c, i: Integer;
  TempMat: TMatrix;
  Determ: Extended;
  Sign: Boolean;
  
begin
  Determ:= Self.Determinant;

  if Abs (Determ)< 1e-10 then
  begin
    Self.ToString;
    raise EDeterminantIsZero.Create ('TMartix.Inverse');
  end;

  Result:= TMatrix.Create (RowSize, ColSize);
  
  for r:= 0 to RowSize- 1 do
  begin
    Sign:= not Odd (r);
    
    for c:= 0 to ColSize- 1 do
    begin
      TempMat:= Self.Copy;
      TempMat.DeleteRowCol (r, c);
      
      if Sign then
        Result.FCells [c, r]:= TempMat.Determinant/ Determ
      else
        Result.FCells [c, r]:= -TempMat.Determinant/ Determ;
      
      TempMat.Free;
      Sign:= not Sign;
      
    end;
    
  end;

end;

function TMatrix.Multiply (AnotherMat: TMatrix): TMatrix;
var
  r, c, i: Integer;
  Sum: TMatEntry;
  
begin
  if FColSize<> AnotherMat.RowSize then
    raise EInvalidSize.Create ('Multiply '+ IntToStr (FColSize)+
           ':'+ IntToStr (AnotherMat.RowSize));
    
  Result:= TMatrix.Create (RowSize, AnotherMat.ColSize);
  
  for r:= 0 to RowSize- 1 do
    for c:= 0 to AnotherMat.ColSize- 1 do
    begin
      Sum:= 0;
      
      for i:= 0 to ColSize- 1 do
        Sum:= Sum+ FCells [r, i]* AnotherMat.FCells [i, c];

      Result.FCells [r, c]:= Sum;
      
    end;
    
end;

procedure TMatrix.AddToRow (RowIndex: Integer; DeltaRow: TVector; Coef: Extended);
var
  i: Integer;
  
begin
  Cells [RowIndex, DeltaRow.Size- 1];// For Range Checking

  for i:= 0 to FColSize- 1 do
    FCells [RowIndex, i]:= FCells [RowIndex, i]+
              Coef* DeltaRow.Cells [i];

end;

procedure TMatrix.ChangeRows (Row1Index, Row2Index: Integer);
var
  i: Integer;
  Temp: Extended;
  
begin
  for i:= 0 to FColSize- 1 do
  begin
    Temp:= FCells [Row1Index, i];
    FCells [Row1Index, i]:= FCells [Row2Index, i];
    FCells [Row2Index, i]:= Temp;
    
  end;
  
end;

{ TVector }

function TVector.GetCells (Index: Integer): TMatEntry;
begin
  Result:= FCells [Index];
  
end;

procedure TVector.SetCells (Index: Integer; AValue: TMatEntry);
begin
  if (Index< 0) or (FSize<= Index) then
    raise ERangeCheckError.Create ('TVector.SetCells ');

  FCells [Index]:= AValue;
  
end;

constructor TVector.Create (VectorSize: Integer);
var
  i: Integer;
begin
  inherited Create;
  
  FSize:= VectorSize;
  SetLength (FCells, VectorSize);
  
  for i:= 0 to FSize- 1 do
    FCells [i]:= 0;

end;

procedure TVector.Free;
begin
  SetLength (FCells, 0);
  
  inherited;
  
end;

function TVector.Copy: TVector;
var
  i: Integer;
  
begin
  Result:= TVector.Create (Size);
  
  for i:= 0 to FSize- 1 do
    Result.FCells [i]:= FCells [i];
    
end;

function TVector.Distance (AnotherVector: TVector): Extended;
var
  i: Integer;
  
begin
  Result:= 0;
  
  for i:= 0 to FSize- 1 do
    Result:= Result+ Sqrt (Sqr (AnotherVector.Cells [i]- Cells [i]));
    
end;

function TVector.Multiply (Mat: TMatrix): TVector;
var
  r, c: Integer;
  Sum: TMatEntry;
  NewVector: TVector;
  
begin
  if Size<> Mat.RowSize then
    raise EInvalidSize.Create ('TVector.Multiply');

  NewVector:= TVector.Create (Size);

  for r:= 0 to FSize- 1 do
  begin
    Sum:= 0;
    
    for c:= 0 to FSize- 1 do
      Sum:= Sum+ FCells [c]* Mat.FCells [r, c];
    NewVector.FCells [r]:= Sum;
    
  end;
  
  for r:= 0 to FSize- 1 do
    FCells [r]:= NewVector.FCells [r];
  NewVector.Free;
    
end;

function TVector.ToString: String;
var
  i: Integer;
  
begin
  Result:= '';
  
  for i:= 0 to FSize- 1 do
  begin
    Write (FCells [i], ' ');
    Result:= Result+ FloatToStr (FCells [i])+ ' ';
    
  end;
  
  WriteLn;
    
end;

end.
