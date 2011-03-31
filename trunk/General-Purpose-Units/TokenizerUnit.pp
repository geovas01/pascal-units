unit TokenizerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DictionaryTreeUnit;
type
  EBoTPassed= class (Exception);
  ERangeCheckError= class (Exception);
  EEofReached= class (Exception);
  
  TFile= file of Byte;
  TTokenKind= (tkStart, tkSpace, tkReturn, tkWord, tkNumber, tkEoF, tkNone);
  TCharKind= (ckStart, ckSpace, ckReturn, ckChar, ckDigit, ckEoF, ckNone);

  TTokenizerChar= record
    Kind: TCharKind;
    Value: TChar;
  end;
  
const
  UndefinedChar: TTokenizerChar= (kind: ckNone; Value: 0);
  
type
  TTokenData= record
//    TokenKind: TTokenKind;
    case TokenKind: TTokenKind of
      tkSpace: (Ch: Char);
      tkNumber: (Digits: TWord);
      tkWord: (Word: TWord);
  end;
  { TToken }

  TToken= class (TObject)
  private
    FData: TTokenData;
    FLastCh: TTokenizerChar;
    
    function GetNextChar (var FileHandle: TFile): TTokenizerChar;
  public
    property TokenKind: TTokenKind read FData.TokenKind;
    property Data: TTokenData read FData;
    property LastChar: TTokenizerChar read FLastCh;

    constructor Create;
    constructor Create (LastCh: TTokenizerChar);
    procedure Free;
    procedure Clear;

    procedure LoadFromFile (var InputFile: TFile);
    procedure SaveToFile (var OutputFile: TFile);
  end;

  { TTokenizer }

  TTokenizer= class (TObject)
  private
    FTokens: array of TToken;
    FCurPos: Integer;
    FFileName: String;
    FFileHandle: TFile;
    FLastCh: TTokenizerChar;

    procedure SetFileName(const AValue: String);
    function GetToken (Index: Integer): TToken;
    function GetNextToken: TToken;
    function IsEof: Boolean;
  public
    property Token [Index: Integer]: TToken read GetToken;
    property NextToken: TToken read GetNextToken;
    property InputFileName: String read FFileName write SetFileName;
    property Eof: Boolean read IsEof;
    property CurPos: Integer read FCurPos;

    constructor Create; overload;
    constructor Create (FileName: String); overload;
    constructor Create (var FileHandle: TFile); overload;
    procedure Free;
    procedure Clear;

    function Rewind (n: Integer= -1): Boolean;
    procedure Tokenize (FileName: String);
    procedure ForgetAllToken (ForgetIndex: Integer= -1);
  end;

implementation
uses
  ExceptionUnit;
{ TToken }

function TToken.GetNextChar (var FileHandle: TFile): TTokenizerChar;
const
  PersianAlphabet: array [0..39] of String= ('آ', 'ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'ك', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی', 'ي',
      'ئ', 'ؤ', 'أ', 'ﺋ', 'ة');
  PersianIndex: array [0..39] of Integer=   (0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 31, 31,
        29, 0, 31, 3);
  IgnoredCharacters: array [0..25] of String= ('ء', '(', ')', '-', '_', '.', 'ً', #13,
     '۰', '۱', '۲', '۳', '۴', '۵', '۶', '۷', '۸', '۹', #194#171, #194#187,
     '،', '!', '?', '؟', ':', '؛');
  SpaceCharSet: array [0..2] of String= (' ', #9, #226#128#140);
  ReturnCharSet: array [0..0] of String= (#10);
  
var
  i: Integer;
  IgnoredChar: Boolean;
  STemp: String;
  b: Byte;
  
begin
  repeat
    repeat
      IgnoredChar:= False;

      if Eof (FileHandle) then
      begin
        Result.Kind:= ckEoF;
        Exit;
      end;

      Read (FileHandle, b);
      STemp:= Char (b);

      for i:= 0 to High (IgnoredCharacters) do
        if STemp= IgnoredCharacters [i] then
        begin
          IgnoredChar:= True;
          Break;
        end;

    until not IgnoredChar;


    for i:= 0 to High (SpaceCharSet) do
      if STemp= SpaceCharSet [i] then
      begin
        Result.Kind:= ckSpace;
        Result.Value:= i;
        Exit;
      end;

    for i:= 0 to High (ReturnCharSet) do
      if STemp= ReturnCharSet [i] then
      begin
        Result.Kind:= ckReturn;
        Result.Value:= i;
        Exit;
      end;

    Read (FileHandle, b);
    STemp:= STemp+ Char (b);

    IgnoredChar:= False;
    for i:= 0 to High (IgnoredCharacters) do
      if STemp= IgnoredCharacters [i] then
      begin
        IgnoredChar:= True;
        Break;
      end;
    if IgnoredChar then
      Continue;

    for i:= 0 to High (PersianAlphabet) do
      if STemp= PersianAlphabet [i] then
      begin
        Result.Kind:= ckChar;
        Result.Value:= PersianIndex [i];
        Exit;
      end;

    Read (FileHandle, b);
    STemp:= STemp+ Char (b);
//    WriteLn (Ord (b));
    if STemp= SpaceCharSet [2] then
    begin
      Result.Kind:= ckSpace;
      Result.Value:= 2;
      Exit;
    end;

    for b:= 1 to Length (STemp) do
      Write ('#', Ord (STemp [b]));
//    WriteLn ('Raising Exception!');
    WriteLn;
    WriteLn (' *', STemp);
    raise EMustNotReachHere.Create ('GetNextToken');
    
  until False;
end;

constructor TToken.Create;
begin
  inherited;

  FData.TokenKind:= tkNone;
  FLastCh:= UndefinedChar;
end;

constructor TToken.Create (LastCh: TTokenizerChar);
begin
  FLastCh:= LastCh;
end;

procedure TToken.Free;
begin
  Clear;

  inherited;
end;

procedure TToken.Clear;
begin
  case FData.TokenKind of
    tkNumber:
      FData.Digits.Free;
    tkWord:
       FData.Word.Free;
  end;

  FData.TokenKind:= tkNone;
end;

procedure TToken.LoadFromFile (var InputFile: TFile);
var
  NextCH: TTokenizerChar;
begin
  if FLastCh.Kind<> ckNone then
    NextCH:= FLastCh
  else
  begin
    if Eof (InputFile) then
    begin
      FData.TokenKind:= tkEoF;
      Exit;
    end;

    NextCh:= Self.GetNextChar (InputFile);
  end;
  
  FLastCh.Kind:= ckNone;

  if NextCH.Kind= ckEoF then
  begin
    FData.TokenKind:= tkEoF;
    Exit;
  end
  else if NextCh.Kind= ckSpace then
  begin
    FData.TokenKind:= tkSpace;
    FData.Ch:= ' ';
  end
  else if NextCh.Kind= ckReturn then
  begin
    FData.TokenKind:= tkReturn;
    FData.Ch:= #10;
  end
  else if NextCh.Kind= ckChar then
  begin
    FData.TokenKind:= tkWord;

    FData.Word:= TWord.Create;
    while NextCH.Kind= ckChar do
    begin
      FData.Word.AddInFront (NextCh.Value);
//      try
        NextCH:= Self.GetNextChar (InputFile);
//      except
//        WriteLn ('Except');
 //       Break;
  //    end;
    end;
    FLastCh:= NextCH;
  end
  else
    raise EMustNotReachHere.Create ('LoadFromFile');
end;

procedure TToken.SaveToFile (var OutputFile: TFile);
begin
  raise ENotImplementedYet.Create ('');
end;

{ TTokenizer }

function TTokenizer.GetNextToken: TToken;
begin
  if FCurPos+ 1<= High (FTokens) then
  begin
    raise ENotImplementedYet.Create ('Not Implemented Yet!');
    Result:= FTokens [FCurPos+ 1];
    Inc (FCurPos);
  end
  else
  begin
    SetLength (FTokens, FCurPos+ 1);
    
    if FLastCh.Kind<> ckNone then
      FTokens [FCurPos]:= TToken.Create (FLastCh)
    else
      FTokens [FCurPos]:= TToken.Create;
      
    try
      FTokens [FCurPos].LoadFromFile (FFileHandle);
      Result:= FTokens [FCurPos];
    except
      on E: EEofReached do
      begin
        FLastCh.Kind:= ckNone;
        
        FTokens [FCurPos].Free;
        SetLength (FTokens, FCurPos);
        Dec (FCurPos);
        Exit;
      end;
    end;
    
    Inc (FCurPos);
    FLastCh:= Result.FLastCh;
  end;
end;

function TTokenizer.IsEof: Boolean;
begin
  Result:= False;
  if not System.Eof (FFileHandle) then
    Exit;

  if FLastCh.Kind= ckEoF then
  begin
    Result:= True;
    Exit;
  end;
  if FLastCh.Kind<> ckNone then
    Exit;
  if System.Eof (FFileHandle) then
  begin
    Result:= True;
    Exit;
  end;
  
end;

function TTokenizer.GetToken (Index: Integer): TToken;
var
  i: Integer;
begin
  if (Index< 0) or (High (FTokens)< Index) then
    raise ERangeCheckError.Create ('GetToken');
    
  Result:= FTokens [Index];
end;

procedure TTokenizer.SetFileName (const AValue: String);
begin
  if FFileName<> '' then
    Clear;

   FFileName:= AValue;
   AssignFile (FFileHandle, AValue);
   Reset (FFileHandle);

end;

constructor TTokenizer.Create;
begin
  FFileName:= '';
  FCurPos:= 0;
  FLastCh:= UndefinedChar;
end;

constructor TTokenizer.Create (FileName: String);
begin
  FFileName:= '';
  FCurPos:= 0;
  InputFileName:= FileName;
  FLastCh:= UndefinedChar;
end;

constructor TTokenizer.Create (var FileHandle: TFile);
begin
  FFileHandle:= FileHandle;
  FLastCh:= UndefinedChar;
end;

procedure TTokenizer.Free;
begin
  Clear;

  CloseFile (FFileHandle);
  inherited;
end;

procedure TTokenizer.Clear;
var
  i: Integer;
begin
  for i:= 0 to High (FTokens) do
    FTokens [i].Free;

  SetLength (FTokens, 0);
  FFileName:= '';
  FCurPos:= 0;
end;

function TTokenizer.Rewind (n: Integer): Boolean;
begin
  Dec (FCurPos, n);

  if FCurPos< 0 then
    raise EBoTPassed.Create ('Invalid Argument For Rewind');
end;

procedure TTokenizer.ForgetAllToken (ForgetIndex: Integer);
var
  i: Integer;
begin
  if ForgetIndex= -1 then
  begin
    ForgetIndex:= High (FTokens);

    if ForgetIndex= -1 then
      Exit;
  end;
  
  if (ForgetIndex< 0) or (High (FTokens)< ForgetIndex) then
    raise ERangeCheckError.Create ('ForgetAllToken');

  for i:= 0 to ForgetIndex do
    FTokens [i].Free;
    
  for i:= ForgetIndex+ 1 to High (FTokens) do
    FTokens [i- ForgetIndex]:= FTokens [i];
  SetLength (FTokens, High (FTokens)- ForgetIndex);
  Dec (FCurPos, ForgetIndex);
  Dec (FCurPos);
  
  if FCurPos< 0 then
    FCurPos:= 0;
end;

procedure TTokenizer.Tokenize (FileName: String);
begin
  raise ENotImplementedYet.Create ('Not Implemented Yet!');
end;

end.

