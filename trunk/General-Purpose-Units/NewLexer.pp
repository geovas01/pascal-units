unit NewLexerUnit;

interface
uses
  SysUtils, Classes;
  
type
  TCharKind= (tckStart, tckNone, tckNumber, tckLetter, tckDot, tckColon, tckSemiColon,
    tckUnderLine, tckDolarSign, tckOpenPar, tckClosePar, tckOpenBracket, tckCloseBracket,
    tckTilda, tckPercent, tckDoubleQuot, tckSingleQuot, tckMultiplySign, tckMinusSign,
    tckDivideSign, tckSumSign, tckEqualSign, tckOpenBrace, tckCloseBrace,
    tckNumberSign, tckAtSign, tckQuestionMark, {tckSlash, }tckBackSlash,
    tckPipe, tckHashtak, tckTab, tckSpace, tckReturn,
    tckCarriage, tckAmpsign, tckComma, tckLessThan, tckBiggerThan, tckExclamationMark,
    tckGraveAccent, tckFormFeed,
      tckEOF, tckEnd);
  TLexerType= (ltStart, ltJava, ltDocument,
              ltIntVector, ltMSEQuery, ltPascal, ltEnd);
  
  TcharSet= set of char;
  
  TChar= record
    Kind: TCharKind;
    Ch: Char;
    
  end;
  
  { EInvalidCharacterEncountered }

  EInvalidCharacterEncountered= class (Exception)
  public
    constructor Create (Ch: char);
    procedure Free;
  end;

  EInvalidLexerConfiguration= class (Exception);
  
  { EStringIsNotKeyword }

  EStringIsNotKeyword= class (Exception)
  public
    constructor Create (RealData: String);
    procedure Free;
    
  end;
  
  { EInvalidToken }

  EInvalidToken= class (Exception)
  public
    constructor Create (TokenStr: String);
    procedure Free;
    
  end;

const
  CharKindSet: array [tckStart..tckEnd] of TCharSet=
    ([], [], ['0'..'9'], ['A'..'Z','a'..'z'], ['.'], [':'], [';'],
     ['_'], ['$'], ['('], [')'], ['['], [']'],
     ['~'], ['%'], ['"'], [''''], ['*'], ['-'],
     ['/'], ['+'], ['='], ['{'], ['}'],
     ['#'], ['@'], ['?'],// ['/'],
     ['\'],
     ['|'],  ['^'], [#9], [' '], [#10],
     [#13], ['&'], [','], ['<'], ['>'], ['!'], ['`'], [#12], [], []);
     
type
  TTokenKind= (tkStart{0}, tkNumber, tkIdentifier, tkDot, tkColon, tkSemiColon, tkString,
    tkChar,
    tkDolarSign, tkOpenPar, tkClosePar{10}, tkOpenBracket, tkCloseBracket,
    tkTilda, tkPercent, tkDoubleQuot, tkSingleQuot, tkMultiplySign, tkMinusSign,
    tkDivideSign, tkSumSign{20}, tkEqualSign, tkEqualEqualSign, tkOpenBrace, tkCloseBrace,
    {?}tkNumberSign, {?}tkAtSign, {?}tkQuestionMark, {?}tkSlash, {?}tkBackSlash,
    tkBiggerThan, tkLessThan, tkComment, tkComma, tkHashtak, tkBitwiseAnd, tkLogicalAnd,
    tkBitwiseOr, tkLogicalOr, tkNotEqualTo,
    tkNot, tkAutoInc, tkAutoDec, tkShiftRight, tkShiftLeft, tkBiggerThanOrEQualTo,
    tkLessThanOrEQualTo, tkPlusAssign, tkMinusAssign, tkMulAssign, tkDivAssign,
    tkGraveAccent, tkFormFeed, tkEof, tkNone, tkEnd);
    
  TKeywordType= (tktStart, tktAbstract, tktAssert,
  tktBreak, tktCase, tktCatch, tktClass, tktConst, tktContinue, tktDefault,
  tktDo, tktElse, tktEnum, tktExtends, tktFinal, tktFinally, tktFor, tktGoto,
  tktIf, tktImplements, tktImport, tktInstanceOf, tktInterface, tktNative, tktNew, tktNull,
  tktPackage, tktPrivate, tktProtected, tktPublic, tktReturn, tktStatic, tktSuper,
  tktSwitch, tktSynchronized, tktThis, tktThrow, tktThrows, tktTransient, tktTry,
  tktVolatile, tktWhile, tktStrictFP,
  tktProcedure, tktFunction, tktNone, tktEnd);
  
  TModifierType= (tmtStart, tmtPrivate, tmtPublic, tmtProtected, tmtStatic, tmtFinal, tmtNative, tmtSynchronized,
  tmtAbstract, tmtThreadSafe, tmtTransient, tmtNone, tmtEnd);
  
const
  KeywordTypeString: array [tktStart..tktEnd] of String=
    (   '', 'abstract', 'assert',
        'break', 'case','catch', 'class','const','continue','default',
        'do', 'else','enum','extends','final', 'finally', 'for', 'goto',
        'if', 'implements', 'import', 'instanceof','interface','native','new','null',
        'package', 'private','protected','public','return','static','super',
        'switch', 'synchronized', 'this', 'throw','throws','transient','try',
        'volatile', 'while', 'strictfp', 'procedure', 'function',
        '', '');
        
  ModifierTypeString: array [tmtStart..tmtEnd] of String=
    ( '', 'private', 'public', 'protected', 'static', 'final', 'native', 'synchronized',
    'abstract', 'threadsafe', 'transient', '', '');

type

  { EExpectFailed }

  EExpectFailed= class (Exception)
  public
    constructor Create (Keyword: TKeywordType; TokenStr: String); overload;
    constructor Create (TokenKind: TTokenKind; TokenStr: String); overload;
    constructor Create (TokenString: String; TokenStr: String); overload;
    procedure Free;
  end;

  { ERangeCheckError }

  ERangeCheckError= class (Exception)
  public
    constructor Create (FunctionName: String);
    procedure Free;
  end;
  
  TCharFile= file of Char;
  TReadFromFilePtr= function (var FileHandle: TCharFile): TChar of Object;
  { TToken }

  TToken= class (TObject)
  private
    FChars: array of TChar;
    FKeywordType: TKeywordType;
    FTokenKind: TTokenKind;
    FLastChar: TChar;
    LexerType: TLexerType;
    function GetCharCount: Integer;
    function GetChars (Index: Integer): TChar;
    procedure AddChar (Ch: TChar);
    function GetKeywordType: TKeywordType;
    function GetModifierType: TModifierType;
    
    function DocumentReadFromFile (var FileHandle: TCharFile): TChar;
    function DocumentRead (Stream: TStream): TChar;
    function JavaReadFromFile (var FileHandle: TCharFile): TChar;
    function JavaRead (Stream: TStream): TChar;

  public
    property Chars [Index: Integer]: TChar read GetChars;
    property CharCount: Integer read GetCharCount;
    property TokenKind: TTokenKind read FTokenKind;
    property KeywordType: TKeywordType read GetKeywordType;
    property ModifierType: TModifierType read GetModifierType;

    constructor Create (TokenString: String);
    function Copy: TToken;
    procedure Free;
    
    function ToString: String;
    
  end;
  
  { TLexerSavePoint }

  TLexerSavePoint= class
  private
    FCurrentToken: TToken;
    FLastChar: Tchar;
    FFilePointerLocation: Integer;
    
  public
    property FilePointerLocation: Integer read FFilePointerLocation ;
    property CurrentToken: TToken read FCurrentToken;
    property LastChar: TChar read FLastChar;

    constructor Create (PointerLocation: Integer; CurToken: TToken; LastCh: TChar);
    procedure Free;
    
  end;
  
  { TLexer }

  TLexer= class
  private
    FStream: TStream;
    Line, Col: Integer;
    FFileName: String;
    FFileHandle: TCharFile;
    FLastChar: TChar;
    FCurrentToken: TToken;
    FLastTokens: array of TToken;
    FLexerType: TLexerType;
    FMessageString: String;

    function GetCurrentToken: TToken;
    function GetLastToken (Index: Integer): TToken;
    procedure SetFileName (const Value: String);
    procedure AddToken (Token: TToken);

  public
    property CurrentToken: TToken read FCurrentToken;
    property LastTokens [Index: Integer]: TToken read GetLastToken;
    property  FileName: String read FFileName write SetFileName;
    property LexerType: TLexerType read FLexerType;
    property MessageString: String read FMessageString write FMessageString;
    property Stream: TStream read FStream write FStream;

    constructor Create (LexType: TLexerType= ltJava);
    procedure Free;

    function  GetNextToken: TToken;
    procedure GetLineCol (var l, c: Integer);
    procedure ClearHistory;

    procedure Expect (TokenString: String); overload;
    procedure Expect (Keyword: TKeywordType); overload;
    procedure Expect (TokenKind: TTokenKind); overload;

    function GetCurrentPosition: TLexerSavePoint;
    procedure RewindTo (Position: TLexerSavePoint);
    procedure GoForward;
    
  end;

implementation
uses
  ExceptionUnit;
  
{ TLexer }

constructor TLexer.Create (LexType: TLexerType);
begin
  inherited Create;
  
  Line:= 0;
  Col := 0;
  FLastChar.Kind:= tckNone;
  FCurrentToken:= nil;
  FLexerType:= LexType;
  FFileName:= '';
  
end;

procedure TLexer.Free;
begin
  if FFileName<> '' then
    CloseFile (FFileHandle);
    
  if FCurrentToken<> nil then
    FCurrentToken.Free;
  ClearHistory;
  
  inherited;
end;

procedure TLexer.getLineCol (var l, C: Integer);
begin
  l:= Line;
  c:= Col; 
  
end;

procedure TLexer.ClearHistory;
var
  i: Integer;
  
begin
  for i:= 0 to High (FLastTokens) do
    FLastTokens [i].Free;
    
  SetLength (FLastTokens, 0);
  
end;

procedure TLexer.Expect (TokenString: String);
begin
  if CurrentToken.ToString= TokenString then
    Self.GetNextToken
  else
    raise EExpectFailed.Create (TokenString, CurrentToken.ToString);
    
end;

procedure TLexer.Expect (Keyword: TKeywordType);
begin
  if CurrentToken.KeywordType= Keyword then
    Self.GetNextToken
  else
    raise EExpectFailed.Create (Keyword, CurrentToken.ToString);
    
end;

procedure TLexer.Expect (TokenKind: TTokenKind);
begin
  if CurrentToken.TokenKind= TokenKind then
    Self.GetNextToken
  else
    raise EExpectFailed.Create (TokenKind, IntToStr (Integer (CurrentToken.TokenKind)));
    
end;

function TLexer.GetCurrentPosition: TLexerSavePoint;
begin
  Result:= TLexerSavePoint.Create (FilePos (FFileHandle), CurrentToken, FLastChar);
  
end;

procedure TLexer.RewindTo (Position: TLexerSavePoint);
var
  Data: array [0..100000] of char;
  Res: Integer;
  
begin
  Self.ClearHistory;
  
  if FCurrentToken<> nil then
  begin
    FCurrentToken.Free;
    FCurrentToken:= nil;
    
  end;
  FCurrentToken:= Position.CurrentToken.Copy;
  
  Reset (FFileHandle);
  BlockRead (FFileHandle, Data, Position.FilePointerLocation, Res);
  FLastChar:= Position.LastChar;

end;

procedure TLexer.GoForward;
begin
  GetNextToken;
  
end;

function TLexer.GetNextToken: TToken;
  function ReadNextToken: TToken;
  var
    TokenString: String;
    
  begin
    TokenString:= '';
    

    Result:= TToken.Create (TokenString);
    
  end;
  
begin
  if FCurrentToken<> nil then
    AddToken (FCurrentToken);
    
  Result:= ReadNextToken;
  while Result.TokenKind= tkComment do
  begin
    Result.Free;
    Result:= ReadNextToken;
    
  end;
  FCurrentToken:= Result;

end;


procedure TLexer.SetFileName (const Value: String);
begin
  if FFileName<> '' then
    CloseFile (FFileHandle);

  FFileName:= Value;
  AssignFile (FFileHandle, Value);
  Reset (FFileHandle);

end;

procedure TLexer.AddToken (Token: TToken);
var
  i: Integer;
  
begin
  SetLength (FLastTokens, Length (FLastTokens)+ 1);
  for i:= High (FLastTokens)- 1 downto 0 do
    FLastTokens [i+ 1]:= FLastTokens [i];
  i:= Length (FLastTokens);
  FLastTokens [0]:= Token;
  
end;

function TLexer.GetLastToken (Index: Integer): TToken;
begin
  if 0<= Index then
    raise ERangeCheckError.Create ('TLexer.GetToken');

  if Length (FLastTokens)< -Index then
    raise ERangeCheckError.Create ('TLexer.GetToken');

  Result:= FLastTokens [1+ Index];
  
end;

function TLexer.GetCurrentToken: TToken;
begin
  Result:= FCurrentToken;
  
end;

{ TToken }

function TToken.GetChars (Index: Integer): TChar;
begin
  if (Index< 0) or (CharCount<= Index) then
    raise ERangeCheckError.Create ('In TToken.GetChars');

  Result:= FChars [Index];
end;

procedure TToken.AddChar (Ch: TChar);
begin
  SetLength (FChars, Length (FChars)+ 1);
  FChars [High (FChars)]:= Ch;
end;

function TToken.GetKeywordType: TKeywordType;
begin
  for Result:= tktStart to tktEnd do
    if Self.ToString= KeywordTypeString [Result] then
      Exit;

  Result:= tktNone;
end;

function TToken.GetModifierType: TModifierType;
begin
  for Result:= tmtStart to tmtEnd do
    if Self.ToString= ModifierTypeString [Result] then
      Exit;

  Result:= tmtNone;
end;

function TToken.GetCharCount: Integer;
begin
  Result:= Length (FChars);
end;

function TToken.Copy: TToken;
var
  i: Integer;
  
begin
  Result:= TToken.Create ('');
  Result.FTokenKind:= FTokenKind;
  
  for i:= 0 to CharCount- 1 do
    Result.AddChar (FChars [i]);
end;

procedure TToken.Free;
begin
  SetLength (FChars, 0);

  inherited;
end;

function TToken.DocumentReadFromFile(var FileHandle: TCharFile): TChar;

  function GetNextChar: TChar;
  var
    CharKind: TCharKind;
    Ch: Char;

  begin

    if FLastChar.Kind<> tckNone then
    begin
      Result:= FLastChar;
      FLastChar.Kind:= tckNone;
      Exit;
    end
    else
    if Eof (FileHandle) then
    begin
      Result.Kind:= tckEOF;
      Result.Ch:= #0;
      Exit;
    end
    else
    begin
      while not Eof (FileHandle) do
      begin
        Read (FileHandle, Ch);
        for CharKind:= tckStart to tckEnd do
          if Ch in CharKindSet [CharKind] then
          begin
            Result.Kind:= CharKind;
            Result.Ch:= Ch;
            Exit;
          end;
//         WriteLn (Ch, 'unf:', Ord (Ch));
       end;
       
       Result.Kind:= tckEOF;
    end;

    raise EInvalidCharacterEncountered.Create (Ch);
  end;

var
  Ch, TempCh: TChar;
  KeyWordIter: TKeywordType;

begin
  Result.Kind:= tckNone;
  if EOF (FileHandle) then
  begin
    Result.Kind:= tckEOF;
    if FLastChar.Kind= tckNone then
    begin
      FTokenKind:= tkEof;
      Exit;
    end;
  end;

  Ch:= GetNextChar;

  while Ch.Kind in [tckSpace, tckTab, tckReturn, tckCarriage] do
    Ch:= GetNextChar;
  if Ch.Kind= tckEOF then
  begin
    FTokenKind:= tkEof;
    Exit;
  end;

  FTokenKind:= tkStart;

  case Ch.Kind of
    tckBackSlash:
      FTokenKind:= tkBackSlash;
    tckAtSign:
      FTokenKind:= tkAtSign;
    tckCloseBrace:
      FTokenKind:= tkCloseBrace;
    tckCloseBracket:
      FTokenKind:= tkCloseBracket;
    tckClosePar:
      FTokenKind:= tkClosePar;
    tckColon:
      FTokenKind:= tkColon;
    tckComma:
      FTokenKind:= tkComma;
    tckDolarSign:
      FTokenKind:= tkDolarSign;
    tckDot:
      FTokenKind:= tkDot;
    tckHashtak:
      FTokenKind:= tkHashtak;
    tckNumberSign:
      FTokenKind:= tkNumberSign;
    tckOpenBrace:
      FTokenKind:= tkOpenBrace;
    tckOpenBracket:
      FTokenKind:= tkOpenBracket;
    tckOpenPar:
      FTokenKind:= tkOpenPar;
    tckPercent:
      FTokenKind:= tkPercent;
    tckQuestionMark:
      FTokenKind:= tkQuestionMark;
    tckSemiColon:
      FTokenKind:= tkSemiColon;
    tckTilda:
      FTokenKind:= tkTilda;
    tckSingleQuot:
      FTokenKind:= tkSingleQuot;
    tckAmpsign:
      FTokenKind:= tkBitwiseAnd;
    tckPipe:
      FTokenKind:= tkNot;
    tckLessThan:
      FTokenKind:= tkLessThan;
    tckBiggerThan:
      FTokenKind:= tkBiggerThan;
    tckDoubleQuot:
      FTokenKind:= tkDoubleQuot;
    tckMultiplySign:
      FTokenKind:= tkMultiplySign;
    tckSumSign:
      FTokenKind:= tkSumSign;
    tckMinusSign:
      FTokenKind:= tkMinusSign;
    tckDivideSign:
      FTokenKind:= tkDivideSign;
    tckEqualSign:
      FTokenKind:= tkEqualSign;
    tckExclamationMark:
      FTokenKind:= tkNot;
    tckGraveAccent:
      FTokenKind:= tkGraveAccent;
    tckFormFeed:
      FTokenKind:= tkFormFeed;
  end;

  if FTokenKind= tkStart then
  begin
    if Ch.Kind in [tckLetter, tckUnderLine] then
    begin
      FTokenKind:= tkIdentifier;
      AddChar (Ch);
      Ch:= GetNextChar;

      while Ch.Kind in [tckLetter, tckNumber, tckUnderLine] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      Result:= Ch;

      FKeywordType:= tktNone;
      for KeyWordIter:= Succ (tktStart) to Pred (tktEnd) do
        if Self.ToString= KeywordTypeString [KeyWordIter] then
        begin
          FKeywordType:= KeyWordIter;
          Break;
        end;

    end else if Ch.Kind in [tckNumber] then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      FTokenKind:= tkNumber;

      while Ch.Kind in [tckNumber] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      if Ch.Kind<> tckDot then
        Result:= Ch
      else
      begin
        AddChar (Ch);
        Ch:= GetNextChar;

        while Ch.Kind in [tckNumber] do
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
        Result:= Ch
      end;
    end
    else
    begin
      raise EInvalidCharacterEncountered.Create (Ch.Ch);
    end
  end
  else
    AddChar (Ch);

end;

function TToken.DocumentRead (Stream: TStream): TChar;

  function GetNextChar: TChar;
  var
    CharKind: TCharKind;
    Ch: Char;

  begin

    if FLastChar.Kind<> tckNone then
    begin
      Result:= FLastChar;
      FLastChar.Kind:= tckNone;
      Exit;
    end
    else
    
    if Stream.Position= Stream.Size then
    begin
      Result.Kind:= tckEOF;
      Result.Ch:= #0;
      Exit;
      
    end
    else
    begin
      while Stream.Position< Stream.Size  do
      begin
        Stream.Read (Ch, 1);
        
        for CharKind:= tckStart to tckEnd do
          if Ch in CharKindSet [CharKind] then
          begin
            Result.Kind:= CharKind;
            Result.Ch:= Ch;
            Exit;
            
          end;
//         WriteLn (Ch, 'unf:', Ord (Ch));

       end;

       Result.Kind:= tckEOF;
       
    end;

    raise EInvalidCharacterEncountered.Create (Ch);
  end;

var
  Ch, TempCh: TChar;
  KeyWordIter: TKeywordType;

begin

  Result.Kind:= tckNone;
  if Stream.Position< Stream.Size then
  begin
    Result.Kind:= tckEOF;
    
    if FLastChar.Kind= tckNone then
    begin
      FTokenKind:= tkEof;
      Exit;
      
    end;
    
  end;

  Ch:= GetNextChar;

  while Ch.Kind in [tckSpace, tckTab, tckReturn, tckCarriage] do
    Ch:= GetNextChar;
  if Ch.Kind= tckEOF then
  begin
    FTokenKind:= tkEof;
    Exit;
  end;

  FTokenKind:= tkStart;

  case Ch.Kind of
    tckBackSlash:
      FTokenKind:= tkBackSlash;
    tckAtSign:
      FTokenKind:= tkAtSign;
    tckCloseBrace:
      FTokenKind:= tkCloseBrace;
    tckCloseBracket:
      FTokenKind:= tkCloseBracket;
    tckClosePar:
      FTokenKind:= tkClosePar;
    tckColon:
      FTokenKind:= tkColon;
    tckComma:
      FTokenKind:= tkComma;
    tckDolarSign:
      FTokenKind:= tkDolarSign;
    tckDot:
      FTokenKind:= tkDot;
    tckHashtak:
      FTokenKind:= tkHashtak;
    tckNumberSign:
      FTokenKind:= tkNumberSign;
    tckOpenBrace:
      FTokenKind:= tkOpenBrace;
    tckOpenBracket:
      FTokenKind:= tkOpenBracket;
    tckOpenPar:
      FTokenKind:= tkOpenPar;
    tckPercent:
      FTokenKind:= tkPercent;
    tckQuestionMark:
      FTokenKind:= tkQuestionMark;
    tckSemiColon:
      FTokenKind:= tkSemiColon;
    tckTilda:
      FTokenKind:= tkTilda;
    tckSingleQuot:
      FTokenKind:= tkSingleQuot;
    tckAmpsign:
      FTokenKind:= tkBitwiseAnd;
    tckPipe:
      FTokenKind:= tkNot;
    tckLessThan:
      FTokenKind:= tkLessThan;
    tckBiggerThan:
      FTokenKind:= tkBiggerThan;
    tckDoubleQuot:
      FTokenKind:= tkDoubleQuot;
    tckMultiplySign:
      FTokenKind:= tkMultiplySign;
    tckSumSign:
      FTokenKind:= tkSumSign;
    tckMinusSign:
      FTokenKind:= tkMinusSign;
    tckDivideSign:
      FTokenKind:= tkDivideSign;
    tckEqualSign:
      FTokenKind:= tkEqualSign;
    tckExclamationMark:
      FTokenKind:= tkNot;
    tckGraveAccent:
      FTokenKind:= tkGraveAccent;
    tckFormFeed:
      FTokenKind:= tkFormFeed;
  end;

  if FTokenKind= tkStart then
  begin
    if Ch.Kind in [tckLetter, tckUnderLine] then
    begin
      FTokenKind:= tkIdentifier;
      AddChar (Ch);
      Ch:= GetNextChar;

      while Ch.Kind in [tckLetter, tckNumber, tckUnderLine] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      Result:= Ch;

      FKeywordType:= tktNone;
      for KeyWordIter:= Succ (tktStart) to Pred (tktEnd) do
        if Self.ToString= KeywordTypeString [KeyWordIter] then
        begin
          FKeywordType:= KeyWordIter;
          Break;
        end;

    end else if Ch.Kind in [tckNumber] then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      FTokenKind:= tkNumber;

      while Ch.Kind in [tckNumber] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      if Ch.Kind<> tckDot then
        Result:= Ch
      else
      begin
        AddChar (Ch);
        Ch:= GetNextChar;

        while Ch.Kind in [tckNumber] do
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
        Result:= Ch
      end;
    end
    else
    begin
      raise EInvalidCharacterEncountered.Create (Ch.Ch);
    end
  end
  else
    AddChar (Ch);

end;

function TToken.JavaReadFromFile(var FileHandle: TCharFile): TChar;

  function GetNextChar: TChar;
  var
    CharKind: TCharKind;
    Ch: Char;

  begin

    if FLastChar.Kind<> tckNone then
    begin
      Result:= FLastChar;
      FLastChar.Kind:= tckNone;
      Exit;
    end
    else
    if Eof (FileHandle) then
    begin
      Result.Kind:= tckEOF;
      Result.Ch:= #0;
      Exit;
    end
    else
    begin
      Read (FileHandle, Ch);
      for CharKind:= tckStart to tckEnd do
        if Ch in CharKindSet [CharKind] then
        begin
          Result.Kind:= CharKind;
          Result.Ch:= Ch;
          Exit;
        end;
    end;

    raise EInvalidCharacterEncountered.Create (Ch);
  end;

var
  Ch, TempCh: TChar;
  KeyWordIter: TKeywordType;

begin
  Result.Kind:= tckNone;
  if EOF (FileHandle) then
  begin
    Result.Kind:= tckEOF;
    if FLastChar.Kind= tckNone then
    begin
      FTokenKind:= tkEof;
      Exit;
    end;
  end;

  Ch:= GetNextChar;

  while Ch.Kind in [tckSpace, tckTab, tckReturn, tckCarriage] do
    Ch:= GetNextChar;
  if Ch.Kind= tckEOF then
  begin
    FTokenKind:= tkEof;
    Exit;
  end;

  FTokenKind:= tkStart;

  case Ch.Kind of
    tckBackSlash:
      FTokenKind:= tkBackSlash;
    tckAtSign:
      FTokenKind:= tkAtSign;
    tckCloseBrace:
      FTokenKind:= tkCloseBrace;
    tckCloseBracket:
      FTokenKind:= tkCloseBracket;
    tckClosePar:
      FTokenKind:= tkClosePar;
    tckColon:
      FTokenKind:= tkColon;
    tckComma:
      FTokenKind:= tkComma;
    tckDolarSign:
      FTokenKind:= tkDolarSign;
    tckDot:
      FTokenKind:= tkDot;
    tckHashtak:
      FTokenKind:= tkHashtak;
    tckNumberSign:
      FTokenKind:= tkNumberSign;
    tckOpenBrace:
      FTokenKind:= tkOpenBrace;
    tckOpenBracket:
      FTokenKind:= tkOpenBracket;
    tckOpenPar:
      FTokenKind:= tkOpenPar;
    tckPercent:
      FTokenKind:= tkPercent;
    tckQuestionMark:
      FTokenKind:= tkQuestionMark;
    tckSemiColon:
      FTokenKind:= tkSemiColon;
    tckTilda:
      FTokenKind:= tkTilda;
  end;

  if FTokenKind= tkStart then
  begin
    if Ch.Kind= tckSingleQuot then
    begin
      Ch:= GetNextChar;
      AddChar (Ch);

      if Ch.Kind= tckBackSlash then
      begin
        Ch:= GetNextChar;
        AddChar (Ch);
      end;

      Ch:= GetNextChar;
      if Ch.Kind<> tckSingleQuot then
        raise EInvalidToken.Create (Ch.Ch);
      FTokenKind:= tkChar;
    end
    else if Ch.Kind= tckAmpsign then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckAmpsign then
      begin
        AddChar (Ch);
        FTokenKind:= tkLogicalAnd;
      end
      else
      begin
        FTokenKind:= tkBitwiseAnd;
        Result:= Ch;
      end;
    end
    else if Ch.Kind= tckPipe then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckPipe then
      begin
        AddChar (Ch);
        FTokenKind:= tkLogicalOr;
      end
      else
      begin
        FTokenKind:= tkBitwiseOr;
        Result:= Ch;
      end;
    end
    else if Ch.Kind= tckLessThan then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckLessThan then
      begin
        AddChar (Ch);
        FTokenKind:= tkShiftLeft;
      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkLessThanOrEQualTo;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkLessThan;
      end;
    end
    else if Ch.Kind= tckBiggerThan then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckBiggerThan then
      begin
        AddChar (Ch);
        FTokenKind:= tkShiftRight;
      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkBiggerThanOrEQualTo;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkBiggerThan;
      end;
    end
    else if Ch.Kind= tckDoubleQuot then
    begin
      FTokenKind:= tkString;
      Ch:= GetNextChar;

      while Ch.Kind<> tckDoubleQuot do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
        if Ch.Kind= tckBackSlash then
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
      end;


    end
    else if Ch.Kind= tckMultiplySign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkMinusAssign;
      end
      else
      begin
        FTokenKind:= tkMultiplySign;
        Result:= Ch;
      end
    end
    else if Ch.Kind= tckSumSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkPlusAssign;
      end
      else if Ch.Kind= tckSumSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkAutoInc;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkSumSign;
      end;
    end
    else if Ch.Kind= tckMinusSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkMinusAssign;
      end
      else if Ch.Kind= tckMinusSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkAutoDec;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkMinusSign;
      end;
    end
    else if Ch.Kind= tckDivideSign then
    begin
      Ch:= GetNextChar;

      if Ch.Kind= tckDivideSign then
      begin
        FTokenKind:= tkComment;

        while not (Ch.Kind in [tckReturn, tckCarriage]) do
        begin
          Ch:= GetNextChar;
          AddChar (Ch);
        end;

        while not (Ch.Kind in [tckReturn]) do
          Ch:= GetNextChar;
      end
      else if Ch.Kind= tckMultiplySign then
      begin
        FTokenKind:= tkComment;
        Ch:= GetNextChar;

        while True do
        begin
          while Ch.Kind<> tckMultiplySign do
          begin
            AddChar (Ch);
            Ch:= GetNextChar;
          end;

          Ch:= GetNextChar;
          if Ch.Kind= tckDivideSign then
            Break
          else
          begin
            TempCh.Kind:= tckMultiplySign;
            TempCh.Ch:= '*';
            AddChar (TempCh);
            AddChar (Ch);
          end;
        end;

      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkDivAssign;
      end
      else
      begin
        FTokenKind:= tkDivideSign;
        Result:= Ch;

        Ch.Kind:= tckDivideSign;
        Ch.Ch:= '/';
        AddChar (Ch);
      end;
    end
    else if Ch.Kind= tckEqualSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkEqualEqualSign
      end
      else
      begin
        FTokenKind:= tkEqualSign;
        Result:= Ch;
        Exit;
      end;
    end
    else if Ch.Kind in [tckLetter, tckUnderLine] then
    begin
      FTokenKind:= tkIdentifier;
      AddChar (Ch);
      Ch:= GetNextChar;

      while Ch.Kind in [tckLetter, tckNumber, tckUnderLine] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      Result:= Ch;

      FKeywordType:= tktNone;
      for KeyWordIter:= Succ (tktStart) to Pred (tktEnd) do
        if Self.ToString= KeywordTypeString [KeyWordIter] then
        begin
          FKeywordType:= KeyWordIter;
          Break;
        end;

    end else if Ch.Kind in [tckNumber] then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      FTokenKind:= tkNumber;

      while Ch.Kind in [tckNumber] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      if Ch.Kind<> tckDot then
        Result:= Ch
      else
      begin
        AddChar (Ch);
        Ch:= GetNextChar;

        while Ch.Kind in [tckNumber] do
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
        Result:= Ch
      end;
    end
    else if Ch.Kind= tckExclamationMark then
    begin
      Ch:= GetNextChar;

      if Ch.Kind<> tckEqualSign then
      begin
        Result:= Ch;
        TempCh.Ch:= '!';
        TempCh.Kind:= tckExclamationMark;
        AddChar (TempCh);

        FTokenKind:= tkNot;
      end
      else
      begin
        TempCh.Ch:= '!';
        TempCh.Kind:= tckExclamationMark;
        AddChar (TempCh);
        AddChar (Ch);
        FTokenKind:= tkNotEqualTo;
      end;
    end
    else
    begin
      raise EInvalidCharacterEncountered.Create (Ch.Ch);
    end
  end
  else
    AddChar (Ch);

end;

function TToken.JavaRead (Stream: TStream): TChar;

  function GetNextChar: TChar;
  var
    CharKind: TCharKind;
    Ch: Char;

  begin

    if FLastChar.Kind<> tckNone then
    begin
      Result:= FLastChar;
      FLastChar.Kind:= tckNone;
      Exit;
    end
    else
    if Stream.Size<= Stream.Position then
    begin
      Result.Kind:= tckEOF;
      Result.Ch:= #0;
      Exit;
      
    end
    else
    begin
      Ch:= Char (Stream.ReadByte);
      
      for CharKind:= tckStart to tckEnd do
        if Ch in CharKindSet [CharKind] then
        begin
          Result.Kind:= CharKind;
          Result.Ch:= Ch;
          Exit;
          
        end;
        
    end;

    raise EInvalidCharacterEncountered.Create (Ch);
  end;

var
  Ch, TempCh: TChar;
  KeyWordIter: TKeywordType;

begin
  Result.Kind:= tckNone;
  
  if Stream.Position<= Stream.Size then
  begin
    Result.Kind:= tckEOF;
    if FLastChar.Kind= tckNone then
    begin
      FTokenKind:= tkEof;
      Exit;
      
    end;
    
  end;

  Ch:= GetNextChar;

  while Ch.Kind in [tckSpace, tckTab, tckReturn, tckCarriage] do
    Ch:= GetNextChar;
    
  if Ch.Kind= tckEOF then
  begin
    FTokenKind:= tkEof;
    Exit;
    
  end;

  FTokenKind:= tkStart;

  case Ch.Kind of
    tckBackSlash:
      FTokenKind:= tkBackSlash;
    tckAtSign:
      FTokenKind:= tkAtSign;
    tckCloseBrace:
      FTokenKind:= tkCloseBrace;
    tckCloseBracket:
      FTokenKind:= tkCloseBracket;
    tckClosePar:
      FTokenKind:= tkClosePar;
    tckColon:
      FTokenKind:= tkColon;
    tckComma:
      FTokenKind:= tkComma;
    tckDolarSign:
      FTokenKind:= tkDolarSign;
    tckDot:
      FTokenKind:= tkDot;
    tckHashtak:
      FTokenKind:= tkHashtak;
    tckNumberSign:
      FTokenKind:= tkNumberSign;
    tckOpenBrace:
      FTokenKind:= tkOpenBrace;
    tckOpenBracket:
      FTokenKind:= tkOpenBracket;
    tckOpenPar:
      FTokenKind:= tkOpenPar;
    tckPercent:
      FTokenKind:= tkPercent;
    tckQuestionMark:
      FTokenKind:= tkQuestionMark;
    tckSemiColon:
      FTokenKind:= tkSemiColon;
    tckTilda:
      FTokenKind:= tkTilda;
  end;

  if FTokenKind= tkStart then
  begin
    if Ch.Kind= tckSingleQuot then
    begin
      Ch:= GetNextChar;
      AddChar (Ch);

      if Ch.Kind= tckBackSlash then
      begin
        Ch:= GetNextChar;
        AddChar (Ch);
      end;

      Ch:= GetNextChar;
      if Ch.Kind<> tckSingleQuot then
        raise EInvalidToken.Create (Ch.Ch);
      FTokenKind:= tkChar;
    end
    else if Ch.Kind= tckAmpsign then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckAmpsign then
      begin
        AddChar (Ch);
        FTokenKind:= tkLogicalAnd;
      end
      else
      begin
        FTokenKind:= tkBitwiseAnd;
        Result:= Ch;
      end;
    end
    else if Ch.Kind= tckPipe then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckPipe then
      begin
        AddChar (Ch);
        FTokenKind:= tkLogicalOr;
      end
      else
      begin
        FTokenKind:= tkBitwiseOr;
        Result:= Ch;
      end;
    end
    else if Ch.Kind= tckLessThan then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckLessThan then
      begin
        AddChar (Ch);
        FTokenKind:= tkShiftLeft;
      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkLessThanOrEQualTo;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkLessThan;
      end;
    end
    else if Ch.Kind= tckBiggerThan then
    begin
      AddChar (Ch);

      Ch:= GetNextChar;

      if Ch.Kind= tckBiggerThan then
      begin
        AddChar (Ch);
        FTokenKind:= tkShiftRight;
      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkBiggerThanOrEQualTo;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkBiggerThan;
      end;
    end
    else if Ch.Kind= tckDoubleQuot then
    begin
      FTokenKind:= tkString;
      Ch:= GetNextChar;

      while Ch.Kind<> tckDoubleQuot do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
        if Ch.Kind= tckBackSlash then
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
      end;


    end
    else if Ch.Kind= tckMultiplySign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkMinusAssign;
      end
      else
      begin
        FTokenKind:= tkMultiplySign;
        Result:= Ch;
      end
    end
    else if Ch.Kind= tckSumSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkPlusAssign;
      end
      else if Ch.Kind= tckSumSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkAutoInc;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkSumSign;
      end;
    end
    else if Ch.Kind= tckMinusSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;

      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkMinusAssign;
      end
      else if Ch.Kind= tckMinusSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkAutoDec;
      end
      else
      begin
        Result:= Ch;
        FTokenKind:= tkMinusSign;
      end;
    end
    else if Ch.Kind= tckDivideSign then
    begin
      Ch:= GetNextChar;

      if Ch.Kind= tckDivideSign then
      begin
        FTokenKind:= tkComment;

        while not (Ch.Kind in [tckReturn, tckCarriage]) do
        begin
          Ch:= GetNextChar;
          AddChar (Ch);
        end;

        while not (Ch.Kind in [tckReturn]) do
          Ch:= GetNextChar;
      end
      else if Ch.Kind= tckMultiplySign then
      begin
        FTokenKind:= tkComment;
        Ch:= GetNextChar;

        while True do
        begin
          while Ch.Kind<> tckMultiplySign do
          begin
            AddChar (Ch);
            Ch:= GetNextChar;
          end;

          Ch:= GetNextChar;
          if Ch.Kind= tckDivideSign then
            Break
          else
          begin
            TempCh.Kind:= tckMultiplySign;
            TempCh.Ch:= '*';
            AddChar (TempCh);
            AddChar (Ch);
          end;
        end;

      end
      else if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkDivAssign;
      end
      else
      begin
        FTokenKind:= tkDivideSign;
        Result:= Ch;

        Ch.Kind:= tckDivideSign;
        Ch.Ch:= '/';
        AddChar (Ch);
      end;
    end
    else if Ch.Kind= tckEqualSign then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      if Ch.Kind= tckEqualSign then
      begin
        AddChar (Ch);
        FTokenKind:= tkEqualEqualSign
      end
      else
      begin
        FTokenKind:= tkEqualSign;
        Result:= Ch;
        Exit;
      end;
    end
    else if Ch.Kind in [tckLetter, tckUnderLine] then
    begin
      FTokenKind:= tkIdentifier;
      AddChar (Ch);
      Ch:= GetNextChar;

      while Ch.Kind in [tckLetter, tckNumber, tckUnderLine] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      Result:= Ch;

      FKeywordType:= tktNone;
      for KeyWordIter:= Succ (tktStart) to Pred (tktEnd) do
        if Self.ToString= KeywordTypeString [KeyWordIter] then
        begin
          FKeywordType:= KeyWordIter;
          Break;
        end;

    end else if Ch.Kind in [tckNumber] then
    begin
      AddChar (Ch);
      Ch:= GetNextChar;
      FTokenKind:= tkNumber;

      while Ch.Kind in [tckNumber] do
      begin
        AddChar (Ch);
        Ch:= GetNextChar;
      end;
      if Ch.Kind<> tckDot then
        Result:= Ch
      else
      begin
        AddChar (Ch);
        Ch:= GetNextChar;

        while Ch.Kind in [tckNumber] do
        begin
          AddChar (Ch);
          Ch:= GetNextChar;
        end;
        Result:= Ch
      end;
    end
    else if Ch.Kind= tckExclamationMark then
    begin
      Ch:= GetNextChar;

      if Ch.Kind<> tckEqualSign then
      begin
        Result:= Ch;
        TempCh.Ch:= '!';
        TempCh.Kind:= tckExclamationMark;
        AddChar (TempCh);

        FTokenKind:= tkNot;
      end
      else
      begin
        TempCh.Ch:= '!';
        TempCh.Kind:= tckExclamationMark;
        AddChar (TempCh);
        AddChar (Ch);
        FTokenKind:= tkNotEqualTo;
      end;
    end
    else
    begin
      raise EInvalidCharacterEncountered.Create (Ch.Ch);
    end
  end
  else
    AddChar (Ch);

end;

constructor TToken.Create (TokenString: String);
begin
  inherited Create;
//??!!
  raise ENotImplementedYet.Create ('');
  
end;

function TToken.ToString: String;
var
 i: Integer;
 
begin
  Result:= '';
  
  for i:= 0 to CharCount- 1 do
    Result:= Result+ FChars [i].Ch;
end;

{ ERangeCheckError }

constructor ERangeCheckError.Create (FunctionName: String);
begin
  inherited Create ('Range Check Error in '+ FunctionName);
  
end;

procedure ERangeCheckError.Free;
begin

  inherited;
end;

{ EInvalidCharacterEncountered }

constructor EInvalidCharacterEncountered.Create (Ch: char);
begin
  inherited Create ('Invalid Character Encountered, ('+ IntToStr (Ord (Ch))+ ':'+ Ch+ ')');
  
end;

procedure EInvalidCharacterEncountered.Free;
begin

  inherited;
end;

{ EStringIsNotKeyword }

constructor EStringIsNotKeyword.Create(RealData: String);
begin
  inherited Create ('String '+ RealData+ 'is not a keyword !');
  
end;

procedure EStringIsNotKeyword.Free;
begin

  inherited;
end;

{ EInvalidToken }

constructor EInvalidToken.Create (TokenStr: String);
begin
  inherited Create (TokenStr+ 'is not a valid Token!');
  
end;

procedure EInvalidToken.Free;
begin

  inherited;
end;

{ EExpectFailed }

constructor EExpectFailed.Create (Keyword: TKeywordType; TokenStr: String);
begin
  inherited Create ('kEYWORD '+ KeywordTypeString [Keyword]+ ' Expected but '+ TokenStr+ ' Found!');
  
end;

constructor EExpectFailed.Create (TokenKind: TTokenKind; TokenStr: String);
begin
  inherited Create ('Token '+ IntToStr (Integer (TokenKind))+ ' Expected but '+ TokenStr+ ' Found!');

end;

constructor EExpectFailed.Create(TokenString: String; TokenStr: String);
begin
  inherited Create (TokenString+ ' Expected but '+ TokenStr+ ' Found!');
  
end;

procedure EExpectFailed.Free;
begin

  inherited;
end;

{ TLexerSavePoint }

constructor TLexerSavePoint.Create (PointerLocation: Integer; CurToken: TToken; LastCh: TChar);
begin
  inherited Create;
  
  FFilePointerLocation:= PointerLocation;
  FCurrentToken:= CurToken.Copy;
  FLastChar:= LastCh;
end;

procedure TLexerSavePoint.Free;
begin
  FCurrentToken.Free;
  
  inherited;
end;

end.

