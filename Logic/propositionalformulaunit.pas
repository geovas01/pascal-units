unit PropositionalFormulaUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type

  { EEndOfStreamReached }

  EEndOfStreamReached= class (Exception)
  public
    constructor Create;
    
  end;
  
  TBoolean= (bTrue, bFalse, bUnknonw);
  TTokenKind= (tkIdentifier, tkNumber, tkAnd, tkOr, tkImplies, tkNot,
    tkOpenPar, tkClosePar);
const
  TokenString: array [tkIdentifier.. tkClosePar] of String=
     ('', '', '\land', '\lor', '\implies', '\lnot', '(', ')');
     
type
  
  { TToken }

  TToken= class (TObject)
  private
    FKind: TTokenKind;
    FText: String;

  public
    property Text: String read FText;
    property Kind: TTokenKind read FKind;
    

  end;
  
  { TTokenCollection }

  TTokenCollection= class (TBaseCollection)
  private
    function GetTokenAt(Index: Integer): TToken;
  public
    property TokenAt [Index: Integer]: TToken read GetTokenAt;
    
  end;
  
  TCharKind= (ckLetter, ckDigit, ckSlash, ckBackslash, ckUnderLine, ckOpenPar,
    ckClosedPar, ckSpace, ckOthers);

  { TChar }

  TChar= class (TObject)
    FCharacter: Char;
    FCharKind: TCharKind;
  private
  public
    property CharKind: TCharKind read FCharKind;
    property Character: Char read FCharacter;
    
    constructor Create (Ch: Char);
    
  end;
  
const
  TCharSet: array [ckLetter..ckOthers] of set of Char= (['A'..'Z','a'..'z'],
      ['0'..'9'], ['/'], ['\'], ['_'], ['('], [')'], [' ', #7, #10, #13], []);
  
type

  { TLexer }

  TLexer= class (TObject)
    FPosition: Integer;
    FALLTokens: TTokenCollection;
    UnUsedChars: TBaseCollection;
    function GetNextToken: TToken;
    function ExtractNextToken: TToken;
    
  private
    FStream: TStream;
    function GetToken: TToken;
    
  public
    property NextToken: TToken read GetNextToken;
    property CurToken: TToken read GetToken;
    property Position: Integer read FPosition;
    
    constructor Create (AStream: TStream);
    destructor Destroy; override;
    

  
  end;
  
  { TAtom }

  TAtom= class (TObject)
  private
    FName: String;
    FValue: TBoolean;
    
  public
    property Name: String read FName;
    property Value: TBoolean read FValue write FValue;
    
    constructor Create (AName: String); overload;
    constructor Create (Lexer: TLexer); overload;

  end;
  
  
 
implementation

{ TAtom }

constructor TAtom.Create(AName: String);
begin
  inherited Create;
  
  FName:= AName;
  Value:= bUnknonw;
  
end;

constructor TAtom.Create (Lexer: TLexer);
begin
  FName:= '';
  FValue:= bUnknonw;
  
//  while Lexer.n;
end;

{ TLexer }

function TLexer.GetNextToken: TToken;
begin
  if FPosition< FALLTokens.Size then
    Result:= FALLTokens.TokenAt [FPosition]
  else
  begin
    ExtractNextToken;
    Result:= GetNextToken;
    
  end;
  
end;

function TLexer.ExtractNextToken: TToken;

  function GetNextChar: TChar;
  var
    Ch: Char;
    
  begin
    if UnUsedChars.Size= 0 then
    begin
      if FStream.Position< FStream.Size then
      begin
        Ch:= Char (FStream.ReadByte);
        Result:= TChar.Create (Ch);

      end
      else
      begin
        raise EEndOfStreamReached.Create;
        
      end;
     
    end
    else
    begin
      Result:= UnUsedChars.Member [0] as TChar;
      UnUsedChars.Member [0]:= nil;
      UnUsedChars.Delete (0);
      
    end;
    
  end;
  
var
  Ch: TChar;
  State: Integer;
  Str: String;
  tk: TTokenKind;
  
begin
  try
    State:= 1;
    Result:= TToken.Create;
    Ch:= GetNextChar;

    case Ch.CharKind of
      ckUnderLine, ckLetter :
      begin
        Result.FKind:= tkIdentifier;
        State:= 2;
        
      end;
      
      ckDigit:
      begin
        Result.FKind:= tkNumber;
        State:= 3;
        
      end;
      
      ckSlash:
      begin
//        Result.FKind:= tkOperator;
        State:= 4;
        
      end;

      ckOpenPar:
      begin
        Result.FKind:= tkOpenPar;
        Result.FText:= '(';
        Ch.Free;
        Exit;

      end;

      ckClosedPar:
      begin
        Result.FKind:= tkClosePar;
        Result.FText:= ')';
        Ch.Free;
        Exit;

      end;

    end;

    while True do
    begin
      Ch.Free;
      Ch:= GetNextChar;

      if State= 2 then
        case Ch.CharKind of
          ckUnderLine, ckLetter, ckDigit :
          begin
            Result.FText:= Result.Text+ Ch.Character;
            State:= 2;

          end
          else
          begin
            UnUsedChars.Add (Ch);
            break;
            
          end;
          
        end
      else if State= 3 then
        case Ch.CharKind of
          ckDigit :
          begin
            Result.FText:= Result.Text+ Ch.Character;
            State:= 3;

          end
          else
          begin
            UnUsedChars.Add (Ch);
            break;

          end;

        end
      else if State= 4 then
        case Ch.CharKind of
          ckUnderLine, ckLetter, ckDigit :
          begin
            Result.FText:= Result.Text+ Ch.Character;
            State:= 4;

          end
          else
          begin
            UnUsedChars.Add (Ch);
            break;

          end;

        end

    end;
    
    if State= 4 then
    begin
      for  tk:= TTokenKind (0) to TTokenKind (High (TTokenKind)) do
        if UpperCase (Result.Text)= UpperCase (TokenString [tk]) then
        begin
          Result.FKind:= tk;
          Break;
          
        end;
        
    end;
    
  except
    on e: EEndOfStreamReached do
     ;
  end;

  Result:= TToken.Create;

end;

function TLexer.GetToken: TToken;
begin
  Result:= FALLTokens.TokenAt [Position];
  
end;

constructor TLexer.Create(AStream: TStream);
begin
  inherited Create;
  
  FStream:= AStream;
  FALLTokens:= TTokenCollection.Create;
  UnUsedChars:= TBaseCollection.Create;
  
end;

destructor TLexer.Destroy;
begin
  FStream.Free;
  FALLTokens.Free;
  UnUsedChars.Free;
  
  inherited Destroy;
  
end;

{ TTokenCollection }

function TTokenCollection.GetTokenAt(Index: Integer): TToken;
begin
  Result:= Member [Index] as TToken;
  
end;

{ EEndOfStreamReached }

constructor EEndOfStreamReached.Create;
begin
  inherited Create ('End of Stream Reached!');
  
end;

{ TChar }

constructor TChar.Create (Ch: Char);
var
  C: TCharKind;
  
begin
  inherited Create;
  
  FCharKind:= ckOthers;
  
  for c:= TCharKind(0) to TCharKind (High (TCharKind)) do
    FCharKind:= c;
  
end;

end.

