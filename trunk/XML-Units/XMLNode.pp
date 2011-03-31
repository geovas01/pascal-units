unit XMLNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit, AttributeUnit;

type
  ENodeNotFound= class (Exception);
  EInvalidXML= class (Exception);

  TXMLNodeCollection= class;

  { TXMLNode }

  TXMLNode= class
  private
    FAttributes: TAttributeCollection;
    FChildren: TXMLNodeCollection;
    FContent: WideString;
    FParent: TXMLNode;
    FTag: WideString;
    function GetChild (Index: Integer): TXMLNode;
    function GetNodeByPath (Path: WideString): TXMLNode;
    procedure SetContent (const AValue: WideString);

  public
    property Parent: TXMLNode read FParent;
    property Child [Index: Integer]: TXMLNode read GetChild;
    property Children: TXMLNodeCollection read FChildren;
    property Tag: WideString read FTag write FTag;
    property Attributes: TAttributeCollection read FAttributes;
    property NodeByPath [Path: WideString]: TXMLNode read GetNodeByPath;
    property Content: WideString read FContent write SetContent;
    
    constructor Create;
    constructor Create (ParentNode: TXMLNode; TagName: WideString; Cont: WideString= '');
    constructor Create (TagName: WideString; Cont: WideString= '');
    destructor Destroy; override;
    
    procedure AddChild (NewNode: TXMLNode);
    function ToStringWithIndent (Space: String= ''): WideString;
    function ToStringWithOutIndent: WideString;
    procedure AddAttribute (NewAttribute: TAttribute);
    procedure AddAttribute (Name, Value: WideString);

    procedure SaveToFile (var OutputFile: TextFile);
    
  end;

  { TXMLDocument }

  TXMLDocument= class (TObject)
  private
    FRoots: TXMLNodeCollection;
    FEncoding: AnsiString;
    FVersion: AnsiString;
  public
    property Roots: TXMLNodeCollection read FRoots;
    property Encoding: AnsiString read FEncoding;
    property Version: AnsiString read FVersion;

    constructor Load (Filename: AnsiString);
    destructor Destroy; override;
    constructor Create (Root: TXMLNode; _Encoding: AnsiString= 'UTF-8'; _Version: AnsiString= '1.0');

    procedure Save (Filename: AnsiString);

  end;
  
  PXMLNode= ^TXMLNode;
  
  { TXMLNodeCollection }

  TXMLNodeCollection= class (TBaseCollection)
  private
    function GetNode (Index: Integer): TXMLNode; overload;
    function GetNode (ChildName: String): TXMLNode; overload;
  
  public
    property Node [Index: Integer]: TXMLNode read GetNode;
    property NodeByName [ChildName: String]: TXMLNode read GetNode;

    constructor Create;
    
    procedure Merge (AnotherCollection: TXMLNodeCollection);
    procedure AddNode (NewNode: TXMLNode);
    
  end;
  

implementation
uses
  StreamUnit, WideStringUnit;

{ TXMLNode }

function TXMLNode.GetChild (Index: Integer): TXMLNode;
begin
  Result:= FChildren.Node [Index];
  
end;

function TXMLNode.GetNodeByPath (Path: WideString): TXMLNode;
var
  S: String;
  
begin
  S:= Copy (Path, 1, Pos ('/', Path)- 1);
  Delete (Path, 1, Pos ('/', Path));
  
  if UpperCase (S)<> UpperCase (FTag) then
    raise ENodeNotFound.Create ('');
  S:= Copy (Path, 1, Pos ('/', Path)- 1);

  if S= '' then
    Result:= Self
  else
  begin
//    Delete (Path, 1, Pos ('/', Path));
    Result:= FChildren.NodeByName [S].NodeByPath [Path];
    
  end;
  
end;

procedure TXMLNode.SetContent (const AValue: WideString);
begin
  FContent:= AValue;

end;

constructor TXMLNode.Create;
begin
  inherited Create;
  
  FTag:= '';
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChildren:= TXMLNodeCollection.Create;

end;

constructor TXMLNode.Create (ParentNode: TXMLNode; TagName: WideString; Cont: WideString= '');
begin
  inherited Create;
  
  FTag:= TagName;
  FParent:= ParentNode;
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChildren:= TXMLNodeCollection.Create;
  FParent.AddChild (Self);
  FContent:= Cont

end;

constructor TXMLNode.Create(TagName: WideString; Cont: WideString);
begin
  inherited Create;

  FTag:= TagName;
  FParent:= nil;
  FContent:= '';
  FAttributes:= TAttributeCollection.Create;
  FChildren:= TXMLNodeCollection.Create;
  FContent:= Cont;

end;

type

  { TXMLLexer }

  TXMLLexer= class (TObject)
  private
    TokenPositions: TIntegerCollection;
    FStream: TMyTextStream;
    function GetPosition: Integer;
    procedure SetPosition(const AValue: Integer);

  public
    property Position: Integer read GetPosition write SetPosition;
    constructor Create (Stream: TMyTextStream);
    destructor Destroy; override;

    function GetNextToken: WideString;
    function GetNextChar: WideString;
    procedure Rewind;
  end;

{ TXMLLexer }

function TXMLLexer.GetPosition: Integer;
begin
  Result:= FStream.Position;

end;

procedure TXMLLexer.SetPosition (const AValue: Integer);
begin
  FStream.Position:= AValue;

end;

constructor TXMLLexer.Create (Stream: TMyTextStream);
begin
  inherited Create;

  FStream:= Stream;
  TokenPositions:= TIntegerCollection.Create;

end;

destructor TXMLLexer.Destroy;
begin
  TokenPositions.Free;

  inherited Destroy;

end;

function TXMLLexer.GetNextToken: WideString;
const
  TwoCharsToken: array [1..3] of array [1..2] of WideString=
    (
       ('<', '/'),
       ('/', '>'),
       ('?', '>')
     );

var
  Last: WideString;
  Pos: Int64;
  i: Integer;

begin
  TokenPositions.Add (FStream.Position);

  Result:= GetNextChar;
  Pos:= FStream.Position;

  try
    Last:= GetNextChar;

  except
    on e: EEoStream do
      Exit;

  end;

  for i:= Low (TwoCharsToken) to High (TwoCharsToken) do
    if (Result= TwoCharsToken [i][1]) and (Last= TwoCharsToken [i][2]) then
      Exit (Result+ Last);

  if Result= '<' then
  begin
    if Last= '!' then
    begin
      while GetNextChar<> '>' do;

      Result:= GetNextToken ();
      Exit;

    end
    else
      FStream.Position:= Pos;

  end
  else
    FStream.Position:= Pos;

end;

function TXMLLexer.GetNextChar: WideString;
const
  BreakChars: array [1..6] of WideChar= ('<', '?', '>', '/', '!', '=');
  IgnoreChars: array [1..2] of WideChar= (#10, #13);

function InBreakChars (Ch: WideChar): Boolean;
var
  i: Integer;

begin
  for i:= Low (BreakChars) to High (BreakChars)  do
    if Ch= BreakChars [i] then
      Exit (True);

  Result:= False;

end;

function InIgnoreChars (Ch: WideChar): Boolean;
var
  i: Integer;

begin
  for i:= Low (IgnoreChars) to High (IgnoreChars)  do
    if Ch= IgnoreChars [i] then
      Exit (True);

  Result:= False;

end;

var
  Ch: WideChar;
  State: Integer;
  {State= 0 : *
   State= 1: "
  }

begin
  State:= 0;
  Result:= '';

  while True do
  begin
    Ch:= FStream.ReadWideChar;
    while InIgnoreChars (Ch) do
      Ch:= FStream.ReadWideChar;

    case State of
      0:
        if InBreakChars (Ch) then
        begin
          if Result= '' then
            Exit (Ch)
          else
          begin
            FStream.Position:= FStream.Position- 1;
            Exit (Result);

          end;

        end
        else if Ch= '"' then
        begin
            if Result= '' then
              State:= 1
            else
            begin
              FStream.Position:= FStream.Position- 1;
              Exit (Result);

            end;

        end
        else if Ch= ' ' then
        begin
          if Result<> '' then
            Exit (Result)
        end
        else
          Result+= Ch;

      1:
      begin
        Result:= '"';

        while Ch<> '"' do
        begin
          Result+= Ch;
          Ch:= FStream.ReadWideChar;

        end;
        Result+= Ch;
        Exit;

      end;
    end;

  end;

end;

procedure TXMLLexer.Rewind;
begin
  FStream.Position:= TokenPositions.MemberAt [TokenPositions.Size- 1];
  TokenPositions.Delete (TokenPositions.Size- 1);

end;

destructor TXMLNode.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  
  inherited;
  
end;

procedure TXMLNode.AddChild (NewNode: TXMLNode);
begin
  FChildren.AddNode (NewNode);
  
end;

function TXMLNode.ToStringWithIndent (Space: String): WideString;
var
  i: Integer;

begin
  Result:= Space+ '<'+ FTag+ ' '+ FAttributes.ToString;

  if FChildren.Size= 0 then
  begin
    Result+= '>';
    if Content<> '' then
      Result+= Content+ ' ';
    Result:= Result+ '</'+ FTag+ '>';

  end
  else
  begin
    Result:= Result+ '>';
    if Content<> '' then
      Result+= Content+ ' ';
    for i:= 0 to FChildren.Size- 1 do
      Result:= Result+ #10+ Child [i].ToStringWithIndent (Space+ '  ');
    Result:= Result+ #10+ Space+ '</'+ FTag+ '>';
      
  end;

end;

function TXMLNode.ToStringWithOutIndent: WideString;
var
  i: Integer;

begin

  Result:= '<'+ FTag+ ' '+ FAttributes.ToString;

  if FChildren.Size= 0 then
    Result:= Result+ '/>'
  else
  begin
    Result:= Result+ '>';
    for i:= 0 to FChildren.Size- 1 do
      Result:= Result+ #10+ Child [i].ToStringWithOutIndent;
      
    Result:= Result+ #10'</'+ FTag+ '>';
    
  end;
      
end;

procedure TXMLNode.AddAttribute (NewAttribute: TAttribute);
begin
  FAttributes.Add (NewAttribute);
  
end;

procedure TXMLNode.AddAttribute (Name, Value: WideString);
begin
  FAttributes.Add (TAttribute.Create (Name, Value));
  
end;

procedure TXMLNode.SaveToFile (var OutputFile: TextFile);
var
  S: AnsiString;

begin
  S:= Self.ToStringWithIndent ('');
  WriteLn (OutputFile, S);
  
end;

{ TXMLNodeCollection }

function TXMLNodeCollection.GetNode (Index: Integer): TXMLNode;
begin
  Result:= Member [Index] as TXMLNode;
  
end;

function TXMLNodeCollection.GetNode (ChildName: String): TXMLNode;
var
  i: Integer;
  
begin
  Result:= nil;
  
  ChildName:= UpperCase (ChildName);
  
  for i:= 0 to Size- 1 do
    if UpperCase (Node [i].Tag)= ChildName then
    begin
      Result:= Node [i];
      Exit;
      
    end;
  Result:= nil;
  
  raise ENodeNotFound.Create (ChildName);
    
end;

constructor TXMLNodeCollection.Create;
begin
  inherited Create;
  
end;

procedure TXMLNodeCollection.Merge (AnotherCollection: TXMLNodeCollection);
var
  i: Integer;
  Ptr: PXMLNode;
  
begin
  Ptr:= @FMembers [0];
  
  for i:= 0 to AnotherCollection.Size- 1 do
  begin
    Self.AddNode (Ptr^);
    Inc (Ptr);
    
  end;
  
end;

procedure TXMLNodeCollection.AddNode (NewNode: TXMLNode);
begin
  inherited Add (NewNode);
  
end;

{ TXMLDocument }

constructor TXMLDocument.Load (FileName: AnsiString);
var
  Lexer: TXMLLexer;

  function Ignore (S: WideString): Widestring; //Ignore chars till it reaches ?>
  var
    Temp: WideString;

  begin
    Result:= Lexer.GetNextToken;
    Temp:= Result;

    while WideStrPos (S, Result)= 0 do
    begin
      Result+= ' '+ Lexer.GetNextToken;

    end;

  end;

  procedure Expect (S: WideString);
  var
    ActiveS: WideString;

  begin
    ActiveS:= Lexer.GetNextToken;
    while Length (ActiveS)< Length (S) do
      ActiveS+= Lexer.GetNextToken;

    if ActiveS<> S then
      raise EInvalidXML.Create (S+ ' expected but '+ ActiveS+ ' visited!');

  end;

  function ParseXML: TXMLNode;
  var
    S, T: WideString;
    TagName: WideString;
    Pos: Integer;

  begin
    S:= Lexer.GetNextToken;
    if S= '<' then
    begin
      TagName:= Lexer.GetNextToken;
      Result:= TXMLNode.Create (TagName);

      S:= Lexer.GetNextToken;
      if S= '/>' then
        Exit
      else if S= '>' then
      //Children
      else
      begin
        Expect ('=');
        T:= Lexer.GetNextToken;
        Result.AddAttribute (S, T);

        S:= Lexer.GetNextToken;
        while S<> '>' do
        begin
          T:= Lexer.GetNextToken;
          Result.AddAttribute (S, T);

          S:= Lexer.GetNextToken;

        end;

      end;

      T:= Result.Tag;
      S:= Ignore ('<');
      Result.Content:= WideStrCopy (S, 1, WideStrPos ('<', S)- 1);
      Lexer.Position:= Lexer.Position- (Length (S)- WideStrPos ('<', S)+ 1);

      while True do
      begin
        Pos:= Lexer.Position;

        if Lexer.GetNextToken= '</' then
        begin
          TagName:= Lexer.GetNextToken;
          Expect ('>');
          if Result.Tag= TagName then
            Exit
          else
            raise EInvalidXML.Create (Result.Tag+ ' expected but '+ ' '+ TagName+ ' Visited!');

        end
        else
        begin
          Lexer.Position:= Pos;
          Result.AddChild (ParseXML ());

        end;

      end;


    end;

  end;

var
  Stream: TMyTextStream;
  S, T, TagName: WideString;
  Root: TXMLNode;
  Pos: Integer;

begin
  inherited Create;

  Stream:= TMyTextStream.Create (TFileStream.Create (FileName, fmOpenRead), True);
  Lexer:= TXMLLexer.Create (Stream);
  FRoots:= TXMLNodeCollection.Create;

  Expect ('<?xml');
  Ignore ('?>');
  S:= Lexer.GetNextToken;
  if S= '<' then
  begin
    Root:= TXMLNode.Create (Lexer.GetNextToken);

    S:= Lexer.GetNextToken;
    if S= '/>' then
    else
    begin
      if S<> '>' then
      begin
        Expect ('=');
        T:= Lexer.GetNextToken;
        Root.AddAttribute (S, T);

        S:= Lexer.GetNextToken;
        while S<> '>' do
        begin
          T:= Lexer.GetNextToken;
          Root.AddAttribute (S, T);

          S:= Lexer.GetNextToken;

        end;

      end;

      T:= Root.Tag;
      S:= Ignore ('<');
      Root.Content:= WideStrCopy (S, 1, WideStrPos ('<', S)- 1);
      Lexer.Position:= Lexer.Position- 1;

      while True do
      begin
        Pos:= Stream.Position;

        if Lexer.GetNextToken= '</' then
        begin
          TagName:= Lexer.GetNextToken;
          Expect ('>');
          if Root.Tag= TagName then
            Break
          else
            raise EInvalidXML.Create (Root.Tag+ ' expected but '+ ' '+ TagName+ ' Visited!');

        end
        else
        begin
          Stream.Position:= Pos;
          Root.AddChild (ParseXML ());

        end;

      end;

      FRoots.Add (Root);

    end;

  end
  else
    raise EInvalidXML.Create ('Invalid String "'+ S+ '"');

  Lexer.Free;
  Stream.Free;

end;

destructor TXMLDocument.Destroy;
var
  i: Integer;

begin
  for i:= 0 to FRoots.Size- 1 do
    FRoots.Node [i].Free;

  inherited Destroy;

end;

constructor TXMLDocument.Create (Root: TXMLNode; _Encoding: AnsiString;
  _Version: AnsiString);
begin
  inherited Create;

  FVersion:= _Version;
  FEncoding:= _Encoding;
  FRoots:= TXMLNodeCollection.Create;
  FRoots.AddNode (Root);

end;

procedure TXMLDocument.Save (Filename: AnsiString);
var
  FileHandle: TextFile;
  i: Integer;

begin
  AssignFile (FileHandle, Filename);
  Rewrite (FileHandle);

  WriteLn (FileHandle, '<?xml version= "', FVersion, '" encoding= "', FEncoding,
    '"?>');
  for i:= 0 to FRoots.Size- 1 do
    FRoots.Node [i].SaveToFile (FileHandle);

  CloseFile (FileHandle);

end;

end.

