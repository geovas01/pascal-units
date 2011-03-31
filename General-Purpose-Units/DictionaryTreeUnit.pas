unit DictionaryTreeUnit;

interface
uses
  LinkListUnit;

const
  AlphabetLength= 32;

type
  TChar= Integer;
  TID= Integer;

  TWord= class (TObject)
  private
    FLength: Integer;
    FCharacters: array of TChar;
    function GetLength: Integer;

    procedure AddLetter (Char: TChar);
    function GetLetter(Index: Integer): TChar;
  public
    property WordLength: Integer read FLength;
    property Letter [Index: Integer]: TChar read GetLetter;

    constructor Create; overload;
    constructor Create (Word: TWord); overload;
    destructor Destroy; override;

    function FirstChar: Integer;
    function CopyWord: TWord; overload;
    function CopyWord (Start, Len: Integer): TWord; overload;

    procedure LoadFromString (S: String);
    procedure LoadFromText (S: String);
    procedure LoadFromTextFile (var InputFile: TextFile);
    procedure LoadFromFile (var InputFile: TextFile);
    procedure SaveToFile (var OutputFile: TextFile);
    procedure SaveToFileAsText (var OutputFile: TextFile);

    function IsSame (AnotherWord: TWord; IgnoreSpaces: Boolean= False): Boolean;
    function ToString (PrintCode: Boolean= False): String;
    function AddInFront (Ch: TChar): TWord;
    function AddInEnd (Ch: TChar): TWord;
    function AddAtEnd (AntoherWord: TWord): TWord;
    procedure Clear;
  end;

  TDictionaryTree= class;

  TWordLinkList= specialize TLinkList<TWord>;

  TWordCollection= class (TObject)
  private
    FWordCount: Integer;
//    FWords: array of TWord;
    WordsLinkList: TWordLinkList;

    function GetWord(Index: Integer): TWord;
  public
    property Word [Index: Integer]: TWord read GetWord;
    property WordCount: Integer read FWordCount;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile (FileName: String);
    procedure LoadFromTextFile (FileName: String);
    procedure SaveToFile (FileName: String);
    procedure SaveToFileAsTextFile (FileName: String);
    function  BuildDictionaryTree: TDictionaryTree;
    function AddWord (_Word: TWord; CheckForExistence: Boolean= False; IgnoreSpaces: Boolean= False): Boolean;
    procedure Add (AnotherWordCollection: TWordCollection);
    procedure Clear;

    function ToString (PrintCode: Boolean= False): String;
    function Purify: TWordCollection;
//    function GetRootNode: TLinkListNode;
  end;

  TDictionaryNode= class;

  TDictionaryTree= class (TObject)
  private
    MaxID, MinID: TID;
    NodesArrayisValid: Boolean;
    FNodes: array of TDictionaryNode;
    FRoot: TDictionaryNode;

    function GetNode (Index: Integer): TDictionaryNode;
    function GetRoot: TDictionaryNode;
    
    property Node [Index: Integer]: TDictionaryNode read GetNode;
  public
    property Root: TDictionaryNode read GetRoot;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromFile (FileName: String);
    procedure SaveToFile (FileName: String; Mode: Integer= 2);
    procedure AddWord (Word: TWord);
    function GetNextAvailableID: TID;
    function GenerateWordCollection: TWordCollection;

    function AddNewNode (Parent: TDictionaryNode): TDictionaryNode;
  end;

  TDictionaryNode= class (TObject)
  private
    FDictionaryTree: TDictionaryTree;
    FFather: TDictionaryNode;
    FChilds: array [0..AlphabetLength] of TDictionaryNode;
    FID: TID;
    FIsEndOfWord: Boolean;
    IndexInFather: Integer;

    function GetChild(Index: Integer): TDictionaryNode;
    function GetFather: TDictionaryNode;
    procedure SetChild(Index: Integer; const Value: TDictionaryNode);
    procedure SetID (const Value: TID);

  public
    property ID: TID read FID write SetID;
    property Father: TDictionaryNode read GetFather;
    property IsEndOfWord: Boolean read FIsEndOfWord;
    property Childs [Index: Integer]: TDictionaryNode read GetChild
         write SetChild;

    constructor Create (DicTree: TDictionaryTree; Parent: TDictionaryNode;
       SetNodeID: Boolean= True);

    procedure AddWordToDictionary (Word: TWord);//??!!
    function FindWord (Word: TWord; Rotate: Boolean= True): Boolean;//??!!
    function GetWordEndingWithThisNode: TWord;

    procedure LoadFromFile (var InputFile: TextFile; Mode: Integer= 2);
    procedure SaveToFile (var OutputFile: TextFile; Mode: Integer= 2);
    function DFS: TWordCollection;
  end;


  TSearchResultCollection= class;
  
  TSearchResult= class
  private
    FNode: TDictionaryNode;
    FFirstChoiceCount,
    FSecondChoiceCount,
    FThirdChoiceCount: Integer;
  public
    property Node: TDictionaryNode read FNode;
    property FirstChoiceCount: Integer read FFirstChoiceCount;
    property SecondChoiceCount: Integer read FSecondChoiceCount;
    property ThirdChoiceCount: Integer read FThirdChoiceCount;

    constructor Create; overload;
    constructor Create (SearchNode: TDictionaryNode); overload;

  end;

  TSearchResultCollection= class
  private
    FSearchResults: array of TSearchResult;
    FSearchResultCount: Integer;
  public
    property Size: Integer read FSearchResultCount;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure AddToCollection (SearchResult: TSearchResult); overload;
    procedure AddToCollection (SearchResultCollection: TSearchResultCollection); overload;
  end;

implementation

uses
  SysUtils, Dialogs;

{ TDictionaryNode }

procedure TDictionaryNode.AddWordToDictionary (Word: TWord);
var
  TempWord: TWord;
  Ch: TChar;
begin
  if Word.WordLength= 0 then
  begin
    FIsEndOfWord:= True;
    Exit;
  end;
  Ch:= Word.FirstChar;

  TempWord:= Word.CopyWord (1, Word.WordLength- 1);
  
  if FChilds [Ch]<> nil then
  begin
    FChilds [Ch].AddWordToDictionary (TempWord);
  end
  else
  begin
    FChilds [Ch]:= TDictionaryNode.Create (FDictionaryTree, Self, True);
    FChilds [Ch].AddWordToDictionary (TempWord);
  end;
  
  TempWord.Free;
  
end;

constructor TDictionaryNode.Create (DicTree: TDictionaryTree; Parent: TDictionaryNode
              ;SetNodeID: Boolean);
var
  i: Integer;
begin
  inherited Create;

  FFather:= nil;
  FIsEndOfWord:= False;
  for i:= 0 to AlphabetLength do
    FChilds [i]:= nil;
  FDictionaryTree:= DicTree;
  
  if SetNodeID then
    FID:= FDictionaryTree.GetNextAvailableID
  else
    FID:= -1;
end;

function TDictionaryNode.DFS: TWordCollection;
var
  TempWordCollection: TWordCollection;
  i, j: Integer;
begin
  Result:= TWordCollection.Create;

  for i:= 0 to High (FChilds) do
    if FChilds [i]<> nil then
    begin
      TempWordCollection:= FChilds [i].DFS;
      
      for j:= 0 to TempWordCollection.FWordCount- 1 do
        Result.AddWord (TempWordCollection.Word [j].CopyWord.AddInFront (i));
      TempWordCollection.Free;
    end;
      
  if FIsEndOfWord then
    Result.AddWord (TWord.Create);
end;

function TDictionaryNode.FindWord (Word: TWord; Rotate: Boolean): Boolean;
var
  FirstChar: TChar;
  TempWord: TWord;
  WordLen: Integer;
begin
  WordLen:= Word.WordLength;
  if WordLen= 0 then
  begin
    Result:= FIsEndOfWord;
    Exit;
  end;

  Result:= False;
  FirstChar:= Word.FirstChar;
  TempWord:= Word.CopyWord (1, WordLen- 1);
  if FChilds [FirstChar]<> nil then
    Result:= FChilds [FirstChar].FindWord (TempWord)
  else if Rotate then
  begin
    if FIsEndOfWord then
      Result:= FDictionaryTree.Root.FindWord (TempWord.AddInFront (FirstChar))
    else
      Result:= False;
  end;
    
  TempWord.Free;
end;

function TDictionaryNode.GetChild (Index: Integer): TDictionaryNode;
begin
  if (Index< 0) or (Index> AlphabetLength) then
    raise Exception.Create ('Range Check Error!');

  Result:= FChilds [Index];
end;

function TDictionaryNode.GetFather: TDictionaryNode;
begin
  if FFather= nil then
    raise Exception.Create ('Father is not initialize');
    
  Result:= FFather;
end;

function TDictionaryNode.GetWordEndingWithThisNode: TWord;
begin
  if FFather<> nil then
    Result:= FFather.GetWordEndingWithThisNode
  else
  begin
    Result:= TWord.Create;
    Exit;
  end;
  Result.AddInEnd (IndexInFather);
end;

procedure TDictionaryNode.LoadFromFile (var InputFile: TextFile; Mode: Integer);
var
  ChildID: Integer;
  Ch: Char;
  S, TempString: String;
  i: Integer;
begin
  Readln (InputFile, Ch, FID, S);
  if (S= '') or (Ch<> '(') then
    raise Exception.Create ('Invalid Input File');
    
  if S [1]= ' ' then
    Delete (S, 1, 1);
  
  if UpperCase (S)= 'TRUE)' then
    FIsEndOfWord:= True
  else
    FIsEndOfWord:= False;

  case Mode of
  1:
    begin    
      for i:= 0 to AlphabetLength do
      begin
        Read (InputFile, ChildID);
        if ChildID<> - 1 then
          FChilds [i]:= FDictionaryTree.Node [ChildID];
      end;
    end;
  2:
    begin
      ReadLn (InputFile, S);
      S:= TrimLeft (TrimRight (S));
      
      while S<> '' do
      begin
        if S [1]<> '[' then
          raise Exception.Create ('Invalid File');

        TempString:= Copy (S, 1, Pos (']', S)- 1);
        Delete (S, 1, Pos (']', S));
        S:= TrimLeft (TrimRight (S));

        ChildID:= StrToInt (Copy (TempString, Pos (',', TempString)+ 1,
                      Pos (']', TempString)- 1));
        Delete (TempString, Pos (',', TempString),
                      Pos (']', TempString));
        i:= StrToInt (TempString);
        FChilds [i]:= FDictionaryTree.Node [ChildID]; 
      end;
      
    end;
  end;
  
  ReadLn (InputFile);
end;

procedure TDictionaryNode.SaveToFile (var OutputFile: TextFile; Mode: Integer);
var
  i: Integer;
begin
  for i:= 0 to AlphabetLength do
    if FChilds [i]<> nil then
      FChilds [i].SaveToFile (OutputFile, Mode);
      
  case Mode of
  1:
    begin

      if FIsEndOfWord then
        Writeln (OutputFile, '(', ID, ' True)')
      else
        Writeln (OutputFile, '(', ID,  ' False)');

      for i:= 0 to AlphabetLength do
        if FChilds [i]= nil then
          Write (OutputFile, -1, ' ')
        else
          Write (OutputFile, FChilds [i].ID, ' ');
    end;
  2:
    begin

      if FIsEndOfWord then
        Writeln (OutputFile, '(', ID, ' True)')
      else
        Writeln (OutputFile, '(', ID,  ' False)');

      for i:= 0 to AlphabetLength do
        if FChilds [i]<> nil then
          Write (OutputFile, '[', i, ', ', FChilds [i].ID, ']');
    end;
  end;
 
  WriteLn (OutputFile);
end;

procedure TDictionaryNode.SetChild (Index: Integer;
  const Value: TDictionaryNode);
begin
  if (Index< 0) or (Index> AlphabetLength) then
    raise Exception.Create ('Range Check Error!');
    
  FChilds [Index]:= Value;
end;

procedure TDictionaryNode.SetID (const Value: TID);
begin
  if FID<> -1 then
    raise Exception.Create ('This Node has already an ID!');
    
  FID:= Value;
end;

{ TWord }

function TWord.CopyWord: TWord;
begin
  Result:= TWord.Create (Self);
end;

procedure TWord.AddLetter (Char: TChar);
begin
  SetLength (FCharacters, GetLength+ 1);
  FCharacters [GetLength- 1]:= Char;
  FLength:= Length (FCharacters); 
end;

function TWord.CopyWord (Start, Len: Integer): TWord;
var
  EndIndex,
  i: Integer;
begin
  Result:= TWord.Create;
  if Start+ Len< FLength then
    EndIndex:= Start+ Len- 1
  else
    EndIndex:= FLength- 1;
  
  for i:= Start to EndIndex do
    Result.AddLetter (Self.Letter [i]);
end;

constructor TWord.Create;
begin
  inherited;
  SetLength (FCharacters, 0);
  FLength:= 0;
end;

function TWord.FirstChar: Integer;
begin
  Result:= FCharacters [0];
end;

destructor TWord.Destroy;
begin
  SetLength (FCharacters, 0);
  
  inherited;
end;

function TWord.GetLength: Integer;
begin
  Result:= Length (FCharacters);
end;

procedure TWord.LoadFromFile (var InputFile: TextFile);
var
  S: String;
begin
  ReadLn (InputFile, S);
  Delete (S, Pos (')', S), Length (S));
  Self.LoadFromString (S);
end;

procedure TWord.SaveToFile (var OutputFile: TextFile);
var
  i: Integer;
begin
  Write (OutputFile, '(');
  for i:= 0 to WordLength- 1 do
    Write (OutputFile, FCharacters [i], ' ');
  WriteLn (OutputFile, ')');
end;

constructor TWord.Create (Word: TWord);
var
  i: Integer;
begin
  inherited Create;
  for i:= 0 to Word.WordLength- 1 do
    Self.AddLetter (Word.Letter [i]);
end;

function TWord.GetLetter (Index: Integer): TChar;
begin
  if (Index< 0) or (WordLength<= Index) then
    raise Exception.Create ('Range Chack Error!');
  Result:= FCharacters [Index];
end;

procedure TWord.LoadFromString (S: String);
var
  TempStr: String;
  Ch: TChar;
begin
  if S= '' then
    raise Exception.Create ('Invalid Word');
    
  while S [1]= ' ' do
  begin
    Delete (S, 1, 1);
    if S= '' then
      Break;
  end;
  if S= '' then
    raise Exception.Create ('Invalid Word');
    
  FLength:= 0;
  S:= S+ ' ';
  
  while S<> '' do
  begin
    TempStr:= Copy (S, 1, Pos (' ', S)- 1);
    Delete (S, 1, Pos (' ', S)); 

    if S= '' then
      Break;
    while S [1]= ' ' do
    begin
      Delete (S, 1, 1);
      if S= '' then
        Break;
    end;
    if S= '' then
      Break;
      
    Ch:= StrToInt (TempStr);
    Self.AddLetter (Ch);
  end;
  if TempStr<> '' then
  begin
    Ch:= StrToInt (TempStr);
    Self.AddLetter (Ch);
  end;
end;

procedure TWord.Clear;
begin
  SetLength (FCharacters, 0);
  FLength:= 0;
end;

function TWord.AddInFront (Ch: TChar): TWord;
var
  i: Integer;
begin
  Inc (FLength);
  SetLength (FCharacters, FLength);
  for i:= FLength- 1 downto 1 do
    FCharacters [i]:= FCharacters [i- 1];
  FCharacters [0]:= Ch;

  Result:= Self;
end;

function TWord.ToString (PrintCode: Boolean): String;
const
  PersianAlphabet: array [0..31] of String= ('ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی');
var
  i: Integer;
begin
  Result:= '';

  for i:= 0 to FLength- 1 do
  begin
    if FCharacters [i]<> AlphabetLength then
    begin
      if PrintCode then
        Result:= Result+ IntToStr (FCharacters [i])+ ' '
      else
        Result:= Result+ PersianAlphabet [FCharacters [i]];
    end
    else if FCharacters [i]= 32 then
      Result:= Result+ ' '
    else
      Result:= Result+ '- ';
  end;
end;

procedure TWord.LoadFromTextFile (var InputFile: TextFile);
const
  PersianAlphabet: array [0..35] of String= ('آ', 'ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'ك', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی', 'ي',
      'ئ');
  PersianIndex: array [0..35] of Integer=   (0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 31, 31);
var
  Found: Boolean;
  i: Integer;
  PerChar: String;
  ReadData,
  TranslatedData: String;
begin
  Read (InputFile, ReadData);
  ReadData:= ReadData+ ' ';
  TranslatedData:= '';

  while ReadData<> '' do
  begin
    while ReadData [1]= ' ' do
    begin
      Delete (ReadData, 1, 1);
      if ReadData= '' then
        Break;
    end;
    if ReadData= '' then
      Break;

    PerChar:= ReadData [1];
    Delete (ReadData, 1, 1);

    Found:= False;
    for i:= 0 to High (PersianAlphabet) do
      if PerChar= PersianAlphabet [i] then
      begin
        Found:= True;
        TranslatedData:= TranslatedData+ ' '+ IntToStr (PersianIndex [i]);
        Break;
      end;
      if not Found then
      begin
        raise Exception.Create ('Invalid Word');
        Exit;
      end;
    end;

  Self.LoadFromString (TranslatedData);
end;

procedure TWord.SaveToFileAsText(var OutputFile: TextFile);
const
  PersianAlphabet: array [0..35] of String= ('ا','آ', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'ك', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی', 'ي',
      'ئ');
  PersianIndex: array [0..35] of Integer=   (0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 31, 31);
var
  i, j: Integer;
begin
  for i:= 0 to WordLength- 1 do
  begin
    for j:= 0 to High (PersianAlphabet) do
      if PersianIndex [j]= FCharacters [i] then
      begin
        Write (OutputFile, PersianAlphabet [j]);
        Break;
      end;
  end;
  WriteLn (OutputFile);
end;

function TWord.IsSame (AnotherWord: TWord; IgnoreSpaces: Boolean= False): Boolean;
var
  i,
  Word1Index, Word2Index: Integer;
begin
  Result:= False;

  if not IgnoreSpaces then
  begin
    if AnotherWord.FLength<> Self.FLength then
      Exit;

    for i:= 0 to FLength- 1 do
      if FCharacters [i]<> AnotherWord.FCharacters [i] then
        Exit;

  end
  else
  begin
    Word1Index:= 0;
    Word2Index:= 0;

    while Self.FCharacters [Word1Index]= 32 do
    begin
      Inc (Word1Index);
      if Word1Index= Self.FLength then
        Break;
    end;

    while AnotherWord.FCharacters [Word2Index]= 32 do
    begin
      Inc (Word2Index);
      if Word2Index= AnotherWord.FLength then
        Break;
    end;
    
    while (Word1Index< Self.FLength) and (Word2Index< AnotherWord.FLength) do
    begin
      if Self.FCharacters [Word1Index]<> AnotherWord.FCharacters [Word2Index] then
        Exit;
        
      Inc (Word2Index);
      Inc (Word1Index);

      if Word1Index< Self.FLength then
        while Self.FCharacters [Word1Index]= 32 do
        begin
          Inc (Word1Index);
          if Word1Index= Self.FLength then
            Break;
        end;

      if Word2Index< AnotherWord.FLength then
        while AnotherWord.FCharacters [Word2Index]= 32 do
        begin
          Inc (Word2Index);
          if Word2Index= AnotherWord.FLength then
            Break;
        end;

    end;


  end;
  Result:= True;
end;

function TWord.AddInEnd (Ch: TChar): TWord;
begin
  Inc (FLength);
  SetLength (FCharacters, FLength);
  FCharacters [FLength- 1]:= Ch;

  Result:= Self;
end;

function TWord.AddAtEnd (AntoherWord: TWord): TWord;
var
  i: Integer;
begin
  for i:= 0 to AntoherWord.FLength- 1 do
    Self.AddInEnd (AntoherWord.FCharacters [i]);
  Result:= Self; 
end;

procedure TWord.LoadFromText (S: String);
const
  PersianAlphabet: array [0..37] of String= ('آ', 'ا', 'ب', 'پ', 'ت', 'ث', 'ج', 'چ', 'ح', 'خ', 'د',
      'ذ', 'ر', 'ز', 'ژ', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ک', 'ك', 'گ', 'ل', 'م', 'ن', 'و', 'ه', 'ی', 'ي',
      'ئ', 'ؤ', 'أ');
  PersianIndex: array [0..37] of Integer=   (0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 31, 31,
        29, 0);
const
  IgnoredCharacters: array [0..7] of String= ('ء', '(', ')', '-', '_', '.', 'ً', #9);
var
  IgnoredChar,
  Found: Boolean;
  i: Integer;
  PerChar: String;
  ReadData,
  TranslatedData: String;
begin
  ReadData:= S;
//  WriteLn (S);
  TranslatedData:= '';

  while ReadData<> '' do
  begin
    IgnoredChar:= False;
    
    while ReadData [1] in [' ', '0'..'9'] do
    begin
      Delete (ReadData, 1, 1);
      if ReadData= '' then
        Break;
    end;
    if ReadData= '' then
      Break;

    PerChar:= ReadData [1];
    Delete (ReadData, 1, 1);

    for i:= 0 to High (IgnoredCharacters) do
      if PerChar= IgnoredCharacters [i] then
      begin
        IgnoredChar:= True;
        Break;
      end;
        
    if not IgnoredChar then
    begin
      Found:= False;
      
      for i:= 0 to High (PersianAlphabet) do
        if PerChar= PersianAlphabet [i] then
        begin
          Found:= True;
          TranslatedData:= TranslatedData+ ' '+ IntToStr (PersianIndex [i]);
          Break;
        end;
      
        if not Found then
        begin
          PerChar:= PerChar+ ReadData [1];
          Delete (ReadData, 1, 1);

          for i:= 0 to High (IgnoredCharacters) do
            if PerChar= IgnoredCharacters [i] then
            begin
              IgnoredChar:= True;
              Break;
            end;

          if not IgnoredChar then
            for i:= 0 to High (PersianAlphabet) do
              if PerChar= PersianAlphabet [i] then
              begin
                Found:= True;
                TranslatedData:= TranslatedData+ ' '+ IntToStr (PersianIndex [i]);
                Break;
              end;
        end;   
      
        if (not Found) and (not IgnoredChar) then
        begin
          WriteLn ('Not Found and Not Ignored!');
          WriteLn (TranslatedData+ PerChar);
          ReadLn;
          raise Exception.Create ('Invalid Word');
          Exit;
        end;
        
      end;
      
    end;

  if TranslatedData= '' then
    raise Exception.Create ('Invalid Word');
  Self.LoadFromString (TranslatedData);
end;

{ TDictionaryTree }

function TDictionaryTree.AddNewNode (Parent: TDictionaryNode): TDictionaryNode;
begin
  Inc (MaxID);
  SetLength (FNodes, MaxID+ 1);
  FNodes [MaxID]:= TDictionaryNode.Create (Self, Parent, False);
  FNodes [MaxID].ID:= MaxID;
  
  Result:= FNodes [MaxID];
end;

procedure TDictionaryTree.AddWord (Word: TWord);
begin
  Root.AddWordToDictionary (Word);
end;
procedure TDictionaryTree.Clear;
var
  i: Integer;
begin
  for i:= 0 to High (FNodes) do
    if FNodes [i]<> nil then
      FNodes [i].Free;
      
  SetLength (FNodes, 0);
  
  FRoot:= nil;
end;

constructor TDictionaryTree.Create;
begin
  inherited;
  MinID:= 1;
  MaxID:= 1;
  
  SetLength (FNodes, 2);
  FNodes [1]:= TDictionaryNode.Create (Self, nil, False);
  FNodes [1].ID:= 1;
  FRoot:= FNodes [1];
end;

destructor TDictionaryTree.Destroy;
var
  i: Integer;
begin
  FRoot:= nil;
  for i:= 1 to High (FNodes) do
    if FNodes [i]<> nil then
      FNodes [i].Free;

  SetLength (FNodes, 0);

  inherited;
end;

function TDictionaryTree.GenerateWordCollection: TWordCollection;
begin
  Result:= Root.DFS; 
end;

function TDictionaryTree.GetNextAvailableID: TID;
begin
  Inc (MaxID);
  Result:= MaxID;
end;

function TDictionaryTree.GetNode (Index: Integer): TDictionaryNode;
var
  i: Integer;
begin
  if (Index< MinID) or (MaxID< Index) then
    raise Exception.Create ('Invalid ID!');
    
  Result:= FNodes [Index];
{
  for i:= 0 to High (FNodes) do
    if FNodes [i].ID= Index then
    begin
      Result:= FNodes [i];
      Exit;
    end;
}
  if (Result= nil) then
    raise Exception.Create ('Invalid ID!');
end;

function TDictionaryTree.GetRoot: TDictionaryNode;
begin
  if FRoot<> nil then
    Result:= FRoot
  else
    raise Exception.Create ('No Root!');
end;

procedure TDictionaryTree.LoadFromFile (FileName: String);
var
  InputFile: TextFile;
  S: String;
  Mode: Integer;
  TempNode: TDictionaryNode;
  NodeCount,
  NewNodeID,
  i, j: Integer;

  function LoadHeader: Integer;
  begin
    ReadLN (InputFile, S);
    ReadLn (InputFile ,S);
    S:= Copy (S, Pos ('Mode:', S)+ 5, Length (S));
    if S<> '' then
      Mode:= StrToInt (S)
    else
      Mode:= 1;
    
    ReadLn (InputFile ,S);
    S:= Copy (S, Pos ('Nodes:', S)+ 7, Length (S));
    if S<> '' then
      Result:= StrToInt (S)
    else
      Result:= 1;
  end;

var
  NewDictionaryNode: TDictionaryNode;
begin
  i:= 0;
  if not FileExists (FileName) then
    raise Exception.Create ('File '+ FileName+ ' Not Found!');

  Clear;  
  AssignFile (InputFile, FileName);
  Reset (InputFile);

  NodeCount:= LoadHeader;
  SetLength (FNodes, NodeCount+ 1);
  for i:= 0 to NodeCount do
    FNodes [i]:= nil;

  MinID:= 1;
  MaxID:= NodeCount;

  while not Eof (InputFile) do
  begin
    Inc (i);
    NewDictionaryNode:= TDictionaryNode.Create (Self, nil, false);
    NewDictionaryNode.LoadFromFile (InputFile, Mode);

    if FNodes [NewDictionaryNode.ID]<> nil then
      raise Exception.Create ('Error!');

    FNodes [NewDictionaryNode.ID]:= NewDictionaryNode;
    TempNode:= NewDictionaryNode;

    for j:= 0 to AlphabetLength do
      if TempNode.FChilds [j]<> nil then
      begin
        GetNode (TempNode.FChilds [j].FID).FFather:= TempNode;
        GetNode (TempNode.FChilds [j].FID).IndexInFather:= j;
      end;
  end;
  SetLength (FNodes, i);
  if i>= 1 then
  begin
    if FRoot<> nil then
      FRoot.Free;
    FRoot:= FNodes [1];
  end;

  CloseFile (InputFile);
  
end;

procedure TDictionaryTree.SaveToFile (FileName: String; Mode: Integer);
var
  OutputFile: TextFile;

  procedure SaveHeader (Mode: Integer);
  begin
    WriteLn (OutputFile, 'This file is the information for a Dictionary Tree, developed in SRRF');
    WriteLn (OutputFile, 'Dictionary Tree Save Mode: ', Mode);
    WriteLn (OutputFile, 'Number of Nodes: ', MaxID); 
  end;
  
begin
  AssignFile (OutputFile, FileName);
  Rewrite (OutputFile);

  SaveHeader (Mode);
  
  Root.SaveToFile (OutputFile, Mode);
  CloseFile (OutputFile);
end;

{ TWordCollection }

procedure TWordCollection.Add (AnotherWordCollection: TWordCollection);
var
  ActiveNode: TLinkListNode;
  i: Integer;

begin
  ActiveNode:= AnotherWordCollection.WordsLinkList.Root;
  for i:= 0 to AnotherWordCollection.WordCount- 2 do
  begin
    Self.AddWord (TWord (ActiveNode.Data).CopyWord);
    ActiveNode:= ActiveNode.Next;
  end;
  Self.AddWord (TWord (ActiveNode.Data).CopyWord);
end;

function TWordCollection.AddWord (Word: TWord; CheckForExistence: Boolean; IgnoreSpaces: Boolean): Boolean;
var
  ActiveNode: TLinkListNode;
  i: Integer;
begin
  Result:= False;
  
  if (0< FWordCount) and CheckForExistence then
  begin
    ActiveNode:= WordsLinkList.Root;

    for i:= 0 to FWordCount- 2 do
    begin
      if Word.IsSame (TWord (ActiveNode.Data), IgnoreSpaces) then
        Exit;
      ActiveNode:= ActiveNode.Next;
    end;

    if Word.IsSame (TWord (ActiveNode.Data), IgnoreSpaces) then
      Exit;
  end;

  Result:= True;
  WordsLinkList.AddData (Word);
  Inc (FWordCount);
end;

function TWordCollection.BuildDictionaryTree: TDictionaryTree;
var
  i: Integer;
  CurNode: TLinkListNode;
begin
  Result:= TDictionaryTree.Create;

  CurNode:= WordsLinkList.Root;

  for i:= 0 to FWordCount- 2 do
  begin
    Result.AddWord (TWord (CurNode.Data));
    CurNode:= CurNode.Next;
  end;
  Result.AddWord (TWord (CurNode.Data));

end;

procedure TWordCollection.Clear;
begin
  WordsLinkList.Free;

  FWordCount:= 0;
end;

constructor TWordCollection.Create;
begin
  inherited;
  
  FWordCount:= 0;
  WordsLinkList:= TLinkList.Create;
end;

destructor TWordCollection.Destroy;
begin
  FWordCount:= 0;

  WordsLinkList.Free;
  inherited;
end;

function TWordCollection.GetRootNode: TLinkListNode;
begin
  Result:= WordsLinkList.Root;
end;

function TWordCollection.GetWord (Index: Integer): TWord;
begin
  if (Index< 0) or (FWordCount<= Index) then
    raise Exception.Create ('Range Check Error!');
    
  Result:= TWord (WordsLinkList.GetDataByIndex (Index));
end;

procedure TWordCollection.LoadFromFile (FileName: String);
var
  TempWord: TWord;
  Ch: Char;
  InputFile: TextFile;
begin
  if not FileExists (FileName) then
    raise Exception.Create ('File Not Found!');
    
  AssignFile (InputFile, FileName);
  Reset (InputFile);
  FWordCount:= 0;

  while not Eof (InputFile) do
  begin
    TempWord:= TWord.Create;
    Read (InputFile, Ch);
    
    if Ch<> '(' then
      raise Exception.Create ('Invalid File!');
    TempWord.LoadFromFile (InputFile);

    AddWord (TempWord);    
  end;
  CloseFile (InputFile);
end;

procedure TWordCollection.LoadFromTextFile (FileName: String);
var
  TempWord: TWord;
  InputFile: TextFile;
  S, TempString, SWithoutSpace: String;
begin
  if not FileExists (FileName) then
    raise Exception.Create ('File '+ FileName+ ' Not Found!');
    
  AssignFile (InputFile, FileName);

  Reset (InputFile);
  TempString:= '';
  
  while not Eof (InputFile) do
  begin
    while TempString= '' do
      ReadLn (InputFile, TempString);

    TempString:= TrimLeft (TrimRight (TempString));
    if TempString [Length (TempString)]<> #10 then
      TempString:= TempString+ #10;

    while TempString<> '' do
    begin
      S:= Copy (TempString, 1, Pos (#10, TempString)- 1);
      Delete (TempString, 1, Pos (#10, TempString));

      while S<> '' do
      begin
        TempWord:= TWord.Create;
//        WriteLn (FWordCount);
        
        try
        {??!!}
          while S [1] in [#48..#57] do
          begin
            Delete (S, 1, 1);
            if S= '' then
              Break;
          end;
          {??!!}
        
          S:= TrimRight (TrimLeft (S));
          if S= '' then
          begin
            TempWord.Free;
            Dec (FWordCount);
            Continue;
          end;

          if Pos (' ', S)<> 0 then
          begin
            SWithoutSpace:= Copy (S, 1, Pos (' ', S)- 1);
            Delete (S, 1, Pos (' ', S));
          end
          else
          begin
            SWithoutSpace:= S;
            S:= '';
          end;
        
          TempWord.LoadFromText (SWithoutSpace);
          AddWord (TempWord);
        except
          on E: Exception do
            if E.Message= 'Invalid Word' then
            begin
              TempWord.Free;
              Dec (FWordCount);
            end
            else
              raise Exception.Create ('An Unhadled Error!');
          end;
        end;
      end;
    end;
  
{
  while not Eof (InputFile) do
  begin
    Inc (FWordCount);
    SetLength (FWords, FWordCount);
    FWords [FWordCount- 1]:= TWord.Create;
    WriteLn (FWordCount);
    try
      FWords [FWordCount- 1].LoadFromTextFile (InputFile);
    except
      on E: Exception do
        if E.Message= 'Invalid Word' then
        begin
          FWords [FWordCount- 1].Free;
          Dec (FWordCount);
        end;

    end;
  end;
  }
  CloseFile (InputFile);
end;

function TWordCollection.Purify: TWordCollection;
  function MaybeCorrect (Word: TWord): Boolean;
  var
    i: Integer;
  begin
    if Word.FLength< 3 then
      Result:= False
    else
    begin
      for i:= 0 to Word.FLength- 1 do
        if Word.FCharacters [i]= Word.FCharacters [i+ 1] then
        begin
          Result:= False;
          Exit;
        end;
    end;
    
    Result:= True;
  end;

var
  NewWordCollection: TWordCollection;
  Ch: Char;
  i: Integer;
  ActiveNode: TLinkListNode;
  NewWord: TWord;

begin
  NewWordCollection:= TWordCollection.Create;

  ActiveNode:= WordsLinkList.Root;

  for i:= 0 to FWordCount- 2 do
  begin
    NewWord:= TWord (ActiveNode.Data);

    if MaybeCorrect (NewWord) then
      NewWordCollection.AddWord (NewWord);
    ActiveNode:= ActiveNode.Next;
  end;

  NewWord:= TWord (ActiveNode.Data);
  if MaybeCorrect (NewWord) then
    NewWordCollection.AddWord (NewWord);
    
  Result:= NewWordCollection;
end;

procedure TWordCollection.SaveToFile (FileName: String);
var
  CurWord: TWord;
  ActiveNode: TLinkListNode;
  i: Integer;
  OutputFile: TextFile;
begin
  AssignFile (OutputFile, FileName);
  ReWrite (OutputFile);
  
  ActiveNode:= WordsLinkList.Root; 
  for i:= 0 to FWordCount- 2 do
  begin
    CurWord:= TWord (ActiveNode.Data);
    CurWord.SaveToFile (OutputFile);
    ActiveNode:= ActiveNode.Next;
  end;
  if ActiveNode.Data<> nil then
  begin
    CurWord:= TWord (ActiveNode.Data);
    CurWord.SaveToFile (OutputFile);
  end;
  
  CloseFile (OutputFile);
end;

procedure TWordCollection.SaveToFileAsTextFile (FileName: String);
var
  ActiveNode: TLinkListNode;
  TempWord: TWord;
  i: Integer;
  OutputFile: TextFile;
begin
  AssignFile (OutputFile, FileName);
  ReWrite (OutputFile);

  ActiveNode:= WordsLinkList.Root;
  
  for i:= 0 to FWordCount- 2 do
  begin
    TempWord:= TWord (ActiveNode.Data);
    TempWord.SaveToFileAsText (OutputFile);
    ActiveNode:= ActiveNode.Next;
  end;
  TempWord:= TWord (ActiveNode.Data);
  TempWord.SaveToFileAsText (OutputFile);

  CloseFile (OutputFile);
end;

function TWordCollection.ToString (PrintCode: Boolean= False): String;
var
  ActiveNode: TLinkListNode;
  TempWord: TWord;
  i: Integer;
begin
  Result:= '';
  ActiveNode:= WordsLinkList.Root;

  for i:= 0 to FWordCount- 2 do
  begin
    TempWord:= TWord (ActiveNode.Data);
    Result:= '('+ TempWord.ToString (PrintCode)+ ')';

    ActiveNode:= ActiveNode.Next;
  end;
  if FWordCount<> 0 then
  begin
    TempWord:= TWord (ActiveNode.Data);
    Result:= Result+ '('+ TempWord.ToString (PrintCode)+ ')';
  end;  
end;

{ TSearchResult }

constructor TSearchResult.Create;
begin
  inherited;

  FNode:= nil;
  FFirstChoiceCount:= 0;
  FSecondChoiceCount:= 0;
  FThirdChoiceCount:= 0;
end;

constructor TSearchResult.Create (SearchNode: TDictionaryNode);
begin
  inherited Create;

  FNode:= SearchNode;
  FFirstChoiceCount:= 0;
  FSecondChoiceCount:= 0;
  FThirdChoiceCount:= 0;
end;

{ TSearchResultCollection }

procedure TSearchResultCollection.AddToCollection(
  SearchResult: TSearchResult);
begin
  Inc (FSearchResultCount);
  SetLength (FSearchResults, FSearchResultCount);
  FSearchResults [FSearchResultCount- 1]:= SearchResult;
end;

procedure TSearchResultCollection.AddToCollection(
  SearchResultCollection: TSearchResultCollection);
var
  i: Integer;
begin
  for i:= 0 to SearchResultCollection.Size- 1 do
    Self.AddToCollection (SearchResultCollection.FSearchResults [i]);
end;

procedure TSearchResultCollection.Clear;
var
  i: Integer;
begin
  for i:= 0 to FSearchResultCount- 1 do
    FSearchResults [i].Free;
  SetLength (FSearchResults, 0);
  FSearchResultCount:= 0;
end;

constructor TSearchResultCollection.Create;
begin
  inherited;

  FSearchResultCount:= 0;
end;

destructor TSearchResultCollection.Destroy;
var
  i: Integer;
  
begin
  for i:=0 to FSearchResultCount- 1 do
    FSearchResults [i].Free;
     
  inherited;
end;

end.
