unit PostProcessorUnit;

interface
uses
  DictionaryTreeUnit, Dialogs, MyTypes;

const
  AverageCompLen: Integer= 5;

type

  TPostProcessorWord= array of TIntegerArray;

  TSearchResultCollection= class;

  TSearchResult= class (TObject)
  private
    FNode: array of TDictionaryNode;
    FComponentUsedTilNow: Integer;
    FChoices: String;
    FCost: Integer;
    FActiveNodeIndex: Integer;

    function GetNode (Index: Integer): TDictionaryNode;
    function GetActiveNode: TDictionaryNode;
  public
    property ComponentUsedTilNow: Integer read FComponentUsedTilNow;
    property Node [Index: Integer]: TDictionaryNode read GetNode;
    property ActiveNode: TDictionaryNode read GetActiveNode; 
    property Choices: String read FChoices;
    property Cost: Integer read FCost;

    constructor Create; overload;
    constructor Create (Node: TDictionaryNode; Path: String; CompTilNow: Integer; NodeCost: Integer= 0); overload;
    constructor Create (Parent: TSearchResult; Node: TDictionaryNode; Path: String; CompTilNow: Integer;
                     NodeCost: Integer= 0; AddNewFNode: Boolean= False); overload;
    destructor Destroy; override;

    function ContinueSearch (Choices: array of Integer; Root: TDictionaryNode;
              MaxChoice: Integer= 3): TSearchResultCollection;

    function ConvertToWord: TWord;
  end;

  TSearchResultCollection= class (TObject)
  private
    FResults: array of TSearchResult;
    FSize: Integer;

    function GetResults(Index: Integer): TSearchResult;
    procedure SetSize (n: Integer= 0);
  public
    property Results [Index: Integer]: TSearchResult read GetResults;
    property Size: Integer read FSize;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure AddResult (Result: TSearchResult);
    procedure AddResultCollection (ResultCollection: TSearchResultCollection);
    procedure Search (Root: TDictionaryNode; Word: TPostProcessorWord);
    function  GetAsWordCollection (ReturnAllWord: Boolean= False): TWordCollection;
    procedure RemoveResult (Index: Integer);
    procedure DelResultsHavingMoreComponentThan (n: Integer);
  end;

  TPostProcessorEngine = class (TObject)
  private
    DicCount: Integer;
    FDictionaryTree: array of TDictionaryTree;
    FDicNames: array of String;
    DictionariesDirectory: String;

    AllExistingDistionaryNickNames: array of String;
    AllExistingDistionaryFileName: array of String;
    function GetNickNameForDictionary(Index: Integer): String;
    function GetLoadedDictionaryCount: Integer;
  public
    property LoadedDictionaryCount: Integer read GetLoadedDictionaryCount;
    property NickNameForLoadedDictionary [Index: Integer]: String read GetNickNameForDictionary;
    
    constructor Create;
    destructor Destroy; override;

    procedure AddDictionary (DicName: String; Dictionary: TDictionaryTree);
    function GetDicionary (Name: String): TDictionaryTree;
    function CheckWord (DictionaryName: String; PostWord: TPostProcessorWord;
      ReturnJustOneWord: Boolean= True): TWordCollection;

    function LoadDictionary (DicName: String): Boolean;

  end;

implementation
uses
  SysUtils, Math;
{ TPostProcessor }

procedure TPostProcessorEngine.AddDictionary (DicName: String;
  Dictionary: TDictionaryTree);
begin
  SetLength (FDictionaryTree, DicCount+ 1);
  SetLength (FDicNames, DicCount+ 1);

  FDictionaryTree [DicCount]:= Dictionary;
  FDicNames [DicCount]:= UpperCase (DicName);
  
  Inc (DicCount);
  
end;

function TPostProcessorEngine.CheckWord (DictionaryName: String; PostWord: TPostProcessorWord;
  ReturnJustOneWord: Boolean): TWordCollection;
var
  i, j, Len: Integer;
  RealPostWord: TPostProcessorWord;
  SearchResultCollection: TSearchResultCollection;
  Dic: TDictionaryTree;
begin
  Dic:= GetDicionary (DictionaryName);

  Len:= Length (PostWord);
  for i:= 0 to High (PostWord) do
    if PostWord [i][0]= -1 then
    begin
      Len:= i;
      Break;
      
    end;
    
  SetLength (RealPostWord, Len);
  for i:= 0 to Len- 1 do
  begin
    SetLength (RealPostWord [i], Length (PostWord [i]));
    for j:= 0 to High (PostWord [i]) do
      RealPostWord [i][j]:= PostWord [i][j];
      
  end;

  SearchResultCollection:= TSearchResultCollection.Create;

  try
    SearchResultCollection.Search (Dic.Root, RealPostWord);

  except
//    ShowMessage ('Search');
    Result:= TWordCollection.Create;

    for i:= 0 to Len- 1 do
      SetLength (RealPostWord [i], 0);
    SetLength (RealPostWord, 0);
    Exit;
    
  end;

  try
    Result:= SearchResultCollection.GetAsWordCollection (not ReturnJustOneWord);

  except
//    ShowMessage ('GetAsWordCollection');
    Result:= TWordCollection.Create;

    for i:= 0 to Len- 1 do
      SetLength (RealPostWord [i], 0);
    SetLength (RealPostWord, 0);

    Exit;

  end;

  SearchResultCollection.Free;

  for i:= 0 to Len- 1 do
    SetLength (RealPostWord [i], 0);

  SetLength (RealPostWord, 0);
  
end;

constructor TPostProcessorEngine.Create;
var
  InputFile: TextFile;
begin
  inherited;

  SetLength (FDictionaryTree, 0);
  SetLength (FDicNames, 0);
  DicCount:= 0;

  SetLength (AllExistingDistionaryNickNames, 0);
  SetLength (AllExistingDistionaryFileName, 0);

  if FileExists ('Config.txt') then
  begin
    AssignFile (InputFile, 'Config.txt');
    Reset (InputFile);

    Readln (InputFile, DictionariesDirectory);
    if DictionariesDirectory [Length (DictionariesDirectory)]<> '\' then
      DictionariesDirectory:= DictionariesDirectory+ '\';

    while not Eof (InputFile) do
    begin
      SetLength (AllExistingDistionaryNickNames,
        Length (AllExistingDistionaryNickNames)+ 1);
      SetLength (AllExistingDistionaryFileName,
        Length (AllExistingDistionaryFileName)+ 1);

      ReadLn (InputFile,
         AllExistingDistionaryNickNames [High (AllExistingDistionaryNickNames)]);
      ReadLn (InputFile,
         AllExistingDistionaryFileName [High (AllExistingDistionaryFileName)]);

      AllExistingDistionaryNickNames [High (AllExistingDistionaryNickNames)]:=
             UpperCase (AllExistingDistionaryNickNames [High (AllExistingDistionaryNickNames)]);
    end;
    CloseFile (InputFile);

  end
  else
    ShowMessage ('Config File not Found!');

end;

destructor TPostProcessorEngine.Destroy;
var
  i: Integer;
  
begin
  for i:= 0 to High (FDictionaryTree) do
    FDictionaryTree [i].Free;
  SetLength (FDictionaryTree, 0);
  SetLength (FDicNames, 0);

  SetLength (AllExistingDistionaryNickNames, 0);
  SetLength (AllExistingDistionaryFileName, 0);
  inherited;
end;

function TPostProcessorEngine.GetDicionary (Name: String): TDictionaryTree;
var
  i: Integer;
begin
  Name:= UpperCase (Name);
  
  Result:= nil;
  for i:= 0 to DicCount- 1 do
    if FDicNames [i]= Name then
    begin
      Result:= FDictionaryTree [i];
      Exit;
    end;
    
  if Result= nil then
    raise Exception.Create ('Name Not Found!');
end;

{ TSearchResult }

function TSearchResult.ContinueSearch (Choices: array of Integer; Root: TDictionaryNode;
                        MaxChoice: Integer= 3): TSearchResultCollection;
const
  PathChar: array [0..32] of Char=
    (#1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15, #16, #17, #18,
     #19, #20, #21, #22, #23, #24, #25, #26, #27, #28, #29, #30, #31, #32, #33);
var
  ChoiceIndex,
  EndCondition,
  i: Integer;
begin
  Result:= TSearchResultCollection.Create;

  if Length (Choices)< MaxChoice then
    EndCondition:= Length (Choices)
  else
    EndCondition:= MaxChoice;

  for i:= 0 to EndCondition- 1 do
  begin
    ChoiceIndex:= Choices [i];
    if ChoiceIndex= -1 then
      Exit;

    if ActiveNode.Childs [ChoiceIndex]<> nil then
    begin
      Result.AddResult (TSearchResult.Create (Self, ActiveNode.Childs [ChoiceIndex],
              FChoices+ PathChar [i], FComponentUsedTilNow, i));
    end;
  end;

  if  ActiveNode.IsEndOfWord then
  begin
    EndCondition:= EndCondition+ 1* 1- 1;
    for i:= 0 to EndCondition- 1 do
    begin
      ChoiceIndex:= Choices [i];
      if ChoiceIndex= -1 then
        Exit;

      if Root.Childs [ChoiceIndex]<> nil then
      begin
        Result.AddResult (TSearchResult.Create (Self, Root.Childs [ChoiceIndex],
                FChoices+ PathChar [i], FComponentUsedTilNow+ 1, i, True));
      end;
    end;
  end;

end;

constructor TSearchResult.Create;
begin
  inherited;
  
  FNode:= nil;
  FActiveNodeIndex:= -1;
end;

constructor TSearchResult.Create (Node: TDictionaryNode; Path: String; CompTilNow: Integer; NodeCost: Integer= 0);
begin
  inherited Create;

  SetLength (FNode, 1);
  FNode [0]:= Node;
  FActiveNodeIndex:= 0;
  FComponentUsedTilNow:= CompTilNow;
  FChoices:= Path;
  FCost:= NodeCost;
end;

function TSearchResult.ConvertToWord: TWord;
var
  Word: TWord;
  i: Integer;
begin
  Result:= FNode [0].GetWordEndingWithThisNode;

  for i:= 1 to High (FNode) do
  begin
    Result.AddInEnd (32);
    Word:= FNode [i].GetWordEndingWithThisNode; 
    Result.AddAtEnd (Word);
    Word.Free;
  end;
end;

constructor TSearchResult.Create (Parent: TSearchResult;
          Node: TDictionaryNode; Path: String; CompTilNow: Integer; NodeCost: Integer; AddNewFNode: Boolean);
var
  i: Integer;
begin
  inherited Create;

  FCost:= Parent.FCost+ NodeCost;
  FComponentUsedTilNow:= CompTilNow;
  if AddNewFNode then
  begin
    SetLength (FNode, Length (Parent.FNode)+ 1);
    for i:= 0 to High (Parent.FNode) do
      FNode [i]:= Parent.FNode [i];

    FActiveNodeIndex:= High (FNode);
    FNode [FActiveNodeIndex]:= Node;
  end
  else
  begin
    SetLength (FNode, Length (Parent.FNode));
    for i:= 0 to High (Parent.FNode) do
      FNode [i]:= Parent.FNode [i];

    FActiveNodeIndex:= High (FNode);
    FNode [FActiveNodeIndex]:= Node;
  end;
end;

destructor TSearchResult.Destroy;
begin
  SetLength (FNode, 0);
  
  inherited;
end;

function TSearchResult.GetActiveNode: TDictionaryNode;
begin
  if FActiveNodeIndex= -1 then
    raise Exception.Create ('No Active Node');
    
  Result:= FNode [FActiveNodeIndex];
end;

function TSearchResult.GetNode (Index: Integer): TDictionaryNode;
begin
  Result:= FNode [Index];
end;

{ TSearchResultCollection }

procedure TSearchResultCollection.AddResult (Result: TSearchResult);
begin
  SetLength (FResults, FSize+ 1);
  FResults [FSize]:= Result;
  Inc (FSize);
end;

procedure TSearchResultCollection.AddResultCollection (
  ResultCollection: TSearchResultCollection);
var
  i: Integer;
begin
  for i:= 0 to ResultCollection.FSize- 1 do
    Self.AddResult (ResultCollection.FResults [i]);
end;

procedure TSearchResultCollection.Clear;
begin
  SetLength (FResults, 0);
  
end;

constructor TSearchResultCollection.Create;
begin
  inherited;

  FSize:= 0;
  SetLength (FResults, 0);
end;

procedure TSearchResultCollection.DelResultsHavingMoreComponentThan (
  n: Integer);
var
  i, Index: Integer;
begin
  Index:= 0;
  
  for i:= 0 to FSize- 1 do
    if n< FResults [i].FComponentUsedTilNow then
    begin
      FResults [i].Free;
      FResults [i]:= nil;
    end
    else
    begin
      FResults [Index]:= FResults [i];
      Inc (Index);
    end;

  SetLength (FResults, Index);
  FSize:= Index;
end;

destructor TSearchResultCollection.Destroy;
var
  i: Integer;
  
begin
  for i:= 0 to High (FResults) do
    FResults [i].Free;
      
  SetLength (FResults, 0);
  
  inherited;
  
end;

function TSearchResultCollection.GetAsWordCollection (ReturnAllWord: Boolean= False): TWordCollection;
var
  Word: TWord;
  MinCost: Integer;
  i: Integer;
begin
  Result:= TWordCollection.Create;

  MinCost:= MaxInt;
  if not ReturnAllWord then
  begin
    for i:= 0 to FSize- 1 do
      if FResults [i].FCost< MinCost then
        MinCost:= FResults [i].FCost;
  end;

  for i:= 0 to FSize- 1 do
    if FResults [i].FCost<= MinCost then
    begin
      Word:= FResults [i].ConvertToWord;
      if not Result.AddWord (Word, True, True) then
        Word.Free;
    end;
end;

function TSearchResultCollection.GetResults (Index: Integer): TSearchResult;
begin
  if (Index< 0) or (Index>= FSize) then
    raise Exception.Create ('Range Check Error!');
    
  Result:= FResults [Index];
end;

procedure TSearchResultCollection.RemoveResult (Index: Integer);
var
  i: Integer;
begin
  FResults [Index].Free;

  Dec (FSize);
  for i:= Index+ 1 to FSize do
    FResults [i- 1]:= FResults [i];

  SetLength (FResults, FSize);

end;

procedure TSearchResultCollection.Search (Root: TDictionaryNode; Word: TPostProcessorWord);
var
  TempResults, MissedSearchResults,
  NewResults: TSearchResultCollection;
  ActiveNode: TSearchResult;
  MissedChar,
  CharIndex,
  FinalResult,
  MaxComponent,
  i: Integer;
begin
  Clear;
  
  NewResults:= TSearchResultCollection.Create;
  AddResult (TSearchResult.Create (Root, '', 0, 0));

  MaxComponent:= (Length (Word)+ AverageCompLen- 1) div AverageCompLen+ 1;

  for CharIndex:= 0 to High (Word) do
  begin
    for i:= 0 to FSize- 1 do
    begin
      ActiveNode:= FResults [0];

      TempResults:= ActiveNode.ContinueSearch (Word [CharIndex], Root);
      TempResults.DelResultsHavingMoreComponentThan (MaxComponent);
      
      NewResults.AddResultCollection (TempResults);
      RemoveResult (0);

      TempResults.Free;
    end;
    Self.AddResultCollection (NewResults);
    NewResults.SetSize (0);
  end;

  FinalResult:= 0;
  for i:= 0 to FSize- 1 do
    if not FResults [i].ActiveNode.IsEndOfWord then
    begin
      FResults [i].Free;
      FResults [i]:= nil;
    end
    else
    begin
      FResults [FinalResult]:= FResults [i];
      Inc (FinalResult);
    end;

  SetSize (FinalResult);
  NewResults.Free;

  if FinalResult= 0 then
  begin
    FSize:= 0;
    Clear ();

    MissedSearchResults:= TSearchResultCollection.Create;
    NewResults:= TSearchResultCollection.Create;
    for MissedChar:= 0 to High (Word) do
    begin
      AddResult (TSearchResult.Create (Root, '', 0));

      for CharIndex:= 0 to High (Word) do
      begin

        for i:= 0 to FSize- 1 do
        begin
        //
          ActiveNode:= FResults [0];

          if MissedChar<> CharIndex then
            TempResults:= ActiveNode.ContinueSearch (Word [CharIndex], Root)
          else
            TempResults:= ActiveNode.ContinueSearch (Word [CharIndex], Root, 32);

         TempResults.DelResultsHavingMoreComponentThan (MaxComponent);
          NewResults.AddResultCollection (TempResults);
          RemoveResult (0);

          TempResults.FSize:= 0;
        end;
        
        Self.AddResultCollection (NewResults);
        NewResults.SetSize (0);
      end;

      FinalResult:= 0;
      for i:= 0 to FSize- 1 do
        if not FResults [i].ActiveNode.IsEndOfWord then
        begin
          FResults [i].Free;
          FResults [i]:= nil;
        end
        else
        begin
          FResults [FinalResult]:= FResults [i];
          Inc (FinalResult);
        end;

      Self.SetSize (FinalResult);
      if FinalResult<> 0 then
      begin
        MissedSearchResults.AddResultCollection (Self);
      end;
      
      Self.SetSize (0);
      TempResults.SetSize (0);
      TempResults.Free;
    end;

    NewResults.Free;
    Self.AddResultCollection (MissedSearchResults);
    MissedSearchResults.SetSize (0);
    MissedSearchResults.Free;
  end;

end;

procedure TSearchResultCollection.SetSize (n: Integer);
begin
  SetLength (FResults, n);
  FSize:= n;
end;

function TPostProcessorEngine.GetNickNameForDictionary (
  Index: Integer): String;
begin
  if (Index< 0) or (Index< LoadedDictionaryCount)then
    Result:=  FDicNames [Index]
  else
    Result:= '';


end;

function TPostProcessorEngine.GetLoadedDictionaryCount: Integer;
begin
  Result:= Length (FDicNames);
  
end;

function TPostProcessorEngine.LoadDictionary (DicName: String): Boolean;
var
  i: Integer;
  NewDictionary: TDictionaryTree;
begin
  try
    GetDicionary (DicName);
    Result:= True;
    Exit;

  except
  end;

  DicName:= UpperCase (DicName);

  for i:= 0 to High (AllExistingDistionaryNickNames) do
    if DicName= AllExistingDistionaryNickNames [i] then
    begin
      NewDictionary:= TDictionaryTree.Create;
      NewDictionary.LoadFromFile (DictionariesDirectory+
                           AllExistingDistionaryFileName [i]);
      Self.AddDictionary (DicName, NewDictionary);
      Result:= True;
      Exit;

    end;
    
  Result:= False;

end;

end.

