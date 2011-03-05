program UseSpellChecker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this }, SpellCheckerUnit, DictionaryTreeUnit,
  QueueUnit, StringKeyUnit;

{$IFDEF WINDOWS}{$R UseSpellChecker.rc}{$ENDIF}

var
  Sp: TSpellChecker;

const
  ToBeInsertedStrings: array [1..4] of String=
     ('laser', 'casern', 'AMin', 'Min');
  ToBeCheckedStrings: array [1..1] of String=
     ('naser');

var
  Tree: TDictionaryTree;
  StrKey: TstringKey;
  Data: TObject;
  PStr: PString;
  Stream: TFileStream;
  Suggestions: TStringList;
  i, j: Integer;

begin
  Tree:= TDictionaryTree.Create;
  StrKey:= TStringKey.Create ('');

  for i:= Low (ToBeInsertedStrings) to High (ToBeInsertedStrings) do
  begin
    PStr:= new (PString);
    PStr^:= ToBeInsertedStrings [i];
    StrKey.SetKey (PStr^);

    Tree.Insert (StrKey, TAbstractDataInNode (PStr), False);
    Data:= Tree.GetDataByKey (StrKey);
    PStr:= PString (Data);

  end;

  Sp:= TSpellChecker.Create (Tree);

  AssignFile (Output, 'Output.txt');
  Rewrite (Output);

  Suggestions:= TStringList.Create;
  for i:= Low (ToBeInsertedStrings) to High (ToBeInsertedStrings) do
  begin
    Sp.DoSpellChecking (ToBeCheckedStrings [i], Suggestions);

    for j:= 0 to Suggestions.Count- 1 do
      WriteLn (Suggestions.Strings [j]);
    WriteLn;

    Flush (Output);
    Suggestions.Clear;

  end;

  Suggestions.Free;

  CloseFile (Output);

  Sp.Free;

end.

