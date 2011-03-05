unit HTMLParser;

interface

uses
  SysUtils, Variants, Classes, HyperParse;

type
  TAnswers = class
  Public
    A : array [1..4] of string;
  end;

  TExamParser = class
  private
    FFileName: string;
    Parser : THyperParse;
    QList : TStringList;
    function GetAnswers(Index: integer): TAnswers;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Answers[Index : integer]:TAnswers read GetAnswers;
    procedure Parse;
    property FileName: string read FFileName write FFileName;
    property Questions: TStringList read QList write QList;
    property Count:Integer read GetCount;
  end;

const
  Tags : array [0..4] of string =('Q','A1','A2','A3','A4');

implementation

{ TExamParser }

constructor TExamParser.Create;
begin
  QList := TStringList.Create;
end;

destructor TExamParser.Destroy;
var
  i : integer;
begin
  if Assigned(Parser) then Parser.Free;
  for i:=0 to QList.Count-1 do
    (QList.Objects[i] as TAnswers).Free;
  QList.Free;
  inherited;
end;

function TExamParser.GetAnswers(Index: integer): TAnswers;
begin
  Result := QList.Objects[Index] as TAnswers;
end;

function TExamParser.GetCount: Integer;
begin
  Result := QList.Count;
end;

procedure TExamParser.Parse;
var
  i,j,k,t,tempi : integer;
  Question : string;
  TagsId : byte;
  CanExit : Boolean;

  procedure IgnoreEndTags;
  begin
    while (not Parser.Item[i].IsTag) or
          (Parser.Item[i].IsTag and (Parser.Item[i].TagName[1]='/')) do
       Inc(i);
  end;

  function IgnoreBeginTags (Place:integer):Integer;
  begin
    While (not Parser.Item[Place].IsTag) or
          (Parser.Item[Place].IsTag and (Parser.Item[Place].TagName[1]<>'/')) do
       Dec(Place);
    Result := Place;
  end;

begin
  TagsId:=0;
  CanExit := False;

  for i:=0 to QList.Count-1 do
    (QList.Objects[i] as TAnswers).Free;
  QList.Clear;

  if Assigned(Parser) then Parser.Free;
  Parser := THyperParse.Create;
  Parser.FileName := FFileName;
  Parser.Execute;

  i:=0;
  repeat
    while i <= parser.Count - 1 do
    begin
      if (not parser.Item[i].IsTag) and ((Parser.Item[i].Text = '{#'+Tags[TagsId]+'#}') or
        (Parser.Item[i].Text = '{#End#}')) then
      begin
        if Parser.Item[i].Text = '{#End#}' then
        begin
          CanExit := True;
          Break;
        end;
        i := i + 1;
        IgnoreEndTags;
        for j:=i to Parser.Count-1 do
        begin
          if (not parser.Item[j].IsTag) and (Parser.Item[j].Text = '{#/'+Tags[TagsId]+'#}') then
          begin
            Question := '';
            Tempi := IgnoreBeginTags(j-1);
            for k:=i to Tempi do
            begin
              if Parser.Item[k].IsTag then
              begin
                Question := Question + '<'+ Parser.Item[k].TagName;
                for t:=0 to Parser.Item[k].ParamCount-1 do
                  Question := Question + ' '+ Parser.Item[k].Params[t].Param;
                Question := Question + '>';
              end
              else
                Question := Question + Parser.Item[k].Text;
            end;
            i:=j+2;
//            ShowMessage(Question);
            if TagsId=0 then
              QList.AddObject(Question,TAnswers.Create)
            else
              (QList.Objects[QList.Count-1] as TAnswers).A[TagsId] := Question;
            TagsId := (TagsId + 1) mod 5;
            Break;
          end;
        end;
      end;
      Inc(i);
    end;
  until CanExit;

  Parser.Free;
  Parser := nil;
end;

end.
