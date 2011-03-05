unit StringSearchUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TFastStringSearchAlgorithm }
{
  http://en.wikipedia.org/wiki/Boyer-Moore_string_search_algorithm
  
   Preprocessing time: \teta(m + |Alphabet|)
   Matching time: \Omega(n/m), O(n)
}

  TCharCharPair= record
    First, Second: PChar;
    
  end;
  
  TFastStringSearchAlgorithm= class (TObject)
  private
    OccTable: array [#0..#256] of Integer;
    SkipTable: array of Integer;
    FNeedle: String;
    Needle: PChar;
    Len: Integer;
    
  public
    constructor Create (Word: String);
    destructor Destroy; override;
    
    function Exists (var _S: String): Boolean;
    function FindFirst (CharPtr: PChar; n: Integer): TCharCharPair;// n is the length of CharPtr
    
  end;
  
implementation

uses
  Math;
  
{ TFastStringSearchAlgorithm }

constructor TFastStringSearchAlgorithm.Create (Word: String);

  procedure BuildTables;
  type
    TIntArray= array of Integer;

    function Suffixes: TIntArray;
    var
      f, g, i: Integer;
      
    begin
      SetLength (Result, Len);
      
      Result [Len- 1]:= Len;
      g:= Len- 1;
       for i:= Len- 2 downto 0 do
       begin
         if (g< i) and (Result [i+ Len- 1- f]< i- g)  then
           Result [i]:= Result [i+ Len- 1- f]
         else
         begin
           if i< g then
             g:= i;
             
           f:= i;
           while (0<= g) and (Needle [g]= Needle [g+ Len- 1- f]) do
             Dec (g);

           Result [i]:= f- g;
           
         end;
         
       end;
       
    end;
    
  var
    WPtr: PChar;
    i, j: Integer;
    Ch: Char;
    Suffix: TIntArray;
    
  begin
    WPtr:= @Word [1];

    FillChar (OccTable, SizeOf (OccTable), 255);//OccTable [i]= -1;
    for i:= 0 to Len- 2 do
      OccTable [(WPtr+ i)^]:= Len- i- 1;
    
    for Ch:= #0 to #255 do
      if OccTable [Ch]= -1 then
        OccTable [Ch]:= Len+ 1;

    for i:= 0 to 255 do
      if OccTable [Chr (i)]<= Len then
        WriteLn (Chr (i), ' ', OccTable [Chr (i)]);

    WriteLn ('Suffix');
    
    Suffix:= Suffixes;
    for i:= 0 to High (Suffix) do
      WriteLn (i, ' ', Needle [i+ 1], ' ', Suffix [i]);

    SetLength (SkipTable, Len+ 1);
    for i:= 0 to Len- 1 do
      SkipTable [i]:= Len;
      
    j:= 0;
    for i:= Len- 1 downto 0 do
      if Suffix [i]= i+ 1 then
        while (j< Len- 1- i) do
        begin
          if SkipTable [j]= Len then
            SkipTable [j]:= Len- 1- i;
            
          Inc (j);
          
        end;
      
    for i:= 0 to Len- 2 do
      SkipTable [Len- 1- Suffix [i]]:= Len- 1- i;
      
    SetLength (Suffix, 0);
    
  end;

var
  i: Integer;
  
begin
  inherited Create;
  
  FNeedle:= Word;
  Needle:= @FNeedle [1];
  Len:= Length (FNeedle);
  BuildTables;
  WriteLn;

  for i:= 0 to Len- 1 do
    WriteLn (i, ':', Needle [i], ':', SkipTable [i]);
    

end;

destructor TFastStringSearchAlgorithm.Destroy;
begin
  SetLength (SkipTable, 0);
  inherited Destroy;
  
end;

function TFastStringSearchAlgorithm.Exists (var _S: String): Boolean;
var
  i, j: Integer;
  n: Integer;
  S: PChar;

begin
  j:= 0;
  n:= Length (_S);
  S:= @_S [1];

  while (j <= n- Len) do
  begin
    i:= Len - 1;
    while (0<= i) and (Needle [i]= S [i+ j]) do
      Dec (i);

    if i< 0 then
    begin
      Result:= True;
      Exit;
      WriteLn (j);
      Inc (j, SkipTable [0]);
      
    end
    else
      Inc (j, Max (SkipTable [i], OccTable [S [i + j]]- Len+ 1+ i));

  end;
  
  Result:= False;
    
end;

function TFastStringSearchAlgorithm.FindFirst (CharPtr: PChar; n: Integer): TCharCharPair;
var
  i, j: Integer;
  S: PChar;

begin
  j:= 0;
  S:= CharPtr;

  while (j <= n- Len) do
  begin
    i:= Len - 1;
    while (0<= i) and (Needle [i]= S [i+ j]) do
      Dec (i);

    if i< 0 then
    begin
      Result.First:= @(S [j]);
      Result.Second:= Result.First+ Len- 1;
      Exit;

    end
    else
      Inc (j, Max (SkipTable [i], OccTable [S [i + j]]- Len+ 1+ i));

  end;

  Result.First:= nil; Result.Second:= nil;

end;

end.

