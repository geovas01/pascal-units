program Project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, {AutomataUnit, }heaptrc
  { you can add units after this }, StreamUnit;

var
  Automata: TAutomataState;
  Stream: TFileStream;
  
begin
  Stream:= TFileStream.Create ('Input.txt', fmOpenRead);
  
  Automata:= TAutomataState.Create (Stream);
  
  Stream.Free;
  Automata.Free;
  
end.

