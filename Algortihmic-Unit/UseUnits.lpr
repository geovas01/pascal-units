program UseUnits;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, LinkedListUnit, MapUnit
  { you can add units after this };

{$IFDEF WINDOWS}{$R UseUnits.rc}{$ENDIF}

begin

end.

