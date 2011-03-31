unit HashUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type

  { THashableObjects }

  THashableObjects= class (TObject)
  private

  public

    function GetKey: Integer; virtual; abstract;
    
  end;
  
implementation

end.

