unit DBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

type

  { TDatabaseConnection }

  TDatabaseConnection= class (TObject)
  protected
    FUserName, FPassword, FHost: AnsiString;
    FActiveDB: AnsiString;

    procedure CheckIfConnected; virtual; abstract;
    function GetActiveDB: AnsiString; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    function GetDatabases: TStringList; virtual; abstract;
    function GetTables: TStringList; virtual; abstract;

    procedure SetActiveDatabase (DBName: AnsiString); virtual; abstract;

  public
    property Connected: Boolean read GetConnected;
    {
      The caller is responsible for deleting the returned TStringlist.
    }
    property Databases: TStringList read GetDatabases;
    {
      The caller is responsible for deleting the returned TStringlist.
    }
    property Tables: TStringList read GetTables;
    property ActiveDB: AnsiString read GetActiveDB write SetActiveDatabase;

    constructor Create (Username, Password, Host: AnsiString);
    destructor Destroy; override;

    function Refresh: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    {
      Returns a TZQuery object which is connected using the activeConnection
    }
    function InitQuery: TZQuery; virtual; abstract;

    {
      Fetch the rows in the output of AQuery and put the content of each row in
      order, i.e., if R1: c1, c2 and R2: c3, c4 the output is going to have
      c1, c2, c3, c4
    }
    function FetchResult (AQuery: TZQuery): TStringList; virtual; abstract;

  end;

  EConnectionFailed= class (Exception);

implementation

type

  { ENotConnected }

  ENotConnected= class (Exception)
  public
    constructor Create;

  end;

  { ENoActiveDB }

  ENoActiveDB= class (Exception)
  public
    constructor Create;

  end;

constructor ENoActiveDB.Create;
begin
  inherited Create ('There is no Active Database!');

end;

constructor ENotConnected.Create;
begin
  inherited Create ('Not Connected!');

end;


{ TDatabaseConnection }

constructor TDatabaseConnection.Create (Username, Password, Host: AnsiString);
begin
  inherited Create;

  Self.FUserName:= Username;
  Self.FPassword:= Password;
  Self.FHost:= Host;

end;

destructor TDatabaseConnection.Destroy;
begin
  if Connected then
    Disconnect;

  inherited Destroy;

end;


end.

