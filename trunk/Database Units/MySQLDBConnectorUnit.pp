unit MySQLDBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBConnectorUnit, ZConnection, ZDataset;


type
  TMySQLDatabaseConnection= class (TDatabaseConnection)
  private
    MySQLConnection: TZConnection;

  protected

    procedure CheckIfConnected; override;
    function GetActiveDB: AnsiString; override;
    function GetConnected: Boolean; override;
    function GetDatabases: TStringList; override;
    function GetTables: TStringList; override;

    procedure SetActiveDatabase (DBName: AnsiString); override;

  public
    constructor Create (Username, Password, Host: AnsiString);
    destructor Destroy; override;

    function Refresh: Boolean; override;
    procedure Disconnect;  override;

    {
      Returns a TZQuery object which is connected using the activeConnection
    }
    function InitQuery: TZQuery;  override;

    {
      Fetch the rows in the output of AQuery and put the context of each row in
      order, i.e., if R1: c1, c2 and R2: c3, c4 the output is going to have
      c1, c2, c3, c4
    }
    function FetchResult (AQuery: TZQuery): TStringList; override;

  end;

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


{ TMySQLDatabaseConnection }

function TMySQLDatabaseConnection.GetDatabases: TStringList;
var
  Query: TZQuery;

begin
  if not Connected then
    raise ENotConnected.Create;

  Query:= InitQuery;
  Query.SQL.Text:= 'SHOW databases;';
  Query.Active:= True;

  Result:= TStringList.Create;
  while not Query.EOF do
  begin
    Result.Add (Query.Fields [0].Text);
    Query.Next;

  end;

  Query.Free;

end;

function TMySQLDatabaseConnection.GetTables: TStringList;
var
 Query: TZQuery;

begin
  ActiveDB;
  if not Connected then
    raise ENotConnected.Create;

  Query:= InitQuery;
  Query.SQL.Text:= 'Show tables;';
  Query.Active:= True;

  Result:= TStringList.Create;
  while not Query.EOF do
  begin
    Result.Add (Query.Fields [0].Text);
    Query.Next;

  end;

  Query.Free;

end;

procedure TMySQLDatabaseConnection.CheckIfConnected;
begin
  if not Connected then
    raise ENotConnected.Create;

end;

function TMySQLDatabaseConnection.GetActiveDB: AnsiString;
begin
  if FActiveDB<> '' then
    Result:= FActiveDB
  else
    raise ENoActiveDB.Create;

end;

function TMySQLDatabaseConnection.GetConnected: Boolean;
begin
  Result:= MySQLConnection.Connected;

end;

constructor TMySQLDatabaseConnection.Create (Username, Password, Host: AnsiString);
begin
  MySQLConnection:= TZConnection.Create (nil);
  MySQLConnection.User:= Username;
  MySQLConnection.Password:= Password;
  MySQLConnection.HostName:= Host;
  MySQLConnection.Protocol:= 'mysql-5';
//  MySQLConnection.Database:= 'mysql';

  Self.FUserName:= Username;
  Self.FPassword:= Password;
  Self.FHost:= Host;
//  FActiveDB:= 'mysql';

  MySQLConnection.Connect;

  if not MySQLConnection.Connected then
    raise EConnectionFailed.Create ('Connection Failed');

end;

destructor TMySQLDatabaseConnection.Destroy;
begin
  MySQLConnection.Free;

  inherited Destroy;

end;

function TMySQLDatabaseConnection.Refresh: Boolean;
begin
  if Connected then
    Disconnect;

  MySQLConnection.User:= FUsername;
  MySQLConnection.Password:= FPassword;
  MySQLConnection.HostName:= FHost;
  MySQLConnection.Protocol:= 'mysql-5';

  MySQLConnection.Connect;

  if not MySQLConnection.Connected then
    raise EConnectionFailed.Create ('Connection Failed');

end;

procedure TMySQLDatabaseConnection.Disconnect;
begin
  CheckIfConnected;
  MySQLConnection.Disconnect;

end;

procedure TMySQLDatabaseConnection.SetActiveDatabase (DBName: AnsiString);
var
  Query: TZQuery;

begin
  MySQLConnection.Disconnect;
  MySQLConnection.Database:= DBName;
  MySQLConnection.Connect;

  Query:= InitQuery;
  Query.SQL.Text:= 'use '+ DBName+ ';';

  Query.Active;
  FActiveDB:= DBName;
  Query.Free;

end;

function TMySQLDatabaseConnection.InitQuery: TZQuery;
begin
  CheckIfConnected;

  Result:= TZQuery.Create (nil);
  Result.Connection:= MySQLConnection;

end;

function TMySQLDatabaseConnection.FetchResult(AQuery: TZQuery): TStringList;
var
  i: Integer;
begin
  Result:= TStringList.Create;

  while not AQuery.EOF do
  begin
    for i:= 0 to AQuery.FieldCount- 1 do
      Result.Add (AQuery.Fields [i].Text);

    AQuery.Next;

  end;

end;

end.

