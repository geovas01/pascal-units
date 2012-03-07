unit PreprocessorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClauseUnit, TSeitinVariableUnit, MyTypes;

type

  { TAbstractPreprocessor }

  TAbstractPreprocessor= class (TObject)
  private
  public type
    TCNFAssignmentPair= specialize TPair<TClauseCollection, TAssignments>;
  public
    constructor Create;
    destructor Destroy; override;

    {
    Processes the CNFCollection and returns the simplification, and modifies the
    Assignment inplace. Return true if Assignments has been changed.
    }
    function Process (CNFCollection: TClauseCollection; Assignment: TAssignments): Boolean;
      virtual; abstract;

  end;

implementation

{ TAbstractPreprocessor }

constructor TAbstractPreprocessor.Create;
begin

end;

destructor TAbstractPreprocessor.Destroy;
begin
  inherited Destroy;
end;

end.

