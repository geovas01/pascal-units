unit ExceptionUnit;

interface
uses
  SysUtils;
  
type
  ENotImplemented= class (Exception);
  ERangeCheckError= class (Exception)
  public
    constructor Create (AMethodName: String);

  end;

implementation

{ ERangeCheckError }

constructor ERangeCheckError.Create (AMethodName: String);
begin
  inherited Create ('Range check Error in '+ AMethodName);
  
end;

end.
