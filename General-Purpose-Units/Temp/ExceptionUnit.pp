unit ExceptionUnit;

interface
uses
  SysUtils;
  
type
  ENotImplemented= class (Exception)
  public
    constructor Create (MethodNam: String);

  end;
  
  ENotImplementedYet= class (Exception)
  public
    constructor Create (MethodNam: String); overload;
    constructor Create (ClassNam, MethodNam: String); overload;

  end;
  
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

{ ENotImplemented }

constructor ENotImplemented.Create (MethodNam: String);
begin
  inherited Create ('the method '+ MethodNam+ ' is not implemented, yet!');
  
end;

{ ENotImplementedYet }

constructor ENotImplementedYet.Create (MethodNam: String);
begin
  inherited Create ('the method '+ MethodNam+ ' is not implemented, yet!');

end;

constructor ENotImplementedYet.Create (ClassNam, MethodNam: String);
begin
  inherited Create ('the method '+ MethodNam+ ' in class '+ ClassNam+ ' is not implemented, yet!');

end;

end.
