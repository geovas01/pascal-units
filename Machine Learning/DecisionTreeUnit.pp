unit DecisionTreeUnit;

interface
uses
  CollectionUnit;
  
type
  TConditionType= (ctEqual, ctLessThan, ctBiggerThan, ctLessThanEqual,
    ctBiggerThanEqual);
    
  { TDecisionTreeNode }

  TDecisionTreeNode= class (TObject)
  private
    FConditionType: TConditionType;
    FLeftChild: TDecisionTreeNode;
    FRightChild: TDecisionTreeNode;
    DataInNode: TIntegerCollection;

  public
    property ConditionType: TConditionType read FConditionType;
    property LeftChild: TDecisionTreeNode read FLeftChild;
    property RightChild: TDecisionTreeNode read FRightChild;

    constructor Create (CondType: TConditionType; DataSet: TIntegerCollection);
    destructor Destroy; override;
    
  end;
  
  { TEqualDecisionTreeNode }

  TEqualDecisionTreeNode= class (TDecisionTreeNode)
  private
  public
    constructor Create (Value: Integer;
      DataSet: TIntegerCollection);
  end;
  
  TDecisionTree= class (TObject)
  private
    FRoot: TDecisionTreeNode;
    
  public
    property Root: TDecisionTreeNode read FRoot;
    
    constructor Create (DataSet: TIntegerCollection);
    destructor Destroy; override;
    
  end;
implementation

{ TDecisionTreeNode }

constructor TDecisionTreeNode.Create (CondType: TConditionType;
  DataSet: TIntegerCollection);
begin
  inherited Create;
  
  DataInNode:= DataSet;
  
end;

destructor TDecisionTreeNode.Destroy;
begin
  FLeftChild.Free;
  FRightChild.Free;
  
  inherited Destroy;
  
end;

{ TEqualDecisionTreeNode }

constructor TEqualDecisionTreeNode.Create (Value: Integer;
  DataSet: TIntegerCollection);
begin
  inherited Create (ctEqual, DataSet);
  
end;

{ TDecisionTree }

constructor TDecisionTree.Create (DataSet: TIntegerCollection);
begin
  inherited Create;
  
  
end;

destructor TDecisionTree.Destroy;
begin
  FRoot.Free;
  
  inherited Destroy;
  
end;

end.
