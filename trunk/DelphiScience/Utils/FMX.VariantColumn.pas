unit FMX.VariantColumn;

interface

uses
  System.Classes, System.Types, System.SysUtils, System.UITypes,System.Variants,System.Math,
  System.Rtti,FMX.Grid, FMX.Types, FMX.Objects, FMX.Controls, FMX.Layouts, FMX.Edit,
  FMX.ExtCtrls, FMX.Menus,FMX.Listbox,FMX.Styles, FMX.Colors, Generics.Collections;

type
  TVariantColumn = class;

  TCellControlClass = class of TStyledControl;

  THackGrid = class(TGrid);


  TCellProxy = class(TObject)
  private
    FReservedControls:TList<TStyledControl>;
    FReleasedControls:TList<TStyledControl>;
    FControlStyleLookup: String;
    FListIndex: Integer;
    function GetCellControl:TStyledControl;
  protected
    FColumn:TVariantColumn;
    procedure DoContentChanged(Sender: TObject);virtual;
    procedure ReleaseControl(aControl:TStyledControl);
    Procedure ReleaseControls;
    Procedure ApplyStyleLookup(Sender:TObject);virtual;
    function CreateCellControl: TStyledControl; virtual;
    procedure LayoutCellControlForRow(aRow:Integer;var cRect:TRectF);virtual;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);virtual;
    procedure SetHitTest(cControl:TStyledControl;Value:Boolean);virtual;
    Function SetCellControlForEdit(aRow:Integer;cControl:TStyledControl;aReadOnly:Boolean):Boolean;virtual;
    Procedure PrepareCellControlForRow(CControl:TStyledControl;aRow:Integer);virtual;
    procedure NilChangeEvent(cControl:TStyledControl);virtual;
    procedure SetChangeEvent(cControl:TStyledControl);virtual;
  public
    Property ControlStyleLookup:String read FControlStyleLookup write FControlStyleLookup;
    Property ListIndex:Integer read FListIndex;
    Constructor Create(AColumn:TVariantColumn);virtual;
    Destructor Destroy;override;
  end;

  TTextProxy = class(TCellProxy)
  protected
    procedure TextTypingProc(Sender: TObject);
    procedure DoTextExit(Sender: TObject);
    procedure DoTextKey(Sender:TObject;var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    function CreateCellControl: TStyledControl; override;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);override;
    Function SetCellControlForEdit(aRow:Integer;cControl:TStyledControl;aReadOnly:Boolean):Boolean;override;

  public
    Constructor Create(AColumn:TVariantColumn);override;
  end;

  TGetCellTextEvent = Function (Sender:TCellProxy;aRow,aCol:Integer;BoolValue:Boolean):String of Object;

  TCheckProxy = class(TCellProxy)
  private
    FGetCellText: TGetCellTextEvent;
  protected
    function GetBooleanText(Value:Boolean):String;virtual;
    procedure RefreshText(bValue:Boolean; aRow:Integer; cControl:TStyledControl);virtual;
    function CreateCellControl: TStyledControl; override;
    procedure DoContentChanged(Sender: TObject);override;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);override;
    Function SetCellControlForEdit(aRow:Integer;cControl:TStyledControl;aReadOnly:Boolean):Boolean;override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    Constructor Create(AColumn:TVariantColumn);override;
    Property OnGetCellText:TGetCellTextEvent read FGetCellText write FGetCellText;
  end;

  TSwitchProxy = class(TCellProxy)
  protected
    function CreateCellControl: TStyledControl; override;
    procedure LayoutCellControlForRow(aRow:Integer; var cRect:TRectF);override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    Constructor Create(AColumn:TVariantColumn);override;
  end;

  TRadioButtonProxy = class(TCellProxy)
  private
    FGetCellText: TGetCellTextEvent;
  protected
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);override;
    function CreateCellControl: TStyledControl; override;
    procedure DoContentChanged(Sender: TObject);override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    Constructor Create(AColumn:TVariantColumn);override;
    Property OnGetCellText:TGetCellTextEvent read FGetCellText write FGetCellText;
  end;

  TProgressProxy = class(TCellProxy)
  private
    FMin: Single;
    FMax: Single;
  protected
    function CreateCellControl: TStyledControl; override;
  public
    constructor Create(AColumn:TVariantColumn);override;
    property Max: Single read FMax write FMax;
    property Min: Single read FMin write FMin;
  end;

  TGetItemsEvent = Function (Sender:TCellProxy;aRow,aCol:Integer):String of Object;

  TPopUpHack=class(TPopupBox)

  end;

  TPopupProxy = class(TCellProxy)
  private
    FItems: TStrings;
    FGetItems: TGetItemsEvent;
    procedure SetItems(const Value: TStrings);
  protected
    function CreateCellControl: TStyledControl; override;
    Function SetCellControlForEdit(aRow:Integer;cControl:TStyledControl;aReadOnly:Boolean):Boolean;override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    constructor Create(AColumn:TVariantColumn); override;
    Procedure PrepareCellControlForRow(CControl:TStyledControl;aRow:Integer);override;
    destructor Destroy; override;
    property Items: TStrings read FItems write SetItems;
    Property OnGetItems:TGetItemsEvent read FGetItems write FGetItems;
  end;

  TComboPopup = class(TCombobox)
  protected
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
  end;

  TComboProxy = class(TPopupProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    constructor Create(AColumn:TVariantColumn); override;
    Procedure PrepareCellControlForRow(CControl:TStyledControl;aRow:Integer);override;
  end;

  TColorComboProxy = class(TCellProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  TComboColorProxy = class(TCellProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  TImageProxy = class(TCellProxy)
  protected
    function CreateCellControl: TStyledControl; override;
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  TDateCombo = class(TCalendarEdit)
  protected
    FArrow:TControl;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
  end;

  TDateProxy = class(TCellProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
    procedure SetHitTest(cControl:TStyledControl;Value:Boolean);override;
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  THackTrackbar = class(TTrackbar);
  TTrackBarProxy = class(TCellProxy)
  private
    FMin: Single;
    FMax: Single;
  protected
    function CreateCellControl: TStyledControl; override;
    procedure NilChangeEvent(cControl:TStyledControl);override;
    procedure SetChangeEvent(cControl:TStyledControl);override;
    procedure LayoutCellControlForRow(aRow: Integer; var cRect: TRectF);override;
    Procedure ApplyStyleLookup(Sender:TObject);override;
    procedure SetHitTest(cControl:TStyledControl;Value:Boolean);override;
  public
    constructor Create(AColumn:TVariantColumn);override;
    property Max: Single read FMax write FMax;
    property Min: Single read FMin write FMin;
  end;

  TCreateCellControlEvent = Function (Sender:TObject):TStyledControl of Object;

  TCustomProxy = class(TCellProxy)
  private
    FCreateCellControl: TCreateCellControlEvent;
  protected
    function CreateCellControl: TStyledControl; override;
  public
    Procedure CellControlChanged(Sender:TObject);
    Property OnCreateCellControl:TCreateCellControlEvent read FCreateCellControl write FCreateCellControl;
  end;
  TCellProxyClass = class of TCellProxy;
  TGetCellProxyIndexEvent = Function (Sender:TObject; Arow:Integer):Integer of Object;
  TLayoutCellControlEvent = Procedure (Sender:TObject;ARow:Integer; var Rect:TRectF) of Object;
  TGetCellReadOnlyEvent = Function (Sender:TObject; Arow:Integer):Boolean of Object;

  TVariantColumn = class(TColumn)
  private
    FDefaultProxy:TCellProxy;
    FCellProxies:TList<TCellProxy>;
    FOnGetCellProxy: TGetCellProxyIndexEvent;
    FLayoutCellControlEvent: TLayoutCellControlEvent;
    FOnGetCellReadOnly: TGetCellReadOnlyEvent;
    Procedure SetHitTestForControlInRow(aRow:Integer;cControl:TStyledControl;Value:Boolean);
    Function SetCellControlForEdit(aRow:Integer;cControl:TStyledControl):Boolean;
  protected
    Procedure ReleaseControl(aControl:TStyledControl);
    Procedure ReleaseControls;
    procedure DoCanFocus(Sender: TObject; var ACanFocus: Boolean);
    function CreateCellControlForRow(aRow:Integer): TStyledControl;
    function GetCellProxyForRow(aRow:Integer):TCellProxy;
    procedure UpdateColumn;override;
    procedure UpdateSelected_;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);
    procedure LayoutCellControlForRow(aRow:Integer;var CRect:TRectF);
    Function GetCellReadOnlyForRow(aRow:Integer):Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy;override;
    Function NewCellProxy(CellProxyClass:TCellProxyClass):TCellProxy;
    Property CellProxies:TList<TCellProxy> read FCellProxies;
    Property OnLayoutCellControl:TLayoutCellControlEvent read FLayoutCellControlEvent write FLayoutCellControlEvent;
    Property OnGetCellProxyIndex:TGetCellProxyIndexEvent read FOnGetCellProxy write FOnGetCellProxy;
    Property OnGetCellReadOnly:TGetCellReadOnlyEvent read FOnGetCellReadOnly write FOnGetCellReadOnly;
  end;

  TPropType = (ptText,ptDouble,ptInteger,ptBoolean,ptPickList);
  TGridProperty = class(TObject)
  private
    FRowIndex: Integer;
    FVUnit: String;
    FReadOnly: Boolean;
    FPropType: TPropType;
    FPicklist: String;
    procedure SetRowIndex(const Value: Integer);
    function GetIndentLevel: Integer;
    procedure SetPropType(const Value: TPropType);
    procedure SetPicklist(const Value: String);
  protected
    FChilds:TList<TGridProperty>;
    FParent:TGridProperty;
    FName: String;
    FValue: TValue;
    FCollapsed: Boolean;
    procedure SetValue(const Value: TValue);
    procedure SetCollapsed(const Value: Boolean);
    function GetRowCount: Integer;virtual;
    Procedure RefreshPropertyView(aProp:TGridProperty);
    Function GetPropForRow(aRow:Integer):TGridProperty;
  public
    Property Childs:TList<TGridProperty> read FChilds;
    Constructor Create(aParent:TGridProperty);
    Procedure ClearChilds;
    Destructor Destroy;override;
    Function FindProp(Path:String):TGridProperty;
    Function GetPath:String;
    Property Name:String read FName write FName;
    Property Value:TValue read FValue write SetValue;
    Property VUnit:String read FVUnit write FVUnit;
    Property Collapsed:Boolean read FCollapsed write SetCollapsed;
    Property RowCount:Integer read GetRowCount;
    Property RowIndex:Integer read FRowIndex write SetRowIndex;
    Property IndentLevel:Integer read GetIndentLevel;
    Property ReadOnly:Boolean read FReadOnly write FReadOnly;
    Property Picklist:String read FPicklist write SetPicklist;
    Property PropType:TPropType read FPropType write SetPropType;
  end;

  TPropValueChangeEvent = Procedure (Sender:TObject;Prop:TGridProperty) of Object;
  TPropertyGridProxy = class(TGridProperty)
  private
    FGrid:TGrid;
    FTreeColumn:TImageColumn;
    FNameColumn:TStringColumn;
    FValueColumn:TVariantColumn;
    FShowTree: Boolean;
    FCollapsedBitmap: TBitmap;
    FexpandedBitmap: TBitmap;
    FEmptyBitmap: TBitmap;
    FValueCount: Integer;
    FPropValueChange: TPropValueChangeEvent;
    Function GetPropForColRow(aCol,aRow:Integer):TGridProperty;
    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: System.Rtti.TValue);
    procedure GridSetValue(Sender: TObject; const Col, Row: Integer;const Value: System.Rtti.TValue);
    Procedure GridMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Single);
    function GetRowCount: Integer;override;
    procedure SetShowTree(const Value: Boolean);
    procedure SetCollapsedBitmap(const Value: TBitmap);
    procedure SetExpandedBitmap(const Value: TBitmap);
    Function GetCellReadOnly(Sender:TObject; Arow:Integer):Boolean;
    Function GetCellProxyIndex(Sender:TObject; Arow:Integer):Integer ;
    procedure SetValueCount(const Value: Integer);
    Function GetPopUpItems(cProxy:TCellProxy;aRow,aCol:Integer):String;
  public
    constructor Create(AGrid: TGrid);
    Destructor Destroy;override;
    Procedure RefreshPropertyView(aProp:TGridProperty);
    Procedure BuildGrid;
    Procedure RefreshGrid;
    Procedure SetPropValue(Path:String;Value:TValue);
    Procedure SetPropReadOnly(Path:String;Value:Boolean);
    Property ShowTree:Boolean read FShowTree write SetShowTree;
    Property CollapsedBitmap:TBitmap read FCollapsedBitmap write SetCollapsedBitmap;
    Property ExpandedBitmap:TBitmap read FexpandedBitmap write SetExpandedBitmap;
    Property ValueCount:Integer read FValueCount write SetValueCount;
    Property OnPropValueChange:TPropValueChangeEvent read FPropValueChange write FPropValueChange;
  end;

implementation

//{$R *.res}

var styleObject:TFmxObject=nil;


{ TVariantColumn }
constructor TVariantColumn.Create(AOwner: TComponent);
begin
  inherited;
  FCellProxies := TList<TCellProxy>.Create;
  FDefaultProxy := TTextProxy.Create(Self);
  Self.FCellProxies.Remove(FDefaultProxy);
end;

function TVariantColumn.CreateCellControlForRow(aRow:Integer): TStyledControl;
var cProxy:TCellProxy;
begin
  Result := nil;
  cProxy := GetCellProxyForRow(aRow);
  if assigned(cProxy) then begin
    Result := cProxy.GetCellControl;
    cProxy.PrepareCellControlForRow(Result,aRow);
    Result.Parent := Self;
    cProxy.SetHitTest(Result,False);
    Result.Locked := True;
    Result.Stored := False;
    Result.OnCanFocus := DoCanFocus;
    Result.OnEnter := DoEnter;
    Result.OnKeyDown := DoKeyDown;
  end;
end;

destructor TVariantColumn.Destroy;
var cProxy:TCellProxy;
begin
  for cProxy in FCellProxies do
  begin
    cProxy.Free;
  end;
  FCellProxies.Free;
  FDefaultProxy.Free;
  inherited;
end;

procedure TVariantColumn.DoCanFocus(Sender: TObject; var ACanFocus: Boolean);
var p: TPointF;
begin
  inherited;
  P := StringToPoint(TFmxObject(Sender).TagString);
  ACanFocus := SetCellControlForEdit(Trunc(P.Y),TStyledControl(Sender));
end;

function TVariantColumn.GetCellProxyForRow(aRow: Integer): TCellProxy;
var pIndex:Integer;
begin
  pIndex := -1;
  result := FDefaultProxy;
  if assigned(FOnGetCellProxy) then pIndex := FOnGetCellProxy(Self,aRow);
  if pIndex > -1 then
  begin
    result := FCellProxies[pIndex];
  end;
end;

function TVariantColumn.GetCellReadOnlyForRow(aRow: Integer): Boolean;
begin
  result := false;
  if assigned(FOnGetCellReadOnly) then
    result := FOnGetCellReadOnly(Self,aRow);
end;

procedure TVariantColumn.LayoutCellControlForRow(aRow:Integer;var CRect: TRectF);
var cProxy:TCellProxy;
begin
  cProxy := Self.GetCellProxyForRow(aRow);
  cProxy.LayoutCellControlForRow(aRow,cRect);
  if assigned(FLayoutCellControlEvent) then
  begin
    FLayoutCellControlEvent(Self,aRow,CRect);
  end;
end;

procedure TVariantColumn.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

end;

function TVariantColumn.NewCellProxy(
  CellProxyClass: TCellProxyClass): TCellProxy;
begin
  result := CellProxyClass.Create(Self);
  Self.FCellProxies.Add(result);
  Result.FListIndex := FCellProxies.Count-1;
end;

procedure TVariantColumn.ReleaseControl(aControl: TStyledControl);
var cProxy:TCellProxy;
begin
  FDefaultProxy.ReleaseControl(aControl);
  for cProxy in FCellProxies do cProxy.ReleaseControl(aControl);
end;

procedure TVariantColumn.ReleaseControls;
var cProxy:TCellProxy;
begin
  FDefaultProxy.ReleaseControls;
  for cProxy in FCellProxies do cProxy.ReleaseControls;
end;

Function TVariantColumn.SetCellControlForEdit(aRow: Integer;
  cControl: TStyledControl):Boolean;
var cProxy:TCellProxy;
begin
  cProxy := GetCellProxyForRow(aRow);
  result := cProxy.SetCellControlForEdit(aRow,cControl,GetCellReadOnlyForRow(aRow));
end;

procedure TVariantColumn.SetCellDataForRow(aRow: Integer;
  cControl: TStyledControl; cData: TValue);
var cProxy:TCellProxy;
begin
  cProxy := Self.GetCellProxyForRow(aRow);
  cProxy.SetCellDataForRow(aRow,cControl,cData);
end;


procedure TVariantColumn.SetHitTestForControlInRow(aRow: Integer;
  cControl: TStyledControl; Value: Boolean);
var cProxy:TCellProxy;
begin
  cProxy := Self.GetCellProxyForRow(aRow);
  cProxy.SetHitTest(cControl,Value);
end;

procedure TVariantColumn.UpdateColumn;
var
  i, C: Integer;
  V: TValue;
  cRect:TRectF;
begin
  if Grid = nil then
    Exit;

  FUpdateColumn := True;
  try
    ReleaseControls;
    for i := 0 to Length(FCellControls)-1 do
    begin
      FCellControls[i].Visible := False;
      FCellControls[i].TagString := '';
    end;
    SetLength(FCellControls,0);

    c := Min(Grid.RowCount, Grid.VisibleRows);
    c := Min(c,Grid.RowCount -  Grid.TopRow);
    SetLength(FCellControls,c);

    for i := 0 to Length(FCellControls)-1 do
    begin
      FCellControls[i] := Self.CreateCellControlForRow(Grid.TopRow + i);
      V := THackGrid(Grid).GetValue(Index, Grid.TopRow + i);
      FCellControls[i].Visible := True;
      FCellControls[i].TagString :=
        String(PointToString(PointF(Index, Grid.TopRow + i)));
      cRect := RectF(0,i * Grid.RowHeight,Width,(i+1)*Grid.RowHeight);
      Self.LayoutCellControlForRow(Grid.TopRow+i,cRect);
      FCellControls[i].BoundsRect := cRect;
      SetCellDataForRow(Grid.TopRow + i,FCellControls[i],V);
    end;

    UpdateSelected_;
  finally
    FUpdateColumn := False;
  end;


end;

procedure TVariantColumn.UpdateSelected_;
var
  i, N: Integer;
  EnableFocus: boolean;
  lGrid: TCustomGrid;
begin

  lGrid := Grid;
  if not assigned(lGrid) then
    Exit;

  EnableFocus := (not ReadOnly) and
                 (Enabled) and
                 (not lGrid.ReadOnly) and
                 (lGrid.Enabled) and
                 (lGrid.ColumnIndex = Index) and
                 (lGrid.IsFocused or lGrid.IsChildFocused);

  N := High(FCellControls);

  if EnableFocus then
    for i := 0 to N do
    begin
      if (THackGrid(lGrid).IsSelected(lGrid.TopRow + i))
      then
      begin
        SetFocus;
        FCellControls[i].CanFocus := True;
        Self.SetHitTestForControlInRow(lGrid.TopRow + i,FCellControls[i],True);
        //FCellControls[i].SetFocus;
        FCellControls[i].BringToFront;
      end
      else
      begin
        FCellControls[i].CanFocus := False;
        Self.SetHitTestForControlInRow(lGrid.TopRow + i,FCellControls[i],False);
        if FCellControls[i] is TEdit then
          TEdit(FCellControls[i]).SelLength := 0;
      end;
    end;

  for i := 0 to N do
    if not THackGrid(lGrid).IsSelected(lGrid.TopRow + i) then
    begin
      FCellControls[i].CanFocus := False;
      Self.SetHitTestForControlInRow(lGrid.TopRow + i,FCellControls[i],False);
      if FCellControls[i].IsFocused then
        lGrid.Root.SetActiveControl(nil);
    end;
end;

{ TCellProxy }

procedure TCellProxy.ApplyStyleLookup(Sender: TObject);
begin
end;

constructor TCellProxy.Create(AColumn: TVariantColumn);
begin
  inherited Create;
  FColumn := aColumn;
  FReservedControls := TList<TStyledControl>.Create;
  FReleasedControls := TList<TStyledControl>.Create;
end;

function TCellProxy.CreateCellControl: TStyledControl;
begin
  Result := nil;
end;

procedure TCellProxy.LayoutCellControlForRow(aRow:Integer; var cRect: TRectF);
begin

end;

procedure TCellProxy.NilChangeEvent(cControl: TStyledControl);
begin

end;

procedure TCellProxy.PrepareCellControlForRow(CControl: TStyledControl;
  aRow: Integer);
begin

end;

destructor TCellProxy.Destroy;
begin
  FReservedControls.Free;
  FReleasedControls.Free;
  inherited;
end;

procedure TCellProxy.DoContentChanged(Sender: TObject);
begin
  FColumn.DoTextChanged(Sender);
end;

function TCellProxy.GetCellControl: TStyledControl;
begin
  Result := nil;
  if FReleasedControls.Count > 0 then begin
    Result := FReleasedControls[0];
    FReleasedControls.Remove(Result);
  end;
  if Result = nil then begin
    Result := Self.CreateCellControl;
    Result.OnApplyStyleLookup := ApplyStyleLookup;
    Result.StyleLookup := FControlStyleLookup;
  end;
  FReservedControls.Add(Result);
end;

procedure TCellProxy.ReleaseControl(aControl: TStyledControl);
begin
  if FReservedControls.Contains(aControl) then
  begin
    FReservedControls.Remove(aControl);
    FReleasedControls.Add(aControl);
  end;
end;

procedure TCellProxy.ReleaseControls;
begin
  FReleasedControls.AddRange(FReservedControls);
  FReservedControls.Clear;
end;


Function TCellProxy.SetCellControlForEdit(aRow: Integer;
  cControl: TStyledControl; aReadOnly: Boolean):Boolean;
begin
  Result := not aReadOnly;
end;

procedure TCellProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  NilChangeEvent(cControl);
  cControl.Data := cData;
  SetChangeEvent(cControl);
end;

procedure TCellProxy.SetChangeEvent(cControl: TStyledControl);
begin

end;

procedure TCellProxy.SetHitTest(cControl: TStyledControl; Value: Boolean);
begin
  cControl.HitTest := Value;
end;

{ TCheckProxy }

constructor TCheckProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := 'checkcellstyle';
end;

function TCheckProxy.CreateCellControl: TStyledControl;
begin
  result :=  TCheckBox.Create(FColumn);
  TCheckBox(Result).OnChange := DoContentChanged;
end;

procedure TCheckProxy.DoContentChanged(Sender: TObject);
var p: TPointF;
begin
  inherited;
  P := StringToPoint(TFmxObject(Sender).TagString);
  RefreshText(TCheckBox(Sender).IsChecked,Trunc(p.Y),TCheckBox(Sender));
end;

function TCheckProxy.GetBooleanText(Value: Boolean): String;
begin
  result := 'True';
  if not Value then result := 'False';
end;

procedure TCheckProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TCheckBox(cControl).OnChange := Nil;
end;

procedure TCheckProxy.RefreshText(bValue:Boolean; aRow:Integer; cControl:TStyledControl);
var bText:String;
begin
  bText := GetBooleanText(bValue);
  if assigned(FGetCelltext) then
    bText := FGetCelltext(Self,aRow,Self.FColumn.Index,bValue);
  TCheckBox(cControl).Text := bText;
end;

Function TCheckProxy.SetCellControlForEdit(aRow: Integer;
  cControl: TStyledControl; aReadOnly: Boolean):Boolean;
begin
  result := not aReadOnly;
end;

procedure TCheckProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  inherited;
  RefreshText(cData.AsBoolean,aRow,cControl);
end;

procedure TCheckProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TCheckBox(cControl).OnChange := DoContentChanged;
end;

{ TProgressProxy }

constructor TProgressProxy.Create(AColumn:TVariantColumn);
begin
  inherited;
  FMax := 100;
  FControlStyleLookup := 'progresscellstyle';
end;

function TProgressProxy.CreateCellControl: TStyledControl;
begin
  Result := TProgressBar.Create(FColumn);
  TProgressBar(Result).Min := FMin;
  TProgressBar(Result).Max := FMax;
end;

{ TPopupProxy }

constructor TPopupProxy.Create(AColumn:TVariantColumn);
begin
  inherited;
  FItems := TStringList.Create;
  FControlStyleLookup := 'popupcellstyle';
end;

function TPopupProxy.CreateCellControl: TStyledControl;
begin
  Result := TPopupBox.Create(FColumn);
  TPopupBox(Result).Items.Assign(FItems);
  TPopupBox(Result).OnChange := DoContentChanged;
end;

destructor TPopupProxy.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TPopupProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TPopupBox(cControl).OnChange := nil;
end;

procedure TPopupProxy.PrepareCellControlForRow(CControl: TStyledControl;
  aRow: Integer);
var itemsText:String;
begin
  inherited;
  if assigned(FGetItems) then begin
    itemsText := FGetItems(Self,aRow,Self.FColumn.Index);
    if CControl is TPopupBox then TPopupBox(CControl).Items.Text := itemsText;
  end;
end;

function TPopupProxy.SetCellControlForEdit(aRow: Integer;
  cControl: TStyledControl; aReadOnly: Boolean): Boolean;
begin
  result := inherited;
end;

procedure TPopupProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TPopupBox(cControl).OnChange := DoContentChanged;
end;

procedure TPopupProxy.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

{ TImageProxy }

constructor TImageProxy.Create(AColumn:TVariantColumn);
begin
  inherited;
  FControlStyleLookup := 'imagecellstyle';
end;

function TImageProxy.CreateCellControl: TStyledControl;
begin
  Result := TImageControl.Create(FColumn);
  TImageControl(Result).EnableOpenDialog := False;
end;

{ TSwitchProxy }


constructor TSwitchProxy.Create(AColumn: TVariantColumn);
begin
  inherited;

end;

function TSwitchProxy.CreateCellControl: TStyledControl;
begin
  result :=  TSwitch.Create(FColumn);
  TSwitch(Result).OnChange := DoContentChanged;
end;

procedure TSwitchProxy.LayoutCellControlForRow(aRow:Integer; var cRect: TRectF);
begin
  inherited;
  cRect := RectF(cRect.Right-75,cRect.Top+2,cRect.Right-5,cRect.Bottom-2);
end;

procedure TSwitchProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TSwitch(cControl).OnChange := Nil;
end;

procedure TSwitchProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TSwitch(cControl).OnChange := DoContentChanged;
end;

{ TTextProxy }

constructor TTextProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := 'textcellstyle';
end;

function TTextProxy.CreateCellControl: TStyledControl;
begin
  Result := TEdit.Create(FColumn);
  //TEdit(Result).OnTyping := TextTypingProc;
  Tedit(result).OnKeyDown := DoTextKey;
  //TEdit(Result).OnChange := DoContentChanged;
  TEdit(Result).OnExit := DoTextExit;
end;

procedure TTextProxy.DoTextExit(Sender: TObject);
begin
  DoContentChanged(Sender);
  FColumn.DoTextExit(Sender);
end;

procedure TTextProxy.DoTextKey(Sender:TObject;var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    DoContentChanged(Sender);
  end;
end;

function TTextProxy.SetCellControlForEdit(aRow: Integer;
  cControl: TStyledControl; aReadOnly: Boolean):Boolean;
begin
  Result := True;
  TEdit(CControl).ReadOnly := aReadOnly;
end;

procedure TTextProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  inherited;
end;

procedure TTextProxy.TextTypingProc(Sender: TObject);
begin
  if FColumn.ApplyImmediately then
    FColumn.DoTextChanged(Sender);
end;


{ TDateProxy }


procedure TDateProxy.ApplyStyleLookup(Sender: TObject);
var bg:TSubImage;
    T:TFmxObject;
begin
  inherited;
  bg := TSubImage(TStyledControl(Sender).FindStyleResource('background'));
  if assigned(bg) then bg.Source := nil;
  TDateCombo(Sender).FArrow := nil;
  T := TDateCombo(Sender).FindStyleResource('arrow');
  if assigned(T) and (T is TControl) then begin
    TDateCombo(Sender).FArrow := TStyledControl(T);
    TDateCombo(Sender).FArrow.HitTest := False;
  end;
end;

constructor TDateProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := '';
end;

function TDateProxy.CreateCellControl: TStyledControl;
begin
  result := TDateCombo.Create(FColumn);
  TDateCombo(Result).OnChange := DoContentChanged;
end;

procedure TDateProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TDateCombo(cControl).OnChange := nil;
end;

procedure TDateProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TDateCombo(cControl).OnChange := DoContentChanged;
end;

procedure TDateProxy.SetHitTest(cControl: TStyledControl; Value: Boolean);
begin
  inherited;
  if assigned(TDateCombo(cControl).FArrow) then
    TDateCombo(cControl).FArrow.HitTest := Value;
end;

{ TRadioButtonProxy }

constructor TRadioButtonProxy.Create(AColumn: TVariantColumn);
begin
  inherited;

end;

function TRadioButtonProxy.CreateCellControl: TStyledControl;
begin
  result := TRadioButton.Create(FColumn);
  TRadioButton(Result).GroupName := 'rcell';
  TRadioButton(Result).OnChange := DoContentChanged;
end;

procedure TRadioButtonProxy.DoContentChanged(Sender: TObject);
var
  cControl: TStyledCOntrol;
begin
  inherited;
  if TRadioButton(Sender).isChecked then
    for cControl in self.FReservedControls do
      if (TRadioButton(cControl) <> Sender) then
        TRadioButton(cControl).IsChecked := False;
end;

procedure TRadioButtonProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TRadioButton(cControl).OnChange := Nil;
end;

procedure TRadioButtonProxy.SetCellDataForRow(aRow: Integer;
  cControl: TStyledControl; cData: TValue);
begin
  inherited;
  if assigned(FGetCelltext) then
    TRadioButton(cControl).Text := FGetCelltext(Self,aRow,Self.FColumn.Index,cData.AsBoolean);
end;

procedure TRadioButtonProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TRadioButton(cControl).OnChange := DoContentChanged;
end;

procedure InitializeStyleResource;
var S: TStream;
begin
  styleObject := nil;
  S := TResourceStream.Create(HInstance, 'variantcolumnstyle', RT_RCDATA);
  try
    styleObject := TStyleManager.LoadFromStream(S);
  finally
    S.Free;
  end;
  if assigned(StyleObject) then FMX.Types.AddResource(StyleObject);
end;


procedure FinalizeStyleResource;
begin
  if assigned(StyleObject) then FMX.Types.RemoveResource(StyleObject);
  FreeAndNil(StyleObject);
end;


{ TComboProxy }

procedure TComboProxy.ApplyStyleLookup(Sender: TObject);
var bg:TSubImage;
begin
  inherited;
  bg := TSubImage(TStyledControl(Sender).FindStyleResource('background'));
  if assigned(bg) then bg.Source := nil;
end;

constructor TComboProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := 'comboboxstyle';
end;

function TComboProxy.CreateCellControl: TStyledControl;
begin
  Result := TComboPopup.Create(FColumn);
  TComboBox(Result).Items.Assign(FItems);
  TComboBox(Result).OnChange := DoContentChanged;
end;


procedure TComboProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TComboBox(cControl).OnChange := nil;
end;

procedure TComboProxy.PrepareCellControlForRow(CControl: TStyledControl;
  aRow: Integer);
var itemsText:String;
begin
  inherited;
  if assigned(FGetItems) then begin
    itemsText := FGetItems(Self,aRow,Self.FColumn.Index);
    if CControl is TComboBox then TComboBox(CControl).Items.Text := itemsText;
  end;
end;

procedure TComboProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TComboBox(cControl).OnChange := DoContentChanged;
end;

{ TColorComboProxy }

procedure TColorComboProxy.ApplyStyleLookup(Sender: TObject);
var bg:TSubImage;
begin
  inherited;
  bg := TSubImage(TStyledControl(Sender).FindStyleResource('background'));
  if assigned(bg) then bg.Source := nil;
end;

constructor TColorComboProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := '';
end;

function TColorComboProxy.CreateCellControl: TStyledControl;
begin
  Result := TColorComboBox.Create(FColumn);
  TColorComboBox(Result).OnChange := DoContentChanged;
end;

procedure TColorComboProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TColorComboBox(cControl).OnChange := nil;
end;

procedure TColorComboProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TColorComboBox(cControl).OnChange := DoContentChanged;
end;

{ TComboColorProxy }

procedure TComboColorProxy.ApplyStyleLookup(Sender: TObject);
var bg:TSubImage;
begin
  inherited;
  bg := TSubImage(TStyledControl(Sender).FindStyleResource('background'));
  if assigned(bg) then bg.Source := nil;
end;

constructor TComboColorProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := '';
end;

function TComboColorProxy.CreateCellControl: TStyledControl;
begin
  Result := TComboColorBox.Create(FColumn);
  TComboColorBox(Result).OnChange := DoContentChanged;
end;

procedure TComboColorProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TComboColorBox(cControl).OnChange := nil;
end;

procedure TComboColorProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TComboColorBox(cControl).OnChange := DoContentChanged;
end;

{ TComboPopup }

function TComboPopup.GetData: TValue;
begin
  result := Self.ItemIndex;
end;

procedure TComboPopup.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsOrdinal then
    ItemIndex := Value.AsOrdinal
  else
    ItemIndex := Items.IndexOf(Value.ToString);
end;

{ TDateCombo }

function TDateCombo.GetData: TValue;
begin
  Result := TValue.From<TDate>(Self.Date);
end;

procedure TDateCombo.SetData(const Value: TValue);
var
  D: TDateTime;
begin
  if Value.IsType<TDate> then
    Date := Value.AsType<TDate>
  else if TryStrToDateTime(Value.ToString, D) then
    Date := D;
end;

{ TTrackBarProxy }

procedure TTrackBarProxy.ApplyStyleLookup(Sender: TObject);
begin
  inherited;
  if assigned(THackTrackbar(Sender).Thumb) then
    THackTrackbar(Sender).Thumb.HitTest := False;
end;

constructor TTrackBarProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FMin := 0;
  FMax := 10;
end;

function TTrackBarProxy.CreateCellControl: TStyledControl;
begin
  Result := TTrackBar.Create(FColumn);
  TTrackBar(Result).Min := FMin;
  TTrackBar(Result).Max := FMax;
  TTrackBar(Result).OnChange := DoContentChanged;
end;

procedure TTrackBarProxy.LayoutCellControlForRow(aRow:Integer; var cRect: TRectF);
begin
  inherited;
  cRect := RectF(cRect.Left+5,cRect.Top+5,cRect.Right-5,cRect.Bottom-5);
end;

procedure TTrackBarProxy.NilChangeEvent(cControl: TStyledControl);
begin
  TTrackBar(cControl).OnChange := nil;
end;

procedure TTrackBarProxy.SetChangeEvent(cControl: TStyledControl);
begin
  TTrackBar(cControl).OnChange := DoContentChanged;
end;

procedure TTrackBarProxy.SetHitTest(cControl: TStyledControl; Value: Boolean);
begin
  inherited;
  if assigned(THackTrackbar(cControl).Thumb) then THackTrackbar(cControl).Thumb.HitTest := Value;
end;

{ TCustomProxy }

procedure TCustomProxy.CellControlChanged(Sender: TObject);
begin
  Self.DoContentChanged(Sender);
end;

function TCustomProxy.CreateCellControl: TStyledControl;
begin
  if assigned(FCreateCellControl) then begin
    result := FCreateCellControl(Self);
    Self.FColumn.AddObject(result);
  end else result := inherited;
end;

{ TPropertyGridProxy }

procedure TPropertyGridProxy.BuildGrid;
var i: Integer;
    fChild:TGridProperty;
    fixWidth: Single;
    fColumn:TFMXObject;
    fProxy:TCellProxy;
begin
  FTreeColumn := TImageColumn.Create(FGrid);
  FGrid.AddObject(FTreeColumn);
  FTreeColumn.Width := 20;
  FTreeColumn.HitTest := False;
  FTreeColumn.ReadOnly := True;
  FTreeColumn.Visible := Self.ShowTree;

  FNameColumn := TStringColumn.Create(FGrid);
  FNameColumn.ReadOnly := True;
  FNameColumn.Width := 140;
  FGrid.AddObject(FNameColumn);

  fixWidth := 140 + 5;
  if Self.ShowTree then fixWidth :=  fixWidth+20;


  for i := 1 to FValueCount do
  begin
    FValueColumn := TVariantColumn.Create(FGrid);
    FValueColumn.Width := (FGrid.Width - fixWidth)/FValueCount;
    FValueColumn.OnGetCellReadOnly := GetCellReadOnly;
    FValueColumn.OnGetCellProxyIndex := GetCellProxyIndex;

    FValueColumn.NewCellProxy(TTextProxy);  //0
    FValueColumn.NewCellProxy(TCheckProxy);  //1
    fProxy := FValueColumn.NewCellProxy(TPopupProxy);  //2
    TPopUpProxy(fProxy).OnGetItems := GetPopupItems;
    FGrid.AddObject(FValueColumn);
  end;

  if FValueCount > 1 then
  begin
    for fChild in fChilds do
      fChild.Collapsed := False;
  end;


end;

constructor TPropertyGridProxy.Create(AGrid: TGrid);
begin
  inherited Create(nil);
  FGrid := aGrid;
  FShowTree := True;
  FCollapsed := False;
  FCollapsedBitmap := TBitmap.Create(0,0);
  FExpandedBitmap := TBitmap.Create(0,0);
  FEmptyBitmap := TBitmap.Create(1,1);

  FGrid.OnGetValue := GridGetValue;
  FGrid.OnSetValue := GridSetValue;
  FGrid.OnMouseDown := GridMouseDown;
  FValueCount := 1;

end;

destructor TPropertyGridProxy.Destroy;
begin
  inherited;
end;

function TPropertyGridProxy.GetCellProxyIndex(Sender: TObject;
  Arow: Integer): Integer;
var fProp:TGridProperty;
begin
   fProp := GetPropForColRow(TColumn(Sender).Index,aRow);
   if assigned(fProp) then
   begin
     case fProp.PropType of
       TPropType.ptText: result := 0;
       TPropType.ptDouble: result := 0;
       TPropType.ptInteger: result := 0;
       TPropType.ptBoolean: result := 1;
       TPropType.ptPicklist: result := 2;
     end;
   end;
end;

function TPropertyGridProxy.GetCellReadOnly(Sender: TObject;
  Arow: Integer): Boolean;
var fProp:TGridProperty;
begin
  result := false;
  fProp := GetPropForColRow( Tcolumn(Sender).Index,aRow);

  if assigned(fProp) then result := fProp.ReadOnly;
end;

function TPropertyGridProxy.GetPopUpItems(cProxy: TCellProxy; aRow,
  aCol: Integer): String;
var fProp:TGridProperty;
begin
  result := '';
  if FValueCount > 1 then begin
    fProp := GetPropForColRow(aCol,aRow);
  end else begin
    fProp := GetPropForRow(aRow);
  end;
  if assigned(fProp) then begin
    result := FProp.Picklist;
  end;
end;

function TPropertyGridProxy.GetPropForColRow(aCol,
  aRow: Integer): TGridProperty;
var fChild: TGridProperty;
begin
  Result := nil;
  if FValueCount = 1 then
  begin
    Result := GetPropForRow(aRow);
  end else begin
    aCol :=  aCol - 2;
    if aCol < 0  then aCol := 0;
    
    if FChilds.Count > aCol then
    begin
      fChild := FChilds[aCol];
      Result := fChild.GetPropForRow(aRow);
    end;
  end;
end;

function TPropertyGridProxy.GetRowCount: Integer;
begin
  if FValueCount = 1 then
  begin
    result := inherited GetRowCount;
    result := result-1;
  end else begin
    result := 1;
    if FChilds.Count > 0 then begin
      result := FChilds[0].RowCount;
    end;
  end;
end;

procedure TPropertyGridProxy.GridGetValue(Sender: TObject; const Col,
  Row: Integer; var Value: System.Rtti.TValue);
var fProp:TGridProperty;
    iLevel,i:Integer;
    indent: String;
begin
   fProp := GetPropForRow(Row);
   if assigned(fProp) then
   begin
     if Col = 0 then begin
       Value := FEmptyBitmap;
       if (fProp.FChilds.Count > 0) and ((fProp.FParent <> Self) or (FValueCount=1)) then
       begin
         if fProp.FCollapsed then Value := FCollapsedBitmap
         else Value := FExpandedBitmap;
       end;
     end;
     if Col = 1 then begin
       iLevel := fProp.IndentLevel;
       indent := '';
       if FValueCount = 1 then
         for i := 2 to iLevel do indent := indent+'   '
       else
         for i := 3 to iLevel do indent := indent+'   ';
       Value := indent+fProp.Name;
       if (FValueCount>1) and (Row = 0) then Value := Self.Name;
       
     end;
     if Col > 1 then begin
       fProp := GetPropForColRow(Col,Row);
       if assigned(fProp) and (Row = 0) and (FValueCount > 1) then begin
         Value := fProp.Name;
       end else if assigned(fProp) then begin
         Value := fProp.Value;
         if fProp.VUnit <> '' then Value := fProp.Value.ToString + '  '+ fProp.FVUnit;
       end;
     end;
   end;
end;

procedure TPropertyGridProxy.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var aRow:Integer;
    aCol: Integer;
    fProp:TGridProperty;
    xCol: TColumn;
begin
  if (button = TMouseButton.mbLeft) then begin
    if FGrid.ColumnByPoint(X,Y) = FTreeColumn then begin
      aRow := FGrid.RowByPoint(X,Y);
      xCol := FGrid.ColumnByPoint(X,Y);
      aCol := 0;
      if assigned(xCol) then aCol := xCol.Index;
      fProp := GetPropForColRow(aCol,aRow);
      if assigned(fProp) and (fProp.Childs.Count > 0) then
      begin
        fProp.Collapsed := not FProp.Collapsed;
        RefreshGrid;
      end;
    end;
  end;
end;

procedure TPropertyGridProxy.GridSetValue(Sender: TObject; const Col,
  Row: Integer; const Value: System.Rtti.TValue);
var fProp:TGridProperty;
    strValue:String;
    intValue: Integer;
    doubleValue: Double;
    Err: Integer;
    fSettings:TFormatSettings;
begin
  fProp := GetPropForColRow(Col,Row);
  if fProp.PropType in [ptInteger,ptDouble] then
  begin
    strValue := Value.ToString;
    strValue := Trim(Stringreplace(strValue,fProp.FVUnit,'',[rfReplaceAll,rfIgnoreCase]));
    if fprop.FPropType = ptInteger then
    begin
      Val(StrValue, intValue, Err);
      if Err > 0 then fProp.Value := intValue;
    end;
    if fprop.FPropType = ptDouble then
    begin
      fSettings :=  FormatSettings;
      fSettings.DecimalSeparator := ',';
      strValue := Stringreplace(strValue,'.',',',[rfReplaceAll]);
      if TextToFloat(strValue,doubleValue) then fProp.Value := doubleValue;
    end;
  end else begin
    fProp.Value := Value;
  end;
  if assigned(FPropValueChange) then  FPropValueChange(Self,fProp);
end;

procedure TPropertyGridProxy.RefreshGrid;
var fChild:TGridProperty;
begin
  if FValueCount = 1 then
  begin
    SetRowIndex(-1);
  end else begin
    FRowIndex := -1;
    for fChild in FChilds do
      fChild.SetRowIndex(0);
  end;
  FGrid.RowCount := 0;
  FGrid.RowCount := Self.RowCount;
  FGrid.Repaint;
end;

procedure TPropertyGridProxy.RefreshPropertyView(aProp: TGridProperty);
begin

end;

procedure TPropertyGridProxy.SetCollapsedBitmap(const Value: TBitmap);
begin
  FCollapsedBitmap.Assign(Value);
end;

procedure TPropertyGridProxy.SetExpandedBitmap(const Value: TBitmap);
begin
  FexpandedBitmap.Assign(Value);
end;

procedure TPropertyGridProxy.SetPropReadOnly(Path: String; Value: Boolean);
var fProp:TGridProperty;
begin
  fProp := FindProp(Path);
  if assigned(fProp) then
  begin
    fProp.ReadOnly := Value;
  end;
end;

procedure TPropertyGridProxy.SetPropValue(Path: String; Value: TValue);
var fProp:TGridProperty;
begin
  fProp := FindProp(Path);
  if assigned(fProp) then
  begin
    fProp.Value := Value;
  end;
end;

procedure TPropertyGridProxy.SetShowTree(const Value: Boolean);
begin
  FShowTree := Value;
end;

procedure TPropertyGridProxy.SetValueCount(const Value: Integer);
begin
  FValueCount := Value;
end;

{ TGridProperty }

procedure TGridProperty.ClearChilds;
var fChild:TGridProperty;
begin
  for fChild in FChilds do
    fChild.Free;
  fChilds.Clear;
end;

constructor TGridProperty.Create(aParent:TGridProperty);
begin
  inherited Create;
  FParent := aParent;
  FChilds := TList<TGridProperty>.Create;
  FCollapsed := True;
  FPropType := ptText;
end;

destructor TGridProperty.Destroy;
begin
  ClearChilds;
  FChilds.Free;
  inherited;
end;

function TGridProperty.FindProp(Path: String): TGridProperty;
var pathName:String;
    FChild:TGridProperty;
begin
  result := nil;
  pathName := Self.GetPath;
  if pathName <> '' then pathName := pathName+'.'+Self.Name
  else PathName := self.Name;
  if SameText(pathName,path) then result := self;
  if not assigned(result) then
  begin
    for fChild in FChilds do
    begin
      result := fChild.FindProp(Path);
      if assigned(result) then break;
    end;
  end;
end;

function TGridProperty.GetIndentLevel: Integer;
begin
  if Assigned(FParent) then begin
    result := FParent.IndentLevel;
    result := result+1;
  end else begin
    Result := 0;
  end;
end;

function TGridProperty.GetPath: String;
var pPath:String;
begin
  result := '';
  if assigned(FParent) then
  begin
    pPath := FParent.GetPath;
    if pPath <> '' then result := pPath+'.'+FParent.Name
    else result := FParent.Name;
  end;
end;

function TGridProperty.GetPropForRow(aRow: Integer): TGridProperty;
var fChild: TgridProperty;
begin
  result := nil;
  if FRowIndex = aRow then begin
    result := Self;
  end else if not FCollapsed then begin
    for fChild in FChilds do
    begin
      result := fChild.GetPropForRow(aRow);
      if assigned(result) then break;
    end;
  end;
end;

function TGridProperty.GetRowCount: Integer;
var fChild:TGridProperty;
begin
  result := 1;
  if not FCollapsed then begin
    for fChild in FChilds do
      result := result+fChild.GetRowCount;
  end;
end;

procedure TGridProperty.RefreshPropertyView(aProp: TGridProperty);
begin
end;

procedure TGridProperty.SetCollapsed(const Value: Boolean);
begin
  FCollapsed := Value;
end;


procedure TGridProperty.SetPicklist(const Value: String);
begin
  FPicklist := Value;
end;

procedure TGridProperty.SetPropType(const Value: TPropType);
begin
  FPropType := Value;
end;

procedure TGridProperty.SetRowIndex(const Value: Integer);
var fChild:TGridProperty;
    rIndex: Integer;
    delta:Integer;
begin
  FRowIndex := Value;
  rIndex := Value;
  delta := 1;
  if not FCollapsed then begin
    for fChild in FChilds do
    begin
      rIndex :=  rIndex+delta;
      fChild.RowIndex := rIndex;
      delta := fChild.RowCount;
    end;
  end;
end;

procedure TGridProperty.SetValue(const Value: TValue);
begin
  FValue := Value;
  RefreshPropertyView(Self);
end;

initialization
  RegisterFmxClasses([TVariantColumn]);
  //InitializeStyleResource;
finalization
  //FinalizeStyleResource;


end.
