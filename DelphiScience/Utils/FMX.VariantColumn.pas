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
    function CreateCellControl: TStyledControl; override;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);override;
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
  public
    Constructor Create(AColumn:TVariantColumn);override;
    Property OnGetCellText:TGetCellTextEvent read FGetCellText write FGetCellText;
  end;

  TSwitchProxy = class(TCellProxy)
  protected
    function CreateCellControl: TStyledControl; override;
    procedure LayoutCellControlForRow(aRow:Integer; var cRect:TRectF);override;
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

  TPopupProxy = class(TCellProxy)
  private
    FItems: TStrings;
    procedure SetItems(const Value: TStrings);
  protected
    function CreateCellControl: TStyledControl; override;
  public
    constructor Create(AColumn:TVariantColumn); override;
    destructor Destroy; override;
    property Items: TStrings read FItems write SetItems;
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
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  TColorComboProxy = class(TCellProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
  public
    constructor Create(AColumn:TVariantColumn); override;
  end;

  TComboColorProxy = class(TCellProxy)
  protected
    Procedure ApplyStyleLookup(Sender:TObject);override;
    function CreateCellControl: TStyledControl; override;
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
  TVariantColumn = class(TColumn)
  private
    FDefaultProxy:TCellProxy;
    FCellProxies:TList<TCellProxy>;
    FOnGetCellProxy: TGetCellProxyIndexEvent;
    FLayoutCellControlEvent: TLayoutCellControlEvent;
    Procedure SetHitTestForControlInRow(aRow:Integer;cControl:TStyledControl;Value:Boolean);
  protected
    Procedure ReleaseControl(aControl:TStyledControl);
    Procedure ReleaseControls;
    function CreateCellControlForRow(aRow:Integer): TStyledControl;
    function GetCellProxyForRow(aRow:Integer):TCellProxy;
    procedure UpdateColumn;override;
    procedure UpdateSelected;reintroduce;
    procedure SetCellDataForRow(aRow:Integer;cControl:TStyledControl;cData:TValue);
    procedure LayoutCellControlForRow(aRow:Integer;var CRect:TRectF);
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy;override;
    Function NewCellProxy(CellProxyClass:TCellProxyClass):TCellProxy;
    Property CellProxies:TList<TCellProxy> read FCellProxies;
    Property OnLayoutCellControl:TLayoutCellControlEvent read FLayoutCellControlEvent write FLayoutCellControlEvent;
    Property OnGetCellProxyIndex:TGetCellProxyIndexEvent read FOnGetCellProxy write FOnGetCellProxy;
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

    UpdateSelected;
  finally
    FUpdateColumn := False;
  end;


end;

procedure TVariantColumn.UpdateSelected;
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
      if THackGrid(lGrid).IsSelected(lGrid.TopRow + i) then
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

destructor TCellProxy.Destroy;
begin
  FReservedControls.Free;
  FReleasedControls.Free;
  inherited;
end;

procedure TCellProxy.DoContentChanged(Sender: TObject);
begin
  if FColumn.ApplyImmediately then
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


procedure TCellProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  cControl.Data := cData;
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

procedure TCheckProxy.RefreshText(bValue:Boolean; aRow:Integer; cControl:TStyledControl);
var bText:String;
begin
  bText := GetBooleanText(bValue);
  if assigned(FGetCelltext) then
    bText := FGetCelltext(Self,aRow,Self.FColumn.Index,bValue);
  TCheckBox(cControl).Text := bText;
end;

procedure TCheckProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  inherited;
  RefreshText(cData.AsBoolean,aRow,cControl);
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

{ TTextProxy }

constructor TTextProxy.Create(AColumn: TVariantColumn);
begin
  inherited;
  FControlStyleLookup := 'textcellstyle';
end;

function TTextProxy.CreateCellControl: TStyledControl;
begin
  Result := TEdit.Create(FColumn);
  TEdit(Result).OnTyping := TextTypingProc;
  TEdit(Result).OnChange := DoContentChanged;
  TEdit(Result).OnExit := DoTextExit;
end;

procedure TTextProxy.DoTextExit(Sender: TObject);
begin
  FColumn.DoTextExit(Sender);
end;

procedure TTextProxy.SetCellDataForRow(aRow: Integer; cControl: TStyledControl;
  cData: TValue);
begin
  inherited;
  //if cData.AsInteger > 55 then
  //  TEdit(cControl).FontColor := TalphaColors.Red
  //else
  //  TEdit(cControl).FontColor := TalphaColors.Black;
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

procedure TRadioButtonProxy.SetCellDataForRow(aRow: Integer;
  cControl: TStyledControl; cData: TValue);
begin
  inherited;
  if assigned(FGetCelltext) then
    TRadioButton(cControl).Text := FGetCelltext(Self,aRow,Self.FColumn.Index,cData.AsBoolean);
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

initialization
  RegisterFmxClasses([TVariantColumn]);
  //InitializeStyleResource;
finalization
  //FinalizeStyleResource;


end.
