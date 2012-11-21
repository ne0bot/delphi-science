unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid,
  FMX.Layouts, FMX.VariantColumn, FMX.Objects, FMX.ExtCtrls, FMX.Colors,
  FMX.Edit, FMX.ListBox;

type

  TSampleData = Record
    Name:String;
    IsMan:Boolean;
    IsActive:Boolean;
    IsOld:Boolean;
    Complete:Integer;
    City:Integer;
    Animal:Integer;
    HairColor: TAlphaColor;
    SkinColor:TAlphaColor;
    Image:TBitmap;
    BirthDate:TDateTime;
    Performance: Single;
    Custom1:Single;
    Custom2:Single;
  End;

  TfrmMain = class(TForm)
    Grid1: TGrid;
    Column1: TColumn;
    Image1: TImage;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const Col, Row: Integer;
      var Value: TValue);
    procedure Grid1SetValue(Sender: TObject; const Col, Row: Integer;
      const Value: TValue);
  private
    { Private declarations }
    Function GetCellProxyIndex(Sender:TObject; Arow:Integer):Integer ;
    Function DoCreateCustomCellControl(Sender:TObject):TStyledControl;
    Procedure LayoutCellControl(Sender:Tobject;aRow:Integer; var Rect:TRectF);
    Function GetCellTextForCheckCells(Sender:TCellProxy;aRow,aCol:Integer;BoolValue:Boolean):String;
  public
    { Public declarations }
    vColumn:TVariantColumn;
    MySampleData:TSampleData;
  end;

var
  frmMain: TfrmMain;
  Const FieldNames: Array [0..14] of String = ('Name','Is Old','Active','Gender','','Completed',
  'City','Animal','Hair Color','Skin Color','Image','BirthDay','Performnce','Custom1','Custom2');

implementation

{$R *.fmx}

function TfrmMain.DoCreateCustomCellControl(Sender: TObject): TStyledControl;
begin
  result := nil;
  if TCustomProxy(Sender).ListIndex = 12 then
  begin
    result := THueTrackbar.Create(vColumn);
    THueTrackbar(Result).OnChange := TCustomProxy(Sender).CellControlChanged;
  end else if TCustomProxy(Sender).ListIndex = 13 then
  begin
    result := TAlphaTrackbar.Create(vColumn);
    TAlphaTrackbar(Result).OnChange := TCustomProxy(Sender).CellControlChanged;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  vColumn := TVariantColumn.Create(Self);
  vColumn.Header := 'Variant Column';
  vColumn.OnGetCellProxyIndex := GetCellProxyIndex;
  vColumn.OnLayoutCellControl := LayoutCellControl;
  Grid1.RowCount := 15;
  Grid1.AddObject(vColumn);

  vColumn.NewCellProxy(TTextProxy);  //0
  with TCheckProxy(vColumn.NewCellProxy(TCheckProxy)) do  //1
  begin
    OnGetCellText := GetCellTextForCheckCells;
  end;

  vColumn.NewCellProxy(TSwitchProxy); //2
  with TRadioButtonProxy(vColumn.NewCellProxy(TRadioButtonProxy)) do//3
  begin
    OnGetCellText := GetCellTextForCheckCells;
  end;

  with TProgressProxy(vColumn.NewCellProxy(TProgressProxy)) do //4
  begin
    Min := 0;
    Max := 100;
  end;

  with  TPopUpProxy(vColumn.NewCellProxy(TPopupProxy)) do //5
  begin
    Items.Add('Istanbul');
    Items.Add('Paris');
    Items.Add('NewYork');
  end;

  with TComboProxy(vColumn.NewCellProxy(TComboProxy)) do//6
  begin
    Items.Add('Dog');
    Items.Add('Cat');
    Items.Add('Bird');
  end;

  vColumn.NewCellProxy(TColorComboProxy); //7
  vColumn.NewCellProxy(TComboColorProxy); //8
  vColumn.NewCellProxy(TImageProxy); //9
  vColumn.NewCellProxy(TDateProxy);  //10
  vColumn.NewCellProxy(TTrackbarProxy);  //11

  With TCustomProxy(vColumn.NewCellProxy(TCustomProxy)) do // 12
  begin
    OnCreateCellControl := DoCreateCustomCellControl;
  end;
  With TCustomProxy(vColumn.NewCellProxy(TCustomProxy)) do  //13
  begin
    OnCreateCellControl := DoCreateCustomCellControl;
  end;

  With MySampleData Do
  begin
    Name := 'John Turner';
    IsMan := True;
    IsActive := True;
    IsOld := True;
    Complete := 75;
    City := 0;
    Animal := 0;
    HairColor := TAlphaColors.Black;
    SkinColor := TalphaColors.Yellow;
    Image := Image1.Bitmap;
    BirthDate := EncodeDate(1975,1,10);
    Performance := 6;
    Custom1 := 0.5;
    Custom2 := 0.5;
  end;

end;

function TfrmMain.GetCellProxyIndex(Sender: TObject; Arow: Integer): Integer;
begin
  result := 0;
  if ARow < 4 then result := ARow
  else if Arow >= 4 then result := aRow-1;

   // I use different proxy in the list for each row.
                   // You can use any kind ofproxy in nay row.
end;

Function TfrmMain.GetCellTextForCheckCells(Sender: TCellProxy; aRow,
  aCol: Integer; BoolValue: Boolean):String;
begin
  case aRow of
    2: if BoolValue then result := 'True' else result := 'False';
    3: Result := 'Male';
    4: Result := 'Female';
  end;
end;

procedure TfrmMain.Grid1GetValue(Sender: TObject; const Col, Row: Integer;
  var Value: TValue);
begin
  if Col = 0 then begin
    Value := FieldNames[Row];
  end else if Col = 1 then begin
    case Row of
      0:Value := MySampleData.Name;
      1:Value := MySampleData.IsOld;
      2:Value := MySampleData.IsActive;
      3:Value := MySampleData.IsMan;
      4:Value := not MySampleData.IsMan;
      5:Value := MySampleData.Complete;
      6:Value := MySampleData.City;
      7:Value := MySampleData.Animal;
      8:Value := MySampleData.HairColor;
      9:Value := MySampleData.SkinColor;
      10:Value := MySampleData.Image;
      11:Value := MySampleData.BirthDate;
      12:Value := MySampleData.Performance;
      13:Value := MySampleData.Custom1;
      14:Value := MySampleData.Custom2;
    end;
  end;
end;

procedure TfrmMain.Grid1SetValue(Sender: TObject; const Col, Row: Integer;
  const Value: TValue);
begin
  if Col = 1 then
  begin
    case Row of
      0: MySampleData.Name := Value.AsString;
      1: MySampleData.IsOld :=  Value.AsBoolean;
      2: MySampleData.IsActive := Value.AsBoolean;
      3: MySampleData.IsMan := Value.AsBoolean;
      4: MySampleData.IsMan := not Value.AsBoolean;
      5: ; // read only
      6: MySampleData.City := Value.AsInteger;
      7: MySampleData.Animal := Value.AsInteger;
      8: MySampleData.HairColor := Value.AsInteger;
      9: MySampleData.SkinColor := Value.AsInteger;
      10: ;// read only
      11: MySampleData.BirthDate := Value.AsType<TDate>;
      12: MySampleData.Performance := Value.AsExtended;
      13: MySampleData.Custom1 := Value.AsExtended;
      14: MySampleData.Custom2 := Value.AsExtended;
    end;
  end;
end;

procedure TfrmMain.LayoutCellControl(Sender: Tobject; aRow: Integer;
  var Rect: TRectF);
begin
  if (aRow = 13) or (aRow=14) then Rect.Inflate(-5,-5);
end;

end.
