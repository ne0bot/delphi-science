unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.ListBox,FMX.PlatformExtensions, FMX.Memo, FMX.TabControl,
  FMX.Objects;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    ListBox1: TListBox;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    Label2: TLabel;
    TabItem2: TTabItem;
    Label3: TLabel;
    ListBox2: TListBox;
    TabItem3: TTabItem;
    PaintBox1: TPaintBox;
    Layout1: TLayout;
    TrackBar1: TTrackBar;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    PaintBox2: TPaintBox;
    Line1: TLine;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure TrackBar1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
  Paintbox1.Repaint;
  Paintbox2.Repaint;
end;

procedure TfrmMain.ComboBox2Change(Sender: TObject);
begin
  Paintbox1.Repaint;
  Paintbox2.Repaint;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var fList:TStringlist;
    i: Integer;
begin
  fList := TStringList.Create;
  PlatformExtensions.GetSystemFonts(fList);
  Listbox1.BeginUpdate;
  Combobox1.BeginUpdate;
  for i := 0 to fList.Count -1 do
  begin
     ListBox1.Items.Add(fList[i]);
     Combobox1.Items.Add(fList[i]);
  end;
  ListBox1.EndUpdate;
  Combobox1.EndUpdate;
  Combobox1.ItemIndex := Combobox1.Items.IndexOf('Cambria');
  fList.Free;

  fList := TStringList.Create;
  PlatformExtensions.GetRunningAplications(fList);
  Listbox2.BeginUpdate;
  for i := 0 to fList.Count -1 do
  begin
     ListBox2.Items.Add(fList[i]);
  end;
  ListBox2.EndUpdate;
  fList.Free;
end;

procedure TfrmMain.ListBox1Change(Sender: TObject);
var fName:String;
begin
  fName := listbox1.Items[listbox1.ItemIndex];
  Memo1.Font.Size := 24;
  Memo1.Font.Family := fName;
end;

procedure TfrmMain.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var textRect:TRectF;
    Ascent, Descent, CapHeight, XHeight: Single;
    TestTxt:String;
    txtX,txtY:Single;
    dy: Single;
begin
  TestTxt := 'AbxyÂ';
  PaintBox1.Canvas.Font.Family := Combobox1.Items[Combobox1.ItemIndex];
  PaintBox1.Canvas.Font.Size := Trackbar1.Value;
  PaintBox1.Canvas.Font.Style := [];
  case Combobox2.ItemIndex of
    1:PaintBox1.Canvas.Font.Style := [TFontStyle.fsBold];
    2:PaintBox1.Canvas.Font.Style := [TFontStyle.fsItalic];
    3:PaintBox1.Canvas.Font.Style := [TFontStyle.fsBold,TFontStyle.fsItalic];
  end;

  PaintBox1.Canvas.Fill.Color := TAlphaColors.Black;
  PlatformExtensions.GetTextMetrics(TestTxt,PaintBox1.Canvas.Font,textRect,
                                    Ascent, Descent, CapHeight, XHeight);
  txtX := (PaintBox1.Width-TextRect.Width)/2;
  txtY := (PaintBox1.Height-TextRect.Height)/2;
  Paintbox1.Canvas.FillText(RectF(txtX,txtY,txtX+TextRect.Width,txtY+TextRect.Height),
                            testTxt,False,1.0,[],TTextAlign.taCenter);
  TextRect.Offset(txtX,txtY);

  PaintBox1.Canvas.Stroke.Color := TAlphaColors.Red;
  PaintBox1.Canvas.StrokeThickness := 1;
  Paintbox1.Canvas.DrawRect(TextRect,0,0,[],1.0);

  dy := Ascent; // draw baseline
  PaintBox1.Canvas.DrawLine(PointF(TextRect.Left,TextRect.Top+dy),
                            PointF(TextRect.Right,TextRect.Top+dy),1.0);
  dy := Ascent-CapHeight; // draw Cap LevelLine
  PaintBox1.Canvas.DrawLine(PointF(TextRect.Left,TextRect.Top+dy),
                            PointF(TextRect.Right,TextRect.Top+dy),1.0);
  dy := Ascent-XHeight; // draw XHeight LevelLine
  PaintBox1.Canvas.DrawLine(PointF(TextRect.Left,TextRect.Top+dy),
                            PointF(TextRect.Right,TextRect.Top+dy),1.0);
  // while XHeight is provided with MacOsX Api, it is not provided with in GDIPlus APi
  // so in in Windows platform X Height is calculated from CapHeight using a statistical
  // multiplier

end;

procedure TfrmMain.PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
var textRect1,textRect2,textRect3:TRectF;
    Ascent1,Ascent2,Ascent3, Descent, CapHeight, XHeight: Single;
    TestTxt:String;
    txtX,txtY:Single;
    dy: Single;
    BaseLine:Single;
    totalWidth: Single;
begin

  PaintBox2.Canvas.Font.Family := Combobox1.Items[Combobox1.ItemIndex];

  PaintBox2.Canvas.Font.Style := [];
  case Combobox2.ItemIndex of
    1:PaintBox2.Canvas.Font.Style := [TFontStyle.fsBold];
    2:PaintBox2.Canvas.Font.Style := [TFontStyle.fsItalic];
    3:PaintBox2.Canvas.Font.Style := [TFontStyle.fsBold,TFontStyle.fsItalic];
  end;
  PaintBox2.Canvas.Fill.Color := TAlphaColors.Black;
  PaintBox2.Canvas.Stroke.Color := TAlphaColors.Red;
  PaintBox2.Canvas.StrokeThickness := 1;

  PaintBox2.Canvas.Font.Size := 40;
  TestTxt := 'Big ';
  PlatformExtensions.GetTextMetrics(TestTxt,PaintBox2.Canvas.Font,textRect1,
                                    Ascent1, Descent, CapHeight, XHeight);


  PaintBox2.Canvas.Font.Size := 20;
  TestTxt := 'Small';
  PlatformExtensions.GetTextMetrics(TestTxt,PaintBox2.Canvas.Font,textRect2,
                                    Ascent2, Descent, CapHeight, XHeight);

  PaintBox2.Canvas.Font.Size := 30;
  TestTxt := ' Medium';
  PlatformExtensions.GetTextMetrics(TestTxt,PaintBox2.Canvas.Font,textRect3,
                                    Ascent3, Descent, CapHeight, XHeight);

  totalWidth := textRect1.Width+textRect2.Width+textRect3.Width;
  txtX := (PaintBox2.Width - totalWidth) /2;
  txtY := (PaintBox2.Height-TextRect1.Height)/2;
  baseLine := txtY+Ascent1;

  PaintBox2.Canvas.DrawLine(PointF(0,baseLine),
                            PointF(Paintbox2.Width,baseLine),1.0);

  PaintBox2.Canvas.Font.Size := 40;
  TestTxt := 'Big ';
  Paintbox2.Canvas.FillText(RectF(txtX,txtY,txtX+TextRect1.Width,txtY+TextRect1.Height),
                            testTxt,False,1.0,[],TTextAlign.taCenter);

  txtX := txtX+TextRect1.Width;
  txtY := baseLine-Ascent2;
  PaintBox2.Canvas.Font.Size := 20;
  TestTxt := 'Small';
  Paintbox2.Canvas.FillText(RectF(txtX,txtY,txtX+TextRect2.Width,txtY+TextRect2.Height),
                            testTxt,False,1.0,[],TTextAlign.taCenter);

  txtX := txtX+TextRect2.Width;
  txtY := baseLine-Ascent3;
  PaintBox2.Canvas.Font.Size := 30;
  TestTxt := ' Medium';
  Paintbox2.Canvas.FillText(RectF(txtX,txtY,txtX+TextRect3.Width,txtY+TextRect3.Height),
                            testTxt,False,1.0,[],TTextAlign.taCenter);

end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
  Paintbox1.Repaint;
end;

end.
