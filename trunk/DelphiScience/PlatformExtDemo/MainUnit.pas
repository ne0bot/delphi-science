unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.ListBox,FMX.PlatformExtensions, FMX.Memo, FMX.TabControl;

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
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
var fList:TStringlist;
    i: Integer;
begin
  fList := TStringList.Create;
  PlatformExtensions.GetSystemFonts(fList);
  Listbox1.BeginUpdate;
  for i := 0 to fList.Count -1 do
  begin
     ListBox1.Items.Add(fList[i]);
  end;
  ListBox1.EndUpdate;
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

end.
