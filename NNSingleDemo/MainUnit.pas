unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,AI.NN.Neuron, FMX.Edit,
  FMX.Grid, FMX.Layouts;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SpinBox1: TSpinBox;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    Label2: TLabel;
    Switch1: TSwitch;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Label5: TLabel;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure SpinBox1ChangeTracking(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StringGrid1EdititingDone(Sender: TObject; const Col,
      Row: Integer);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure DoAndGate;
    Procedure DoOrGate;
    Procedure ShowNeuron;
  end;

var
  frmMain: TfrmMain;
  MyNeuron: TNeuron = nil;

implementation

{$R *.fmx}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  MyNeuron.RandomInitialize;
  ShowNeuron;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  ShowMessage('The Neuron will be arranged as to represent the AND logical '+
              'gate. Just change the Input values (0,1) and see the logical '+
              'result in output box');
  DoAndGate;
  ShowNeuron;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  ShowMessage('The Neuron will be arranged as to represent the OR logical '+
              'gate. Just change the Input values (0,1) and see the logical '+
              'result in output box');
  DoOrGate;
  ShowNeuron;
end;

procedure TfrmMain.DoAndGate;
begin
  MyNeuron.InputCount := 2;
  MyNeuron.Weights[0] := 1;
  MyNeuron.Weights[1] := 1;
  MyNeuron.Threshold := 1.5;
  MyNeuron.Inputs[0] := 0;
  MyNeuron.Inputs[1] := 1;
end;

procedure TfrmMain.DoOrGate;
begin
  MyNeuron.InputCount := 2;
  MyNeuron.Weights[0] := 1;
  MyNeuron.Weights[1] := 1;
  MyNeuron.Threshold := 0.5;
  MyNeuron.Inputs[0] := 0;
  MyNeuron.Inputs[1] := 1;
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
begin
  MyNeuron.Threshold := StrToFloatDef(Edit1.Text,0);
  ShowNeuron;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  MyNeuron := TNeuron.Create(4);
  ShowNeuron;
end;

procedure TfrmMain.ShowNeuron;
var i: Integer;
begin
  SpinBox1.Value := MyNeuron.InputCount;
  StringGrid1.RowCount := MyNeuron.InputCount;
  for i := 0 to MyNeuron.InputCount-1 do
  begin
    StringGrid1.Cells[0,i] := IntToStr(i+1);
    StringGrid1.Cells[1,i] := FloatToStr(MyNeuron.Weights[i]);
    StringGrid1.Cells[2,i] := FloatToStr(MyNeuron.Inputs[i]);
  end;

  Edit1.Text := FloatToStr(MyNeuron.Threshold);
  Edit2.Text := FloatToStr(MyNeuron.Output);
end;

procedure TfrmMain.SpinBox1ChangeTracking(Sender: TObject);
begin
  if assigned(myNeuron) then
  begin
    MyNeuron.InputCount := Round(SpinBox1.Value);
    ShowNeuron;
  end;
end;

procedure TfrmMain.StringGrid1EdititingDone(Sender: TObject; const Col,
  Row: Integer);
var value: Single;
begin
  Value := StrToFloatDef(StringGrid1.Cells[Col,Row],0);
  case Col of
    1:MyNeuron.Weights[Row] := Value;
    2:MyNeuron.Inputs[Row] := Value;
  end;
  ShowNeuron;
end;

procedure TfrmMain.Switch1Switch(Sender: TObject);
begin
  MyNeuron.AnalogOutput := Switch1.IsChecked;
  ShowNeuron;
end;

end.
