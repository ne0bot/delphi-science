program NNSingleDemo;

uses
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  AI.NN.Neuron in '..\AIUtils\AI.NN.Neuron.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
