program VairantColumnDemo;

uses
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  FMX.VariantColumn in '..\Utils\FMX.VariantColumn.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
