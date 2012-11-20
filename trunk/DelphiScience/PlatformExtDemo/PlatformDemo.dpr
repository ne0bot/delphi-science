program PlatformDemo;

uses
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  FMX.PlatformExtensions in '..\Utils\FMX.PlatformExtensions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
