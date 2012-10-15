program RandomNature;

uses
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  OnlineRandomizer in '..\Utils\OnlineRandomizer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
