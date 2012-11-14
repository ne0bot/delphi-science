program CustomMeshDemo;

uses
  FMX.Forms,
  FMX.MeshObjects in '..\3dUtils\FMX.MeshObjects.pas',
  FMX.Scene3DForm in 'FMX.Scene3DForm.pas' {frm3dScene};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm3dScene, frm3dScene);
  Application.Run;
end.
