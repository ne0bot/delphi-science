unit FMX.Scene3DForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D,
  FMX.Objects3D, FMX.Materials, FMX.Edit, FMX.MeshObjects, FMX.ListBox,
  FMX.Objects;

type
  Tfrm3dScene = class(TForm)
    Scene3d: TViewport3D;
    UtilsPanel: TPanel;
    MainStBar: TStatusBar;
    StLabel: TLabel;
    MainGrid: TGrid3D;
    CamDummy1: TDummy;
    CamDummy2: TDummy;
    MyCamera: TCamera;
    camLight: TLight;
    DummyStart: TDummy;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    LightMatSrc1: TLightMaterialSource;
    LightMatSrc2: TLightMaterialSource;
    TextureMatSrc1: TTextureMaterialSource;
    TextureMatSrc2: TTextureMaterialSource;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    Button2: TButton;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label2: TLabel;
    TrackBar2: TTrackBar;
    Line1: TLine;
    Label3: TLabel;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    TrackBar3: TTrackBar;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Scene3dMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Scene3dMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Scene3dMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Scene3dMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure ClearScene;
  end;

var
  frm3dScene: Tfrm3dScene;
  FISPressed: Boolean =  False;
  FOldPoint:TPointF;
  Annulus: TAnnulus = Nil;
  Pipe: TPipe = Nil;

  Twister: TTwistModifier = nil;
  Bender1: TBendModifier = nil;
  Bender2: TBendModifier = nil;
  Emboss: TEmbossModifier = nil;


implementation

{$R *.fmx}

procedure Tfrm3dScene.Button1Click(Sender: TObject);
begin
  ClearScene;

  Trackbar1.Enabled := True;
  CheckBox1.Enabled := True;
  CheckBox2.Enabled := True;
  CheckBox3.Enabled := True;
  Trackbar1.Value := 20;
  Checkbox1.IsChecked := False;
  Checkbox2.IsChecked := False;
  Checkbox3.IsChecked := False;

  Annulus := TAnnulus.Create(Self);
  Annulus.Width := 1;
  Annulus.Depth := 1;
  Annulus.Thickness := 0.2;
  Annulus.MaterialSource := LightMatSrc1;
  DummyStart.AddObject(Annulus);

  CamDummy1.AnimateFloat('RotationAngle.Z',10);
  CamDummy2.AnimateFloat('RotationAngle.X',-40);
  MyCamera.AnimateFloat('Position.Z',10);
end;

procedure Tfrm3dScene.Button2Click(Sender: TObject);
begin
  ClearScene;
  Trackbar2.Enabled := True;
  CheckBox4.Enabled := True;
  CheckBox5.Enabled := True;
  CheckBox6.Enabled := True;
  Combobox1.Enabled := True;
  TrackBar3.Enabled := True;
  Trackbar2.Value := 20;
  Checkbox4.IsChecked := False;
  Checkbox5.IsChecked := False;
  Checkbox6.IsChecked := False;
  TRackbar3.Value := 45;
  Combobox1.ItemIndex := 0;
  Button3.Enabled := True;
  Button4.Enabled := True;
  Button5.Enabled := True;
  Button6.Enabled := True;
  Button7.Enabled := True;
  Button8.Enabled := True;


  Pipe := TPipe.Create(Self);
  Pipe.Width := 1;
  Pipe.Depth := 1;
  Pipe.Height := 8;
  Pipe.Thickness := 0.2;
  Pipe.SectionType := TSectionType.sctNone;
  Pipe.SectionDegree := 45;
  Pipe.MaterialSource := LightMatSrc1;
  DummyStart.AddObject(Pipe);

  CamDummy1.AnimateFloat('RotationAngle.Z',10);
  CamDummy2.AnimateFloat('RotationAngle.X',-40);
  MyCamera.AnimateFloat('Position.Z',10);
end;

procedure Tfrm3dScene.Button3Click(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if Pipe.OuterFrameType = ftEllipse then
    begin
      ShowMessage('You can see twist effect best on rectangular pipe. Set the outer frame type to Rectangle!');
    end;
    if assigned(Twister) then begin
      Pipe.Modifiers.Remove(Twister);
      Twister.Free;
    end;
    Twister := TTwistModifier.Create(Pipe);
    Twister.StartPosition := 5;
    Twister.EndPosition := 8;
    Twister.TotalRotation := 360;
    Twister.Subdivisions := 4;
    Pipe.Modifiers.Add(Twister);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Button4Click(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if assigned(Emboss) then begin
      Pipe.Modifiers.Remove(Emboss);
      Emboss.Free;
    end;
    Emboss := TEmbossModifier.Create(Pipe);
    Emboss.StartPosition := 0.5;
    Emboss.EndPosition := 1.0;
    Emboss.ThicknessRatio := 0.1;
    Pipe.Modifiers.Add(Emboss);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Button5Click(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if assigned(Bender1) then begin
      Pipe.Modifiers.Remove(Bender1);
      Bender1.Free;
    end;
    Bender1 := TBendModifier.Create(Pipe);
    Bender1.StartPosition := 1.0;
    Bender1.EndPosition := 2.5;
    Bender1.BendAngle := 90;
    Bender1.Subdivisions := 30;
    Pipe.Modifiers.Add(Bender1);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Button6Click(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if assigned(Bender1) then begin
      Pipe.Modifiers.Remove(Bender1);
      Bender1.Free;
    end;
    Bender1 := TBendModifier.Create(Pipe);
    Bender1.StartPosition := 1.0;
    Bender1.EndPosition := 2.5;
    Bender1.BendAngle := 90;
    Bender1.TurnAngle := 90;
    Bender1.Subdivisions := 30;
    Pipe.Modifiers.Add(Bender1);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Button7Click(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if assigned(Bender2) then begin
      Pipe.Modifiers.Remove(Bender2);
      Bender2.Free;
    end;
    Bender2 := TBendModifier.Create(Pipe);
    Bender2.StartPosition := 2.5;
    Bender2.EndPosition := 4;
    Bender2.BendAngle := -90;
    Pipe.Modifiers.Add(Bender2);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Button8Click(Sender: TObject);
begin
  if assigned(Pipe) then
  begin
    Pipe.ClearModifiers;
    Twister := nil;
    Bender1 := nil;
    Bender2 := nil;
    Emboss := nil;
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox1Change(Sender: TObject);
begin
  if assigned(Annulus) then begin
    if Checkbox1.IsChecked then begin
      Annulus.OuterFrameType := TFrameType.ftRectangle;
    end else begin
      Annulus.OuterFrameType := TFrameType.ftEllipse;
    end;
    Annulus.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox2Change(Sender: TObject);
begin
  if assigned(Annulus) then begin
    if Checkbox2.IsChecked then begin
      Annulus.MaterialSource := TextureMatSrc1;
    end else begin
      Annulus.MaterialSource := LightMatSrc1;
    end;
    Annulus.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox3Change(Sender: TObject);
begin
  if assigned(Annulus) then begin
    if Checkbox3.IsChecked then begin
      Annulus.InnerFrameType := TFrameType.ftRectangle;
    end else begin
      Annulus.InnerFrameType := TFrameType.ftEllipse;
    end;
    Annulus.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox4Change(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if Checkbox4.IsChecked then begin
      Pipe.MaterialSource := TextureMatSrc2;
    end else begin
      Pipe.MaterialSource := LightMatSrc1;
    end;
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox5Change(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if Checkbox5.IsChecked then begin
      Pipe.OuterFrameType := TFrameType.ftRectangle;
    end else begin
      Pipe.OuterFrameType := TFrameType.ftEllipse;
    end;
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.CheckBox6Change(Sender: TObject);
begin
  if assigned(Pipe) then begin
    if Checkbox6.IsChecked then begin
      Pipe.InnerFrameType := TFrameType.ftRectangle;
    end else begin
      Pipe.InnerFrameType := TFrameType.ftEllipse;
    end;
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.ClearScene;
begin
  DummyStart.DeleteChildren;
  Annulus := Nil;
  Pipe := Nil;
  Twister := nil;
  Bender1 := nil;
  Bender2 := nil;
  Emboss := nil;
  Trackbar1.Enabled := False;
  CheckBox1.Enabled := False;
  CheckBox2.Enabled := False;
  CheckBox3.Enabled := False;
  Trackbar2.Enabled := False;
  CheckBox4.Enabled := False;
  CheckBox5.Enabled := False;
  CheckBox6.Enabled := False;
  Combobox1.Enabled := False;
  Trackbar3.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button5.Enabled := False;
  Button6.Enabled := False;
  Button7.Enabled := False;
  Button8.Enabled := False;
end;

procedure Tfrm3dScene.ComboBox1Change(Sender: TObject);
begin
  if assigned(Pipe) then
  begin
    Pipe.SectionType := TSectionType(Combobox1.ItemIndex);
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.Scene3dMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsPressed := True;
  FOldPoint := PointF(X,Y);
end;

procedure Tfrm3dScene.Scene3dMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FIsPressed then
  begin
    CamDummy1.RotationAngle.Z := CamDummy1.RotationAngle.Z + (X-FOldPoint.X)*0.5;
    CamDummy2.RotationAngle.X := CamDummy2.RotationAngle.X + (Y-FOldPoint.Y)*0.5;
    FOldPoint := PointF(X,Y);
  end;
end;

procedure Tfrm3dScene.Scene3dMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsPressed := False;
end;

procedure Tfrm3dScene.Scene3dMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  MyCamera.Position.Vector := MyCamera.Position.Vector +
                             Vector3D(0, 0, 1).Scale((WheelDelta / 40) * 0.8);

end;

procedure Tfrm3dScene.TrackBar1Change(Sender: TObject);
begin
  if assigned(Annulus) then begin
    Annulus.Thickness := TRackbar1.Value/100;
    Annulus.Repaint;
  end;
end;

procedure Tfrm3dScene.TrackBar2Change(Sender: TObject);
begin
  if assigned(Pipe) then begin
    Pipe.Thickness := TRackbar2.Value/100;
    Pipe.Repaint;
  end;
end;

procedure Tfrm3dScene.TrackBar3Change(Sender: TObject);
begin
  if assigned(Pipe) then begin
    Pipe.SectionDegree := Round(TRackbar3.Value);
    Pipe.Repaint;
  end;
end;

end.
