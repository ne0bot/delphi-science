unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, FMX.Layouts, FMX.Memo,OnlineRandomizer,
  FMX.Objects,System.Math;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PaintBox1: TPaintBox;
    GroupBox1: TGroupBox;
    Text1: TText;
    Button2: TButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    Button3: TButton;
    Text2: TText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    Procedure CalculateControlPoints(points:Array of TpointF;var cPoints:Array of TPointF);
    Procedure AnalyzeErrorDist;
  public
    { Public declarations }
    GroupErrors: Array of Integer;
    ErrorMax: Integer;
    cntTotalHead:Integer;
    cntTotalTail: Integer;
    totalError: Single;

  end;

var
  Form1: TForm1;
  rndGen:TOnlineRandomizer;

implementation

{$R *.fmx}

procedure TForm1.AnalyzeErrorDist;
var cntGroup,errorStep,i,cntZero:Integer;
    digit: String;
    error: Integer;
begin
  errorStep := 14;
  SetLength(GroupErrors,0);
  cntTotalHead := 0;
  cntTotalTail := 0;;
  totalError := 0;

  if not assigned(rndGen) then exit;
  cntGroup := rndGen.Count div errorStep;
  if cntGroup = 0 then exit;
  cntZero := 0;

  SetLength(GroupErrors,errorStep+1);

  for i := 0 to RndGen.Count-1 do begin
    digit := rndGen.RandomString[i];
    if digit = '0' then begin
      inc(cntZero);
      inc(cntTotalHead);
    end else inc (cntTotalTail);
    if ((i+1) mod errorStep) = 0 then
    begin
      error := (errorStep div 2) - cntZero;
      inc(GroupErrors[error+errorStep div 2]);
      cntZero := 0;
    end;
  end;
  totalError := ((rndGen.Count div 2) - cntTotalHead) / rndGen.Count;

  errorMax := 1;

  for I := 0 to errorStep do
    if GroupErrors[i] > errorMax then errorMax := GroupErrors[i];
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  self.Cursor := crHourGlass;
  rndGen.GetRandomIntegers(0,1,10000);
  Label1.Text := inttostr(rndGen.Count)+' digits analyzed. Heads:'+ inttostr(cntTotalHead)+' Tails:'+inttostr(cntTotalTail)+' Error:'+floattostr(totalError);
  AnalyzeErrorDist;
  PaintBox1.Repaint;

  self.Cursor := crDefault;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  rndGen.Clear;
  Label1.Text := inttostr(rndGen.Count)+' digits to analyze';
  AnalyzeErrorDist;
  PaintBox1.Repaint;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  self.Cursor := crHourGlass;
  rndGen.GetPseudoRandomIntegers(0,1,10000);
  AnalyzeErrorDist;
  Label1.Text := inttostr(rndGen.Count)+' digits analyzed. Heads:'+ inttostr(cntTotalHead)+' Tails:'+inttostr(cntTotalTail)+' Error:'+floattostr(totalError);
  PaintBox1.Repaint;
  self.Cursor := crDefault;
end;

procedure TForm1.CalculateControlPoints(points: array of TpointF;
  var cPoints: array of TPointF);
var i,cnt:Integer;
    cp1,cp2:TPointF;
    p0,p1,p2: TpointF;
    da,ra: Single;


    Function AngleBetweenLines(ap1,ap0,ap2:TPointf):Single;
    var cosa:Single;
    begin
      ap1 := Pointf(ap1.X-ap0.X,ap1.Y-ap0.Y);
      ap2 := Pointf(ap2.X-ap0.X,ap2.Y-ap0.Y);
      cosa := (ap1.X*ap2.X+ap1.Y*ap2.Y)/(sqrt(ap1.x*ap1.x+ap1.Y*ap1.Y)*sqrt(ap2.x*ap2.x+ap2.Y*ap2.Y));
      result := arccos(cosa);
    end;

    Function RotatePoint(ap1,ap0:TPointF;angle:Single):TPointF;
    var x1,y1:Single;
    begin
      ap1 := Pointf(ap1.X-ap0.X,ap1.Y-ap0.Y);
      x1 := (ap1.X * cos(angle)) - (ap1.Y * sin(angle));
      y1 := (ap1.x * sin(angle)) + (ap1.y * cos(angle));
      result := Pointf(x1+p0.X,y1+p0.Y);
    end;


begin
  cnt := Length(Points);
  cPoints[0] := points[0];
  cPoints[High(cPoints)] := points[high(points)];
  for i := 1 to cnt -2 do
  begin
    p1 := points[i-1];
    p0 := points[i];
    p2 := points[i+1];

    da := AngleBetweenLines(p1,p0,p2);
    ra := (pi - da) / 2;

    cp1 := RotatePoint(p1,p0,ra);
    cp2 := RotatePOint(p2,p0,-ra);

    da := AngleBetweenLines(cp1,p0,cp2);
    if abs(da-pi) > 0.001 then begin
      cp1 := RotatePoint(p1,p0,-ra);
      cp2 := RotatePOint(p2,p0,ra);
    end;

    cp1 := Pointf((cp1.X+p0.x)/2,(cp1.y+p0.y)/2);
    cp2 := Pointf((cp2.X+p0.x)/2,(cp2.y+p0.y)/2);

    cp1 := Pointf((cp1.X+p0.x)/2,(cp1.y+p0.y)/2);
    cp2 := Pointf((cp2.X+p0.x)/2,(cp2.y+p0.y)/2);

    cPoints[i*2-1] := cp1;
    cPoints[i*2] := cp2;

  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  rndGen := TOnlineRandomizer.Create;
  AnalyzeErrorDist;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var i,errorStep: Integer;
    ex,ey:Single;
    pathData:TPathData;
    points:array of TPointF;
    cp1,cp2,p:TPointF;
    cPoints:array of TPointF;
begin
  ErrorStep := Length(groupErrors)-1;
  if ErrorStep = -1 then exit;


  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  Canvas.Fill.Color := TalphaCOlors.Red;
  Canvas.Stroke.Kind := TbrushKind.bkSolid;
  Canvas.Stroke.Color := TAlphaColors.Black;
  Canvas.StrokeThickness := 0.5;
  Canvas.DrawLine(POintF(0,PaintBox1.Height-1),PointF(PaintBox1.Width,PaintBox1.Height-1),1);
  ex := (ErrorStep div 2)*((PaintBox1.Width-10)/ErrorStep)+5;
  Canvas.DrawLine(POintF(ex,PaintBox1.Height-1),PointF(ex,0),1);

  SetLength(points,errorStep+1);
  for I := 0 to errorStep do
  begin
    if i=(errorstep div 2) then begin
       Canvas.Stroke.Color := TAlphaColors.Red;
    end else begin
       Canvas.Stroke.Color := TAlphaColors.Black;
    end;
    ex := i*((PaintBox1.Width-10)/ErrorStep)+5;
    ey := (groupErrors[i]/errorMax)*(Paintbox1.Height-60)+5;
    ey := Paintbox1.Height - ey;
    points[i] := PointF(ex,ey);
    Canvas.DrawEllipse(RectF(ex-2,ey-2,ex+2,ey+2),1);
  end;

  SetLength(cPoints,ErrorStep*2);
  CalculateControlPoints(points,cPoints);

  pathData := TPathData.Create;
  pathData.MoveTo(points[0]);
  for i := 1 to high(Points) do
  begin
    p := points[i];
    cp1 := cPoints[i*2-2];
    cp2 := cPoints[i*2-1];
    pathData.CurveTo(cp1,cp2,p);
  end;
  Canvas.DrawPath(pathData,1);
  pathData.Free;
  Canvas.EndScene;

end;

end.
