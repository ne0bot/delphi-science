unit FMX.PlatformExtensions.Win;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,FMX.Types,FMX.Text,FMX.Canvas.GDIP,Winapi.Windows,
  WinApi.Messages,WinApi.TlHelp32,Winapi.GDIPOBJ,Winapi.GDIPAPI,
  Winapi.ShellAPI,FMX.PlatformExtensions;

Type

  TPlatformExtensionsWin = class(TPlatformExtensions)
  public
    Class Procedure GetSystemFonts(FontList:TStrings);override;
    Class Procedure GetRunningAplications(Applist:TStrings);override;
    Class Procedure GetTextMetrics(Text:String; Font:TFont; var TextRect:TRectF;
                                   var Ascent,Descent:Single;
                                   var CapHeight,XHeight:Single);override;
    Class Procedure ShellOpen(Url:String);override;
  end;

implementation

{ TPlatformExtensionsWin }

function EnumFontsList(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  List: TStrings;
  fName: string;
begin
  List := TStrings(Data);
  fName := LogFont.lfFaceName;
  if (List.Count = 0) or (AnsiCompareText(List[List.Count-1], fName) <> 0) then
    List.Add(fName);
  Result := 1;
end;

class procedure TPlatformExtensionsWin.GetRunningAplications(
  Applist: TStrings);
var
 PE: TProcessEntry32;
 Snap: THandle;
 fName: String;
begin
  pe.dwsize:=sizeof(PE);
  Snap:= CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snap <> 0 then
  begin
    if Process32First(Snap, PE) then
    begin
     fName := String(PE.szExeFile);
     Applist.Add(fName);
     while Process32Next(Snap, PE) do
     begin
       fName := String(PE.szExeFile);
       Applist.Add(fName);
     end;
    end;
    CloseHandle(Snap);
  end;
end;

class procedure TPlatformExtensionsWin.GetSystemFonts(FontList: TStrings);
var
  dContext: HDC;
  LFont: TLogFont;
begin
  dContext := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(dContext, LFont, @EnumFontsList, Winapi.Windows.LPARAM(FontList), 0);
  ReleaseDC(0, dContext);
end;

class procedure TPlatformExtensionsWin.GetTextMetrics(Text: String; Font: TFont;
  var TextRect: TRectF; var Ascent, Descent, CapHeight, XHeight: Single);
var RttiField: TRttiField;
    RttiContext: TRttiContext;
    FGPFont:TGPFont;
    FGPFamily:TGPFontFamily;
    FGraphics: TGPGraphics;
    emHeight,fSize,
    cAscent,cDescent,lSpacing,iLeading,eLeading:Single;
    FontStyle:Integer;
    aRect: TRectf;
    Layout: TTextLayout;
begin
  inherited;
  aRect := RectF(0,0,1000,1000);
  if Text.IsEmpty then
  begin
    ARect.Right := ARect.Left;
    ARect.Bottom := ARect.Top;
  end else begin
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      Layout.BeginUpdate;
      Layout.TopLeft := ARect.TopLeft;
      Layout.MaxSize := PointF(ARect.Width, ARect.Height);
      Layout.Text := Text;
      Layout.WordWrap := False;
      Layout.HorizontalAlign := TTextAlign.taCenter;
      Layout.VerticalAlign := TTextAlign.taCenter;
      Layout.Font := Font;
      Layout.RightToLeft := False;
      Layout.EndUpdate;
      ARect := Layout.TextRect;

      RttiField := RttiContext.GetType(Layout.ClassType).GetField('FGPFont');
      if assigned(RttiField) then
      begin
        FGPFont := TGpFont(RttiField.GetValue(Layout).AsObject);
        RttiField := RttiContext.GetType(Layout.ClassType).GetField('FGraphics');
        FGraphics := TGpGraphics(RttiField.GetValue(Layout).AsObject);

        FGPFamily := TGPFontFamily.Create;
        FGPFont.GetFamily(FGPFamily);

        //fSize := Layout.Font.Size;
        fSize := FGPFont.GetHeight(FGraphics);
        FontStyle := FontStyleRegular;
        If System.UITypes.TFontStyle.fsBold in Layout.Font.Style then FontStyle := FontStyle or FontStyleBold;
        If System.UITypes.TFontStyle.fsItalic in Layout.Font.Style then FontStyle := FontStyle or FontStyleItalic;

        cAscent := FGpFamily.GetCellAscent(FontStyle);
        cDescent := FGpFamily.GetCellDescent(FontStyle);
        emHeight := FGpFamily.GetEmHeight(FontStyle);
        lSpacing := FGPFamily.GetLineSpacing(FontStyle);
        iLeading := cAscent+cDescent - emHeight;
        eLeading := lSpacing - cAscent-cDescent;
        Ascent := fSize * (cAscent)/lSpacing;
        Descent := fSize * (descent)/lSpacing;
        CapHeight := fSize * (cAscent-iLeading)/lSpacing;
        XHeight :=  fsize * (cAscent-iLeading) *(7/10) /lSpacing;
        FreeAndNil(FGPFamily);
      end;
      TextRect := aRect;
      TextRect.Offset(-aRect.Left,-aRect.Top);
    finally
      FreeAndNil(Layout);
    end;
  end;
end;

class procedure TPlatformExtensionsWin.ShellOpen(Url: String);
begin
  ShellExecute(0, 'OPEN', PChar(Url), '', '', SW_SHOWNORMAL);
end;

end.
