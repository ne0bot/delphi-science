unit FMX.PlatformExtensions.Win;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,Winapi.Windows,WinApi.Messages,WinApi.TlHelp32, FMX.PlatformExtensions;

Type

  TPlatformExtensionsWin = class(TPlatformExtensions)
  public
    Class Procedure GetSystemFonts(FontList:TStringlist);override;
    Class Procedure GetRunningAplications(Applist:TStringlist);override;
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
  Applist: TStringlist);
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

class procedure TPlatformExtensionsWin.GetSystemFonts(FontList: TStringlist);
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

end.
