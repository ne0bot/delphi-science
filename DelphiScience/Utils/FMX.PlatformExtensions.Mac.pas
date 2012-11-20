unit FMX.PlatformExtensions.Mac;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,MacApi.Appkit,Macapi.CoreFoundation,Macapi.Foundation,
  FMX.PlatformExtensions;

Type
  TPlatformExtensionsMac = class(TPlatformExtensions)
  public
    Class Procedure GetSystemFonts(FontList:TStringlist);override;
  end;

implementation

{ TPlatformExtensionsMac }

class procedure TPlatformExtensionsMac.GetSystemFonts(FontList: TStringlist);
var
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
begin
  fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fManager.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      FontList.Add(String(lItem.UTF8String))
    end;
  end;
end;

end.
