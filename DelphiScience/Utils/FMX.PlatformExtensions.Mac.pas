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
    Class Procedure GetRunningAplications(Applist:TStringlist);override;
  end;

implementation

{ TPlatformExtensionsMac }

class procedure TPlatformExtensionsMac.GetRunningAplications(
  Applist: TStringlist);
var
  fWorkSpace:NSWorkSpace;
  list:NSArray;
  i: Integer;
  lItem:NSDictionary;
  key,value: NSString;
begin
  fWorkSpace := TNsWorkspace.Wrap(TNsWorkSpace.OCClass.sharedWorkspace);
  list := fWorkspace.launchedApplications;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to list.count-1 do
    begin
      lItem := TNSDictionary.Wrap(List.objectAtIndex(i));
      key := NSSTR(String(PAnsiChar(UTF8Encode('NSApplicationBundleIdentifier'))));
      // You can also use NSApplicationPath or NSApplicationName
      value := TNSString.Wrap(lItem.valueForKey(key));
      Applist.Add(String(value.UTF8String));
    end;
  end;
end;

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
