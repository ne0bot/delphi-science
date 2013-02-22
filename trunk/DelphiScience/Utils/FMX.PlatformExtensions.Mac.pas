unit FMX.PlatformExtensions.Mac;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants,FMX.Types,FMX.Text,MacApi.Appkit,Macapi.CoreFoundation,
  Posix.StdLib, Macapi.Foundation, Macapi.CoreText,Macapi.CocoaTypes,FMX.PlatformExtensions;

Type
  TPlatformExtensionsMac = class(TPlatformExtensions)
  public
    Class Procedure GetSystemFonts(FontList:TStrings);override;
    Class Procedure GetRunningAplications(Applist:TStrings);override;
    Class Procedure GetTextMetrics(Text:String; Font:TFont; var TextRect:TRectF;
                                   var Ascent,Descent:Single;
                                   var CapHeight,XHeight:Single);override;
    Class Procedure ShellOpen(Url:String);override;
  end;

implementation

{ TPlatformExtensionsMac }

class procedure TPlatformExtensionsMac.GetRunningAplications(
  Applist: TStrings);
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

class procedure TPlatformExtensionsMac.GetSystemFonts(FontList: TStrings);
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

class procedure TPlatformExtensionsMac.GetTextMetrics(Text: String; Font: TFont;
  var TextRect: TRectF; var Ascent, Descent, CapHeight, XHeight: Single);
var aRect:TrectF;
    Layout: TTextLayout;
    LFontRef, NewFontRef: CTFontRef;
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
      LFontRef := CTFontCreateWithName(CFSTR(Layout.Font.Family), Layout.Font.Size, nil);
      if TFontStyle.fsBold in Layout.Font.Style then
      begin
        NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, nil,
          kCTFontBoldTrait, kCTFontBoldTrait);
        if Assigned(NewFontRef) then
        begin
          CFRelease(LFontRef);
          LFontRef := NewFontRef;
        end;
      end;
      if TFontStyle.fsItalic in Layout.Font.Style then
      begin
        NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, nil,
          kCTFontItalicTrait, kCTFontItalicTrait);
        if Assigned(NewFontRef) then
        begin
          CFRelease(LFontRef);
          LFontRef := newFontRef;
        end;
      end;
      Ascent := CTFontGetAscent(LFontRef);
      Descent :=CTFontGetDescent(LFontRef);
      CapHeight := CTFontGetCapHeight(LFontRef);
      XHeight := CTFontGetXHeight(LFontRef);
      CFRelease(LFontRef);
      TextRect := aRect;
      TextRect.Offset(-aRect.Left,-aRect.Top);
    finally
      FreeAndNil(Layout);
    end;
  end;

end;

class procedure TPlatformExtensionsMac.ShellOpen(Url: String);
var
  Workspace : NSWorkspace;
  mURL : NSURL;
begin
  Workspace := TNSWorkspace.Create;
  if fileexists(Url) then
  begin
    Workspace.openFile(NSSTR(Url));
  end else begin
    mURL := TNSURL.Create;
    mURL.initWithString(NSSTR(Url));
    Workspace.openURL(mURL);
  end;
end;

end.
