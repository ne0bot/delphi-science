unit FMX.PlatformExtensions;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,FMX.Layouts;

Type

  TPlatformExtensions = class(TObject)
  public
    Class Procedure GetSystemFonts(FontList:TStrings);virtual;abstract;
    Class Procedure GetRunningAplications(Applist:TStrings);virtual;abstract;
    Class Procedure GetTextMetrics(Text:String; Font:TFont; var TextRect:TRectF;
                                   var Ascent,Descent:Single;
                                   var CapHeight,XHeight:Single);virtual;abstract;
    Class Procedure ShellOpen(Url:String);virtual;abstract;
  end;

var
  PlatformExtensions:TPlatformExtensions;

implementation

uses
  {$IFDEF MACOS}
  FMX.PlatformExtensions.Mac;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FMX.PlatformExtensions.Win;
  {$ENDIF}


initialization
  {$IFDEF MACOS}
  PlatformExtensions := TPlatformExtensionsMac.Create;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  PlatformExtensions := TPlatformExtensionsWin.Create;
  {$ENDIF}
finalization
  PlatformExtensions.Free;

end.
