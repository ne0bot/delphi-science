unit OnlineRandomizer;

interface

Uses System.SysUtils, System.Types, System.UITypes, System.Classes,
System.Variants,System.StrUtils,IdHttp,IdStream;

Type
  TOnlineRandomizer = class(TObject)
  private
    FResultList: TStringList;
    FIntUrlPattern:String;
    FDecUrlPattern:String;
    FSettings:TFormatSettings;
    Procedure GetResultsFromWeb(qUrl:String);
    function GetRandomInteger(Index: Integer): Integer;
    function GetRandomDecimal(Index: Integer): Single;
    function GetCount: Integer;
    function GetRandomString(Index: Integer): String;
  public
    Constructor Create;
    Procedure Clear;
    Destructor Destroy;override;
    Procedure GetRandomIntegers(iMin,iMax,iCount:Integer);
    Procedure GetPseudoRandomIntegers(iMin,iMax,iCount:Integer);
    Procedure GetRandomDecimals(dPrecission,dCount:Integer);
    Procedure GetPseudoRandomDecimals(dPrecission,dCount:Integer);
    Property Count:Integer read GetCount;
    Property RandomInteger[Index:Integer]:Integer read GetRandomInteger;
    Property RandomDecimal[Index:Integer]:Single read GetRandomDecimal;
    Property RandomString[Index:Integer]:String read GetRandomString;
  end;

implementation

{ TOnlineRandomizer }

procedure TOnlineRandomizer.Clear;
begin
  FresultList.Clear;
end;

constructor TOnlineRandomizer.Create;
begin
  inherited Create;

  FIntUrlPattern := 'http://www.random.org/integers/'+
                    '?min=%d&max=%d&num=%d&col=1&base=10&format=plain&rnd=new';
  FDecUrlPattern := 'http://www.random.org/decimal-fractions/'+
                    '?dec=%d&num=%d&col=1&format=plain&rnd=new';
  FResultList := TSTringList.Create;
  FSettings := TFormatSettings.Create;
  FSettings.DecimalSeparator := '.';
end;

destructor TOnlineRandomizer.Destroy;
begin
  FResultList.Free;
  inherited;
end;

function TOnlineRandomizer.GetCount: Integer;
begin
  Result := FResultList.Count;
end;

procedure TOnlineRandomizer.GetPseudoRandomDecimals(dPrecission,
  dCount: Integer);
var
  I: Integer;
  rndDec:Single;
begin
  Randomize;
  for I := 1 to dCount do
  begin
    rndDec :=   Random;
    FResultList.Add(FloatToStr(rndDec,fSettings));
  end;
end;

procedure TOnlineRandomizer.GetPseudoRandomIntegers(iMin, iMax,
  iCount: Integer);
var
  I,rndInt: Integer;
begin
  Randomize;
  for I := 1 to iCount do
  begin
    rndInt := Random(iMax-iMin+1)+iMin;
    FResultList.Add(intToStr(rndInt));
  end;
end;

function TOnlineRandomizer.GetRandomDecimal(Index: Integer): Single;
begin
  result := StrToFloat(FResultList[Index],fSettings);
end;

procedure TOnlineRandomizer.GetRandomDecimals(dPrecission, dCount: Integer);
var qUrl: String;
begin
  qUrl := Format(FDecUrlPattern,[dPrecission, dCount]);
  Self.GetResultsFromWeb(qUrl);
end;

function TOnlineRandomizer.GetRandomInteger(Index: Integer): Integer;
begin
  result := StrToInt(FResultList[Index]);
end;

procedure TOnlineRandomizer.GetRandomIntegers(iMin, iMax, iCount: Integer);
var qUrl: String;
begin
  qUrl := Format(FIntUrlPattern,[iMin,iMax,iCount]);
  Self.GetResultsFromWeb(qUrl);
end;

function TOnlineRandomizer.GetRandomString(Index: Integer): String;
begin
  Result := FresultList[Index];
end;

procedure TOnlineRandomizer.GetResultsFromWeb(qUrl: String);
var httpObj:TIdHttp;
    FResultStream: TMemoryStream;
    Flist: TStringList;
begin
  FResultStream := TMemoryStream.Create;
  FList := TStringList.Create;
  httpObj := TIdHttp.Create(nil);
  httpObj.Get(qUrl,FResultStream);
  FResultStream.Position := 0;
  FList.LoadFromStream(FResultStream);
  FResultStream.Free;
  FResultList.AddStrings(FList);
  FList.Free;
  httpObj.Free;
end;

end.
