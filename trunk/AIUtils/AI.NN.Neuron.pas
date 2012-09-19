unit AI.NN.Neuron;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes,
System.Variants,System.Math;

type
TNeuron = class(TObject)
  private
    FThreshold: Single;
    FInputCount: Integer;
    FInputs:TSingleDynArray;
    FWeights:TSingleDynArray;
    FAnalogOutput: Boolean;
    function GetInput(Index: Integer): Single;
    function GetOutput: Single;
    function GetWeight(Index: Integer): Single;
    procedure SetInput(Index: Integer; const Value: Single);
    procedure SetWeight(Index: Integer; const Value: Single);
    procedure SetInputCount(const Value: Integer);
    function GetLoad: Single;
  public
    Constructor Create(aInputCount:Integer);
    procedure RandomInitialize;
    Property Load:Single read GetLoad;
    property Output:Single read GetOutput;
    property Threshold:Single read FThreshold write FThreshold;
    property Inputs[Index: Integer]: Single read GetInput write SetInput;
    property Weights[Index: Integer]: Single read GetWeight write SetWeight;
    property InputCount:Integer read FInputCount write SetInputCount;
    property AnalogOutput:Boolean read FAnalogOutput write FAnalogOutput;
end;

implementation

{ TNeuron }

constructor TNeuron.Create(aInputCount:Integer);
begin
  inherited Create;
  FAnalogOutput := False;
  InputCount := aInputCount;
  RandomInitialize;
end;

function TNeuron.GetInput(Index: Integer): Single;
begin
  Result := 0;
  if (Index > -1) and (Index < FInputCount) then
  begin
    Result := FInputs[Index];
  end;
end;

function TNeuron.GetLoad: Single;
var i: Integer;
begin
  Result := 0;
  for i := 0 to FInputCount-1 do
  begin
    Result := Result+ FInputs[i]*FWeights[i];
  end;
end;

function TNeuron.GetOutput: Single;
var i: Integer;
begin
  Result := 0;
  if Load > FThreshold  then Result := 1;
  if FAnalogOutput then Result := 1/1+Exp(-Load+FThreshold);
end;

function TNeuron.GetWeight(Index: Integer): Single;
begin
  Result := 0;
  if (Index > -1) and (Index < FInputCount) then
  begin
    Result := FWeights[Index];
  end;
end;

procedure TNeuron.RandomInitialize;
var i:Integer;
begin
  Randomize;
  for i := 0 to FInputCount-1 do
  begin
    FInputs[i] := RandomRange(0,2);
    FWeights[i] := Random-Random;
  end;
  FThreshold := Random-Random;
end;

procedure TNeuron.SetInput(Index: Integer; const Value: Single);
begin
  if (Index > -1) and (Index < FInputCount) then
  begin
    FInputs[Index] := Value;
  end;
end;

procedure TNeuron.SetInputCount(const Value: Integer);
begin
  FInputCount := Value;
  SetLength(FInputs,Value);
  SetLength(FWeights,Value);
end;

procedure TNeuron.SetWeight(Index: Integer; const Value: Single);
begin
  if (Index > -1) and (Index < FInputCount) then
  begin
    FWeights[Index] := Value;
  end;
end;

end.
