program Advent;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web,
  Generics.Defaults, Generics.Collections;

const
  PartsTotal = 2;

type
  TIntegerList = specialize TList<Integer>;

  TPart = record
    RunButton: TJSHTMLButtonElement;
    Answer: TJSHTMLDivElement;
  end;

  TColumns = array[1..2] of TIntegerList;

  { TDefaultComparer }

  generic TDefaultComparer<T> = class(specialize TComparer<T>)
  public
    function Compare(const ALeft, ARight: T): Integer; override; overload;
  end;

  TIntegerComparer = specialize TDefaultComparer<Integer>;

  { TAdventApp }

  TAdventApp = class(Tbrowserapplication)

  private
    FInput: TJSHTMLTextAreaElement;
    FParts: array[1..PartsTotal] of TPart;
    procedure BindElements();
    function GetColumns(): TColumns;
    function GetPart1Answer(const AValues: TColumns): Integer;
    function GetPart2Answer(const AValues: Tcolumns): Integer;
    procedure RunPart(ANumber: Integer);
    property Columns: TColumns read GetColumns;
  protected
    procedure DoRun; override;
  public
  end;

{ TDefaultComparer }

generic function TDefaultComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  if ALeft < ARight then
    Result := -1
  else if ALeft > ARight then
    Result := 1
  else
    Result := 0;
end;

procedure TAdventApp.Bindelements();
begin
  FInput := TJSHTMLTextAreaElement(Document.GetElementById('Input'));

  FParts[1].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part1Run'));
  FParts[1].Answer := TJSHTMLDivElement(Document.GetElementById('Part1Answer'));
  FParts[1].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(1);
    end);

  FParts[2].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part2Run'));
  FParts[2].Answer := TJSHTMLDivElement(Document.GetElementById('Part2Answer'));
  FParts[2].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(2);
    end);
end;

function TAdventApp.GetColumns: TColumns;
var
  Lines: TStringList;
  Line: String;
  Items: array of String;
begin
  Lines := TStringList.Create;
  Lines.Text := FInput.Value;

  Result[1] := TIntegerList.Create(TIntegerComparer.Create);
  Result[2] := TIntegerList.Create(TIntegerComparer.Create);

  for Line in Lines do
  begin
    if Line.Trim().IsEmpty then continue;

    Items := Line.Split(' ', TStringSplitOptions.ExcludeEmpty);
    Result[1].Add(Items[0].ToInteger);
    Result[2].Add(Items[1].ToInteger);
  end;

  Result[1].Sort;
  Result[2].Sort;
end;

function TAdventApp.GetPart1Answer(const Avalues: Tcolumns): Integer;
var
  I: Integer;
begin
  Assert(AValues[1].Count = AValues[2].Count);

  AValues[1].Sort;
  AValues[2].Sort;

  Result := 0;
  for I := 0 to AValues[1].Count - 1 do
    Inc(Result, Abs(AValues[1][I] - AValues[2][I]));
end;

function TAdventApp.GetPart2Answer(const AValues: Tcolumns): Integer;
var
  Number, Value: Integer;
  Counts: specialize TDictionary<Integer, Integer>;
begin
  Assert(AValues[1].Count = AValues[2].Count);

  Counts := specialize TDictionary<Integer, Integer>.Create();
  for Number in AValues[2] do
  begin
    if not Counts.ContainsKey(Number) then
      Counts[Number] := 0;

    Counts[Number] := Counts[Number] + 1;
  end;

  for Number in AValues[1] do
    if Counts.TryGetValue(Number, Value) then
      Inc(Result, Number * Value);
end;

procedure TAdventApp.RunPart(Anumber: Integer);
var
  Answer: Integer;
begin
  try
    case ANumber of
      1: Answer := GetPart1Answer(Columns);
      2: Answer := GetPart2Answer(Columns);
    else
      raise Exception.Create(Format('Invalid step: %s', [ANumber]));
    end;
    FParts[ANumber].Answer.InnerHtml := Format('Answer: <b>%d</b>', [Answer]);
  except
    on E: Exception do
      FParts[ANumber].Answer.InnerHtml := Format('Error: %s', [E.Message]);
  end;
end;

procedure Tadventapp.Dorun;
begin
  Registerserviceworker('/ServiceWorker.js');

  BindElements();
end;

var
  Application : TAdventApp;

begin
  Application := TAdventApp.Create(nil);
  Application.Initialize;
  Application.Run;
end.
