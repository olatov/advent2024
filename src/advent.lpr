program Advent;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, Math,
  Generics.Defaults, Generics.Collections;

const
  PartsTotal = 4;

type
  TRow = specialize TList<Integer>;

  TPart = record
    Input: TJSHTMLTextAreaElement;
    RunButton: TJSHTMLButtonElement;
    Answer: TJSHTMLDivElement;
  end;

  TRows = array of TRow;

  { TDefaultComparer }

  generic TDefaultComparer<T> = class(specialize TComparer<T>)
  public
    function Compare(const ALeft, ARight: T): Integer; override; overload;
  end;

  TIntegerComparer = specialize TDefaultComparer<Integer>;

  { TAdventApp }

  TAdventApp = class(TBrowserApplication)

  private
    FParts: array[1..PartsTotal] of TPart;
    procedure BindElements();
    function GetRows(APart: Integer): TRows;
    function GetPart1Answer(const ARows: TRows): Integer;
    function GetPart2Answer(const ARows: TRows): Integer;
    function GetPart3Answer(const ARows: TRows): Integer;
    function GetPart4Answer(const ARows: TRows): Integer;
    procedure RunPart(ANumber: Integer);
    property Rows[Part: Integer]: TRows read GetRows;
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

procedure Tadventapp.BindElements;
begin
  FParts[1].Input := TJSHTMLTextAreaElement(Document.GetElementById('Part1Input'));
  FParts[1].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part1Run'));
  FParts[1].Answer := TJSHTMLDivElement(Document.GetElementById('Part1Answer'));
  FParts[1].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(1);
    end);

  FParts[2].Input := TJSHTMLTextAreaElement(Document.GetElementById('Part2Input'));
  FParts[2].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part2Run'));
  FParts[2].Answer := TJSHTMLDivElement(Document.GetElementById('Part2Answer'));
  FParts[2].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(2);
    end);

  FParts[3].Input := TJSHTMLTextAreaElement(Document.GetElementById('Part3Input'));
  FParts[3].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part3Run'));
  FParts[3].Answer := TJSHTMLDivElement(Document.GetElementById('Part3Answer'));
  FParts[3].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(3);
    end);

  FParts[4].Input := TJSHTMLTextAreaElement(Document.GetElementById('Part4Input'));
  FParts[4].RunButton := TJSHTMLButtonElement(Document.GetElementById('Part4Run'));
  FParts[4].Answer := TJSHTMLDivElement(Document.GetElementById('Part4Answer'));
  FParts[4].RunButton.AddEventListener('click',
    procedure
    begin
      RunPart(4);
    end);
end;

function TAdventApp.GetRows(APart: Integer): TRows;
var
  Lines: TStringList;
  Line: String;
  Item: String;
  Row: TRow;
begin
  Lines := TStringList.Create;
  Lines.Text := FParts[APart].Input.Value;

  SetLength(Result, 0);
  for Line in Lines do
  begin
    if Line.Trim().IsEmpty then continue;

    Row := TRow.Create();
    for Item in Line.Split(' ', TStringSplitOptions.ExcludeEmpty) do
      Row.Add(Item.ToInteger());

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Row;
  end;
end;

function TAdventApp.GetPart1Answer(const ARows: TRows): Integer;
var
  Row, Left, Right: TRow;
  Comparer: TIntegerComparer;
  I: Integer;
begin
  Comparer := TIntegerComparer.Create();
  Left := TRow.Create(Comparer);
  Right := TRow.Create(Comparer);

  for Row in ARows do
  begin
    Left.Add(Row[0]);
    Right.Add(Row[1]);
  end;

  Left.Sort;
  Right.Sort;

  Result := 0;
  for I := 0 to Left.Count - 1 do
    Inc(Result, Abs(Left[I] - Right[I]));
end;

function TAdventApp.GetPart2Answer(const ARows: TRows): Integer;
var
  Number, Value: Integer;
  Counts: specialize TDictionary<Integer, Integer>;
  Row, Left, Right: TRow;
begin
  Left := TRow.Create;
  Right := TRow.Create;

  for Row in ARows do
  begin
    Left.Add(Row[0]);
    Right.Add(Row[1]);
  end;

  Counts := specialize TDictionary<Integer, Integer>.Create();
  for Number in Right do
  begin
    if not Counts.ContainsKey(Number) then
      Counts[Number] := 0;

    Counts[Number] := Counts[Number] + 1;
  end;

  for Number in Left do
    if Counts.TryGetValue(Number, Value) then
      Inc(Result, Number * Value);
end;

function TAdventApp.GetPart3Answer(const ARows: TRows): Integer;
var
  I: Integer;
  Row: TRow;
  Delta: Integer;
  IsAscending, IsDescending, IsSameValue: Boolean;
begin
  Result := Length(ARows);
  for Row in ARows do
  begin
    IsAscending := false;
    IsDescending := false;
    IsSameValue := false;

    for I := 1 to Row.Count - 1 do
    begin
      Delta := Row[I] - Row[I - 1];
      case Sign(Delta) of
        1: IsAscending := true;
        0: IsSameValue := true;
       -1: IsDescending := true;
      end;

      if IsSameValue or (IsAscending and IsDescending) or (Abs(Delta) > 3) then
      begin
        Dec(Result);
        break;
      end;
    end;
  end;
end;

function TAdventApp.GetPart4Answer(const ARows: TRows): Integer;
var
  I: Integer;
  Row, TempRow: TRow;

  function IsSafe(ARow: TRow): Boolean;
  var
    Delta: Integer;
    IsAscending, IsDescending, IsSameValue: Boolean;
    I: Integer;
  begin
    IsAscending := false;
    IsDescending := false;
    IsSameValue := false;

    for I := 1 to ARow.Count - 1 do
    begin
      Delta := ARow[I] - ARow[I - 1];
      case Sign(Delta) of
        1: IsAscending := true;
        0: IsSameValue := true;
       -1: IsDescending := true;
      end;

      if IsSameValue or (IsAscending and IsDescending) or (Abs(Delta) > 3) then
      begin
        Result := false;
        Exit;
      end;
    end;
    Result := true;
  end;

begin
  Result := 0;
  for Row in ARows do
  begin
    if IsSafe(Row) then
      Inc(Result)
    else
    begin
      for I := 0 to Row.Count - 1 do
      begin
        TempRow := TRow.Create(Row);
        TempRow.Delete(I);
        if IsSafe(TempRow) then
        begin
          Inc(Result);
          break;
        end;
      end;
    end;
  end;
end;


procedure TAdventApp.RunPart(ANumber: Integer);
var
  Answer: Integer;
begin
  try
    case ANumber of
      1: Answer := GetPart1Answer(Rows[1]);
      2: Answer := GetPart2Answer(Rows[2]);
      3: Answer := GetPart3Answer(Rows[3]);
      4: Answer := GetPart4Answer(Rows[4]);
    else
      raise Exception.Create(Format('Invalid step: %s', [ANumber]));
    end;
    FParts[ANumber].Answer.InnerHtml := Format('Answer: <b>%d</b>', [Answer]);
  except
    on E: Exception do
      FParts[ANumber].Answer.InnerHtml := Format('Error: %s', [E.Message]);
  end;
end;

procedure TAdventApp.DoRun;
begin
  RegisterServiceWorker('/ServiceWorker.js');

  BindElements();
end;

var
  Application : TAdventApp;

begin
  Application := TAdventApp.Create(nil);
  Application.Initialize;
  Application.Run;
end.
