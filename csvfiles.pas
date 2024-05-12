unit CSVFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8;

type

  { TCsvData }

  TCsvData = class
  private
    FData: array of array of String;
    FColCount, FRowCount: Integer;
    FDelimiter: Char;
    function GetCells(Col, Row: Integer): String;
    procedure SetCells(Col, Row: Integer; AValue: String);
    procedure SetColCount(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
  public
    constructor Create;
    procedure LoadFromFile(const AFileName: String; FromANSI: Boolean);
    procedure LoadFromStream(AStream: TStream; FromANSI: Boolean);
    procedure SaveToFile(const AFileName: String; ToANSI: Boolean);
    procedure SavetoStream(AStream: TStream; ToANSI: Boolean);
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColCount: Integer read FColCount write SetColCount;
    property Cells[Col, Row: Integer]: String read GetCells write SetCells; default;
    property Delimiter: Char read FDelimiter write FDelimiter;
  end;

implementation

uses
  apputils;

{ TCsvData }

function TCsvData.GetCells(Col, Row: Integer): String;
begin
  Result := FData[Row, Col];
end;

procedure TCsvData.SetCells(Col, Row: Integer; AValue: String);
begin
  FData[Row, Col] := AValue;
end;

procedure TCsvData.SetColCount(AValue: Integer);
var
  i: Integer;
begin
  FColCount := AValue;
  for i := 0 to FRowCount - 1 do
    SetLength(FData[i], AValue);
end;

procedure TCsvData.SetRowCount(AValue: Integer);
var
  i: Integer;
begin
  SetLength(FData, AValue);
  for i := FRowCount to AValue - 1 do
    SetLength(FData[i], FColCount);
  FRowCount := AValue;
end;

constructor TCsvData.Create;
begin
  FDelimiter := ';';
end;

procedure TCsvData.LoadFromFile(const AFileName: String; FromANSI: Boolean);
var
  FS: TFileStream;
  MS: TMemoryStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  MS := TMemoryStream.Create;
  try
    MS.CopyFrom(FS, FS.Size);
    MS.Position := 0;
    LoadFromStream(MS, FromANSI);
  finally
    FS.Free;
    MS.Free;
  end;
end;

procedure TCsvData.LoadFromStream(AStream: TStream; FromANSI: Boolean);

  function NextCharIs(AChar: Char): Boolean;
  var
    Buf: Char;
  begin
    Result := False;
    if AStream.Read(Buf, 1) > 0 then
    begin
      if Buf = AChar then Result := True
      else AStream.Position := AStream.Position - 1;
    end;
  end;

var
  c, r: Integer;
  Ch: Char;
  S: String;
  FirstChar, IsQuote: Boolean;     // Первый символ ячейки
begin
  r := 0; c := 0;
  S := '';
  FirstChar := True;
  IsQuote := False;
  RowCount := 0;
  ColCount := 0;
  RowCount := 100;
  SkipBOM(AStream);
  while AStream.Read(Ch, 1) > 0 do
  begin
    if Ch = '"' then
    begin
      if FirstChar then
      begin
        IsQuote := True;
        FirstChar := False;
      end
      else if IsQuote then
      begin
        if NextCharIs('"') then S := S + Ch
        else IsQuote := False;
      end
      else S := S + Ch;
    end
    else if (Ch in [#13, FDelimiter]) and not IsQuote then
    begin
      // Игнорируем пустые строки.
      if (c = 0) and FirstChar and (Ch <> FDelimiter) then
      else
      begin
        if r = 0 then ColCount := c + 1;
        if c < ColCount then
        begin
          if FromANSI then S := WinCPToUtf8(S);
          Cells[c, r] := S;
        end;
        Inc(c);
        if Ch = #13 then
        begin
          Inc(r);
          if r = RowCount then RowCount := RowCount + 100;
          c := 0;
        end;
        S := '';
        FirstChar := True;
      end;
      if Ch = #13 then NextCharIs(#10);
    end
    else
    begin
      S := S + Ch;
      FirstChar := False;
    end;
  end;
  if S <> '' then
  begin
    if ColCount = 0 then ColCount := 1;
    if c < ColCount then
    begin
      if FromANSI then S := WinCPToUtf8(S);
      Cells[c, r] := S;
    end;
  end;
  // Игнорируем пустые строки.
  if (c = 0) and FirstChar then Dec(r);
  if r < RowCount - 1 then RowCount := r + 1;
end;

procedure TCsvData.SaveToFile(const AFileName: String; ToANSI: Boolean);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(FS, ToANSI);
  finally
    FS.Free
  end;
end;

procedure TCsvData.SavetoStream(AStream: TStream; ToANSI: Boolean);
var
  r, c, i: Integer;
  NeedQuotes, HasQuotes: Boolean;
  S: String;
begin
  for r := 0 to RowCount - 1 do
  begin
    for c := 0 to ColCount - 1 do
    begin
      NeedQuotes := False;
      HasQuotes := False;
      S := Cells[c, r];
      for i := 1 to Length(S) do
        if S[i] in [#13, #10, FDelimiter] then NeedQuotes := True
        else if S[i] = '"' then
        begin
          HasQuotes := True;
          Break;
        end;
      if ToANSI then S := Utf8ToWinCP(S);
      if HasQuotes then S := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
      else if NeedQuotes then S := '"' + S + '"';
      if c < ColCount - 1 then S := S + FDelimiter;
      AStream.Write(S[1], Length(S));
    end;
    S := #13#10;
    AStream.Write(S[1], 2);
  end;
end;

end.

