{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit MyTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  TVariantArray2d = array of array of Variant;

  { TIntegerList }

  TIntegerList = class(TList)
  private
    function GetValues(Index: Integer): Integer;
  public
    function AddValue(Value: Integer): Integer;
    procedure DeleteValue(Value: Integer);
    function FindValue(Value: Integer): Integer;
    property Values[Index: Integer]: Integer read GetValues; default;
  end;

  { TCardinalList }

  TCardinalList = class(TList)
  private
    function GetValues(Index: Integer): Cardinal;
  public
    function AddValue(Value: Cardinal): Integer;
    procedure Clear; override;
    property Values[Index: Integer]: Cardinal read GetValues; default;
  end;

  { TStringListUtf8 }

  TStringListCompareEvent = function(Sender: TStringList; const s1, s2: String): Integer of object;

  TStringListUtf8 = class(TStringList)
  private
    FOnCompare: TStringListCompareEvent;
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  public
    // Добавляет строку, если такой не существует.
    procedure AddStr(const S: String);
    property OnCompare: TStringListCompareEvent read FOnCompare write FOnCompare;
  end;

  PVrData = ^TVrData;
  TVrData = record
    Name: String;
    Value: Variant;
  end;

  { TVarList }

  TVarList = class(TList)
  private
    function GetVars(Index: Integer): PVrData;
  public
    function AddVar(const aName: String; aValue: Variant): PVrData;
    function FindVar(const aName: String): PVrData;
    procedure Clear; override;
    property Vars[Index: Integer]: PVrData read GetVars; default;
  end;

  { TJSONFloatNumberEx }

  TJSONFloatNumberEx = class(TJSONFloatNumber)
  protected
    function GetAsString: TJSONStringType; override;
  end;

  { TComponentReader }

  {TComponentReader = class
  private
    procedure PropNotFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
    procedure ReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
  public
    function ReadFromStream(St: TStream): TComponent;
  end;   }

var
  JSONFormatFloatEnabled: Boolean;

implementation

uses
  LazUtf8, apputils, LConvEncoding;

{ TJSONFloatNumberEx }

function TJSONFloatNumberEx.GetAsString: TJSONStringType;
var
  FS: TFormatSettings;
begin
  if JSONFormatFloatEnabled then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStr(AsFloat, FS);
    if Frac(AsFloat) = 0 then Result := Result + '.0';
  end
  else
    Result := inherited GetAsString;
end;

{ TComponentReader }

{procedure TComponentReader.PropNotFound(Reader: TReader; Instance: TPersistent;
  var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Handled := True;
  Skip := True;
end;

procedure TComponentReader.ReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := True;
  ErrMsg(Message);
end;

function TComponentReader.ReadFromStream(St: TStream): TComponent;
var
  MS: TMemoryStream;
  Reader: TReader;
begin
  MS := TMemoryStream.Create;
  St.Position:=0;
  ObjectTextToResource(St, MS);
  MS.Position:=0;
  Reader := TReader.Create(MS, 4096);
  Reader.OnPropertyNotFound:=@PropNotFound;
  Reader.OnError:=@ReaderError;
  try
    MS.ReadResHeader;
    Result := Reader.ReadRootComponent(nil);
  finally
    Reader.Free;
    MS.Free;
  end;
end;  }


{ TVarList }

function TVarList.GetVars(Index: Integer): PVrData;
begin
  Result := PVrData(Items[Index]);
end;

function TVarList.AddVar(const aName: String; aValue: Variant): PVrData;
begin
  New(Result);
  Result^.Name := aName;
  Result^.Value := aValue;
  Add(Result);
end;

function TVarList.FindVar(const aName: String): PVrData;
var
  i: Integer;
  pV: PVrData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pV := Vars[i];
    if MyUtf8CompareText(pV^.Name, aName) = 0 then Exit(pV);
  end;
end;

procedure TVarList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Vars[i]);
  inherited Clear;
end;


{ TCardinalList }

function TCardinalList.GetValues(Index: Integer): Cardinal;
begin
  Result := PCardinal(Items[Index])^;
end;

function TCardinalList.AddValue(Value: Cardinal): Integer;
var
  pV: PCardinal;
begin
	New(pV);
  pV^ := Value;
  Result := Add(pV);
end;

procedure TCardinalList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Dispose(PCardinal(Items[i]));
  inherited Clear;
end;

{ TIniFileEx }

{constructor TIniFileEx.Create(const AFileName: string; AOptions: TIniFileoptions);
var
  MarkHolder: Cardinal;
begin
  if not FileExists(AFileName) then
  	FS := TFileStream.Create(AFileName, fmCreate + fmOpenReadWrite + fmShareDenyWrite)
  else
	  FS := TFileStream.Create(AFileName, fmOpenReadWrite + fmShareDenyWrite);

  if FS.Size > 3 then
  begin
	  MarkHolder := FS.ReadDWord;
  	if (MarkHolder and $00FFFFFF) = $00BFBBEF then
    	FS.Position := 3
	  else
  	  FS.Position := 0;
  end;
  Create(FS, AOptions);
end;

destructor TIniFileEx.Destroy;
begin
  if FS <> nil then
	  FS.Position := 0;   // Устанавливаю в начало, т. к. в Ubuntu записывается мусор.
  inherited Destroy;
  FreeAndNil(FS);
end;     }

{ TStringListUtf8 }

function TStringListUtf8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if FOnCompare <> nil then Result := FOnCompare(Self, s1, s2)
  else
  begin
    if not CaseSensitive then
      Result := MyUtf8CompareText(s1, s2)
    else
      Result := Utf8CompareStr(s1, s2);
  end;
end;

procedure TStringListUtf8.AddStr(const S: String);
begin
  if IndexOf(S) < 0 then Add(S);
end;

{ TIntegerList }

function TIntegerList.GetValues(Index: Integer): Integer;
begin
  Result := Integer(Items[Index]);
end;

function TIntegerList.AddValue(Value: Integer): Integer;
begin
  Result := Add(Pointer(Value));
end;

procedure TIntegerList.DeleteValue(Value: Integer);
var
  i: Integer;
begin
  i := FindValue(Value);
  if i >= 0 then Delete(i);
end;

function TIntegerList.FindValue(Value: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  	if Values[i] = Value then Exit(i);
end;

initialization
  JSONFormatFloatEnabled := False;
  SetJSONInstanceType(jitNumberFloat, TJSONFloatNumberEx);

end.

