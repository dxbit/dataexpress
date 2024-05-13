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

unit MyClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strconsts;

type
  { TFilterField }

  TFilterField = class
  private
    FForm: TComponent;
    FFId: Integer;
    FIsNot: Boolean;
    FIsNull: Boolean;
    FValues: TStringList;
    function GetFieldName: String;
    function GetValue(Index: Integer): String;
    function GetEndValue(Index: Integer): String;
  public
    constructor Create(AForm: TComponent);
    destructor Destroy; override;
    property FId: Integer read FFId write FFId;
    property IsNull: Boolean read FIsNull write FIsNull;
    property IsNot: Boolean read FIsNot write FIsNot;
    property Values: TStringList read FValues;
    property Value[Index: Integer]: String read GetValue;
    property EndValue[Index: Integer]: String read GetEndValue;
    property FieldName: String read GetFieldName;
  end;

  { TFilterObject }

  TFilterObject = class(TList)
  private
    FForm: TComponent;
    function GetFields(Index: Integer): TFilterField;
  public
    constructor Create(AForm: TComponent);
    function AddField: TFilterField;
    function FindField(FId: Integer): TFilterField;
    procedure DeleteField(F: TFilterField);
    procedure Clear; override;
    procedure Load(const Filter: String);
    procedure Save(var Filter: String);
    function ValuesExists: Boolean;
    property Fields[Index: Integer]: TFilterField read GetFields; default;
  public
    function AddFieldByName(const FieldName: String): TFilterField;
    function FindFieldByName(const FieldName: String): TFilterField;
  end;

implementation

uses
  apputils, dxctrls;

{ TFilterObject }

function TFilterObject.GetFields(Index: Integer): TFilterField;
begin
  Result := TFilterField(Items[Index]);
end;

constructor TFilterObject.Create(AForm: TComponent);
begin
  inherited Create;
  FForm := AForm;
end;

function TFilterObject.AddField: TFilterField;
begin
  Result := TFilterField.Create(FForm);
  Add(Result);
end;

function TFilterObject.FindField(FId: Integer): TFilterField;
var
  i: Integer;
  F: TFilterField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    F := Fields[i];
    if F.FId = FId then Exit(F);
  end;
end;

procedure TFilterObject.DeleteField(F: TFilterField);
begin
  Remove(F);
  F.Free;
end;

procedure TFilterObject.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Fields[i].Free;
  inherited Clear;
end;

procedure TFilterObject.Load(const Filter: String);
var
  SL, SL2: TStringList;
  i: Integer;
  S: String;
  F: TFilterField;
begin
  Clear;
  S := Filter;
  if Copy(S, 1, 7) <> 'FILTER:' then Exit;
  System.Delete(S, 1, 7);
  SL := TStringList.Create;
  SL2 := TStringList.Create;
  SplitStr2(S, ' ~~ ', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    SplitStr(S, '|', SL2);
    F := AddField;
    F.FId := StrToInt(SL2[0]);
    F.IsNot := Str2Bool(SL2[1]);
    F.IsNull := Str2Bool(SL2[2]);
    SplitStr(SL2[3], ';', F.Values);
  end;
  SL2.Free;
  SL.Free;
end;

// поле|not|null|значение;значение... ~~ поле|... ~~ ...
procedure TFilterObject.Save(var Filter: String);
var
  i: Integer;
  S: String;
  F: TFilterField;
begin
  S := '';
  for i := 0 to Count - 1 do
  begin
    F := Fields[i];
    S := S + IntToStr(F.FId) + '|' + Bool2Str(F.IsNot) + '|' +
    	Bool2Str(F.IsNull) + '|' + GetDelimitedText(F.Values, ';');
    if i < Count - 1 then S := S + ' ~~ ';
  end;
  if S <> '' then S := 'FILTER:' + S;
  Filter := S;
end;

function TFilterObject.ValuesExists: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Fields[i].IsNull then Exit(True);
  	for j := 0 to Fields[i].Values.Count - 1 do
  		if Fields[i].Values[j] <> '' then Exit(True);
  end;
end;

function TFilterObject.AddFieldByName(const FieldName: String): TFilterField;
var
  C: TComponent;
begin
  C := FindComponentByFieldName(TdxForm(FForm), FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
  Result := AddField;
  Result.FId := GetId(C);
end;

function TFilterObject.FindFieldByName(const FieldName: String): TFilterField;
var
  C: TComponent;
begin
  C := FindComponentByFieldName(TdxForm(FForm), FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
	Result := FindField(GetId(C));
end;

{ TFilterField }

function TFilterField.GetFieldName: String;
var
  C: TComponent;
begin
  Result := '';
  C := FindById(TdxForm(FForm), FFId);
  if C <> nil then Result := dxctrls.GetFieldName(C);
end;

constructor TFilterField.Create(AForm: TComponent);
begin
  FForm := AForm;
  FValues := TStringList.Create;
  FValues.Delimiter:=';';
  FValues.QuoteChar:=#0;
  FValues.StrictDelimiter:=True;
end;

destructor TFilterField.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TFilterField.GetValue(Index: Integer): String;
var
  p: SizeInt;
begin
	Result := FValues[Index];
  p := Pos(' .. ', Result);
  Result := Copy(Result, 1, p - 1);
end;

function TFilterField.GetEndValue(Index: Integer): String;
var
  p: SizeInt;
begin
  Result := FValues[Index];
  p := Pos(' .. ', Result);
  if p = 0 then Exit;
  Result := Copy(Result, p + 4, 255);
end;

end.

