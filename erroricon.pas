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

unit ErrorIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, strconsts;

type
  TErrMsg = class
  public
    Time: TDateTime;
    Name, Tp, Prop, Msg, Expr: String;
    Position: Integer;
  end;

  { TErrList }

  TErrList = class(TList)
  private
    function GetErrs(Index: Integer): TErrMsg;
  public
    function AddErr(const FmNm, FlNm, FlTp, Prop, Expr: String; E: Exception): Integer;
    procedure Clear; override;
    property Errs[Index: Integer]: TErrMsg read GetErrs; default;
  end;

  { TCalcError }

  TCalcError = class(TCustomControl)
  private
    FImage: TImage;
    FErrs: TStringList;
    FErrList: TErrList;
    procedure ImageClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddErr(const FmNm, FlNm, FlTp, Prop, Expr: String; E: Exception);
    procedure AddErrC(C: TComponent; const Prop, Expr: String; E: Exception);
    property ErrList: TErrList read FErrList;
  end;

implementation

uses
  calcerrsform, expressions, dxreports, sqlgen, apputils, dxctrls;

{ TErrList }

function TErrList.GetErrs(Index: Integer): TErrMsg;
begin
  Result := TErrMsg(Items[Index]);
end;

function TErrList.AddErr(const FmNm, FlNm, FlTp, Prop, Expr: String;
  E: Exception): Integer;
var
  M: TErrMsg;
begin
  M := TErrMsg.Create;
  M.Time := Now;
  M.Name := FlNm;
  M.Tp := FlTp;
  M.Prop := Prop;
  M.Expr := Expr;
  M.Msg := E.Message;
  if E is ECalcError then
    M.Position := ECalcError(E).Position
  else if E is ESourceFilterError then
    with ESourceFilterError(E) do
    begin
      M.Prop := rsSourceFilter + ' ' + IntToStr(SourceNum);
      M.Expr := Expr;
      M.Position := Position;
    end
  else if E is EFilterParserError then
    M.Position := EFilterParserError(E).Position
  else if E is ESQLSelectStatementError then
    M.Position := ESQLSelectStatementError(E).Position;
  Result := Add(M);
  LogString(E.Message + ' (' + IIF(FmNm <> '', FmNm + ' -> ', '') +
    FlNm + ' (' + FlTp + ') -> ' + Prop + ')', 'Expression');
end;

procedure TErrList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Errs[i].Free;
  inherited Clear;
end;

{ TCalcError }

procedure TCalcError.ImageClick(Sender: TObject);
begin
  Visible := False;
  ShowCalcErrsForm(Self);
end;

constructor TCalcError.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 16; Height := 16;
  FImage := TImage.Create(Self);
  FImage.Align := alClient;
  FImage.Parent := Self;
  FImage.Picture.LoadFromLazarusResource('error16');
  FImage.ShowHint:=True;
  FImage.Hint := rsErrorsOccured;
  FImage.OnClick := @ImageClick;
  FErrs := TStringList.Create;
  FErrList := TErrList.Create;
  Visible := False;
end;

destructor TCalcError.Destroy;
begin
  FImage.Free;
  FErrList.Free;
  FErrs.Free;
  inherited Destroy;
end;

procedure TCalcError.AddErr(const FmNm, FlNm, FlTp, Prop, Expr: String;
  E: Exception);
begin
  FErrList.AddErr(FmNm, FlNm, FlTp, Prop, Expr, E);
  Visible := True;
end;

procedure TCalcError.AddErrC(C: TComponent; const Prop, Expr: String;
  E: Exception);
var
  FlNm: String;
begin
  if C is TdxLabel then
    FlNm := TdxLabel(C).FieldName
  else
    FlNm := GetFieldName(C);
  AddErr(TdxForm(C.Owner).FormCaption, FlNm, GetComponentType(C),
    Prop, Expr, E);
end;

end.

