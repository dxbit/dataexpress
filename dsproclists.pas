{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit DSProcLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dxctrls, expressions, Graphics, sqlgen, db;

type

  TColorData = class
  public
    RecId: Integer;
    Color: TColor;
  end;

  { TColorList }

  TColorList = class(TList)
  private
    function GetColors(Index: Integer): TColorData;
  public
    function AddColor(RecId: Integer; Color: TColor): TColorData;
    function FindColor(RecId: Integer): TColorData;
    procedure DeleteColor(RecId: Integer);
    procedure Clear; override;
    property Colors[Index: Integer]: TColorData read GetColors; default;
  end;

  { TExprData }

  TExprData = class
  public
    C: TComponent;
    E: TExpression;
  end;

  { TExprList }

  TExprList = class(TList)
  private
    function GetExpr(Index: Integer): TExprData;
  public
    function AddExpr(aC: TComponent; aE: TExpression): Integer;
    function FindExpr(Id: Integer): TExprData;
    procedure DeleteExpr(Index: Integer);
    procedure Clear; override;
    property Expr[Index: Integer]: TExprData read GetExpr; default;
  end;

  { TQueryColorData }

  TQueryColorData = class
  public
    Color: TColor;
    FieldName: String;
    RecNo: Integer;
  end;

  { TQueryColorList }

  TQueryColorList = class(TList)
  private
    function GetColors(Index: Integer): TQueryColorData;
  public
    function AddColor: TQueryColorData;
    function FindColor(const FieldName: String; RecNo: Integer): TQueryColorData;
    procedure Clear; override;
    property Colors[Index: Integer]: TQueryColorData read GetColors; default;
  end;

  { TDSPAppendRecParser }

  TDSPAppendRecParser = class(TSQLFilterParser)
  private
    FDS: TDataSet;
    FForm: TdxForm;
    //FParDS: TDataSet;
    FParForm: TdxForm;
    FSrcDS: TDataSet;
    FSrcForm: TdxForm;
    FCmp: TComponent;
    FOp: String;
  protected
    function FieldNameParse(const FieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
  public
    function Parse(const Flt: String): String;
    property SrcForm: TdxForm read FSrcForm write FSrcForm;
    property Form: TdxForm read FForm write FForm;
    property ParForm: TdxForm read FParForm write FParForm;
    property DS: TDataSet read FDS write FDS;
    property SrcDS: TDataSet read FSrcDS write FSrcDS;
    //property ParDS: TDataSet read FParDS write FParDS;
  end;

  TControlEvents = class
  public
    Control: TControl;
    OnEnter: TNotifyEvent;
  end;

  { TControlEventsList }

  TControlEventsList = class(TList)
  public
    function AddControl(aC: TControl): TControlEvents;
    function FindControl(aC: TControl): TControlEvents;
    procedure Clear; override;
  end;

implementation

uses
  LazUtf8, apputils;

{ TControlEventsList }

function TControlEventsList.AddControl(aC: TControl): TControlEvents;
begin
  Result := TControlEvents.Create;
  Result.Control:=aC;
  Add(Result);
end;

function TControlEventsList.FindControl(aC: TControl): TControlEvents;
var
  i: Integer;
  CE: TControlEvents;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CE := TControlEvents(Items[i]);
    if CE.Control = aC then Exit(CE);
  end;
end;

procedure TControlEventsList.Clear;
begin
  ClearList(Self);
  inherited Clear;
end;

{ TDSPAppendRecParser }

function TDSPAppendRecParser.FieldNameParse(const FieldName: String): String;
var
  S: String;
begin
  S := FieldName;
  if Copy(S, 1, 1) = '!' then Delete(S, 1, 1);
  FCmp := FindComponentByFieldName(FSrcForm, S);
  Result := FieldName;
end;

function TDSPAppendRecParser.CheckValue(var Value: String): Boolean;
begin
  Result:=True;
  if (FOp <> '=') or (FCmp = nil) or (not CheckType(FCmp, Value)) then Exit;
  if FCmp is TdxLookupComboBox then
  begin
    with TdxLookupComboBox(FCmp) do
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        if Value <> '0' then
        begin
          KeyValue:=Value;
          Field.Value := GetObjFieldValue(FCmp, KeyValue, True);
        end;
      end;
  end
  else
    FSrcDS.FieldByName(FieldStr(FCmp)).Value := Value;
end;

function TDSPAppendRecParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=True;
end;

function TDSPAppendRecParser.Parse(const Flt: String): String;
begin
  Result := '';
  ExprBuilder := TExpressionBuilder.Create;
  ExprBuilder.Form := FForm;
  ExprBuilder.ParentForm := FParForm;
  ExprBuilder.DataSet := FDS;
  ExprBuilder.SkipLabels:=True;
  try
    inherited Parse(Flt);
  finally
    ExprBuilder.Free;
    ExprBuilder := nil;
  end;
end;

{ TQueryColorList }

function TQueryColorList.GetColors(Index: Integer): TQueryColorData;
begin
  Result := TQueryColorData(Items[Index]);
end;

function TQueryColorList.AddColor: TQueryColorData;
begin
  Result := TQueryColorData.Create;
  Add(Result);
end;

function TQueryColorList.FindColor(const FieldName: String; RecNo: Integer
  ): TQueryColorData;
var
  i: Integer;
  CD: TQueryColorData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Colors[i];
    if (CD.RecNo = RecNo) and (CompareText(CD.FieldName, FieldName) = 0) then Exit(CD);
  end;
end;

procedure TQueryColorList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Colors[i].Free;
  inherited Clear;
end;

{ TColorList }

function TColorList.GetColors(Index: Integer): TColorData;
begin
  Result := TColorData(Items[Index]);
end;

function TColorList.AddColor(RecId: Integer; Color: TColor): TColorData;
var
  CD: TColorData;
begin
  CD := TColorData.Create;
  CD.RecId := RecId;
  CD.Color := Color;
  Add(CD);
  Result := CD;
end;

function TColorList.FindColor(RecId: Integer): TColorData;
var
  i: Integer;
  CD: TColorData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Colors[i];
    if CD.RecId = RecId then Exit(CD);
  end;
end;

procedure TColorList.DeleteColor(RecId: Integer);
var
  CD: TColorData;
begin
  CD := FindColor(RecId);
  if CD = nil then Exit;
  Remove(CD);
  CD.Free;
end;

procedure TColorList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Colors[i].Free;
  inherited Clear;
end;

{ TExprList }

function TExprList.GetExpr(Index: Integer): TExprData;
begin
  Result := TExprData(Items[Index]);
end;

function TExprList.AddExpr(aC: TComponent; aE: TExpression): Integer;
var
  LE: TExprData;
begin
  LE := TExprData.Create;
  LE.C := aC;
  LE.E := aE;
  Result := Add(LE);
end;

function TExprList.FindExpr(Id: Integer): TExprData;
var
  i: Integer;
  E: TExprData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    E := Expr[i];
    if GetId(E.C) = Id then Exit(E);
  end;
end;

procedure TExprList.DeleteExpr(Index: Integer);
begin
  Expr[Index].Free;
  Delete(Index);
end;

procedure TExprList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Expr[i].E.Free;
    Expr[i].Free;
  end;
  inherited Clear;
end;

end.

