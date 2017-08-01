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
unit Lists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type

  { TSortColData }

  TSortColData = class
  public
    Col: TGridColumn;
    Desc: Boolean;
  end;

  { TSortColList }

  TSortColList = class(TList)
  private
    function GetCols(Index: Integer): TSortColData;
  public
    function AddCol(Col: TGridColumn; Desc: Boolean): TSortColData;
    function FindCol(Col: TGridColumn): TSortColData;
    procedure RemoveCol(CD: TSortColData);
    procedure Clear; override;
    property Cols[Index: Integer]: TSortColData read GetCols; default;
  end;

  { TShopData }

  TShopData = class
  public
    ObjId: Integer;
    QttyInput: Boolean;
    QttyFId: Integer;
    PriceInput: Boolean;
    PriceObjFId, PriceFId: Integer;
    AddToExisting: Boolean;
    procedure Clear;
  end;

  { TFormFilterField }

  {TFormFilterField = class
  public
    Id: Integer;
    No, Nul: Boolean;
    Values: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;  }

  {TFormFilter = class(TList)
  private
    function GetFields(Index: Integer): TFormFilterField;
  public
    function AddField(Id: Integer; No, Nul: Boolean): TFormFilterField;
    function FindField(Id: Integer): TFormFilterField;
    procedure Clear; override;
    property Fields[Index: Integer]: TFormFilterField read GetFields; default;
  end;    }

implementation

{ TFormFilter }

(*function TFormFilter.GetFields(Index: Integer): TFormFilterField;
begin
  Result := TFormFilterField(Items[Index]);
end;

function TFormFilter.AddField(Id: Integer; No, Nul: Boolean): TFormFilterField;
begin
  Result := TFormFilterField.Create;
  Result.Id := Id;
  Result.No := No;
  Result.Nul := Nul;
  Add(Result);
end;

function TFormFilter.FindField(Id: Integer): TFormFilterField;
var
  i: Integer;
  FF: TFormFilterField;
begin
  for i := 0 to Count - 1 do
  begin
    FF := Fields[i];
    if FF.Id = Id then Exit(FF);
  end;
end;

procedure TFormFilter.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Fields[i].Free;
  inherited Clear;
end;    *)

{ TFormFilterField }

(*constructor TFormFilterField.Create;
begin
  Values := TStringList.Create;
end;

destructor TFormFilterField.Destroy;
begin
  Values.Free;
  inherited Destroy;
end; *)

{ TShopData }

procedure TShopData.Clear;
begin
  ObjId := 0;
  QttyInput := False;
  QttyFId := 0;
  PriceInput := False;
  PriceObjFId := 0;
  PriceFId := 0;
  AddToExisting:=False;
end;

{ TSortColList }

function TSortColList.GetCols(Index: Integer): TSortColData;
begin
  Result := TSortColData(Items[Index]);
end;

function TSortColList.AddCol(Col: TGridColumn; Desc: Boolean): TSortColData;
begin
  Result := TSortColData.Create;
  Result.Col := Col;
  Result.Desc := Desc;
  Add(Result);
end;

function TSortColList.FindCol(Col: TGridColumn): TSortColData;
var
  i: Integer;
  CD: TSortColData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Cols[i];
    if CD.Col = Col then Exit(CD);
  end;
end;

procedure TSortColList.RemoveCol(CD: TSortColData);
begin
  Remove(CD);
  CD.Free;
end;

procedure TSortColList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Cols[i].Free;
  inherited Clear;
end;

end.

