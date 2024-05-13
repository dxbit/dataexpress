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

unit Lists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type

  { TSortColumn }

  TSortColumn = class
  public
    //Grid: TComponent;
    Col: TGridColumn;
    Desc: Boolean;
  end;

  { TSortColumns }

  TSortColumns = class(TList)
  private
    function GetCols(Index: Integer): TSortColumn;
  public
    Grid: TComponent;
    function AddCol(Col: TGridColumn; Desc: Boolean): TSortColumn;
    function FindCol(Col: TGridColumn): TSortColumn;
    procedure RemoveCol(CD: TSortColumn);
    procedure Clear; override;
    property Cols[Index: Integer]: TSortColumn read GetCols; default;
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

{ TSortColumns }

function TSortColumns.GetCols(Index: Integer): TSortColumn;
begin
  Result := TSortColumn(Items[Index]);
end;

function TSortColumns.AddCol(Col: TGridColumn; Desc: Boolean): TSortColumn;
begin
  Result := TSortColumn.Create;
  Result.Col := Col;
  Result.Desc := Desc;
  //Result.Grid := Grid;
  Add(Result);
end;

function TSortColumns.FindCol(Col: TGridColumn): TSortColumn;
var
  i: Integer;
  CD: TSortColumn;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Cols[i];
    if CD.Col = Col then Exit(CD);
  end;
end;

procedure TSortColumns.RemoveCol(CD: TSortColumn);
begin
  Remove(CD);
  CD.Free;
end;

procedure TSortColumns.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Cols[i].Free;
  inherited Clear;
end;

end.

