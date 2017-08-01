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
unit TimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, dbctrlsex, EditBtn, Controls, Forms, Grids, LclType,
  Db;

type

  TTimeForm = class;

  { TDBTimeEdit }

  TDBTimeEdit = class(TCustomDBEditButton)
    procedure FormHide(Sender: TObject);
  private
    FForm: TTimeForm;
  protected
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TTimeEdit }

  TTimeEdit = class(TCustomEditButton)
  private
    FForm: TTimeForm;
    procedure FormHide(Sender: TObject);
  protected
    procedure ButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate: Boolean;
    procedure EditingDone; override;
    property Button;
  end;

  { TTimeIntervalEdit }

  TTimeIntervalEdit = class(TCustomControl)
    procedure EditorEditingDone(Sender: TObject);
  private
    FBeginT: TTimeEdit;
    FEndT: TTimeEdit;
  protected
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    function Validate: Boolean;
    property BeginT: TTimeEdit read FBeginT;
    property EndT: TTimeEdit read FEndT;
    property OnEditingDone;
  end;

  { TTimeForm }

  TTimeForm = class(TForm)
  private
    FAccept: Boolean;
    FHrs, FMins: TStringGrid;
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetTime: TDateTime;
    procedure SetTime(AValue: TDateTime);
  protected
    procedure Deactivate; override;
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    property Time: TDateTime read GetTime write SetTime;
    property Accept: Boolean read FAccept write FAccept;
  end;

implementation

uses
  DateUtils;

{ TTimeForm }

procedure TTimeForm.GridDblClick(Sender: TObject);
begin
  FAccept := True;
  Hide;
end;

procedure TTimeForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_SPACE] then
  begin
    FAccept := True;
    Hide
  end
  else if Key = VK_ESCAPE then
    Hide;
end;

function TTimeForm.GetTime: TDateTime;
begin
  Result := EncodeTime(StrToInt(FHrs.Cells[FHrs.Col, FHrs.Row]),
    StrToInt(FMins.Cells[FMins.Col, FMins.Row]), 0, 0);
end;

procedure TTimeForm.SetTime(AValue: TDateTime);
var
  H, M: Word;
begin
  H := HourOf(AValue);
  M := MinuteOf(AValue);
  FHrs.Row:= H div 12;
  FHrs.Col := H - ((H div 12) * 12);
  FMins.Row := M div 12;
  FMins.Col := M - ((M div 12) * 12);
end;

procedure TTimeForm.Deactivate;
begin
  inherited Deactivate;
  Hide;
end;

procedure TTimeForm.DoShow;
begin
  inherited DoShow;
  FAccept := False;
end;

constructor TTimeForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  BorderStyle:=bsNone;
  FHrs := TStringGrid.Create(Self);
  FHrs.Parent := Self;
  FHrs.DefaultRowHeight:=22;
  FHrs.DefaultColWidth:=20;
  FHrs.FixedCols:=0;
  FHrs.FixedRows:=0;
  FHrs.RowCount:=2;
  FHrs.ColCount:=12;
  FHrs.Flat := True;
  FHrs.Font.Name:='Tahoma';
  FHrs.Font.Size := 10;
  FHrs.Width := 241;
  FHrs.Height := 44;
  FHrs.Options:=FHrs.Options - [goRangeSelect] + [goDrawFocusSelected];
  FHrs.FocusColor:=FHrs.SelectedColor;
  FHrs.Rows[0].AddStrings(['00', '01', '02', '03', '04', '05', '06', '07', '08',
    '09', '10', '11']);
  FHrs.Rows[1].AddStrings(['12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23']);
  FHrs.OnDblClick:=@GridDblClick;
  FHrs.OnKeyDown:=@GridKeyDown;

  FMins := TStringGrid.Create(Self);
  FMins.Parent := Self;
  FMins.DefaultRowHeight:=22;
  FMins.DefaultColWidth:=20;
  FMins.FixedCols:=0;
  FMins.FixedRows:=0;
  FMins.RowCount:=5;
  FMins.ColCount:=12;
  FMins.Flat := True;
  FMins.Font.Name:='Tahoma';
  FMins.Font.Size := 10;
  FMins.Top := 48;
  FMins.Width := 241;
  FMins.Height := 111;
  FMins.Options:=FMins.Options - [goRangeSelect] + [goDrawFocusSelected];
  FMins.FocusColor:=FMins.SelectedColor;
  FMins.Rows[0].AddStrings(['00', '01', '02', '03', '04', '05', '06', '07', '08',
    '09', '10', '11']);
  FMins.Rows[1].AddStrings(['12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23']);
  FMins.Rows[2].AddStrings(['24', '25', '26', '27', '28', '29', '30', '31', '32',
    '33', '34', '35']);
  FMins.Rows[3].AddStrings(['36', '37', '38', '39', '40', '41', '42', '43', '44',
    '45', '46', '47']);
  FMins.Rows[4].AddStrings(['48', '49', '50', '51', '52', '53', '54', '55', '56',
    '57', '58', '59']);
  FMins.OnDblClick:=@GridDblClick;
  FMins.OnKeyDown:=@GridKeyDown;
  Width := 241;
  Height := 159;
end;

{ TTimeIntervalEdit }

procedure TTimeIntervalEdit.EditorEditingDone(Sender: TObject);
begin
  EditingDone;
end;

procedure TTimeIntervalEdit.DoOnResize;
begin
  inherited DoOnResize;
  FBeginT.Width := ClientWidth div 2  - 2;
  FEndT.Left := ClientWidth div 2 + 2;
  FEndT.Width := FBeginT.Width;
end;

constructor TTimeIntervalEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBeginT := TTimeEdit.Create(Self);
  FBeginT.Parent := Self;
  FBeginT.OnEditingDone:=@EditorEditingDone;
  FEndT := TTimeEdit.Create(Self);
  FEndT.Parent := Self;
  FEndT.OnEditingDone:=@EditorEditingDone;
  ClientHeight := FEndT.Height;
end;

procedure TTimeIntervalEdit.SetFocus;
begin
  inherited SetFocus;
  FBeginT.SetFocus;
end;

function TTimeIntervalEdit.Focused: Boolean;
begin
  Result:=FBeginT.Focused or FEndT.Focused;
end;

function TTimeIntervalEdit.Validate: Boolean;
begin
  Result := ((FBeginT.Text = '') or FBeginT.Validate) and
    ((FEndT.Text = '') or FEndT.Validate);
end;

{ TTimeEdit }

procedure TTimeEdit.FormHide(Sender: TObject);
begin
  if FForm.Accept then
    Text := TimeToStr(FForm.Time);
end;

procedure TTimeEdit.ButtonClick;
var
  P: TPoint;
  T: TDateTime;
begin
  inherited ButtonClick;
  if FForm = nil then
  begin
    FForm := TTimeForm.CreateNew(nil);
    P := ClientToScreen(Point(0, 0));
    FForm.Top := P.y + Height;
    FForm.Left := P.x;
    FForm.OnHide:=@FormHide;
  end;
  T := 0;
  TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), T);
  FForm.Time:=T;
  FForm.Show;
end;

constructor TTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Flat:=True;
  Button.Width := 23;
  Button.Height := 23;
  Button.LoadGlyphFromLazarusResource('clock16');
end;

destructor TTimeEdit.Destroy;
begin
  FreeAndNil(FForm);
  inherited Destroy;
end;

function TTimeEdit.Validate: Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), D);
  if Result then
    Text := TimeToStr(D);
end;

procedure TTimeEdit.EditingDone;
begin
  inherited EditingDone;
  Validate;
end;

{ TDBTimeEdit }

procedure TDBTimeEdit.FormHide(Sender: TObject);
begin
  if FForm.Accept then
  begin
    Text := TimeToStr(FForm.Time);
    if (Field <> nil) then
    begin
      if not (Field.DataSet.State in [dsInsert, dsEdit]) then
        Field.DataSet.Edit;
      if Field.DataSet.State in [dsInsert, dsEdit] then
        Field.AsDateTime:=FForm.Time;
    end;
  end;
end;

function TDBTimeEdit.GetDefaultGlyphName: String;
begin
  Result:='clock16';
end;

procedure TDBTimeEdit.DoButtonClick(Sender: TObject);
var
  P: types.TPoint;
  T: TDateTime;
begin
  inherited DoButtonClick(Sender);
  if FForm = nil then
  begin
    FForm := TTimeForm.CreateNew(nil);
    P := ClientToScreen(Point(0, 0));
    FForm.Top := P.y + Height;
    FForm.Left := P.x;
    FForm.OnHide:=@FormHide;
  end;
  T := 0;
  TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), T);
  FForm.Time:=T;
  FForm.Show;
end;

procedure TDBTimeEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
var
  S: String;
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then S := 'clock48'
  else if AHeight > 36 then S := 'clock32'
  else if AHeight > 28 then S := 'clock24'
  else S := 'clock16';
  Button.LoadGlyphFromLazarusResource(S);
end;

constructor TDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Flat:=True;
  Button.Width := 23;
  Button.Height := 23;
end;

destructor TDBTimeEdit.Destroy;
begin
  FreeAndNil(FForm);
  inherited Destroy;
end;

end.

