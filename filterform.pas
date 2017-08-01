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
unit FilterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, strconsts, dxctrls, filtercontrol,
  myclasses;

type
  { TFilterFm }

  TFilterFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FForm: TdxForm;
    FGrid: TFormFilterControl;
    { private declarations }
  public
    { public declarations }
    function ShowForm(var Filter: TFilterObject): Integer;
    property Form: TdxForm read FForm write FForm;
  end;

var
  FilterFm: TFilterFm;

implementation

uses
  helpform;

{$R *.lfm}

{ TFilterFm }

procedure TFilterFm.FormCreate(Sender: TObject);
begin
  Caption := rsFilter;
  FGrid := TFormFilterControl.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.BorderSpacing.Left:=4;
  FGrid.BorderSpacing.Right:=4;
  FGrid.Buttons := ToolBar1;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
  ToolButton1.Caption:=rsAddField;
  ToolButton2.Hint:=rsDeleteField;
  ToolButton4.Caption:=rsAddValue;
  ToolButton5.Hint := rsDeleteValue;
end;

procedure TFilterFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('filter');
end;

function TFilterFm.ShowForm(var Filter: TFilterObject): Integer;
begin
  FGrid.Form := FForm;
  FGrid.Filter := Filter;
  FGrid.Init;
  FGrid.Load;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FGrid.Save;
    Filter := FGrid.Filter;
  end;
end;

end.

