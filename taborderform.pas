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
unit TabOrderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ButtonPanel, Buttons, strconsts, dxctrls;

type

  { TTabOrderFm }

  TTabOrderFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Tree: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillTree(Ctrl: TWinControl; aNode: TTreeNode);
    procedure SetOrder(Ctrl: TWinControl);
    procedure UpdateControlState;
  public
    { public declarations }
    function ShowForm(Fm: TdxForm): Integer;
  end;

var
  TabOrderFm: TTabOrderFm;

implementation

uses
  helpform, myctrls;

{$R *.lfm}

{ TTabOrderFm }

procedure TTabOrderFm.FormShow(Sender: TObject);
begin
  Tree.SetFocus;
  UpdateControlState;
end;

procedure TTabOrderFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('taborder');
end;

procedure TTabOrderFm.FormCreate(Sender: TObject);
begin
  Caption := rsTabOrder;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  SpeedButton1.Hint:=rsMoveUp;
  SpeedButton2.Hint:=rsMoveDown;
  SpeedButton3.Hint := rsAutoTabOrder;
end;

procedure TTabOrderFm.SpeedButton1Click(Sender: TObject);
var
  N, PN: TTreeNode;
begin
  N := Tree.Selected;
  PN := N.GetPrevSibling;
  N.MoveTo(PN, naInsert);
  TWinControl(N.Data).TabOrder:=TWinControl(PN.Data).TabOrder;
  UpdateControlState;
end;

procedure TTabOrderFm.SpeedButton2Click(Sender: TObject);
var
  N, NN: TTreeNode;
begin
  N := Tree.Selected;
  NN := N.GetNextSibling;
  NN.MoveTo(N, naInsert);
  TWinControl(NN.Data).TabOrder:=TWinControl(N.Data).TabOrder;
  UpdateControlState;
end;

procedure TTabOrderFm.SpeedButton3Click(Sender: TObject);
begin
  SetOrder(TWinControl(Tree.Selected.Data));
  Tree.Items.Clear;
  FillTree(FFm, nil);
  Tree.Items[0].Expand(True);
end;

procedure TTabOrderFm.TreeSelectionChanged(Sender: TObject);
begin
  UpdateControlState;
end;

procedure TTabOrderFm.FillTree(Ctrl: TWinControl; aNode: TTreeNode);
var
  i: Integer;
  N: TTreeNode;

  function AddNode: TTreeNode;
  var
    j: Integer;
    C: TWinControl;
  begin
    if aNode <> nil then
      for j := 0 to aNode.Count - 1 do
      begin
        C := TWinControl(aNode.Items[j].Data);
        if Ctrl.TabOrder < C.TabOrder then
        begin
          Result := Tree.Items.Insert(aNode.Items[j], GetComponentName(Ctrl));
          Exit;
        end;
      end;
    Result := Tree.Items.AddChild(aNode, GetComponentName(Ctrl));
  end;

begin
  N := AddNode;
  N.Data := Ctrl;
  for i := 0 to Ctrl.ControlCount - 1 do
  begin
    if (Ctrl.Controls[i] is TWinControl) and (not (Ctrl.Controls[i] is TGridButtons)) then
      FillTree(TWinControl(Ctrl.Controls[i]), N);
  end;
end;

procedure TTabOrderFm.SetOrder(Ctrl: TWinControl);
var
  i: Integer;
  L: TList;
  C: TWinControl;

  procedure AddToList(CC: TWinControl);
  var
    j: Integer;
    C: TWinControl;
  begin
    for j := 0 to L.Count - 1 do
    begin
      C := TWinControl(L[j]);
      if (CC.Top < C.Top) or ((CC.Top = C.Top) and (CC.Left < C.Left)) then
      begin
        L.Insert(j, CC);
        Exit;
      end;
    end;
    L.Add(CC);
  end;

begin
  L := TList.Create;
  try
    for i := 0 to Ctrl.ControlCount - 1 do
      if Ctrl.Controls[i] is TWinControl then
      begin
        C := TWinControl(Ctrl.Controls[i]);
        AddToList(C);
      end;
    for i := 0 to L.Count - 1 do
      TWinControl(L[i]).TabOrder:=i;
  finally
    L.Free;
  end;
end;

procedure TTabOrderFm.UpdateControlState;
begin
  SpeedButton1.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.GetPrevSibling <> nil);
  SpeedButton2.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.GetNextSibling <> nil);
  SpeedButton3.Enabled:=(Tree.Selected <> nil) and (Tree.Selected.Count > 0);
end;

function TTabOrderFm.ShowForm(Fm: TdxForm): Integer;
begin
  FFm := Fm;
  Tree.Items.Clear;
  FillTree(Fm, nil);
  Tree.Items[0].Expand(True);
  Result := ShowModal;
end;

end.

