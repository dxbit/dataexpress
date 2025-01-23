{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit StructTree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, treepanel, strconsts, ComCtrls, Controls;

type

  { TStructTree }

  TStructTree = class(TTreePanel)
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildTree; override;
  end;

implementation

uses
  formmanager, apputils, dximages, dxfiles;

function GetImageIdx(C: TComponent): Integer;
var
  n: Integer;
begin
  n := -1;
  if C is TdxEdit then n := 2
  else if C is TdxCalcEdit then n := 3
  else if C is TdxDateEdit then n := 4
  else if C is TdxTimeEdit then n := 5
  else if C is TdxMemo then n := 6
  else if C is TdxCheckBox then n := 7
  else if C is TdxComboBox then n := 8
  else if C is TdxLookupComboBox then n := 9
  else if C is TdxCounter then n := 10
  else if C is TdxDBImage then n := 11
  else if C is TdxFile then n := 12
  else if C is TdxRecordId then n := 13;
  Result := n;
end;

procedure SetImageIdx(N: TTreeNode; i: Integer);
begin
  N.ImageIndex:=i;
  N.SelectedIndex:=i;
end;

{ TStructTree }

constructor TStructTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Filter.TextHint := rsFindField;
  Tree.Images := TImageList.Create(Self);
  SetupImageList(Tree.Images, ['form16', 'grid16', 'text16', 'calc16', 'date16', 'clock16', 'memo16',
    'checkbox16', 'combobox16', 'object16', 'counter16', 'dbimage16', 'file16', 'key16']);
end;

procedure TStructTree.BuildTree;
var
  i, j: Integer;
  Fm: TdxForm;
  FmNode, N: TTreeNode;
  C: TComponent;
begin
  inherited BuildTree;

  Tree.BeginUpdate;
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    if Fm.ViewType = vtSimpleForm then Continue;

    FmNode := Tree.Items.AddChild(nil, Fm.FormCaption + ' (T' + IntToStr(Fm.Id) + ')');
    if Fm.PId = 0 then
      SetImageIdx(FmNode, 0)
    else
      SetImageIdx(FmNode, 1);

    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if not IsField(C) then Continue;

      N := Tree.Items.AddChild(FmNode, GetFieldName(C) + ' (F' + IntToStr(GetId(C)) + ')');
      SetImageIdx(N, GetImageIdx(C));
    end;
  end;
  Tree.AlphaSort;
  Tree.EndUpdate;
end;

end.

