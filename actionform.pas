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
unit ActionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls, dxctrls, strconsts, actionpans, DxActions,
  LResources;

type

  { TActionFm }

  TActionFm = class(TForm)
    Actions: TComboBox;
    ButtonPanel1: TButtonPanel;
    Help: TIpHtmlPanel;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Title: TLabel;
    procedure ActionsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FPan: TBasicActionPanel;
    FBn: TdxButton;
    FOldIdx: Integer;
    function GetActionType: TdxActionType;
    procedure CreatePanel(act: TdxActionType);
    procedure SetDefaultCaption;
    function LoadHelp(act: TdxActionType): String;
  public
    { public declarations }
    procedure ShowForm(Bn: TdxButton);
  end;

var
  ActionFm: TActionFm;

implementation

uses
  mytypes, apputils, helpform, langmanager;

{$R *.lfm}

function ActionTypeToStr(act: TdxActionType): String;
var
  S: String;
begin
  S := '';
  case act of
    actGotoForm: S := rsGotoForm;
    actPrint: S := rsPrint;
    actMassCalc: S := rsMassCalc;
    actOpenReport: S := rsOpenReport;
    actSaveChanges: S := rsSaveChanges;
    actUserMonitor: S := rsUserMonitor;
    actCallFunc: S := rsCallFunction;
    actClearFields: S := rsClearFields;
  end;
  Result := S;
end;

{ TActionFm }

procedure TActionFm.FormCreate(Sender: TObject);
begin
  Actions.Items.AddObject(rsGoToForm, TObject(actGotoForm));
  Actions.Items.AddObject(rsPrint, TObject(actPrint));
  Actions.Items.AddObject(rsMassCalc, TObject(actMassCalc));
  Actions.Items.AddObject(rsOpenReport, TObject(actOpenReport));
  Actions.Items.AddObject(rsSaveChanges, TObject(actSaveChanges));
  Actions.Items.AddObject(rsUserMonitor, TObject(actUserMonitor));
  Actions.Items.AddObject(rsCallFunction, TObject(actCallFunc));
  Actions.Items.AddObject(rsClearFields, TObject(actClearFields));
  Caption := rsAction;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TActionFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('buttonaction');
end;

function TActionFm.GetActionType: TdxActionType;
begin
  Result := actNone;
  if Actions.ItemIndex >= 0 then
    Result := TdxActionType(Actions.Items.Objects[Actions.ItemIndex]);
end;

procedure TActionFm.CreatePanel(act: TdxActionType);
begin
  case act of
    actGotoForm: FPan := TGotoFormPanel.Create(Self, TGotoFormAction);
    actPrint: FPan := TPrintActionPanel.Create(Self, TPrintAction);
    actMassCalc: FPan := TMassCalcActionPanel.Create(Self, TMassCalcAction);
    actOpenReport: FPan := TOpenReportActionPanel.Create(Self, TOpenReportAction);
    actSaveChanges: FPan := TSaveChangesActionPanel.Create(Self, TSaveChangesAction);
    actUserMonitor: FPan := TUserMonitorActionPanel.Create(Self, TUserMonitorAction);
    actCallFunc: FPan := TCallFuncActionPanel.Create(Self, TCallFuncAction);
    actClearFields: FPan := TClearFieldsActionPanel.Create(Self, TClearFieldsAction);
  end;
  FPan.Form := TdxForm(FBn.Owner);
end;

procedure TActionFm.SetDefaultCaption;
var
  S: String;
begin
  S := ActionTypeToStr(FBn.ActionType);
  if S <> '' then FBn.Caption := S;
end;

function ToHtml(const S: String): String;
begin
  Result := '<html><head><meta content="text/html;charset=UTF-8" http-equiv="Content-Type">' +
    '</head><body bgcolor=#fff8dc>' + S + '</body></html>';
end;

function TActionFm.LoadHelp(act: TdxActionType): String;
var
  SL: TStrings;
begin
  if not FileExists(LangMan.ActionsFile) then Exit;
  SL := TStringList.Create;
  //S := AppPath + 'languages' + DirectorySeparator + 'actions.ru.dat';
  with TIniFileEx.Create(LangMan.ActionsFile) do
  try
    ReadSectionRaw(IntToStr(Ord(act)), SL);
    Result := ToHtml(SL.Text);
  finally
    Free;
    SL.Free;
  end;
end;

procedure TActionFm.ActionsChange(Sender: TObject);
begin
  if Actions.ItemIndex = FOldIdx then Exit;

  FreeAndNil(FPan);
  CreatePanel(GetActionType);
  if FPan <> nil then
  begin
    FPan.Parent := Panel1;
    FPan.Align:=alClient;
  end;
  FPan.Load('');
  FPan.Init;
  Title.Caption := ActionTypeToStr(GetActionType);
  Help.SetHtmlFromStr(LoadHelp(GetActionType));
  FOldIdx := Actions.ItemIndex;
end;

procedure TActionFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrOk) and (FPan <> nil) then
    CanClose := FPan.Validate;
end;

procedure TActionFm.ShowForm(Bn: TdxButton);
var
  OldAct: TdxActionType;
begin
  OldAct := Bn.ActionType;
  FBn := Bn;
  FOldIdx := -1;
  Title.Caption := ' ';
  Actions.ItemIndex := Actions.Items.IndexOfObject(TObject(FBn.ActionType));
  Actions.OnChange(Actions);
  if FPan <> nil then
  begin
    FPan.Load(FBn.ActionProps);
    FPan.Init;
  end;
  if (ShowModal = mrOk) and (FPan <> nil) then
  begin
    FBn.ActionType := GetActionType;
    FBn.ActionProps := FPan.Save;
    if ((OldAct <> Bn.ActionType) and FBn.Glyph.Empty) or (FBn.ResName <> '') then FBn.SetDefaultGlyph;
    if FBn.Name = FBn.Caption then SetDefaultCaption;

  end;
  FreeAndNil(FPan);
end;

end.

