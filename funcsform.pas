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
unit FuncsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls, strconsts;

type
  PHelpRec = ^THelpRec;
  THelpRec = record
    Name, Help: String;
    Group: Integer;
    FuncIdx: Integer;
  end;

  { THelpList }

  THelpList = class(TList)
  private
    FGroups: TStrings;
    function GetFuncs(Index: Integer): THelpRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile;
    procedure LoadFromExprModules;
    function FindFunc(const Name: String): Integer;
    procedure Clear; override;
    property Funcs[Index: Integer]: THelpRec read GetFuncs; default;
    property Groups: TStrings read FGroups;
  end;

  { TFuncsFm }

  TFuncsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Help: TIpHtmlPanel;
    Image1: TImage;
    Label1: TLabel;
    Groups: TListBox;
    Funcs: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Title: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncsDblClick(Sender: TObject);
    procedure FuncsSelectionChange(Sender: TObject; User: boolean);
    procedure GroupsSelectionChange(Sender: TObject; User: boolean);
    procedure HelpButtonClick(Sender: TObject);
  private
    FFuncName: String;
    FHelpList: THelpList;
    { private declarations }
    procedure FillFuncs;
  public
    { public declarations }
    property FuncName: String read FFuncName;
    function ShowForm(aFuncName: String): Integer;
    procedure UpdateHelp;
  end;

var
  FuncsFm: TFuncsFm;

implementation

uses
  apputils, inifiles, appsettings, helpform, scriptmanager, mytypes,
  langmanager;

{$R *.lfm}

{ THelpList }

function THelpList.GetFuncs(Index: Integer): THelpRec;
begin
  Result := PHelpRec(Items[Index])^;
end;

constructor THelpList.Create;
begin
  inherited Create;
  FGroups := TStringList.Create;
end;

destructor THelpList.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

procedure THelpList.LoadFromFile;
var
  SL, SL2: TStringList;
  i: Integer;
  S: String;
  pH: PHelpRec;
begin
  SL := nil;
  SL2 := nil;
  {if AppConfig.Language <> '' then
    S := 'funcs.' + AppConfig.Language + '.dat'
  else
    S := 'funcs.dat';
  S := AppPath + 'languages' + DirectorySeparator + S; }
  if not FileExists(LangMan.FuncsFile) then Exit;
  with TIniFileEx.Create(LangMan.FuncsFile) do
  try
    SL := TStringList.Create;
    SL2 := TStringList.Create;
    ReadSectionRaw('groups', FGroups);
    ReadSections(SL);
    SL.Delete(0);    // удаляем группы
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      SL2.Clear;
      ReadSectionRaw(S, SL2);
      New(pH);
      pH^.Name := S;
      pH^.Group := StrToInt(SL2[0]);
      SL2.Delete(0);
      pH^.Help := SL2.Text;
      pH^.FuncIdx:=-1;
      Add(pH);
    end;
  finally
    Free;
    FreeAndNil(SL2);
    FreeAndNil(SL);
  end;
end;

procedure THelpList.LoadFromExprModules;
var
  i, n: Integer;
  F: TExprFunc;
  pH: PHelpRec;
begin
  for i := 0 to ScriptMan.Funcs.Count - 1 do
  begin
    F := ScriptMan.Funcs[i];
    n := FGroups.IndexOf(F.Group);
		if n < 0 then
      n := FGroups.Add(F.Group);
    New(pH);
    pH^.Name:=F.Name;
    pH^.Group:=n;
    pH^.Help:=F.Description;
    pH^.FuncIdx:=-1;
    Add(pH);
  end;
end;

function THelpList.FindFunc(const Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if CompareText(Funcs[i].Name, Name) = 0 then Exit(i);
  end;
end;

procedure THelpList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PHelpRec(Items[i]));
  inherited Clear;
end;


{ TFuncsFm }

procedure TFuncsFm.FormCreate(Sender: TObject);
begin
  Caption := rsFunctions;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  FHelpList := THelpList.Create;
  Title.Caption := '';
end;

procedure TFuncsFm.FormDestroy(Sender: TObject);
begin
  FHelpList.Free;
end;

procedure TFuncsFm.FormShow(Sender: TObject);
begin
  //Groups.ItemIndex := 0;
  Funcs.SetFocus;
end;

procedure TFuncsFm.FuncsDblClick(Sender: TObject);
begin
  if Funcs.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TFuncsFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOk then Exit;
  if Funcs.ItemIndex >= 0 then FFuncName := Funcs.Items[Funcs.ItemIndex];
end;

procedure TFuncsFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  if Funcs.ItemIndex < 0 then
  begin
    ErrMsg(rsFuncNotSel);
    CanClose := False;
  end;
end;

function ToHtml(const S: String): String;
begin
  Result := '<html><head><meta content="text/html;charset=UTF-8" http-equiv="Content-Type">' +
    '</head><body bgcolor=#fff8dc>' + S + '</body></html>';
end;

procedure TFuncsFm.FuncsSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
  HR: THelpRec;
begin
  if Funcs.ItemIndex < 0 then Exit;
  i := Integer(Funcs.Items.Objects[Funcs.ItemIndex]);
  HR := FHelpList[i];
  Title.Caption := Funcs.Items[Funcs.ItemIndex] + ' (' +
  	FHelpList.Groups[HR.Group] + ')';
  Self.Help.SetHtmlFromStr(ToHtml(HR.Help));
end;

procedure TFuncsFm.GroupsSelectionChange(Sender: TObject; User: boolean);
begin
  FillFuncs;
  if Funcs.Items.Count > 0 then
  	Funcs.ItemIndex := 0
  else
	  Title.Caption := '';
end;

procedure TFuncsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('funcs');
end;

procedure TFuncsFm.FillFuncs;
var
  i, gr: Integer;
  H: THelpRec;
begin
  Funcs.Clear;
  Help.SetHtmlFromStr(ToHtml(' '));
  if Groups.ItemIndex < 0 then Exit;
  gr := Groups.ItemIndex - 1;
  for i := 0 to FHelpList.Count - 1 do
  begin
    H := FHelpList[i];
    if (H.Group = gr) or (gr < 0) then
      Funcs.Items.AddObject(H.Name, TObject(i));
  end;
end;

function TFuncsFm.ShowForm(aFuncName: String): Integer;
var
  i: Integer;
  HR: THelpRec;
begin
  i := -1;
  if aFuncName <> '' then
	  i := FHelpList.FindFunc(aFuncName);
  if i >= 0 then
  begin
    HR := FHelpList[i];
		Groups.ItemIndex := 0;
    Funcs.ItemIndex := Funcs.Items.IndexOf(HR.Name);
  end
  else if Groups.ItemIndex < 0 then
  	Groups.ItemIndex := 0;
  Result := ShowModal;
end;

procedure TFuncsFm.UpdateHelp;
begin
  FHelpList.Clear;
  FHelpList.Groups.Clear;
  FHelpList.LoadFromFile;
  FHelpList.LoadFromExprModules;
  Groups.Clear;
  Groups.Items.AddStrings(FHelpList.Groups);
  Groups.Items.Insert(0, rsAllFunctions);
end;

end.

