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
    SDi: Integer;
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
    Link: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncsDblClick(Sender: TObject);
    procedure FuncsSelectionChange(Sender: TObject; User: boolean);
    procedure GroupsSelectionChange(Sender: TObject; User: boolean);
    procedure HelpButtonClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
  private
    FFuncName: String;
    FHelpList: THelpList;
    FHomePage: String;
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

function ShowFuncsForm(aFuncName: String): Integer;

implementation

uses
  apputils, inifiles, helpmanager, scriptmanager, langmanager, myctrls;

function ShowFuncsForm(aFuncName: String): Integer;
begin
  if FuncsFm = nil then
  begin
  	FuncsFm := TFuncsFm.Create(Application);
    FuncsFm.UpdateHelp;
  end;
  Result := FuncsFm.ShowForm(aFuncName);
end;

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
  with TIniFile.Create(LangMan.FuncsFile) do
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
      pH^.SDi := -1;
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
    pH^.SDi := F.SDi;
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
  Link.Hint := rsOpenHomePage;
  Help.DataProvider := THtmlProvider.Create(Self);
end;

procedure TFuncsFm.FormDestroy(Sender: TObject);
begin
  FHelpList.Free;
end;

procedure TFuncsFm.FormShow(Sender: TObject);
begin
  //Groups.ItemIndex:=-1;
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

{function ToHtml(const S: String): String;
begin
  Result := '<html><head><meta content="text/html;charset=UTF-8" http-equiv="Content-Type">' +
    '</head><body bgcolor=#fff8dc>' + S + '</body></html>';
end; }

procedure TFuncsFm.FuncsSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
  HR: THelpRec;
  S: String;
  SD: TScriptData;
begin
  if Funcs.ItemIndex < 0 then Exit;
  i := PtrInt(Funcs.Items.Objects[Funcs.ItemIndex]);
  HR := FHelpList[i];
  Title.Caption := Funcs.Items[Funcs.ItemIndex] + ' (' +
  	FHelpList.Groups[HR.Group] + ')';
  S := rsGroup + ': ' + FHelpList.Groups[HR.Group];
  if HR.SDi >= 0 then
  begin
    SD := ScriptMan.Scripts[HR.SDi];
    S := S + LineEnding + rsModule + ': ' + SD.Name;
    if SD.Author <> '' then S := S + LineEnding + rsAuthor + ': ' + SD.Author;
    if SD.Version <> '' then S := S + LineEnding + rsVersion + ': ' + SD.Version;
    FHomePage := SD.HomePage;
  end
  else
    FHomePage := '';
  Link.Visible := FHomePage <> '';
  Title.Hint := S;

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

procedure TFuncsFm.LinkClick(Sender: TObject);
begin
  OpenUrl(FHomePage);
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
      Funcs.Items.AddObject(H.Name, TObject(PtrInt(i)));
  end;
end;

type
  THackListBox = class(TListBox);

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
    // Временный костыль, который все-таки меняет выделение в списке.
    THackListBox(Groups).SendItemIndex;
    //
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

