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
unit HelpForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus, EditBtn, strconsts, LclType,
  IpMsg;

type
  TMyAction = (actSelect, actBrowse, actBack, actFwd);

  { TMyFileDataProvider }

  TMyFileDataProvider = class(TIpFileDataProvider)
  public
    function BuildURL(const Old, New: string): string; override;
  end;

  { TUrlStack }

  TUrlStack = class(TList)
  private
    FPos: Integer;
    function GetIndex: Integer;
  public
    procedure AddIndex(idx: Integer);
    procedure DeleteForward;
    procedure GoBack;
    procedure GoFwd;
    function CanBack: Boolean;
    function CanFwd: Boolean;
    property Index: Integer read GetIndex;
  end;

  { THelpDB }

  THelpDB = class
  private
    FBounds: TRect;
    FShowIndex: Boolean;
    FStack: TUrlStack;
    FUrlList: TStringList;
    FContent: TStringStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    property Stack: TUrlStack read FStack;
    property Content: TStringStream read FContent;
    property UrlList: TStringList read FUrlList;
    property ShowIndex: Boolean read FShowIndex write FShowIndex;
    property Bounds: TRect read FBounds write FBounds;
  end;

  { THelpFm }

  THelpFm = class(TForm)
    BackButton: TToolButton;
    StaticText1: TStaticText;
    TextSearch: TEditButton;
    ForwardButton: TToolButton;
    Image1: TImage;
    ImageList1: TImageList;
    IndexButton: TToolButton;
    Founds: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Title: TLabel;
    ToolBar1: TToolBar;
    Tree: TTreeView;
    procedure BackButtonClick(Sender: TObject);
    procedure FoundsSelectionChange(Sender: TObject; User: boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure TextSearchButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure IHPDocumentOpen(Sender: TObject);
    procedure IndexButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure TextSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TextSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure TreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FAct: TMyAction;
    FDisableSelection: Boolean;
    IPFileDataProvider: TMyFileDataProvider;
    IHP: TIPHtmlPanel;
    FSearchText: String;
    procedure IpGetHtml(Sender: TObject; const URL: string;
      const PostData: TIpFormDataEntity; var Stream: TStream);
    procedure LoadContent;
    procedure OpenUrl(Url: String);
    procedure SetControlState;
    procedure DoSearchText(const aText: String);
  public
    { public declarations }
    procedure ShowUrl(Url: String);
  end;

var
  HelpFm: THelpFm;
  HelpDB: THelpDB;

procedure OpenHelp(const Url: String);

implementation

uses
  AppSettings, apputils, clipbrd, LazUtf8, langmanager;

{function GetHelpDir: String;
begin
  Result := AppPath + 'help' + DirectorySeparator + {AppConfig.Language} 'ru' +
    DirectorySeparator;
end;  }

procedure OpenHelp(const Url: String);
begin
  {if AppConfig.Language <> 'ru' then
  begin
    ErrMsg('Not ready');
    Exit;
  end;      }
  FreeAndNil(HelpFm);
  HelpFm := THelpFm.Create(Application);
  HelpFm.LoadContent;
  HelpFm.ShowUrl(Url);
end;

{function GetHtmlMarkedFounds(const FileName, aSearchText: String): String;
var
  S: String;
begin
	with TFileStream.Create(FileName, fmOpenRead) do
  try
  	SetLength(S, Size);
    Read(Pointer(S)^, Size);
  finally
    Free;
  end;
  Result := Utf8StringReplace(S, aSearchText, '<font color=green size=5><b><u>' +
  	aSearchText + '</u></b></font>', [rfReplaceAll, rfIgnoreCase]);
end;    }

{$R *.lfm}

{ TMyFileDataProvider }

function TMyFileDataProvider.BuildURL(const Old, New: string): string;
begin
  Result:=inherited BuildURL(Old, New);
  if Pos('file://', Result) = 0 then
    Result := 'file://' + StringReplace(LangMan.HelpDir + Result, '\', '/', [rfReplaceAll]);
end;

{ THelpDB }

constructor THelpDB.Create;
begin
  FStack := TUrlStack.Create;
  FUrlList := TStringList.Create;
  FContent := TStringStream.Create('');
  FBounds := Rect(100, 100, 740, 540);
end;

destructor THelpDB.Destroy;
begin
  FContent.Free;
  FUrlList.Free;
  FStack.Free;
  inherited Destroy;
end;

procedure THelpDB.Load;
var
  SL: TStringList;
  S: String;
  i, p: Integer;
begin
  //if AppConfig.Language <> 'ru' then Exit;
  {S := AppConfig.Language;
  if S = '' then S := 'ru';
  S := AppPath + 'help' + DirectorySeparator + S +
    DirectorySeparator + 'index'; }
  S := LangMan.HelpIndexFile;
  if not FileExists(S) then Exit;
  {begin
    ErrMsg(Format(rsFileNotExists, [S]));
    Exit;
  end;  }
  SL := TStringList.Create;
  SL.LoadFromFile(S);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos('=', S);
    FUrlList.Add(Copy(S, p + 1, 255));
    Delete(S, p, 255);
    S := S + LineEnding;
    FContent.Write(Pointer(S)^, Length(S));
  end;
  SL.Free;
end;

{ TUrlStack }

function TUrlStack.GetIndex: Integer;
begin
  Result := -1;
  if FPos >= 0 then
    Result := Integer(Items[FPos]);
end;

procedure TUrlStack.AddIndex(idx: Integer);
begin
  DeleteForward;
  FPos := Add(Pointer(idx));
end;

procedure TUrlStack.DeleteForward;
var
  i: Integer;
begin
  for i := Count - 1 downto FPos + 1 do
    Delete(i);
end;

procedure TUrlStack.GoBack;
begin
  if CanBack then
    Dec(FPos);
end;

procedure TUrlStack.GoFwd;
begin
  if CanFwd then
    Inc(FPos);
end;

function TUrlStack.CanBack: Boolean;
begin
  Result := FPos > 0;
end;

function TUrlStack.CanFwd: Boolean;
begin
  Result := FPos < Count - 1;
end;

{ THelpFm }

procedure THelpFm.IHPDocumentOpen(Sender: TObject);
var
  S, Url: String;
  i: Integer;
begin
  S := IHP.Title;
  if (S = '') and (Tree.Selected <> nil) then
  	S := Tree.Selected.Text;
  Title.Caption := S;
  if FAct = actBrowse then
  begin
    Url := ChangeFileExt(ExtractFileName(IHP.CurURL), '');
    i := HelpDB.UrlList.IndexOf(Url);
    if (i >= 0) and (i < Tree.Items.Count) then
    begin
      HelpDB.Stack.AddIndex(i);
      FDisableSelection := True;
      Tree.Items[i].Selected:=True;
      with Founds do
      	ItemIndex := Items.IndexOfObject(TObject(i));
      FDisableSelection := False;
    end;
  end;
  FAct := actBrowse;
  SetControlState;
end;

procedure THelpFm.IndexButtonClick(Sender: TObject);
begin
  if IndexButton.Down then
  begin
    PageControl1.Parent := Self;
    Splitter1.Parent := Self;
  end
  else
  begin
    Splitter1.Parent := nil;
    PageControl1.Parent := nil;
  end;
end;

procedure THelpFm.MenuItem1Click(Sender: TObject);
var
  S: String;
begin
  IHP.CopyToClipboard;
  Clipboard.Open;
  try
    S := StringReplace(Clipboard.AsText, LineEnding + ' ' + LineEnding, '', [rfReplaceAll]);
    Clipboard.Clear;
    Clipboard.AsText := S;
  finally
    Clipboard.Close;
  end;
end;

procedure THelpFm.MenuItem2Click(Sender: TObject);
begin
  IHP.SelectAll;
end;

procedure THelpFm.TextSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (Founds.Count > 0) then
  begin
    Key := 0;
    Founds.SetFocus;
    Founds.ItemIndex := 0;
    Founds.Click;
  end;
end;

procedure THelpFm.TextSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DoSearchText(Utf8LowerCase(TextSearch.Text));
  end;
end;

procedure THelpFm.TreeSelectionChanged(Sender: TObject);
var
  i: Integer;
begin
  if FDisableSelection then Exit;
  FAct := actSelect;
  i := Tree.Selected.AbsoluteIndex;
  HelpDB.Stack.AddIndex(i);
  OpenUrl(HelpDB.UrlList[i]);
  with Founds do
  	ItemIndex := Items.IndexOfObject(TObject(i));
end;

procedure THelpFm.OpenUrl(Url: String);
begin
  if Url = '' then Exit;
  Url := LangMan.HelpDir + Url + '.html';
  if FileExists(Url) then
  begin
    //if FSearchText = '' then
	    IHP.OpenURL('file://' + StringReplace(Url, DirectorySeparator, '/', [rfReplaceAll]))
    //else
    //  IHP.SetHtmlFromStr(GetHtmlMarkedFounds(Url, FSearchText));
  end
  else
    ErrMsg(Format(rsFileNotExists, [Url]));
  SetControlState;
end;

procedure THelpFm.SetControlState;
begin
  BackButton.Enabled:=HelpDB.Stack.CanBack;
  ForwardButton.Enabled := HelpDB.Stack.CanFwd;
end;

procedure THelpFm.DoSearchText(const aText: String);
var
  i: Integer;
  N: TTreeNode;
  S: String;
  SL: TStringList; // хранит список обработанных файлов. Чтобы
                   // не искать в одном файле несколько раз.
                   // В содержании есть ссылки на один и тот же файл.

  function SearchInFile(const FileName: String): Boolean;
  var
    S: String;
  begin
    Result := False;
    with TFileStream.Create(FileName, fmOpenRead) do
    try
      SetLength(S, Size);
      Read(Pointer(S)^, Size);
    finally
      Free;
    end;
    S := Utf8LowerCase(S);
    Result := Utf8Pos(aText, S, 1) > 0;
  end;

  function FindNodeByAbsIndex(idx: Integer): TTreeNode;
  var
    j: Integer;
    N: TTreeNode;
  begin
    Result := nil;
  	for j := Tree.Items.Count - 1 downto 0 do
		begin
    	N := Tree.Items[j];
      if N.AbsoluteIndex = idx then Exit(N);
    end;
  end;

begin
  SL := TStringList.Create;
	Founds.Clear;
  for i := 0 to HelpDB.UrlList.Count - 1 do
  begin
  	S := HelpDB.UrlList[i];
    if (S <> '') and (SL.IndexOf(S) < 0) and
    	SearchInFile(LangMan.HelpDir + S + '.html') then
    begin
      N := FindNodeByAbsIndex(i);
      if N <> nil then
      	Founds.Items.AddObject(N.Text, TObject(i));
      SL.Add(S);
    end;
  end;
  SL.Free;
  FSearchText := TextSearch.Text;
  StaticText1.Visible := Founds.Count = 0;
end;

procedure THelpFm.LoadContent;
begin
  HelpDB.Content.Position:=0;
  Tree.LoadFromStream(HelpDB.Content);
end;

procedure THelpFm.IpGetHtml(Sender: TObject; const URL: string;
  const PostData: TIpFormDataEntity; var Stream: TStream);
var
  FileName, Buf: String;
begin
	FileName := Url;
  Delete(FileName, 1, 7);	// file://
  FileName := StringReplace(FileName, '/', DirectorySeparator, [rfReplaceall]);
  if not FileExists(FileName) then Exit;

  with TFileStream.Create(FileName, fmOpenRead) do
  try
    SetLength(Buf, Size);
    Read(Pointer(Buf)^, Size);
  finally
    Free;
  end;

  if FSearchText <> '' then
		Buf := Utf8StringReplace(Buf, FSearchText, '<font color=green size=5><b><u>' +
  		FSearchText + '</u></b></font>', [rfReplaceAll, rfIgnoreCase]);

  Stream := TStringStream.Create(Buf);
end;

procedure THelpFm.ForwardButtonClick(Sender: TObject);
var
  i: Integer;
begin
  FAct := actFwd;
  HelpDB.Stack.GoFwd;
  i := HelpDB.Stack.Index;
  FDisableSelection := True;
  Tree.Items[i].Selected:=True;
  with Founds do
  	ItemIndex := Items.IndexOfObject(TObject(i));
  FDisableSelection := False;
  OpenUrl(HelpDb.UrlList[i]);
end;

procedure THelpFm.BackButtonClick(Sender: TObject);
var
  i: Integer;
begin
  FAct := actBack;
  HelpDB.Stack.GoBack;
  i := HelpDB.Stack.Index;
  FDisableSelection := True;
  Tree.Items[i].Selected:=True;
  with Founds do
  	ItemIndex := Items.IndexOfObject(TObject(i));
  FDisableSelection := False;
  OpenUrl(HelpDb.UrlList[i]);
end;

procedure THelpFm.FoundsSelectionChange(Sender: TObject; User: boolean);
//var
//  N: TTreeNode;
begin
  with Founds do
  	if ItemIndex >= 0 then
    begin
      Tree.Items[ Integer(Items.Objects[ItemIndex]) ].Selected := True;
      {N := Tree.Items.FindNodeWithText(Items[ItemIndex]);
      if N <> nil then N.Selected := True;  }
		  //OpenUrl(HelpDB.UrlList[Integer(Items.Objects[ItemIndex])],
      //	FSearchText);
    end;
end;

procedure THelpFm.PageControl1Change(Sender: TObject);
begin
	case PageControl1.ActivePageIndex of
  	0: Tree.SetFocus;
    1: Founds.SetFocus;
  end;
end;

procedure THelpFm.TextSearchButtonClick(Sender: TObject);
begin
  TextSearch.Text := '';
  Founds.Clear;
  FSearchText := '';
  StaticText1.Visible := False;
end;

procedure THelpFm.FormCreate(Sender: TObject);
begin
  Caption := rsHelp;
  MenuItem1.Caption := rsCopy;
  MenuItem2.Caption := rsSelectAll;
  IPFileDataProvider := TMyFileDataProvider.Create(Self);
  IHP := TIPHtmlPanel.Create(Self);
  IHP.DataProvider := IPFileDataProvider;
  IHP.Parent := Panel2;
  IHP.Align := alClient;
  IHP.OnDocumentOpen:=@IHPDocumentOpen;
  IHP.PopupMenu := PopupMenu1;
  IPFileDataProvider.OnGetHtml:=@IpGetHtml;
  TabSheet1.Caption := rsContents;
  TabSheet2.Caption := rsSearch;
  TextSearch.Button.LoadGlyphFromLazarusResource('delete16');
  TextSearch.TextHint:=rsEnterSearchText;
  StaticText1.Caption := rsNothingFound;
end;

procedure THelpFm.FormDestroy(Sender: TObject);
begin
  HelpDB.Bounds := BoundsRect;
  HelpDB.ShowIndex := IndexButton.Down;
end;

procedure THelpFm.ShowUrl(Url: String);
var
  i: Integer;
begin
  if HelpDB.UrlList.Count = 0 then Exit;
  IndexButton.Down := HelpDB.ShowIndex;
  if Url = '' then IndexButton.Down := True;
  IndexButton.Click;
  Show;
  BoundsRect := HelpDB.Bounds;
  {OpenUrl(Url); }
  i := HelpDB.UrlList.IndexOf(Url);
  if Url = '' then i := 0;
  if (i >= 0) and (i < Tree.Items.Count) then
    Tree.Items[i].Selected:=True;
end;

end.

