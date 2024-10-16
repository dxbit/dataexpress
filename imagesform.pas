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

unit ImagesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, ComCtrls, Menus, dximages, strconsts, SqlDb, DxCtrls, LazUtf8,
  TreeFilterEdit, LazFileUtils, LCLType, Buttons, mytypes, TreeViewEx;

type

  { TImagesFm }

  TImagesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Filter: TTreeFilterEdit;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    AddBn: TSpeedButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Tree: TTreeViewEx;
    procedure AddBnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    FImg: TdxImage;
    FPPIndex: Integer;
    FAddOnly: Boolean;
    FErrorLoadImageMsg: String;
    procedure FillTree;
    procedure ImgClick(Sender: TObject);
    procedure ImgPaint(Sender: TObject);
    procedure SetImage;
    function GetImgName: String;
    function GetImgIndex: Integer;
    procedure UpdateImage(const AImageName: String);
    procedure AddImage(AIndex: Integer);
    procedure AddImages;
    function GetUniqueImageName(const AName: String): String;
    procedure SetPopupState;
  public
    function ShowForm(ASelect: Boolean; ASelImageName: String; AddOnly: Boolean): Integer;
    function SelectedImageName: String;
  end;

var
  ImagesFm: TImagesFm;

function ShowImagesForm(ASelect: Boolean; ASelImageName: String; AddOnly: Boolean): Integer;

implementation

uses
  imagemanager, apputils, formmanager, dbengine, helpmanager, appsettings;

type

  { TImageNameForm }

  TImageNameForm = class(TForm)
  private
    FEdit: TEdit;
    FOldName: String;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(const ACaption: String; var ImgName: String; ANew: Boolean): Integer;
    function CloseQuery: boolean; override;
  end;

function ShowImageNameForm(const ACaption: String; var ImgName: String; ANew: Boolean): Integer;
begin
  with TImageNameForm.CreateNew(nil) do
  begin
    Result := ShowForm(ACaption, ImgName, ANew);
    Free;
  end;
end;

function ShowImagesForm(ASelect: Boolean; ASelImageName: String;
  AddOnly: Boolean): Integer;
begin
  if ImagesFm = nil then
    ImagesFm := TImagesFm.Create(Application);
  Result := ImagesFm.ShowForm(ASelect, ASelImageName, AddOnly);
end;

procedure CheckExistsImage(const AName: String; out Msg: String);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  S: String;
begin
  Msg := '';
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxButton then
        S := TdxButton(C).ImageName
      else if C is TdxImage then
        S := TdxImage(C).ImageName
      else
        Continue;

      if MyUtf8CompareText(S, AName) = 0 then
        Msg := Msg + Fm.FormCaption + '->' + GetComponentName(C) + LineEnding;
    end;
  end;
end;

{$R *.lfm}

{ TImageNameForm }

constructor TImageNameForm.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterImageName;
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
		Parent := Self;
    AnchorSideTop.Control := L;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 2;
    Text := '';
    Left := 8;
    Width := 328;
    MaxLength := 50;
  end;
  with TButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
  end;
  ClientWidth := 345;
  ClientHeight := 104;
end;

function TImageNameForm.ShowForm(const ACaption: String; var ImgName: String;
  ANew: Boolean): Integer;
begin
  Caption := ACaption;
  FEdit.Text := ImgName;
  if not ANew then
    FOldName := ImgName;
  Result := ShowModal;
  if Result = mrOk then
    ImgName:=FEdit.Text;
end;

function CheckImageName(const S: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do
    if S[i] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
    begin
      ErrMsg(rsInvalidCharInImageName);
      Exit;
    end;
  Result := True;
end;

procedure ReplaceInvalidChars(var S: String);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
      S[i] := '_';
end;

function TImageNameForm.CloseQuery: boolean;
begin
  if (ModalResult <> mrOk) or (MyUtf8CompareText(FOldName, FEdit.Text) = 0) then Exit;
  Result := False;
  if Trim(FEdit.Text) = '' then
  begin
    ErrMsg(rsEnterImageName);
    FEdit.SetFocus;
  end
  else if not CheckImageName(FEdit.Text) then
    FEdit.SetFocus
  else if ImageMan.ImageExists(FEdit.Text) then
  begin
    ErrMsg(rsImageAlreadyExists);
    FEdit.SetFocus;
  end
  else
    Result := True;
end;

{ TImagesFm }

procedure TImagesFm.MenuItem1Click(Sender: TObject);
begin
  AddImages;
  SetPopupState;
end;

procedure TImagesFm.MenuItem2Click(Sender: TObject);
var
  S, OldName: String;
begin
  S := GetImgName;
  OldName := S;
  if ShowImageNameForm(rsRenameImage, S, False) = mrOk then
  begin
    ImageMan.RenameImage(OldName, S);
    RenameImages(OldName, S);
    RenameInActions(nil, renImage, OldName, S);
    ImageCache.RenameImage(OldName, S);
    Tree.Selected.Text := S;
  end;
end;

procedure TImagesFm.MenuItem3Click(Sender: TObject);
var
  S, Msg, MsgAct: String;
  N: TTreeNode;
begin
  if Confirm(rsWarning, rsConfirmDeleteImage) = mrYes then
  begin
    S := GetImgName;
    CheckExistsImage(S, Msg);
    CheckImageExistsInActions(S, MsgAct);
    if Msg <> '' then
      Msg := rsImageUsedInCmp + Spaces + Msg + LineEnding;
    if MsgAct <> '' then
      Msg := Msg + rsImageUsedInAct + Spaces + MsgAct + LineEnding;
    if (Msg = '') or ((Msg <> '') and (Confirm(rsWarning, Msg + rsContinueToDelete) = mrYes)) then
    begin
      RenameImages(S, '');
      RenameInActions(nil, renImage, S, '');
      ImageMan.DeleteImage(S);
      ImageCache.DeleteImage(S);
      N := Tree.Selected.GetNextSibling;
      if N = nil then N := Tree.Selected.GetPrevSibling;
      Tree.Selected.Delete;
      if N <> nil then N.Selected := True
      else SetImage;
    end;

    SetPopupState;
  end;
end;

procedure TImagesFm.MenuItem4Click(Sender: TObject);
var
  FlName, S: String;
  i: Integer;
begin
  if Tree.Items.Count = 0 then
  begin
    AddImage(GetImgIndex);
    Exit;
  end;

  FlName := OpenPictureDialog;
  if FlName <> '' then
  begin
    try
      FImg.LoadFromFile(FlName);
      S := GetImgName;
      i := GetImgIndex;
      ImageMan.SetImage(S, i, FlName);
      // Вкладка соответствует текущей плотности пикселя
      if FPPIndex = i then
      begin
        ImageCache.DeleteImage(S);
        UpdateImage(S);
      end;
    except
      on E: Exception do
        ErrMsg(rsFailedToLoadImage + Spaces + E.Message);
    end;
  end;
end;

procedure TImagesFm.MenuItem5Click(Sender: TObject);
var
  FlName, Ext: String;
begin
  Ext := ImageMan.GetImageExt(GetImgName, GetImgIndex);
  if (Ext = '') or (Pos('*.' + Ext, rsSavePicturesFilter) = 0) then Ext := 'png';
  FlName := SavePictureDialog(GetImgName + '.' + Ext);
  if FlName <> '' then
    try
      FImg.SaveToFile(FlName);
    except
      on E: Exception do
        ErrMsg(rsFailedToSaveImage + Spaces + E.Message);
    end;
end;

procedure TImagesFm.MenuItem6Click(Sender: TObject);
var
  S: String;
  i: Integer;
begin
  FImg.Clear;
  S := GetImgName;
  i := GetImgIndex;
  ImageMan.ClearImage(S, i);
  // Вкладка соответствует текущей плотности пикселя
  if FPPIndex = i then UpdateImage(S);
end;

procedure TImagesFm.PageControl1Change(Sender: TObject);
begin
  SetImage;
  FImg.Parent := PageControl1.ActivePage;
end;

procedure TImagesFm.PopupMenu2Popup(Sender: TObject);
begin
  MenuItem4.Enabled := (Tree.Items.Count = 0) or (Tree.Selected <> nil);
  MenuItem5.Enabled := not FImg.BGRABitmap.Empty;
  MenuItem6.Enabled := not FImg.BGRABitmap.Empty;
end;

procedure TImagesFm.TreeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Utf8Key < ' ' then Exit;
  Filter.SetFocus;
  Filter.Text:=Utf8Key;
  Filter.SelStart := 2;
end;

procedure TImagesFm.FillTree;
var
  DS: TSQLQuery;
begin
  DS := ImageMan.DataSet;
  DS.First;
  while not DS.Eof do
  begin
    Tree.Items.AddChild(nil, DS.Fields[1].AsString);
    DS.Next;
  end;
  Tree.AlphaSort;
end;

procedure TImagesFm.ImgClick(Sender: TObject);
begin
  FImg.PopupMenu.PopUp;
end;

procedure TImagesFm.ImgPaint(Sender: TObject);
var
  TS: TTextStyle;
  Msg: String;
begin
  if FImg.BGRABitmap.Empty then
  begin
    TS := FImg.Canvas.TextStyle;
    TS.Alignment:=taCenter;
    TS.Layout:=tlCenter;
    TS.Wordbreak:=True;
    TS.SingleLine:=False;

    if FErrorLoadImageMsg <> '' then
    begin
      FImg.Canvas.Font.Color := clRed;
      Msg := FErrorLoadImageMsg;
    end
    else
    begin
      FImg.Canvas.Font.Color := clDkGray;
      Msg := rsClickToLoadImage;
    end;

    FImg.Canvas.TextRect(Rect(0, 0, FImg.Width, FImg.Height), 0, 0, Msg, TS);
  end;
end;

procedure TImagesFm.SetImage;
var
  St: TStream;
begin
  FErrorLoadImageMsg := '';
  FImg.Clear;
  if Tree.Selected = nil then Exit;
  ImageMan.GetImageStream(GetImgName, GetImgIndex, St);
  if St <> nil then
    try try
      FImg.LoadFromStream(St);
    except
      on E: Exception do
        FErrorLoadImageMsg := E.Message;
    end;
    finally
      St.Free;
    end;
end;

function TImagesFm.GetImgName: String;
begin
  with Tree do
    if Selected <> nil then Result := Selected.Text
    else Result := '';
end;

function TImagesFm.GetImgIndex: Integer;
begin
  Result := PageControl1.ActivePageIndex;
end;

procedure TImagesFm.UpdateImage(const AImageName: String);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
  S: String;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if (C is TdxButton) or (C is TdxImage) then
      begin
        S := GetImageName(C);
        if (S <> '') and (S = AImageName) then
        begin
          SetImageName(C, '');
          SetImageName(C, S);
        end;
      end;
    end;
  end;
end;

procedure TImagesFm.AddImage(AIndex: Integer);
var
  FileName, S: String;
  N: TTreeNode;
begin
  FileName := OpenPictureDialog;
  if FileName = '' then Exit;
  S := ExtractFileNameOnly(FileName);
  if ShowImageNameForm(rsAddImage, S, True) = mrOk then
  begin
    ImageMan.AddImage(S);
    N := Tree.Items.AddChild(nil, S);
    N.Selected := True;
    try
      if GetImgIndex = AIndex then FImg.LoadFromFile(FileName)
      else FImg.Clear;
      ImageMan.SetImage(S, AIndex, FileName);
    except
      on E: Exception do
        ErrMsg(rsFailedToLoadImage + Spaces + E.Message);
    end;
  end;
end;

procedure TImagesFm.AddImages;
var
  i: Integer;
  FL: TStringList;
  ImgNm, FileNm, RenamesBuf: String;
  N: TTreeNode;
begin
  N := nil;
  RenamesBuf := '';
  FL := TStringList.Create;

  if OpenPictureDialogMulti(FL) then
  try try
    Tree.BeginUpdate;
    for i := 0 to FL.Count - 1 do
    begin
      FileNm := ExtractFileNameOnly(FL[i]);
      ImgNm := GetUniqueImageName(FileNm);
      ImageMan.AddImage(ImgNm);
      N := Tree.Items.AddChild(nil, ImgNm);
      ImageMan.SetImage(ImgNm, 0, FL[i]);

      if FileNm <> ImgNm then
        RenamesBuf := RenamesBuf + Format(rsImageRenameItem, [FileNm, ImgNm]);
    end;
  finally
    Tree.EndUpdate;
    if N <> nil then N.Selected := True;
  end;
  except
    on E: Exception do
      ErrMsg(rsFailedToLoadImage + Spaces + E.Message);
  end;

  FL.Free;

  if RenamesBuf <> '' then
    Info(rsRenameAddedImagesMsg + LineEnding + LineEnding + RenamesBuf);
end;

function TImagesFm.GetUniqueImageName(const AName: String): String;
var
  n: Integer;
  BaseName: String;
begin
  n := 1;
  BaseName := Utf8Copy(AName, 1, 50);
  ReplaceInvalidChars(BaseName);
  Result := BaseName;

  while ImageMan.ImageExists(Result) do
  begin
    Result := BaseName + IntToStr(n);
    Inc(n);

    if Utf8Length(Result) > 50 then
    begin
      BaseName := Utf8Copy(BaseName, 1, 50 - Length(IntToStr(n)));
      Result := BaseName;
      n := 1;
    end;
  end;
end;

procedure TImagesFm.SetPopupState;
begin
  MenuItem2.Enabled := (Tree.Selected <> nil) and not FAddOnly;
  MenuItem3.Enabled := (Tree.Selected <> nil) and not FAddOnly;
  MenuItem2.Visible := not FAddOnly;
  MenuItem3.Visible := not FAddOnly;
end;

procedure TImagesFm.FormCreate(Sender: TObject);
begin
  FImg := TdxImage.Create(Self);
  with FImg do
  begin
    Parent := TabSheet1;
    Align := alClient;
    Center := True;
    Proportional := True;
    Stretch := True;
    KeepSize := True;
    PopupMenu := PopupMenu2;
    OnClick := @ImgClick;
    OnPaint:=@ImgPaint;
  end;

  Caption := rsGallery;
  Filter.TextHint := rsFilter;
  MenuItem1.Caption := rsAppend;
  MenuItem2.Caption := rsRename;
  MenuItem3.Caption := rsDelete;
  MenuItem4.Caption := rsLoadImage;
  MenuItem5.Caption := rsSaveImage;
  MenuItem6.Caption := rsClear;
  SetupImageList(Images, ['add16', 'edit16', 'delete16', 'db16', 'save16', 'delete16']);
  SetupSpeedButton(AddBn, 'add16');
  AddBn.Hint := rsAppend;

  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  Tree.IsWine := AppConfig.IsWine;
end;

procedure TImagesFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TImagesFm.AddBnClick(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TImagesFm.FormShow(Sender: TObject);
begin
  Tree.SetFocus;
end;

procedure TImagesFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('gallery');
end;

procedure TImagesFm.TreeDblClick(Sender: TObject);
var
  P: TPoint;
  N: TTreeNode;
begin
  P := Tree.ScreenToClient(Mouse.CursorPos);
  N := Tree.GetNodeAt(P.x, P.y);
  if (N <> nil) and N.Selected then
  begin
    if ButtonPanel1.OkButton.Visible then
      ModalResult := mrOk
    else
      MenuItem2.Click;
  end;
end;

procedure TImagesFm.TreeSelectionChanged(Sender: TObject);
begin
  SetImage;
end;

function TImagesFm.ShowForm(ASelect: Boolean; ASelImageName: String;
  AddOnly: Boolean): Integer;
var
  N: TTreeNode;
begin
  FPPIndex := GetPPIndex;
  FAddOnly := AddOnly;

  if ASelect then
  begin
    KeyPreview := False;
    MenuItem2.Default := False;
    ButtonPanel1.ShowButtons := [pbOk, pbCancel, pbHelp]
  end
  else
  begin
    KeyPreview := True;
    MenuItem2.Default := True;
    ButtonPanel1.ShowButtons := [pbClose, pbHelp];
  end;

  Tree.Items.Clear;
  Filter.Text := '';
  FillTree;
  PageControl1.ActivePageIndex := 0;
  if Tree.Items.Count > 0 then
  begin
    if ASelImageName = '' then
      Tree.Items.GetFirstNode.Selected := True
    else
    begin
      N := Tree.Items.FindNodeWithText(ASelImageName);
      if N <> nil then N.Selected := True;
    end;
  end;

  SetPopupState;

  Result := ShowModal;
  //if FChangeList.Count > 0 then UpdateImages;
end;

function TImagesFm.SelectedImageName: String;
begin
  Result := GetImgName;
end;

end.

