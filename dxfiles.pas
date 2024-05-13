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

unit dxFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dbctrlsex, Menus, strconsts, Db,
  BufDataSet, LclType, LMessages, LclIntf;

type

  { TdxFile }

  TdxFile = class(TCustomDBEditButton)
  private
    FCanEdit: Boolean;
    FCheckExpression: String;
    FFieldName: String;
    FFieldSize: Integer;
    FId: Integer;
    FOldSize: Integer;
    FRequired: Boolean;
    FStopTab: Boolean;
    FStorageFolder: String;
    FStorageType: Integer;
    FPopup: TPopupMenu;
    function GetDescription: String;
    function GetSourceFileName: String;
    function GetStoredFileName: String;
    procedure PopupPopup(Sender: TObject);
    procedure PopupHandler(Sender: TObject);
    function DS: TDataSet;
    procedure SetDescription(AValue: String);
  protected
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure Loaded; override;
    procedure SetReadOnly(AValue: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    function GetDrawText: String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property OldSize: Integer read FOldSize write FOldSize;
    property CanEdit: Boolean read FCanEdit write FCanEdit;
  public
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure SaveToStream(St: TStream);
    function WasChanged: Boolean;
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
    property Description: String read GetDescription write SetDescription;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property ButtonWidth;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property Flat stored False;
    property Glyph stored False;
    property NumGlyphs stored False;
    property PopupMenu stored False;
  end;

function LoadFileFromFile(const FileName: String; aFile: TdxFile; DS: TDataSet): String;
function SaveFileToFile(const FileName: String; aFile: TdxFile; DS: TDataSet): String;
function GetFileStream(aFile: TdxFile; DS: TDataSet): TStream;
function GetFileFileName(aFile: TdxFile; DS: TDataSet): String;

implementation

uses
  apputils, sqlgen, LazUtf8, FileUtil, Dialogs, appimagelists;

const
  StorageTypeDB = 0;
  StorageTypeFolder = 1;
  StorageTypeLink = 2;

function LoadFileFromFile(const FileName: String; aFile: TdxFile; DS: TDataSet
  ): String;
var
  FNm, S, ErrStr: String;
  FS: TFileStream;
begin
  Result := '';
  FNm := FieldStr(aFile.Id);

  try

  if aFile.StorageType = StorageTypeDB then
  begin
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
      S := ExtractFileName(FileName);
      DS.FieldByName(FNm + 'dest').SetData(nil);
    finally
      FS.Free;
    end;
  end
  else if (aFile.StorageType = StorageTypeFolder) and (aFile.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, aFile.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(aFile.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := Utf8Copy(S, 1, 150)
    else
      Exit(ErrStr);
  end
  else
  	S := FileName;
  DS.FieldByName(FNm + 'd').AsString := Utf8Copy(S, 1, aFile.FieldSize);
  DS.FieldByName(FNm + 'src').AsString := Utf8Copy(FileName, 1, 255);
  if aFile.StorageType <> StorageTypeDB then
    DS.FieldByName(FNm).SetData(nil);
  // Просто меняем значение поля для определения, что blob был изменен.
  DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;

  except
    on E: Exception do
    	Result := E.Message;
      //ErrMsg(E.Message);
  end;
end;

function SaveFileToFile(const FileName: String; aFile: TdxFile; DS: TDataSet
  ): String;
var
  FS: TFileStream;
  St: TStream;
begin
  Result := '';
  FS := nil; St := nil;
  try try
    FS := TFileStream.Create(FileName, fmCreate);
    St := GetFileStream(aFile, DS);
    if St <> nil then
      FS.CopyFrom(St, St.Size);
  except
    on E: Exception do
    	Result := E.Message;
  end;
  finally
    FreeAndNil(St);
    FreeAndNil(FS);
  end;
end;

function GetFileStream(aFile: TdxFile; DS: TDataSet): TStream;
var
  FNm, FlName: String;
begin
  Result := nil;
  FNm := fieldStr(aFile.Id);
  case aFile.StorageType of
    StorageTypeDb:
        Result := DS.CreateBlobStream(DS.FieldByName(FNm), bmRead);
    StorageTypeFolder:
      begin
        FlName := GetAbsolutePath(aFile.StorageFolder) + DS.FieldByName(FNm + 'dest').AsString;
        if FileExists(FlName) then
          Result := TFileStream.Create(FlName, fmOpenRead + fmShareDenyNone);
      end;
    StorageTypeLink:
      begin
        FlName := DS.FieldByName(FNm + 'src').AsString;
        if FileExists(FlName) then
          Result := TFileStream.Create(FlName, fmOpenRead + fmShareDenyNone);
      end;
  end;
  if (Result <> nil) and (Result.Size = 0) then FreeAndNil(Result);
end;

function GetFileFileName(aFile: TdxFile; DS: TDataSet): String;
var
  S: String;
begin
  Result := '';
  case aFile.StorageType of
    StorageTypeDB, StorageTypeLink:
      Result := DS.FieldByName(FieldStr(aFile.Id) + 'src').AsString;
    StorageTypeFolder:
      if aFile.StorageFolder <> '' then
      begin
        S := DS.FieldByName(FieldStr(aFile.Id) + 'dest').AsString;
        if S <> '' then
	        Result := GetAbsolutePath(aFile.StorageFolder) + S;
      end;
  end;
end;

function OpenFileDialog: String;
begin
  Result := '';
  with TOpenDialog.Create(nil) do
  try
    Title := rsLoadFile;
    Filter := rsAllFilesFilter;
    Options := Options + [ofFileMustExist];
    if Execute then Result := FileName;
  finally
    Free;
  end;
end;

{ TdxFile }

procedure TdxFile.PopupPopup(Sender: TObject);
var
  F: TField;
  b: Boolean;
begin
  if CanFocus then SetFocus;
  F := DS.FieldByName(FieldStr(FId) + 'src');
  b := (not F.IsNull){ and (
    ((F.OldValue = F.Value) and (FStorageType = StorageTypeDB)) or
    (FStorageType <> StorageTypeDB))};
  FPopup.Items[0].Enabled:=b;
  FPopup.Items[1].Enabled := (DS.State in [dsInsert, dsEdit]) and CanEdit;
  FPopup.Items[2].Enabled := b;
  FPopup.Items[3].Enabled := (DS.State in [dsInsert, dsEdit]) and
    (not F.IsNull) and CanEdit;
  FPopup.Items[5].Enabled := SelText <> '';
end;

function TdxFile.GetSourceFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'src').AsString;
end;

function TdxFile.GetDescription: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'd').AsString;
end;

function TdxFile.GetStoredFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'dest').AsString;
end;

procedure TdxFile.PopupHandler(Sender: TObject);
var
  FNm, FlName, ErrStr: String;
begin
  FNm := FieldStr(FId);
  case TMenuItem(Sender).Tag of
    0:
      begin
        FlName := GetFileFileName(Self, DS);
        if FStorageType = StorageTypeDB then
        begin
          FlName := GetOutputDir + ExtractFileName(FlName);
          ErrStr := SaveFileToFile(FlName, Self, DS);
          if ErrStr <> '' then ErrMsg(ErrStr, True, 'OpenFile');
        end;
        if FileExists(FlName) then
          OpenFile(FlName);
      end;
    1:
      begin
        FlName := OpenFileDialog;
        if (FlName <> '') and CheckFileName(FlName) then
        begin
          ErrStr := LoadFileFromFile(FlName, Self, DS);
          if ErrStr <> '' then
          	ErrMsg(ErrStr, True, 'LoadFile')
        end;
      end;
    2:
      begin
        FlName := ExtractFileName(DS.FieldByName(FNm + 'src').AsString);
        FlName := SaveFileDialog(rsSaveFile, FlName);
        if FlName = '' then Exit;
        ErrStr := SaveFileToFile(FlName, Self, DS);
        if ErrStr <> '' then ErrMsg(ErrStr, True, 'SaveFile');
      end;
    3: Clear;
    5: CopyToClipboard;
  end;
end;

function TdxFile.DS: TDataSet;
begin
  Result := DataSource.DataSet;
end;

procedure TdxFile.SetDescription(AValue: String);
begin
  with DS.FieldByName(FieldStr(FId) + 'd') do
    if AValue <> '' then AsString := AValue
    else SetData(nil);
end;

function TdxFile.GetDefaultGlyphName: String;
begin
  Result:='file16';
end;

procedure TdxFile.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  if Button.PopupMenu <> nil then
	  Button.PopupMenu.PopUp;
end;

procedure TdxFile.Loaded;
begin
  inherited Loaded;
  FOldSize := FFieldSize;
end;

procedure TdxFile.SetReadOnly(AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  if Assigned(Button) then Button.Enabled := Enabled;
end;

procedure TdxFile.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(Button) then Button.Enabled := Enabled;
end;

procedure TdxFile.WMPaint(var Msg: TLMPaint);
begin
  if csDesigning in ComponentState then
  begin
    Include(FControlState, csCustomPaint);
    inherited WMPaint(Msg);
    Exclude(FControlState, csCustomPaint);
  end
  else
    inherited WMPaint(Msg);
end;

procedure TdxFile.PaintWindow(DC: HDC);
var
  S: String;
  R: TRect;
begin
  inherited PaintWindow(DC);
  if csDesigning in ComponentState then
  begin
    S := GetDrawText;
    if S <> '' then
    begin
      R := ClientRect;
      InflateRect(R, -1, 0);
      if EditMask <> '' then
        EraseBackground(DC);
      SelectObject(DC, Font.Handle);
      SetBkMode(DC, TRANSPARENT);
      SetTextColor(DC, Font.Color);
      DrawText(DC, PChar(S), -1, R, DT_VCENTER or DT_SINGLELINE);
    end;
  end;
end;

function TdxFile.GetDrawText: String;
begin
  Result := FieldName;
end;

constructor TdxFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  FStopTab := True;
  FPopup := TPopupMenu.Create(Self);
  FPopup.OnPopup:=@PopupPopup;
  FPopup.Images := Images16;
  FPopup.Items.Add( CreateMenuItem(FPopup, rsOpenFile, 0, 0, @PopupHandler, IMG16_EYES) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsLoadFile, 1, 0, @PopupHandler, IMG16_DB) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsSaveFile, 2, 0, @PopupHandler, IMG16_SAVE) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClear, 3, 0, @PopupHandler, IMG16_DELETE) );
  FPopup.Items.Add( CreateMenuItem(FPopup, '-', 4, 0, nil) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsCopy, 5, ShortCut(VK_C, [ssCtrl]), @PopupHandler, IMG16_COPY) );
  FFieldSize := 50; FOldSize := 50;
  Button.PopupMenu := FPopup;
  PopupMenu := FPopup;
  ReadOnly := True;
  CanEdit := True;
end;

destructor TdxFile.Destroy;
begin
  //FPopup.Free;
  inherited Destroy;
end;

procedure TdxFile.Clear;
var
  FNm: String;
begin
  FNm := FieldStr(FId);
  DS.FieldByName(FNm).SetData(nil);
  DS.FieldByName(FNm + 'dest').SetData(nil);
  DS.FieldByName(FNm + 'd').SetData(nil);
  DS.FieldByName(FNm + 'src').SetData(nil);
  DS.FieldByName(FNm + 'c').SetData(nil);
end;

procedure TdxFile.LoadFromFile(const FileName: String);
var
  ErrStr: String;
begin
  ErrStr := LoadFileFromFile(FileName, Self, DS);
  if ErrStr <> '' then raise Exception.Create(ErrStr)
end;

procedure TdxFile.SaveToFile(const FileName: String);
var
  ErrStr: String;
begin
  ErrStr := SaveFileToFile(FileName, Self, DS);
  if ErrStr <> '' then raise Exception.Create(ErrStr);
end;

procedure TdxFile.SaveToStream(St: TStream);
var
  FS: TStream;
begin
  FS := GetFileStream(Self, DS);
  if FS <> nil then
    try
	    St.CopyFrom(FS, FS.Size);
    finally
      FS.Free;
    end;
end;

function TdxFile.WasChanged: Boolean;
var
  F: TField;
begin
  F := DS.FieldByName(FieldStr(FId) + 'c');
  Result := F.Value <> F.OldValue;
end;

end.

