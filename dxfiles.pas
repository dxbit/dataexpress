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
unit dxFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbctrlsex, Menus, strconsts, Db;

type

  { TdxFile }

  TdxFile = class(TCustomDBEditButton)
  private
    FFieldName: String;
    FFieldSize: Integer;
    FId: Integer;
    FOldSize: Integer;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property OldSize: Integer read FOldSize write FOldSize;
  public
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
    property Description: String read GetDescription write SetDescription;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property ButtonWidth;
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
  apputils, sqlgen, LazUtf8, FileUtil, Dialogs;

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
    FS := TFileStream.Create(FileName, fmOpenRead);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
    finally
      FS.Free;
    end;
  end
  else if (aFile.StorageType = StorageTypeFolder) and (aFile.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, aFile.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(aFile.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := S
    else
      Exit(ErrStr);
  end;
  DS.FieldByName(FNm + 'src').AsString := FileName;
  DS.FieldByName(FNm + 'd').AsString := ChangeFileExt(ExtractFileName(FileName), '');

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
      //ErrMsg(E.Message);
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
          Result := TFileStream.Create(FlName, fmOpenRead);
      end;
    StorageTypeLink:
      begin
        FlName := DS.FieldByName(FNm + 'src').AsString;
        if FileExists(FlName) then
          Result := TFileStream.Create(FlName, fmOpenRead);
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
  F := DS.FieldByName(FieldStr(FId) + 'src');
  b := (not F.IsNull) and (
    ((F.OldValue = F.Value) and (FStorageType = StorageTypeDB)) or
    (FStorageType <> StorageTypeDB));
  FPopup.Items[0].Enabled:=b;
  FPopup.Items[1].Enabled := (DS.State in [dsInsert, dsEdit]) and (not ReadOnly);
  FPopup.Items[2].Enabled := b;
  FPopup.Items[3].Enabled := (DS.State in [dsInsert, dsEdit]) and
    (not F.IsNull) and (not ReadOnly);
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
          if ErrStr <> '' then ErrMsg(ErrStr);
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
          	ErrMsg(ErrStr);
        end;
      end;
    2:
      begin
        FlName := ExtractFileName(DS.FieldByName(FNm + 'src').AsString);
        FlName := SaveFileDialog(rsSaveFile, FlName);
        if FlName = '' then Exit;
        ErrStr := SaveFileToFile(FlName, Self, DS);
        if ErrStr <> '' then ErrMsg(ErrStr);
      end;
    3: Clear;
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
  Button.Enabled:=True;
end;

constructor TdxFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopup := TPopupMenu.Create(nil);
  FPopup.OnPopup:=@PopupPopup;
  FPopup.Items.Add( CreateMenuItem(FPopup, rsOpen, 0, 0, @PopupHandler) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsLoadFile, 1, 0, @PopupHandler) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsSaveFile, 2, 0, @PopupHandler) );
  FPopup.Items.Add( CreateMenuItem(FPopup, rsClear, 3, 0, @PopupHandler, 'delete16') );
  FFieldSize := 150; FOldSize := 150;
  Button.PopupMenu := FPopup;
end;

destructor TdxFile.Destroy;
begin
  FPopup.Free;
  inherited Destroy;
end;

procedure TdxFile.Clear;
var
  FNm: String;
begin
  FNm := FieldStr(FId);
  DS.FieldByName(FNm).SetData(nil);
  DS.FieldByName(FNm + 'src').SetData(nil);
  DS.FieldByName(FNm + 'dest').SetData(nil);
  DS.FieldByName(FNm + 'd').SetData(nil);
end;

procedure TdxFile.LoadFromFile(const FileName: String);
var
  ErrStr: String;
begin
  ErrStr := LoadFileFromFile(FileName, Self, DS);
  if ErrStr <> '' then raise Exception.Create(ErrStr);
end;

procedure TdxFile.SaveToFile(const FileName: String);
var
  ErrStr: String;
begin
  ErrStr := SaveFileToFile(FileName, Self, DS);
  if ErrStr <> '' then raise Exception.Create(ErrStr);
end;

end.

