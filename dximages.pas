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
unit dxImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, BGRABitmap, BGRABitmapTypes, BGRAThumbnail,
  FPimage, Db, Graphics, strconsts, ExtCtrls, Dialogs, forms;

type

  { TdxImage }

  TdxImage = class(TGraphicControl)
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  private
    FBmp: TBGRABitmap;
    FCenter: Boolean;
    FKeepSize: Boolean;
    FProportional: Boolean;
    FQuality: Integer;
    FStretch: Boolean;
    function GetBitmap: TBitmap;
    procedure SetBitmap(AValue: TBitmap);
  protected
    procedure Paint; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(St: TStream);
    procedure SaveToFile(const FileName: String);
    procedure Clear; virtual;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
  published
    property Center: Boolean read FCenter write FCenter;
    property Proportional: Boolean read FProportional write FProportional;
    property Stretch: Boolean read FStretch write FStretch;
    property Quality: Integer read FQuality write FQuality;
    property KeepSize: Boolean read FKeepSize write FKeepSize;

    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
    property Anchors;
  end;

  { TdxDBImage }

  TdxDBImage = class(TdxImage)
  private
    procedure PopupHandler(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FDataSource: TDataSource;
    FFieldName: String;
    FId: Integer;
    FReadOnly: Boolean;
    FStorageFolder: String;
    FStorageType: Integer;
    FThumbSize: Integer;
    FLoader: TThread;
    FIsNewImage: Boolean; // Используется при StorageTypeDB для отображения загруженного изображения
    FLoading: Boolean;
    FLoadFailed: Boolean;
    function DS: TDataSet;
    function GetSourceFileName: String;
    function GetStoredFileName: String;
  protected
    procedure GetStream;
    procedure LoadingDone;
    procedure LoadingFail;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowImage;
    procedure Clear; override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  public
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property ThumbSize: Integer read FThumbSize write FThumbSize;
  end;

procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
function LoadImageFromFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): String;
function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): String;
procedure SaveImageToFileConvert(const FileName: String; Image: TdxDBImage; DS: TDataSet);
procedure SaveImageToFile(const FileName: String; MaxSize: Integer; Image: TdxDBImage; DS: TDataSet);
function GetImageStream(Image: TdxDBImage; DS: TDataSet): TStream;
function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
procedure CreateBlankImage(const FileName: String);

implementation

uses
  bgrareadjpeg, LazUtf8, fpreadjpeg, Types, sqlgen, apputils, Menus, fileUtil;

const
  StorageTypeDB = 0;
  StorageTypeFolder = 1;
  StorageTypeLink = 2;

type

  { TImageLoader }

  TImageLoader = class(TThread)
  private
    FBitmap, FTempBmp: TBGRABitmap;
    FImage: TdxDBImage;
    FStream: TStream;
  protected
    procedure Execute; override;
  public
    constructor Create(Image: TdxDBImage);
    property Bitmap: TBGRABitmap read FBitmap;
    property Stream: TStream read FStream write FStream;
  end;

procedure LoadImageFromStream(aBitmap: TBGRABitmap; aQuality: Integer; St: TStream);
var
  fmt: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  fmt := DetectFileFormat(St);
  if fmt = ifJpeg then
  begin
    reader := TBGRAReaderJpeg.Create;
    with TBGRAReaderJpeg(reader) do
    begin
      if aQuality in [0..3] then
        Scale:=TJpegScale(aQuality);
      Performance:=jpBestSpeed;
    end;
  end
  else
    reader := CreateBGRAImageReader(fmt);
  try
    aBitmap.LoadFromStream(St,reader);
  finally
    reader.Free;
  end;
end;

procedure SaveImageToFileConvert(const FileName: String; Image: TdxDBImage;
  DS: TDataSet);
var
  St: TStream;
  Bmp: TBGRABitmap;
begin
  St := GetImageStream(Image, DS);
  if St = nil then Exit;
  try
    Bmp := nil;
    Bmp := TBGRABitmap.Create(St);
    Bmp.SaveToFileUTF8(FileName);
  finally
    St.Free;
    FreeAndNil(Bmp);
  end;
end;

procedure SaveImageToFile(const FileName: String; MaxSize: Integer;
  Image: TdxDBImage; DS: TDataSet);
var
  St: TStream;
  FBmp, FBmp2: TBGRABitmap;
  w, h: Integer;
  e: Extended;
begin
  St := GetImageStream(Image, DS);
  if St = nil then Exit;
  FBmp := TBGRABitmap.Create(0, 0);
  FBmp2 := TBGRABitmap.Create(0, 0);
  try
    FBmp.LoadFromStream(St);
    w := FBmp.Width;
    h := FBmp.Height;
    if (w > MaxSize) or (h > MaxSize) then
    begin
      e := w / h;
      if e >= 1 then
      begin
        w := MaxSize; h := Round(MaxSize / e);
      end else
      begin
        h := MaxSize; w := Round(MaxSize * e);
      end;
    end;
    FBmp2.SetSize(w, h);
    FBmp2.StretchPutImage(Rect(0, 0, w, h), FBmp, dmSet);
    FBmp2.SaveToFileUTF8(FileName);
  finally
    FBmp2.Free;
    FBmp.Free;
    St.Free;
  end;
end;

function GetImageStream(Image: TdxDBImage; DS: TDataSet): TStream;
var
  FNm, FlName: String;
  Tp: Integer;
begin
  Result := nil;
  FNm := fieldStr(Image.Id);
  Tp := Image.StorageType;
  //if (Tp = StorageTypeDB) and (DS.State in [dsInsert, dsEdit]) then Tp := StorageTypeLink;
  case Tp of
    StorageTypeDb:
        Result := DS.CreateBlobStream(DS.FieldByName(FNm), bmRead);
    StorageTypeFolder:
      begin
        FlName := GetAbsolutePath(Image.StorageFolder) + DS.FieldByName(FNm + 'dest').AsString;
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

function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
var
  S: String;
begin
  Result := '';
  case Image.StorageType of
    StorageTypeDB, StorageTypeLink:
      Result := DS.FieldByName(FieldStr(Image.Id) + 'src').AsString;
    StorageTypeFolder:
      if Image.StorageFolder <> '' then
      begin
        S := DS.FieldByName(FieldStr(Image.Id) + 'dest').AsString;
        if S <> '' then
	        Result := GetAbsolutePath(Image.StorageFolder) + S;
      end;
  end;
end;

procedure CreateBlankImage(const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(2, 2);
  try
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
var
  Bmp: TBGRABitmap;
  St: TMemoryStream;
begin
  if Image.ThumbSize <= 0 then
  begin
    DS.FieldByName(FieldStr(Image.Id) + 'thumb').SetData(nil);
    Exit;
  end;
  Bmp := GetFileThumbnail(FileName, Image.ThumbSize, Image.ThumbSize, BGRAWhite, False, nil);
  if Bmp = nil then Exit;
  St := TMemoryStream.Create;
  try
    Bmp.SaveToStreamAsPng(St);
    TBlobField(DS.FieldByName(FieldStr(Image.Id) + 'thumb')).LoadFromStream(St);
  finally
    St.Free;
    Bmp.Free;
  end;
end;

function LoadImageFromFile(const FileName: String; Image: TdxDBImage;
  DS: TDataSet): String;
var
  FS: TFileStream;
  S, FNm, ErrStr: String;
begin
  Result := '';
  FNm := FieldStr(Image.Id);
  try

  if Image.StorageType = StorageTypeDB then
  begin
    FS := TFileStream.Create(FileName, fmOpenRead);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
    finally
      FS.Free;
    end;
  end
  else if (Image.StorageType = StorageTypeFolder) and (Image.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, Image.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(Image.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := S
    else Exit(ErrStr);
  end;
  DS.FieldByName(FNm + 'src').AsString := FileName;
  CreateThumbnail(FileName, Image, DS);

  except
    on E: Exception do
    	Result := E.Message;
      //ErrMsg(E.Message);
  end;
end;

function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet
  ): String;
var
  St: TStream;
  FS: TFileStream;
begin
  Result := '';
  FS := nil; St := nil;
  try try
    FS := TFileStream.Create(FileName, fmCreate);
    St := GetImageStream(Image, DS);
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

function DestRect(Img: TdxImage; aWidth, aHeight: Integer): TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
begin
  PicWidth := aWidth;
  PicHeight := aHeight;
  ImgWidth := Img.ClientWidth;
  ImgHeight := Img.ClientHeight;
  if Img.KeepSize and (PicWidth < ImgWidth) and (PicHeight < ImgHeight) then

  else if Img.Stretch or (Img.Proportional
  and ((PicWidth > ImgWidth) or (PicHeight > ImgHeight))) then begin
    if Img.Proportional and (PicWidth > 0) and (PicHeight > 0) then begin
      w:=ImgWidth;
      h:=(PicHeight*w) div PicWidth;
      if h>ImgHeight then begin
        h:=ImgHeight;
        w:=(PicWidth*h) div PicHeight;
      end;
      PicWidth:=w;
      PicHeight:=h;
    end
    else begin
      PicWidth := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;

  Result:=Rect(0,0,PicWidth,PicHeight);

  if Img.Center then
    OffsetRect(Result,(ImgWidth-PicWidth) div 2,(ImgHeight-PicHeight) div 2);
end;

{ TImageLoader }

procedure TImageLoader.Execute;
var
  R: Classes.TRect;
begin
  FBitmap := TBGRABitmap.Create(0, 0);
  FTempBmp := TBGRABitmap.Create(0, 0);
  FStream := nil;

  try try
    Synchronize(Self, @FImage.GetStream);
    if (not Terminated) and (FStream <> nil) then
      LoadImageFromStream(FTempBmp, FImage.Quality, FStream);
    if not Terminated then
    begin
      R := DestRect(FImage, FTempBmp.Width, FTempBmp.Height);
      FBitmap.SetSize(R.Right - R.Left, R.Bottom - R.Top);
      FBitmap.StretchPutImage(Rect(0, 0, FBitmap.Width, FBitmap.Height), FTempBmp, dmSet);
    end;
    if not Terminated then
      Synchronize(Self, @FImage.LoadingDone);
  except
    on E: Exception do
      Synchronize(Self, @FImage.LoadingFail);
  end;
  finally
    FreeAndNil(FStream);
    FreeAndNil(FTempBmp);
    FreeAndNil(FBitmap);
  end;
end;

constructor TImageLoader.Create(Image: TdxDBImage);
begin
  inherited Create(True);
  //FreeOnTerminate:=True;
  FImage := Image;
  Start;
end;

{ TdxDBImage }

procedure TdxDBImage.PopupHandler(Sender: TObject);
var
  FNm, FlName, ErrStr: String;
begin
  FNm := FieldStr(FId);
  case TMenuItem(Sender).Tag of
    0:
      begin
        FlName := OpenPictureDialog;
        if (FlName = '') or (not CheckFileName(FlName)) then Exit;
        ErrStr := LoadImageFromFile(FlName, Self, DS);
        if ErrStr = '' then
        begin
          FIsNewImage := True;
          ShowImage;
        end
        else
        	ErrMsg(ErrStr);
      end;
    1:
      begin
        FlName := ExtractFileName(DS.FieldByName(FNm + 'src').AsString);
        FlName := SaveFileDialog(rsSaveImage, FlName);
        if FlName = '' then Exit;
        ErrStr := SaveImageToFile(FlName, Self, DS);
        if ErrStr <> '' then ErrMsg(ErrStr);
      end;
    2:
      begin
        Clear;
        //Repaint;
      end;
    3:
      begin
        FlName := GetImageFileName(Self, DS);
        if FStorageType = StorageTypeDB then
        begin
          FlName := GetOutputDir + ExtractFileName(FlName);
          //FlName := GetOutputDir + FlName;
          ErrStr := SaveImageToFile(FlName, Self, DS);
          if ErrStr <> '' then
          begin
            ErrMsg(ErrStr);
            Exit;
          end;
        end;
        //if FileExistsUtf8(FlName) then
        OpenFile(FlName);
      end;
  end;
end;

procedure TdxDBImage.PopupMenuPopup(Sender: TObject);
var
  F: TField;
  b: Boolean;
begin
  F := DS.FieldByName(FieldStr(Id) + 'src');
  b := (not F.IsNull) and (
    ((F.OldValue = F.Value) and (FStorageType = StorageTypeDB)) or
    (FStorageType <> StorageTypeDB));
  PopupMenu.Items[0].Enabled:=(DS.State in [dsInsert, dsEdit]) and (not ReadOnly);
  PopupMenu.Items[1].Enabled:=b;//not FBmp.Empty;
  PopupMenu.Items[2].Enabled:=(DS.State in [dsInsert, dsEdit]) and
    (not F.IsNull) and (not ReadOnly);
  PopupMenu.Items[3].Enabled:=b;//not FBmp.Empty;
end;

function TdxDBImage.DS: TDataSet;
begin
  Result := DataSource.DataSet;
end;

procedure TdxDBImage.GetStream;
var
  St: TStream;
begin
  if FLoader <> nil then
  begin
    St := nil;
    if (StorageType = StorageTypeDB) and FIsNewImage then
    begin
      StorageType:=StorageTypeLink;
      St := GetImageStream(Self, DS);
      StorageType:=StorageTypeDB;
    end
    else St := GetImageStream(Self, DS);
    TImageLoader(FLoader).Stream := St;
    FLoadFailed := (St = nil) and (not DS.FieldByName(FieldStr(FId) + 'src').IsNull);
  end;
end;

procedure TdxDBImage.LoadingDone;
begin
  with TImageLoader(FLoader) do
    if not Bitmap.Empty then
    begin
      FBmp.Assign(Bitmap);
    end;
    //else Clear;

  FLoading := False;
  FIsNewImage := False;
  Repaint;
end;

procedure TdxDBImage.LoadingFail;
begin
  FLoadFailed:=True;
  Repaint;
end;

procedure TdxDBImage.Paint;
var
  TS: TTextStyle;
begin
  inherited Paint;
  TS := Canvas.TextStyle;
  TS.Alignment:=taCenter;
  TS.Layout:=tlCenter;
  TS.SingleLine:=False;
  TS.Wordbreak:=True;
  if FLoadFailed then
  begin
    Canvas.TextRect(ClientRect, 0, 0, rsFailedToLoadImage, TS);
  end
  else if FLoading then
  begin
    Canvas.TextRect(ClientRect, 0, 0, rsLoading, TS);
  end;
end;

constructor TdxDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Center := True;
  Stretch:=True;
  Proportional:=True;
  Quality:=1;
  KeepSize := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsLoadImage, 0, 0, @PopupHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsSaveImage, 1, 0, @PopupHandler) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 2, 0, @PopupHandler, 'delete16'));
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsOpen, 3, 0, @PopupHandler));
  PopupMenu.OnPopup:=@PopupMenuPopup;
end;

destructor TdxDBImage.Destroy;
begin
  if FLoader <> nil then
  begin
    FLoader.Terminate;
    FLoader.WaitFor;
  end;
  FreeAndNil(FLoader);
  inherited Destroy;
end;

procedure TdxDBImage.ShowImage;
begin
  if FLoader <> nil then
  begin
    FLoader.Terminate;
    FLoader.WaitFor;
    FreeAndNil(FLoader);
  end;
  FBmp.SetSize(0, 0);
  FLoading := not DS.FieldByName(FieldStr(FId) + 'src').IsNull;
  Repaint;
  FLoader := TImageLoader.Create(Self);
end;

procedure TdxDBImage.Clear;
var
  FNm: String;
begin
  inherited Clear;
  if not (DS.State in [dsInsert, dsEdit]) then Exit;
  FNm := FieldStr(FId);
  DS.FieldByName(FNm).SetData(nil);
  DS.FieldByName(FNm + 'src').SetData(nil);
  DS.FieldByName(FNm + 'dest').SetData(nil);
  DS.FieldByName(FNm + 'thumb').SetData(nil);
  FLoadFailed:=False;
  FLoading:=False;
  Repaint;
end;

procedure TdxDBImage.LoadFromFile(const FileName: String);
begin
  LoadImageFromFile(FileName, Self, DS);
  FIsNewImage:=True;
  ShowImage;
end;

procedure TdxDBImage.SaveToFile(const FileName: String);
begin
  SaveImageToFileConvert(FileName, Self, DS);
end;

function TdxDBImage.GetSourceFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'src').AsString;
end;

function TdxDBImage.GetStoredFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'dest').AsString;
end;


{ TdxImage }

procedure TdxImage.Clear;
begin
  FBmp.SetSize(0, 0);
end;

procedure TdxImage.ReadData(Stream: TStream);
begin
  LoadImageFromStream(FBmp, FQuality, Stream);
end;

procedure TdxImage.WriteData(Stream: TStream);
begin
  FBmp.SaveToStreamAsPng(Stream);
end;

function TdxImage.GetBitmap: TBitmap;
begin
  Result := FBmp.Bitmap;
end;

procedure TdxImage.SetBitmap(AValue: TBitmap);
begin
  FBmp.Assign(AValue);
  Refresh;
end;

procedure TdxImage.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

begin
  if csDesigning in ComponentState
  then DrawFrame;

  FBmp.Draw(Canvas, DestRect(Self, FBmp.Width, FBmp.Height), False);
end;

procedure TdxImage.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    Result := not FBmp.Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

constructor TdxImage.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  Width := 100; Height := 100;
  FBmp := TBGRABitmap.Create(0, 0);
end;

destructor TdxImage.Destroy;
begin
  FreeAndNil(FBmp);
  inherited Destroy;
end;

procedure TdxImage.LoadFromFile(const FileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename,fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TdxImage.LoadFromStream(St: TStream);
begin
  try
    LoadImageFromStream(FBmp, FQuality, St);
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

procedure TdxImage.SaveToFile(const FileName: String);
begin
  try
    FBmp.SaveToFileUTF8(FileName);
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

end.

