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

unit dxImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, BGRABitmap, BGRABitmapTypes, BGRAThumbnail,
  FPimage, Db, BufDataSet, Graphics, strconsts, ExtCtrls, Dialogs,
  forms, BGRAReadTiff;

type

  { TdxImage }

  TdxImage = class(TGraphicControl)
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  private
    FBmp: TBGRABitmap;
    FCenter: Boolean;
    FImageName: String;
    FKeepSize: Boolean;
    FProportional: Boolean;
    FQuality: Integer;
    FStretch: Boolean;
    function GetBitmap: TBitmap;
    procedure SetBitmap(AValue: TBitmap);
    procedure SetCenter(AValue: Boolean);
    procedure SetKeepSize(AValue: Boolean);
    procedure SetProportional(AValue: Boolean);
    procedure SetQuality(AValue: Integer);
    procedure SetStretch(AValue: Boolean);
    procedure SetImageName(AValue: String);
    procedure DrawFrame;
  protected
    procedure Paint; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(St: TStream);
    procedure LoadFromStringBase64(const S: String);
    procedure SaveToFile(const FileName: String);
    procedure SaveToStream(St: TStream);
    procedure Clear; virtual;
    procedure RenameImage(const S: String);
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property BGRABitmap: TBGRABitmap read FBmp;
  published
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch;
    property Quality: Integer read FQuality write SetQuality;
    property KeepSize: Boolean read FKeepSize write SetKeepSize;
    property ImageName: String read FImageName write SetImageName;

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

  TImageLoadStatus = (lsLoading, lsComplete, lsFail);

  TImageLoadEvent = procedure (Sender: TObject; Status: TImageLoadStatus) of object;

  TdxDBImage = class(TdxImage)
  private
    procedure ClickHandler(Sender: TObject);
    procedure PopupHandler(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FCheckExpression: String;
    FDataSource: TDataSource;
    FFieldName: String;
    FId: Integer;
    FOnImageLoad: TImageLoadEvent;
    FPrintSize: Integer;
    FReadOnly: Boolean;
    FRequired: Boolean;
    FShowThumbnail: Boolean;
    FStorageFolder: String;
    FStorageType: Integer;
    FThumbSize: Integer;
    FLoader: TThread;
    FIsNewImage: Boolean; // Используется при StorageTypeDB для отображения загруженного изображения
    FLoading: Boolean;
    FLoadFailed: Boolean;
    FOldWidth, FOldHeight: Integer;
    function DS: TDataSet;
    function GetSourceFileName: String;
    function GetStoredFileName: String;
    procedure DoLoadImage(Status: TImageLoadStatus);
    procedure SetFieldName(AValue: String);
  protected
    procedure GetStream;
    procedure LoadingDone;
    procedure LoadingFail;
    procedure Paint; override;
    procedure BoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowImage;
    procedure Clear; override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  public
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure SaveToStream(St: TStream);
    function WasChanged: Boolean;
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property ThumbSize: Integer read FThumbSize write FThumbSize;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property PrintSize: Integer read FPrintSize write FPrintSize;
    property ShowThumbnail: Boolean read FShowThumbnail write FShowThumbnail;
    property OnImageLoad: TImageLoadEvent read FOnImageLoad write FOnImageLoad;
  end;

procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
function LoadImageFromFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): String;
function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): String;
procedure SaveImageToFileConvert(const FileName: String; Image: TdxDBImage; DS: TDataSet);
procedure SaveImageToFile(const FileName: String; MaxSize: Integer; Image: TdxDBImage; DS: TDataSet);
function GetImageStream(Image: TdxDBImage; DS: TDataSet): TStream;
function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
procedure CreateBlankImage(const FileName: String);
procedure GetImageSize(const FileName: String; var Size: TPoint);

implementation

uses
  bgrareadjpeg, LazUtf8, fpreadjpeg, Types, sqlgen, apputils, Menus, fileUtil,
  imagemanager, appimagelists, scriptfuncs;

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

function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
var
  S: String;
begin
  Result := '';
  case Image.StorageType of
    StorageTypeDB:
      begin
        S := DS.FieldByName(FieldStr(Image.Id) + 'src').AsString;
        if S <> '' then
          Result := ExtractFilePath(S) + GetUniqueFileName(DS.FieldByName('id').AsInteger,
            Image.Id, ExtractFileName(S));
      end;
    StorageTypeFolder:
      if Image.StorageFolder <> '' then
      begin
        S := DS.FieldByName(FieldStr(Image.Id) + 'dest').AsString;
        if S <> '' then
	        Result := GetAbsolutePath(Image.StorageFolder) + S;
      end;
    StorageTypeLink:
      Result := DS.FieldByName(FieldStr(Image.Id) + 'src').AsString;
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

procedure GetImageSize(const FileName: String; var Size: TPoint);
var
  FBmp: TBGRABitmap;
begin
  Size.x := 0;
  Size.y := 0;
  FBmp := TBGRABitmap.Create(0, 0);
  try try
    FBmp.LoadFromFile(FileName);
    Size.x := FBmp.Width;
    Size.y := FBmp.Height;
  except
    ;
  end;

  finally
    FBmp.Free;
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
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
      DS.FieldByName(FNm + 'dest').SetData(nil);
    finally
      FS.Free;
    end;
  end
  else if (Image.StorageType = StorageTypeFolder) and (Image.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, Image.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(Image.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := Utf8Copy(S, 1, 150)
    else Exit(ErrStr);
  end;
  CreateThumbnail(FileName, Image, DS);
  DS.FieldByName(FNm + 'src').AsString := Utf8Copy(FileName, 1, 255);
  if Image.StorageType <> StorageTypeDB then
    DS.FieldByName(FNm).SetData(nil);
  // Просто меняем значение поля для определения, что blob был изменен.
  DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;

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
  if Img.Stretch then
  begin
    if Img.KeepSize and (PicWidth < ImgWidth) and (PicHeight < ImgHeight) then
    else if Img.KeepSize and not Img.Proportional then
    begin
      if PicWidth > ImgWidth then PicWidth := ImgWidth;
      if PicHeight > ImgHeight then PicHeight := ImgHeight;
    end
    else {if Img.Stretch or (Img.Proportional
    and ((PicWidth > ImgWidth) or (PicHeight > ImgHeight))) then} begin
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

procedure TdxDBImage.ClickHandler(Sender: TObject);
begin
  if PopupMenu <> nil then PopupMenu.PopUp;
end;

procedure TdxDBImage.PopupHandler(Sender: TObject);
var
  FNm, FlName, ErrStr: String;
begin
  FNm := FieldStr(FId);
  case TMenuItem(Sender).Tag of
    0:
      begin
        FlName := GetImageFileName(Self, DS);
        if FStorageType = StorageTypeDB then
        begin
          FlName := GetOutputDir + ExtractFileName(FlName);
          //FlName := GetOutputDir + FlName;
          ErrStr := SaveImageToFile(FlName, Self, DS);
          if ErrStr <> '' then
          begin
            ErrMsg(ErrStr, True, 'OpenImage');
            Exit;
          end;
        end;
        //if FileExistsUtf8(FlName) then
        OpenFile(FlName);
      end;
    1:
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
        	ErrMsg(rsFailedToLoadImage + Spaces + ErrStr, True, 'LoadImage');
      end;
    2:
      begin
        FlName := ExtractFileName(DS.FieldByName(FNm + 'src').AsString);
        FlName := SaveFileDialog(rsSaveImage, FlName);
        if FlName = '' then Exit;
        ErrStr := SaveImageToFile(FlName, Self, DS);
        if ErrStr <> '' then ErrMsg(rsFailedToSaveImage + Spaces + ErrStr, True, 'SaveImage');
      end;
    3:
      begin
        Clear;
        //Repaint;
      end;
  end;
end;

procedure TdxDBImage.PopupMenuPopup(Sender: TObject);
var
  F: TField;
  b: Boolean;
begin
  F := DS.FieldByName(FieldStr(Id) + 'src');
  b := (not F.IsNull) {and (
    ((F.OldValue = F.Value) and (FStorageType = StorageTypeDB)) or
    (FStorageType <> StorageTypeDB))};
  PopupMenu.Items[0].Enabled:=b;
  PopupMenu.Items[1].Enabled:=(DS.State in [dsInsert, dsEdit]) and (not ReadOnly);
  PopupMenu.Items[2].Enabled:=b;
  PopupMenu.Items[3].Enabled:=(DS.State in [dsInsert, dsEdit]) and
    (not F.IsNull) and (not ReadOnly);
end;

function TdxDBImage.DS: TDataSet;
begin
  Result := DataSource.DataSet;
end;

procedure TdxDBImage.GetStream;
var
  St: TStream;
begin
  DoLoadImage(lsLoading);
  if FLoader <> nil then
  begin
    St := nil;
    if FShowThumbnail then
      St := DS.CreateBlobStream(DS.FieldByName(FieldStr(FId) + 'thumb'), bmRead)
    else
      St := GetImageStream(Self, DS);
    TImageLoader(FLoader).Stream := St;
    FLoadFailed := (St = nil) and (not DS.FieldByName(FieldStr(FId) + 'src').IsNull);
  end;
  if FLoadFailed then DoLoadImage(lsFail);
end;

procedure TdxDBImage.LoadingDone;
begin
  with TImageLoader(FLoader) do
    if not Bitmap.Empty then
    begin
      FBmp.Assign(Bitmap);
    end;

  FLoading := False;
  FIsNewImage := False;
  Repaint;
  DoLoadImage(lsComplete);
end;

procedure TdxDBImage.LoadingFail;
begin
  FLoadFailed:=True;
  Repaint;
  DoLoadImage(lsFail);
end;

procedure TdxDBImage.Paint;
var
  TS: TTextStyle;
  S: String;
begin
  TS := Canvas.TextStyle;
  TS.Alignment:=taCenter;
  TS.Layout:=tlCenter;
  TS.SingleLine:=False;
  TS.Wordbreak:=True;

  if csDesigning in ComponentState then
  begin
    DrawFrame;
    Canvas.TextRect(ClientRect, 0, 0, FieldName, TS);
  end;
  FBmp.Draw(Canvas, DestRect(Self, FBmp.Width, FBmp.Height), False);

  if FLoadFailed then
  begin
    if FShowThumbnail then
      S := rsNoThumbnail
    else
      S := rsFailedToLoadImage;
    Canvas.TextRect(ClientRect, 0, 0, S, TS);
  end
  else if FLoading then
  begin
    Canvas.TextRect(ClientRect, 0, 0, rsLoading, TS);
  end;

  if OnPaint <> nil then OnPaint(Self);
end;

procedure TdxDBImage.BoundsChanged;
begin
  inherited BoundsChanged;
  if (DataSource = nil) or not DataSource.DataSet.Active then Exit;
  if (Width > FOldWidth) or (Height > FOldHeight) then
    ShowImage;
end;

constructor TdxDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Center := True;
  Stretch:=True;
  Proportional:=True;
  Quality:=1;
  KeepSize := True;
  FPrintSize := 640;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsLook, 0, 0, @PopupHandler, IMG16_EYES));
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsLoadImage, 1, 0, @PopupHandler, IMG16_DB) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsSaveImage, 2, 0, @PopupHandler, IMG16_SAVE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 3, 0, @PopupHandler, IMG16_DELETE));
  PopupMenu.OnPopup:=@PopupMenuPopup;
  OnClick:=@ClickHandler;
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
  FOldWidth := Width;
  FOldHeight := Height;
end;

procedure TdxDBImage.Clear;
var
  FNm: String;
begin
  inherited Clear;
  FNm := FieldStr(FId);
  DS.FieldByName(FNm).SetData(nil);
  DS.FieldByName(FNm + 'dest').SetData(nil);
  DS.FieldByName(FNm + 'thumb').SetData(nil);
  DS.FieldByName(FNm + 'src').SetData(nil);
  DS.FieldByName(FNm + 'c').SetData(nil);
  FLoadFailed:=False;
  FLoading:=False;
  Invalidate;
end;

procedure TdxDBImage.LoadFromFile(const FileName: String);
var
  Err: String;
begin
  Err := LoadImageFromFile(FileName, Self, DS);
  if Err = '' then
  begin
    FIsNewImage := True;
    ShowImage;
  end
  else raise Exception.Create(Err);
end;

procedure TdxDBImage.SaveToFile(const FileName: String);
begin
  SaveImageToFileConvert(FileName, Self, DS);
end;

procedure TdxDBImage.SaveToStream(St: TStream);
var
  Stream: TStream;
begin
  Stream := GetImageStream(Self, DS);
  if Stream <> nil then
    try
  	  St.CopyFrom(Stream, Stream.Size);
    finally
      Stream.Free;
    end;
end;

function TdxDBImage.WasChanged: Boolean;
var
  F: TField;
begin
  F := DS.FieldByName(FieldStr(FId) + 'c');
  Result := F.Value <> F.OldValue;
end;

function TdxDBImage.GetSourceFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'src').AsString;
end;

function TdxDBImage.GetStoredFileName: String;
begin
  Result := DS.FieldByName(FieldStr(FId) + 'dest').AsString;
end;

procedure TdxDBImage.DoLoadImage(Status: TImageLoadStatus);
begin
  if FOnImageLoad <> nil then FOnImageLoad(Self, Status);
end;

procedure TdxDBImage.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

{ TdxImage }

procedure TdxImage.Clear;
begin
  FBmp.SetSize(0, 0);
  FImageName := '';
  Invalidate;
end;

procedure TdxImage.RenameImage(const S: String);
begin
  FImageName := S;
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

procedure TdxImage.SetCenter(AValue: Boolean);
begin
  if FCenter=AValue then Exit;
  FCenter:=AValue;
  Invalidate;
end;

procedure TdxImage.SetImageName(AValue: String);
var
  St: TStream;
begin
  if FImageName=AValue then Exit;
  FImageName:=AValue;
  FBmp.SetSize(0, 0);
  Invalidate;
  if FImageName = '' then Exit;
  ImageMan.GetImageStreamPPI(FImageName, St);
  if St = nil then Exit;
  try
    LoadFromStream(St);
  finally
    St.Free;
  end;
end;

procedure TdxImage.SetKeepSize(AValue: Boolean);
begin
  if FKeepSize=AValue then Exit;
  FKeepSize:=AValue;
  Invalidate;
end;

procedure TdxImage.SetProportional(AValue: Boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;
  Invalidate;
end;

procedure TdxImage.SetQuality(AValue: Integer);
begin
  if FQuality=AValue then Exit;
  FQuality:=AValue;
  Invalidate;
end;

procedure TdxImage.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;
  Invalidate;
end;

procedure TdxImage.DrawFrame;
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

procedure TdxImage.Paint;
var
  TS: TTextStyle;
begin
  FBmp.Draw(Canvas, DestRect(Self, FBmp.Width, FBmp.Height), False);
  if csDesigning in ComponentState then
  begin
    DrawFrame;
    if FBmp.Empty then
    begin
      TS := Canvas.TextStyle;
      TS.Alignment:=taCenter;
      TS.Layout:=tlCenter;
      Canvas.TextRect(ClientRect, 0, 0, Name, TS);
    end;
  end;
  if OnPaint <> nil then OnPaint(Self);
end;

procedure TdxImage.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    Result := not FBmp.Empty and (FImageName = '');
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

procedure TdxImage.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  Invalidate;
end;

constructor TdxImage.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100); Height := ScaleToScreen(100);
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
  stream := TFileStream.Create(filename,fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(stream);
    Invalidate;
  finally
    stream.Free;
  end;
end;

procedure TdxImage.LoadFromStream(St: TStream);
begin
  LoadImageFromStream(FBmp, FQuality, St);
  Invalidate;
end;

procedure TdxImage.LoadFromStringBase64(const S: String);
var
  St: TStringStream;
begin
  St := TStringStream.Create(DecodeBase64(S, False));
  try
    LoadFromStream(St);
  finally
    St.Free;
  end;
end;

procedure TdxImage.SaveToFile(const FileName: String);
begin
  FBmp.SaveToFileUTF8(FileName);
end;

procedure TdxImage.SaveToStream(St: TStream);
begin
  FBmp.Bitmap.SaveToStream(St);
end;

end.

