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

unit MyZipper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, LConvEncoding;

type

  { TMyZipper }

  TMyZipper = class(TZipper)
  protected
    procedure ZipOneFile(Item: TZipFileEntry); override;
  end;

  { TMyUnZipper }

  TMyUnZipper = class(TUnZipper)
  private
    procedure UnZipperCreateStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure UnZipperDoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  public
    constructor Create;
  end;

implementation

uses
  apputils;

{ TMyZipper }

procedure TMyZipper.ZipOneFile(Item: TZipFileEntry);
begin
  Item.Stream := TFileStream.Create(Item.DiskFileName, fmOpenRead + fmShareDenyNone);
  try
    inherited ZipOneFile(Item);
  finally
    Item.Stream.Free;
    Item.Stream := nil;
  end;
end;

{ TMyUnZipper }

procedure TMyUnZipper.UnZipperCreateStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
var
  S, Dir: String;
begin
  S := OutputPath + StringReplace(CP866ToUTF8(AItem.ArchiveFileName), '/', PathDelim, [rfReplaceAll]);
  Dir := ExtractFilePath(S);
  if Dir <> '' then ForceDirectories(Dir);
  if not AItem.IsDirectory then AStream := TFileStream.Create(S, fmCreate);
end;

procedure TMyUnZipper.UnZipperDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  FlNm: String;
begin
  FlNm := TFileStream(AStream).FileName;
  //SetFileDateTime(TFileStream(AStream).Handle, AItem.DateTime);
  FreeAndNil(AStream);
  SetFileDateTime(FlNm, AItem.DateTime);
end;

constructor TMyUnZipper.Create;
begin
  inherited Create;
  OnCreateStream:=@UnZipperCreateStream;
  OnDoneStream:=@UnZipperDoneStream;
end;

end.

