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
  public
    constructor Create;
  end;

implementation

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

constructor TMyUnZipper.Create;
begin
  inherited Create;
  OnCreateStream:=@UnZipperCreateStream;
end;

end.

