unit MyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyLogger }

  TMyLogger = class
  private
    FFileName: String;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToFile(const S: String);
    property FileName: String read FFileName write FFileName;
  end;

var
  MyLog: TMyLogger;

implementation

uses
  appsettings;

{ TMyLogger }

constructor TMyLogger.Create;
begin
  InitCriticalSection(FLock);
end;

destructor TMyLogger.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TMyLogger.WriteToFile(const S: String);
var
  Mode: Integer;
  Buf: String;
  FS: TFileStream;
begin
  if (FFileName = '') or not AppConfig.ErrLogging then Exit;

  EnterCriticalSection(FLock);
  FS := nil;
  try try

    if not FileExists(FFileName) then
    begin
      ForceDirectories(ExtractFilePath(FileName));
      Mode := fmCreate;
    end
    else
      Mode := fmOpenWrite + fmShareDenyNone;

    FS := TFileStream.Create(FFileName, Mode);
    Buf := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ' + S + LineEnding;
    FS.Position := FS.Size;
    FS.Write(Pointer(Buf)^, Length(Buf));

  except
    ; // Глушим ошибку.
  end;
  finally
    FreeAndNil(FS);
    LeaveCriticalSection(FLock);
  end;
end;

initialization
  MyLog := TMyLogger.Create;

finalization
  MyLog.Free;

end.

