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

