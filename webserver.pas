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

unit WebServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, fptemplate, mytypes, ssockets, openssl;

type

  { TTemplate }

  TTemplate = class(TFPTemplate)
  private
    FTags: TVarList;
    function GetTagByIndex(Index: Integer): Variant;
    function GetTagCount: Integer;
    function GetTags(Index: String): Variant;
    procedure SetTagByIndex(Index: Integer; AValue: Variant);
    procedure SetTags(Index: String; AValue: Variant);
  protected
    {procedure GetParam(Sender: TObject; const ParamName: String; out AValue: String);
      override;}
    procedure ReplaceTag(Sender: TObject; const TagName: String; TagParams: TStringList;
      out AValue: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearTags;
    property Tags[Index: String]: Variant read GetTags write SetTags;
    property TagByIndex[Index: Integer]: Variant read GetTagByIndex write
      SetTagByIndex;
    property TagCount: Integer read GetTagCount;
  end;

  { TFPHttpServerEx }

  TFPHttpServerEx = class(TFPHttpServer)
  private
    FIOTimeout: Integer;
  protected
    procedure DoConnect(Sender: TObject; Data: TSocketStream); override;
  published
    property IOTimeout: Integer read FIOTimeout write FIOTimeout;
  end;

  THttpServerErrorHandler = procedure (Sender: TObject; const ErrorMsg: String) of object;

  { THttpServer }

  THttpServer = class(TThread)
  private
    FIOTimeout: Integer;
    FOnError: THttpServerErrorHandler;
    FOnRequest: THTTPServerRequestHandler;
    FPort: Word;
    FAcceptIdleTimeout: Integer;
    FServer: TFPHttpServerEx;
    FRequest: TFPHTTPConnectionRequest;
    FResponse: TFPHTTPConnectionResponse;
    FErrorMsg: String;
    //FSafeSec, FSafeSecErr: TCriticalSection;
    procedure ErrorHandler(Sender: TObject; E: Exception);
    function GetActive: Boolean;
    procedure RequestHandler(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure DoHandleRequest;
    procedure DoHandleError;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Port: Word read FPort write FPort;
    property Active: Boolean read GetActive;
    property AcceptIdleTimeout: Integer read FAcceptIdleTimeout write FAcceptIdleTimeout;
    property IOTimeout: Integer read FIOTimeout write FIOTimeout;
    property OnRequest: THTTPServerRequestHandler read FOnRequest write FOnRequest;
    property OnError: THttpServerErrorHandler read FOnError write FOnError;
  end;

implementation

uses
  Variants, scriptmanager, apputils;

{ TFPHttpServerEx }

procedure TFPHttpServerEx.DoConnect(Sender: TObject; Data: TSocketStream);
begin
  Data.IOTimeout := FIOTimeout;
  inherited DoConnect(Sender, Data);
end;

{ TTemplate }

function TTemplate.GetTags(Index: String): Variant;
var
  pV: PVrData;
begin
  pV := FTags.FindVar(Index);
  if pV <> nil then Result := pV^.Value
  else Result := Null;
end;

function TTemplate.GetTagByIndex(Index: Integer): Variant;
begin
  Result := FTags[Index]^.Value;
end;

function TTemplate.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

procedure TTemplate.SetTagByIndex(Index: Integer; AValue: Variant);
begin
  FTags[Index]^.Value := AValue;
end;

procedure TTemplate.SetTags(Index: String; AValue: Variant);
var
  pV: PVrData;
begin
  pV := FTags.FindVar(Index);
  if pV = nil then FTags.AddVar(Index, AValue)
  else pV^.Value := AValue;
end;

{procedure TTemplate.GetParam(Sender: TObject; const ParamName: String; out
  AValue: String);
var
  pV: PVrData;
begin
  pV := FParams.FindVar(ParamName);
  if pV <> nil then
    AValue := VarToStr(pV^.Value)
  else
    inherited GetParam(Sender, ParamName, AValue);
end;  }

procedure TTemplate.ReplaceTag(Sender: TObject; const TagName: String;
  TagParams: TStringList; out AValue: String);
var
  pV: PVrData;
begin
  pV := FTags.FindVar(TagName);
  if pV <> nil then
    AValue := VarToStr(pV^.Value)
  else
    inherited ReplaceTag(Sender, TagName, TagParams, AValue);
end;

constructor TTemplate.Create;
begin
  FTags := TVarList.Create;
  AllowTagParams:=True;
  StartDelimiter := '{+';
  EndDelimiter := '+}';
end;

destructor TTemplate.Destroy;
begin
  FTags.Free;
  inherited Destroy;
end;

procedure TTemplate.ClearTags;
begin
  FTags.Clear;
end;

{ THttpServer }

procedure THttpServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  if FOnRequest = nil then Exit;
  //FSafeSec.Enter;
  FRequest := ARequest;
  FResponse := AResponse;
  //try
    Synchronize(@DoHandleRequest);
  {finally
    FSafeSec.Leave;
  end;  }
end;

procedure THttpServer.ErrorHandler(Sender: TObject; E: Exception);
begin
  if FOnError = nil then Exit;
  //FSafeSecErr.Enter;
  if ScriptLastError.ExObj = E then
    FErrorMsg := ScriptLastErrorToString
  else
  begin
    FErrorMsg := E.Message;
    // Игнорим эти ошибки.
    if (FErrorMsg = 'Error reading data from the socket') or
      (FErrorMsg = 'Stream write error') then Exit;
    //if FErrorMsg = 'Missing HTTP protocol version in request' then Exit;
  end;

  //try
    Synchronize(@DoHandleError);
  {finally
    FSafeSecErr.Leave;
  end; }
end;

function THttpServer.GetActive: Boolean;
begin
  Result := (FServer <> nil) and FServer.Active and not Suspended;
end;

procedure THttpServer.DoHandleRequest;
begin
  FOnRequest(Self, FRequest, FResponse);
end;

procedure THttpServer.DoHandleError;
begin
  FOnError(Self, FErrorMsg);
end;

procedure THttpServer.Execute;
begin
  FServer := TFPHttpServerEx.Create(nil);
  FServer.IOTimeout := FIOTimeout;
  with FServer do
  try try
    AcceptIdleTimeout:=FAcceptIdleTimeout;
    ServerBanner := 'DataExpress Web Server';
    Port := Self.FPort;
    //Threaded := True;
    OnRequest := @RequestHandler;
    OnRequestError:=@ErrorHandler;
    Active := True;
  except
    on E: Exception do
      ErrorHandler(Self, E);
  end;
  finally
    Free;
    FServer := nil;
  end;
end;

constructor THttpServer.Create;
begin
  inherited Create(True);
  FPort := 80;
  FAcceptIdleTimeout := 200;
  {FSafeSec := TCriticalSection.Create;
  FSafeSecErr := TCriticalSection.Create;  }
end;

destructor THttpServer.Destroy;
begin
  if FServer <> nil then
  begin
    FServer.OnRequest := nil;
    FServer.OnRequestError := nil;
    FServer.Active := False;
    //WaitFor;
  end;
  {FSafeSec.Free;
  FSafeSecErr.Free;  }
  inherited Destroy;
end;

end.

