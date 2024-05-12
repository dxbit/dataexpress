unit HttpClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets;

type
  THttpClientErrorEvent = procedure (Sender: TObject; const ErrorMsg: String) of object;

  THttpClient = class;

  { THCThread }

  THCThread = class(TThread)
  private
    FHC: THttpClient;
    FHttpDataReceived: TDataEvent;
    FHttpHeaders: TNotifyEvent;
    FHttpPassword: TPasswordEvent;
    FHttpRedirect: TRedirectEvent;
    FHttpFinish: TNotifyEvent;
    FHttpError: THttpClientErrorEvent;
    FMethod, FUrl: String;
    FCodes: array of Integer;
    FContentLength, FCurrentPos: Int64;
    FRepeatRequest: Boolean;
    FSrc, FDest: String;
    FErrorMsg: String;
    procedure DoDataReceived;
    procedure DoHeaders;
    procedure DoPassword;
    procedure DoRedirect;
    procedure DoFinish;
    procedure DoError;
    procedure DataReceivedHandler(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
    procedure HeadersHandler(Sender: TObject);
    procedure PasswordHandler(Sender: TObject; var RepeatRequest: Boolean);
    procedure RedirectHandler(Sender: TObject; const ASrc: String;
      var ADest: String);
  protected
    procedure Execute; override;
  public
    constructor Create(Http: THttpClient; const AMethod, AUrl: String;
      const AllowedResponseCodes: array of Integer);
    destructor Destroy; override;
    procedure Stop;
  end;

  { THttpClient }

  THttpClient = class(TFPHttpClient)
  private
    FMultiThreaded: Boolean;
    FContentStream: TStream;
    FOnError: THttpClientErrorEvent;
    FOnFinish: TNotifyEvent;
    FStream: TStringStream;
    FThreads: TThreadList;
    function GetConnectionCount: Integer;
    function GetContent: String;
    function GetStream: TStream;
    function ResetStream: TStream;
    procedure ThreadTerminate(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure HTTPMethod(const AMethod, AURL: String; Stream: TStream;
      const AllowedResponseCodes: array of Integer); override;
    procedure Send(const AMethod, AURL: String);
    procedure MyFormPost(const URL : string; FormData:  TStrings);
    procedure MyStreamFormPost(const AURL: string; FormData: TStrings; const AFieldName, AFileName: string; const AStream: TStream);
    function MyIndexOfHeader(const AHeader: String): Integer;
    procedure MyAddHeader(const AHeader, AValue: String);
    function MyGetHeader(const AHeader: String): String;
    procedure Terminate; reintroduce;
  published
    property MultiThreaded: Boolean read FMultiThreaded write FMultiThreaded;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: THttpClientErrorEvent read FOnError write FOnError;
    property ContentStream: TStream read GetStream write FContentStream;
    property Content: String read GetContent;
    property ConnectionCount: Integer read GetConnectionCount;
  end;

procedure HttpGet(const AUrl: String; var AContent, AStatusText: String; var AStatusCode: Integer);

implementation

uses
  opensslsockets, openssl, apputils;

procedure HttpGet(const AUrl: String; var AContent, AStatusText: String;
  var AStatusCode: Integer);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  with TFPHttpClient.Create(nil) do
  try try
    HttpMethod('GET', AUrl, SS, []);
    AContent := SS.DataString;
    AStatusText := ResponseStatusText;
    AStatusCode := ResponseStatusCode;
  except
    on E: ESocketError do
    begin
      AStatusText := E.Message;
      AStatusCode := 0;
    end;
  end;
  finally
    SS.Free;
    Free;
  end;
end;

{ THCThread }

procedure THCThread.DoDataReceived;
begin
  FHttpDataReceived(FHC, FContentLength, FCurrentPos);
end;

procedure THCThread.DoHeaders;
begin
  FHttpHeaders(FHC);
end;

procedure THCThread.DoPassword;
begin
  FHttpPassword(FHC, FRepeatRequest);
end;

procedure THCThread.DoRedirect;
begin
  FHttpRedirect(FHC, FSrc, FDest);
end;

procedure THCThread.DoFinish;
begin
  FHttpFinish(FHC);
end;

procedure THCThread.DoError;
begin
  FHttpError(FHC, FErrorMsg);
end;

procedure THCThread.DataReceivedHandler(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  FContentLength:=ContentLength;
  FCurrentPos:=CurrentPos;
  Synchronize(@DoDataReceived);
end;

procedure THCThread.HeadersHandler(Sender: TObject);
begin
  Synchronize(@DoHeaders);
end;

procedure THCThread.PasswordHandler(Sender: TObject; var RepeatRequest: Boolean
  );
begin
  FRepeatRequest:=RepeatRequest;
  Synchronize(@DoPassword);
  RepeatRequest:=FRepeatRequest;
end;

procedure THCThread.RedirectHandler(Sender: TObject; const ASrc: String;
  var ADest: String);
begin
  FSrc := ASrc; FDest := ADest;
  Synchronize(@DoRedirect);
  ADest := FDest;
end;

procedure THCThread.Execute;
begin
  try

    FHC.HTTPMethod(FMethod, FUrl, FHC.ResetStream, FCodes);
    if (FHttpFinish <> nil) and not Terminated then Synchronize(@DoFinish);

  except
    on E: Exception do
    begin
      FErrorMsg := E.Message;
      if (FHttpError <> nil) and not Terminated then Synchronize(@DoError);
    end;
  end;
end;

constructor THCThread.Create(Http: THttpClient; const AMethod, AUrl: String;
  const AllowedResponseCodes: array of Integer);
var
  i: Integer;
begin
  inherited Create(True);
  FMethod := AMethod;
  FUrl := AUrl;
  SetLength(FCodes, Length(AllowedResponseCodes));
  for i := 0 to High(AllowedResponseCodes) do
    FCodes[i] := AllowedResponseCodes[i];
  FHttpDataReceived := Http.OnDataReceived;
  FHttpHeaders := Http.OnHeaders;
  FHttpPassword := Http.OnPassword;
  FHttpRedirect := Http.OnRedirect;
  FHttpFinish := Http.OnFinish;
  FHttpError := Http.OnError;
  FHC := THttpClient.Create;
  with FHC do
  begin
    //KeepConnection := Http.KeepConnection;
    IOTimeout := Http.IOTimeout;
    RequestHeaders := Http.RequestHeaders;
    if Http.RequestBody <> nil then
    begin
      Http.RequestBody.Position := 0;
      RequestBody := TMemoryStream.Create;
      RequestBody.CopyFrom(Http.RequestBody, Http.RequestBody.Size);
      RequestBody.Position := 0;
    end;
    HTTPversion := Http.HTTPversion;
    Cookies := Http.Cookies;
    AllowRedirect := Http.AllowRedirect;
    MaxRedirects := Http.MaxRedirects;
    UserName := Http.UserName;
    Password := Http.Password;
    Proxy.Assign(Http.Proxy);
    if FHttpDataReceived <> nil then OnDataReceived := @DataReceivedHandler;
    if FHttpHeaders <> nil then OnHeaders := @HeadersHandler;
    if FHttpPassword <> nil then OnPassword := @PasswordHandler;
    if FHttpRedirect <> nil then OnRedirect := @RedirectHandler;
  end;
end;

destructor THCThread.Destroy;
begin
  SetLength(FCodes, 0);
  if FHC.RequestBody <> nil then FHC.RequestBody.Free;
  FHC.Free;
  inherited Destroy;
end;

procedure THCThread.Stop;
begin
  Terminate;
  FHC.Terminate;
  {FHttpError:=nil;
  FHttpFinish:=nil;
  FHC.OnDataReceived:=nil;
  FHC.OnHeaders:=nil;
  FHC.OnPassword:=nil;
  FHC.OnRedirect:=nil;
  OnTerminate:=nil;    }
end;


{ THttpClient }

function THttpClient.GetConnectionCount: Integer;
var
  L: TList;
begin
  L := FThreads.LockList;
  Result := L.Count;
  FThreads.UnlockList;
end;

function THttpClient.GetContent: String;
var
  Len: Int64;
begin
  if FContentStream <> nil then
  begin
    Len := FContentStream.Size;
    SetLength(Result, Len);
    FContentStream.Position := 0;
    FContentStream.Read(Pointer(Result)^, Len);
  end
  else
    Result := FStream.DataString;
end;

function THttpClient.GetStream: TStream;
begin
  if FContentStream <> nil then
    Result := FContentStream
  else
  begin
    Result := FStream;
    //Result.Size := 0;
  end;
end;

function THttpClient.ResetStream: TStream;
begin
  Result := GetStream;
  Result.Size := 0;
end;

procedure THttpClient.ThreadTerminate(Sender: TObject);
begin
  if FThreads <> nil then FThreads.Remove(Sender);
end;

constructor THttpClient.Create;
begin
  inherited Create(nil);
  FStream := TStringStream.Create('');
  FThreads := TThreadList.Create;
  if not IsSSLLoaded then InitSSLInterface;
end;

destructor THttpClient.Destroy;
begin
  Terminate;
  FreeAndNil(FThreads);
  FStream.Free;
  inherited Destroy;
end;

procedure THttpClient.HTTPMethod(const AMethod, AURL: String; Stream: TStream;
  const AllowedResponseCodes: array of Integer);
var
  Th: THCThread;
begin
  if FMultiThreaded then
  begin
    Th := THCThread.Create(Self, AMethod, AUrl, AllowedResponseCodes);
    Th.FreeOnTerminate := True;
    Th.OnTerminate:=@ThreadTerminate;
    Th.Start;
    FThreads.Add(Th);
  end
  else
    inherited HttpMethod(AMethod, AUrl, Stream, AllowedResponseCodes);
end;

procedure THttpClient.Send(const AMethod, AURL: String);
begin
  HttpMethod(AMethod, AURL, ResetStream, []);
end;

procedure THttpClient.MyFormPost(const URL: string; FormData: TStrings);
Var
  I : Integer;
  S,N,V : String;
begin
  S:='';
  For I:=0 to FormData.Count-1 do
  begin
    If (S<>'') then
      S:=S+'&';
    FormData.GetNameValue(i,n,v);
    S:=S+EncodeURLElement(N)+'='+EncodeURLElement(V);
  end;
  RequestBody:=TStringStream.Create(S);
  try
    AddHeader('Content-Type','application/x-www-form-urlencoded');
    Post(URL, ResetStream);
  finally
    if not FMultiThreaded then
    begin
      RequestBody.Free;
      RequestBody:=Nil;
    end;
  end;
end;

procedure THttpClient.MyStreamFormPost(const AURL: string; FormData: TStrings;
  const AFieldName, AFileName: string; const AStream: TStream);
const
  CRLF = #13#10;
Var
  S, Sep : string;
  SS : TStringStream;
  I: Integer;
  N,V: String;
begin
  Sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
  AddHeader('Content-Type','multipart/form-data; boundary='+Sep);
  SS:=TStringStream.Create('');
  try
    if (FormData<>Nil) then
      for I:=0 to FormData.Count -1 do
        begin
        // not url encoded
        FormData.GetNameValue(I,N,V);
        S :='--'+Sep+CRLF;
        S:=S+Format('Content-Disposition: form-data; name="%s"'+CRLF+CRLF+'%s'+CRLF,[N, V]);
        SS.WriteBuffer(S[1],Length(S));
        end;
    S:='--'+Sep+CRLF;
    s:=s+Format('Content-Disposition: form-data; name="%s"; filename="%s"'+CRLF,[AFieldName,ExtractFileName(AFileName)]);
    s:=s+'Content-Type: application/octet-string'+CRLF+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    AStream.Seek(0, soFromBeginning);
    SS.CopyFrom(AStream,AStream.Size);
    S:=CRLF+'--'+Sep+'--'+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    SS.Position:=0;
    RequestBody:=SS;
    Post(AURL,ResetStream);
  finally
    if not FMultiThreaded then
    begin
      RequestBody:=Nil;
      SS.Free;
    end;
  end;
end;

function THttpClient.MyIndexOfHeader(const AHeader: String): Integer;
begin
  Result := IndexOfHeader(AHeader);
end;

procedure THttpClient.MyAddHeader(const AHeader, AValue: String);
begin
  AddHeader(AHeader, AValue);
end;

function THttpClient.MyGetHeader(const AHeader: String): String;
begin
  Result := GetHeader(AHeader);
end;

procedure THttpClient.Terminate;
var
  i: Integer;
  L: TList;
begin
  inherited Terminate;
  L := FThreads.LockList;
  for i := 0 to L.Count - 1 do
  begin
    //THCThread(L[i]).OnTerminate:=nil;
    THCThread(L[i]).Stop;
  end;
  L.Clear;
  FThreads.UnlockList;
end;

end.

