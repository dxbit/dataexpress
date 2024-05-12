unit HelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpclient, strconsts;

type

  { THelpMan }

  THelpMan = class
  private
    FIndex: TStringList;
    function LoadIndex: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenHelp(const Name: String);
  end;

var
  HelpMan: THelpMan;

procedure OpenHelp(const Name: String);

implementation

uses
  apputils;

procedure OpenHelp(const Name: String);
begin
  if HelpMan <> nil then HelpMan.OpenHelp(Name);
end;

{ THelpMan }

function THelpMan.LoadIndex: Boolean;
var
  Code: Integer;
  S, StatusText, Msg: String;
begin
  HttpGet('http://mydataexpress.ru/files/helpindex.txt', S, StatusText, Code);
  if Code <> 200 then
  begin
    Msg := StatusText;
    if Code <> 0 then
      Msg := IntToStr(Code) + ': ' + Msg;
    ErrMsgFmt(rsCantDownloadHelpIndex, [Msg]) ;
  end
  else
    FIndex.Text := S;
  Result := Code = 200;
end;

constructor THelpMan.Create;
begin
  FIndex := TStringList.Create;
  FIndex.TextLineBreakStyle := tlbsCRLF;
end;

destructor THelpMan.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

procedure THelpMan.OpenHelp(const Name: String);
var
  i: Integer;
  Url: String;
begin
  if (FIndex.Count = 0) and not LoadIndex then Exit;
  i := FIndex.IndexOfName(Name);
  if i >= 0 then
  begin
    Url := Trim(FIndex.ValueFromIndex[i]);
    if Url <> '' then OpenUrl(Url)
    else ErrMsg(rsHelpTopicNotAvailable);
  end
  else ErrMsgFmt(rsNameNotFoundInHelpIndex, [Name])
end;

end.

