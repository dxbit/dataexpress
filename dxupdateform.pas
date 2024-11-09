unit DXUpdateForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, FileUtil, LazUtf8, IniFiles, Translations, LclProc, dxupdatestrconsts;

type

  { TDXUpdateFm }

  TDXUpdateFm = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    Msg: TLabel;
    Progress: TProgressBar;
    Shape1: TShape;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure DoUpdate;
  public
  end;

var
  DXUpdateFm: TDXUpdateFm;

implementation

uses
  ShellApi, LazStringUtils;

{$R *.lfm}

function Translate(Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
begin
  case StringCase(Value,['&Retry', 'Abort']) of
   0: Result:=rsRetry;
   1: Result:=rsAbort;
   else Result:='';
  end;
end;

function AppPath: String;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function GetLanguage: String;
begin
  with TIniFile.Create(AppPath + 'dataexpress.cfg') do
  begin
    Result := ReadString('UI', 'Language', 'ru');
    Free;
  end;
end;

procedure DoTranslate;
var
  lng, S: String;
begin
  lng := GetLanguage;
  S := AppPath + 'languages' + PathDelim + lng + PathDelim + 'dxupdate.' + lng + '.po';
  TranslateUnitResourceStrings('dxupdatestrconsts', S);
  SetResourceStrings(@Translate, nil);
end;

procedure ErrMsg(const Msg: String; E: Exception);
var
  S: String;
begin
  if E <> nil then
    S := Format(rsFullErrMsg, [Msg, E.ClassName, E.Message])
  else
    S := Msg;
  MessageDlg(rsError, S, mtError, [mbOk], 0);
end;

function ShellExec(const Operation, FileName, Params, WorkDir: String;
  ShowCmd: LongInt): Boolean;
var
  OutS, S: String;
  Arr: TStringArray;
begin
  {$ifdef windows}
  Result := ShellExecute(0, PChar(Operation), PChar(Utf8ToWinCP(FileName)),
    PChar(Utf8ToWinCP(Params)), PChar(Utf8ToWinCP(WorkDir)), ShowCmd) > 32;
  {$else}
  S := RemoveQuotes(FileName);
  if IsUrl(S) or IsMail(S) then
    Result := LclIntf.OpenUrl(S)
  else if ExtractFileExt(S) <> '' then
    Result := LclIntf.OpenDocument(S)
  else
  begin
    ParamsToArray(Params, Arr);
    Result := RunCommandInDir(WorkDir, S, Arr, OutS, [poNoConsole]);
    SetLength(Arr, 0);
  end;
  {$endif}
end;

function CheckWriteFile(const FileName: String): Boolean;
begin
  try
    TFileStream.Create(FileName, fmOpenReadWrite + fmShareExclusive).Free;
    Result := True;
  except
    Result := False;
  end;
end;

{ TDXUpdateFm }

procedure TDXUpdateFm.FormShow(Sender: TObject);
begin
  Msg.Caption := rsUpdating;
  Timer.Enabled := True;
end;

procedure TDXUpdateFm.FormCreate(Sender: TObject);
begin
  DoTranslate;
end;

procedure TDXUpdateFm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  Progress.Style := pbstMarquee;
  DoUpdate;
end;

procedure TDXUpdateFm.DoUpdate;
var
  TmpDir, S, Dest: String;
  SrcL, DestL: TStringList;
  i: Integer;
begin
  TmpDir := GetTempDir + 'dxupdates.tmp' + PathDelim;
  Application.ProcessMessages;
  //DirL := FindAllDirectories(TmpDir, True);
  SrcL := FindAllFiles(TmpDir, AllFilesMask);
  Application.ProcessMessages;
  DestL := FindAllFiles(AppPath, AllFilesMask);

  try

  // Проверяем возможность перезаписи файлов
  for i := 0 to SrcL.Count - 1 do
  begin
    S := SrcL[i];
    Delete(S, 1, Length(TmpDir));
    S := AppPath + S;
    if DestL.IndexOf(S) >= 0 then
    begin
      while not CheckWriteFile(S) do
        if MessageDlg(rsError, Format(rsStartUpdateError, [S]), mtConfirmation,
          [mbRetry, mbAbort], 0) <> mrRetry then
        begin
          MessageDlg(rsWarning, rsUpdateAbortedUser,
            mtInformation, [mbOk], 0);
          Exit;
        end;
    end;
  end;
  Application.ProcessMessages;

  {for i := 0 to DirL.Count - 1 do
  begin
    S := DirL[i];
    Delete(S, 1, Length(TmpDir));
    S := AppPath + S;
    if not ForceDirectories(S) then
    begin
      ErrMsg(Format(rsCantCreateFolder, [S, AppPath]), nil);
      Exit;
    end;
  end;  }

  try
    for i := 0 to SrcL.Count - 1 do
    begin
      S := SrcL[i];
      Dest := S;
      Delete(Dest, 1, Length(TmpDir));
      Dest := AppPath + Dest;
      CopyFile(S, Dest, [cffOverwriteFile, cffPreserveTime, cffCreateDestDirectory], True);
      Application.ProcessMessages;
    end;
    DeleteDirectory(TmpDir, True);
  except
    on E: Exception do
    begin
      ErrMsg(Format(rsCopyFileError, [S, Dest]), E);
    end;
  end;

  ShellExec('open', AppPath + 'dataexpress.exe', '', '', 5);

  finally
    //DirL.Free;
    SrcL.Free;
    DestL.Free;
    Application.Terminate;
  end;
end;

end.

