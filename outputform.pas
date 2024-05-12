unit OutputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, Types, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, ExtCtrls, strconsts, LclIntf, LclType, crossapi;

type
  { TOutputFm }

  TOutputFm = class(TForm)
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
  public
    procedure Clear;
  end;

var
  OutputFm: TOutputFm;

procedure ShowOutputForm(const S: String);
//procedure EnableOutputForm;
procedure RecreateOutputForm;

implementation

uses
  appsettings, apputils;

procedure CreateOutputForm;
begin
  OutputFm := TOutputFm.Create(nil);
  if AppConfig.OutputBounds.Right > 0 then
    OutputFm.BoundsRect := ScaleRectToScreen(AppConfig.OutputBounds)
  else
  begin
    OutputFm.BoundsRect := Rect(ScaleToScreen(20), Screen.Height - ScaleToScreen(250),
      Screen.Width - ScaleToScreen(40), Screen.Height - ScaleToScreen(100));
  end;
  if not AppConfig.OutputFormPosCorrected then
  begin
    CorrectFormPos(OutputFm, OutputFm);
    AppConfig.OutputFormPosCorrected := True;
  end;
end;

procedure ShowOutputForm(const S: String);
var
  Fm: TForm;
begin
  if OutputFm = nil then
    CreateOutputForm;
  if not OutputFm.Visible then
  begin
    Fm := Screen.ActiveForm;
    OutputFm.Show;
    if OutputFm.Active and (Fm <> nil) and Fm.CanFocus then Fm.SetFocus;
  end;
  OutputFm.Memo.Lines.Add(S);
end;

procedure EnableOutputForm;
begin
  if OutputFm <> nil then
    EnableWindow(OutputFm.Handle, True);
end;

procedure RecreateOutputForm;
var
  SL: TStringList;
  R: Types.TRect;
  p, spx, spy, l: Integer;
  Vis: Boolean;
begin
  {$ifdef linux}
  if OutputFm = nil then
  begin
    CreateOutputForm;
    Exit;
  end;
  {$endif}

  SL := TStringList.Create;
  SL.Assign(OutputFm.Memo.Lines);
  R := OutputFm.BoundsRect;
  p := OutputFm.Memo.SelStart;
  l := OutputFm.Memo.SelLength;
  spy := OutputFm.Memo.VertScrollBar.ScrollPos;
  spx := OutputFm.Memo.HorzScrollBar.ScrollPos;
  Vis := OutputFm.Visible;

  FreeAndNil(OutputFm);
  OutputFm := TOutputFm.Create(nil);

  OutputFm.BoundsRect := R;
  OutputFm.Memo.Lines := SL;
  OutputFm.Memo.SelStart := p;
  OutputFm.Memo.SelLength := l;

	if Vis then OutputFm.Show;
  if OutputFm.HandleAllocated then
  begin
    OutputFm.Memo.VertScrollBar.Position:=spy;
    OutputFm.Memo.HorzScrollBar.Position:=spx;
  end;

  SL.Free;
end;

{$R *.lfm}

{ TOutputFm }

procedure TOutputFm.FormCreate(Sender: TObject);
begin
  Caption := rsOutput;
  Memo.Clear;
  MenuItem1.Caption:=rsClear;
  MenuItem2.Caption := rsCopy;
  {$ifdef windows}
  FormStyle := fsStayOnTop;
  //PopupMode := pmExplicit;
  {$endif}
end;

procedure TOutputFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AppConfig.OutputBounds := ScaleRectTo96(GetFormRealBounds(Self));
end;

procedure TOutputFm.FormDestroy(Sender: TObject);
begin
  //SaveOutputFmState;
end;

procedure TOutputFm.MenuItem1Click(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TOutputFm.MenuItem2Click(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TOutputFm.Clear;
begin
  Memo.Clear;
end;

end.

