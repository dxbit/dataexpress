unit CounterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, strconsts, dxctrls;

type

  { TCounterFm }

  TCounterFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Chk: TCheckBox;
    Label1: TLabel;
    Val: TSpinEdit;
    procedure ChkChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxCounter);
  end;

var
  CounterFm: TCounterFm;

procedure ShowCounterForm(C: TdxCounter);

implementation

uses
  helpmanager, formdesigner;

procedure ShowCounterForm(C: TdxCounter);
begin
  if CounterFm = nil then
  	CounterFm := TCounterFm.Create(Application);
  CounterFm.ShowForm(C);
end;

{$R *.lfm}

{ TCounterFm }

procedure TCounterFm.FormCreate(Sender: TObject);
begin
  Val.MaxValue:=MaxInt;
  Caption := rsStartWith;
  Chk.Caption:=rsChangeCounter;
  Label1.Caption := rsBeginValue;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TCounterFm.FormShow(Sender: TObject);
begin
  Chk.SetFocus;
end;

procedure TCounterFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('startwith');
end;

procedure TCounterFm.ChkChange(Sender: TObject);
begin
  Val.Enabled:=Chk.Checked;
end;

procedure TCounterFm.ShowForm(C: TdxCounter);
begin
  Chk.Checked:=C.Restart;
  Val.Value:=C.StartWith;

  Chk.OnChange(Chk);
  if ShowModal = mrOk then
  begin
    C.Restart := Chk.Checked;
    C.StartWith:=Val.Value;
    Cache.SetCounter(C, Val.Value);
  end;
end;

end.

