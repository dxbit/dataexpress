unit DeleteRecsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls, strconsts;

type

  { TDeleteRecsFm }

  TDeleteRecsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  DeleteRecsFm: TDeleteRecsFm;

function ShowDeleteRecsForm: Integer;

implementation

uses
  helpmanager;

function ShowDeleteRecsForm: Integer;
begin
  if DeleteRecsFm = nil then
  	DeleteRecsFm := TDeleteRecsFm.Create(Application);
  Result := DeleteRecsFm.ShowForm;
end;

{$R *.lfm}

{ TDeleteRecsFm }

procedure TDeleteRecsFm.CheckBox1Change(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled:=CheckBox1.Checked;
end;

procedure TDeleteRecsFm.FormCreate(Sender: TObject);
begin
  Caption := rsDeleteRecords;
  Label1.Caption := Format(rsDelRecsMsg, [LineEnding + LineEnding, LineEnding + LineEnding]);
  CheckBox1.Caption := rsDelRecsConfirm;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TDeleteRecsFm.FormShow(Sender: TObject);
begin
  ButtonPanel1.CancelButton.SetFocus;
end;

procedure TDeleteRecsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('deleterecs');
end;

function TDeleteRecsFm.ShowForm: Integer;
begin
  CheckBox1.Checked := False;
  ButtonPanel1.OkButton.Enabled:=False;
  Result := ShowModal;
end;

end.

