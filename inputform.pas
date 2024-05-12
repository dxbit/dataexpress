unit InputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TInputFm }

  TInputFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FUrl: String;
    FNumbers, FCheckName, FIsFloat: Boolean;
    FMinV, FMaxV: Extended;
    procedure EditKeyPress(Sender: TObject; var Key: char);
  public
    { public declarations }
    function ShowForm(const Title, Prompt, Url: String; var S: String;
      Numbers, IsFloat, CheckName: Boolean; MinV, MaxV: Extended): Boolean;
  end;

function InputStr(const Title, Prompt, Url: String; var S: String; CheckName: Boolean): Boolean;
function InputInt(const Title, Prompt, Url: String; var S: String; MinV, MaxV: Integer): Boolean;
function InputFloat(const Title, Prompt, Url: String; var S: String; MinV, MaxV: Extended): Boolean;

var
  InputFm: TInputFm;

implementation

uses
  helpmanager, apputils;

function ShowInputForm(const Title, Prompt, Url: String; var S: String;
	Numbers, IsFloat, CheckName: Boolean; MinV, MaxV: Extended): Boolean;
begin
  if InputFm = nil then
  	InputFm := TInputFm.Create(Application);
  Result := InputFm.ShowForm(Title, Prompt, Url, S, Numbers, IsFloat, CheckName,
  	MinV, MaxV);
end;

function InputStr(const Title, Prompt, Url: String; var S: String;
  CheckName: Boolean): Boolean;
begin
  Result := ShowInputForm(Title, Prompt, Url, S, False, False, CheckName, 0, 0);
end;

function InputInt(const Title, Prompt, Url: String; var S: String; MinV,
  MaxV: Integer): Boolean;
begin
  Result := ShowInputForm(Title, Prompt, Url, S, True, False, False, MinV, MaxV);
end;

function InputFloat(const Title, Prompt, Url: String; var S: String; MinV,
  MaxV: Extended): Boolean;
begin
  Result := ShowInputForm(Title, Prompt, Url, S, True, True, False, MinV, MaxV);
end;

{$R *.lfm}

{ TInputFm }

procedure TInputFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TInputFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  N: integer;
  b: Boolean;
  E: Extended;
begin
  if ModalResult = mrOk then
  begin
    if FNumbers then
    begin
      if Trim(Edit1.Text) = '' then
      begin
        ErrMsg(rsEnterValue);
        CanClose := False;
      end;
      if FIsFloat then
        b := TryStrToFloat(Edit1.Text, E)
      else
      begin
        b := TryStrToInt(Edit1.Text, N);
        E := N;
      end;
      if b then
      begin
        if ((FMinV <> 0) or (FMaxV <> 0)) and ((E < FMinV) or (E > FMaxV)) then
        begin
          ErrMsg(Format(rsRangeMsg2, [FloatToStr(FMinV), FloatToStr(FMaxV)]));
          CanClose := False;
        end;
      end
      else
      begin
        ErrMsgFmt(rsInvalidNumber, [Edit1.Text]);
        CanClose := False;
      end;
    end
    else if FCheckName then
    begin
      if not CheckFieldName(Edit1.Text) then CanClose := False;
    end;
  end;
end;

procedure TInputFm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
  Edit1.SelectAll;
end;

procedure TInputFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TInputFm.EditKeyPress(Sender: TObject; var Key: char);
var
  Ch: Char;
begin
  if FNumbers then
  begin
    Ch := DefaultFormatSettings.DecimalSeparator;
    if Key in [#8, '0'..'9', '-'] then
    else if FIsFloat and (Key in [' ', '.', ',']) then Key := Ch
    else Key := #0;
  end;
end;

function TInputFm.ShowForm(const Title, Prompt, Url: String; var S: String;
  Numbers, IsFloat, CheckName: Boolean; MinV, MaxV: Extended): Boolean;
begin
  FUrl := Url;
  ButtonPanel1.HelpButton.Visible := Url > '';
  FNumbers:=Numbers;
  FIsFloat := IsFLoat;
  FCheckName := CheckName;
  FMinV := MinV;
  FMaxV := MaxV;
  Caption := Title;
  Label1.Caption := Prompt;
  Edit1.Text := S;
  Edit1.OnKeyPress:=@EditKeyPress;
  if ShowModal = mrOk then
  begin
    S := Edit1.Text;
    Result := True;
  end
  else Result := False;
end;

end.

