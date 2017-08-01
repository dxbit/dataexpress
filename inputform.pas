{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
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
  public
    { public declarations }
    function ShowForm(const Title, Prompt, Url: String; var S: String;
      Numbers, IsFloat, CheckName: Boolean; MinV, MaxV: Extended): Boolean;
  end;

var
  InputFm: TInputFm;

function InputStr(const Title, Prompt, Url: String; var S: String; CheckName: Boolean): Boolean;
function InputInt(const Title, Prompt, Url: String; var S: String; MinV, MaxV: Integer): Boolean;
function InputFloat(const Title, Prompt, Url: String; var S: String; MinV, MaxV: Extended): Boolean;

implementation

uses
  helpform, apputils;

function InputStr(const Title, Prompt, Url: String; var S: String;
  CheckName: Boolean): Boolean;
begin
  Result := InputFm.ShowForm(Title, Prompt, Url, S, False, False, CheckName, 0, 0);
end;

function InputInt(const Title, Prompt, Url: String; var S: String; MinV,
  MaxV: Integer): Boolean;
begin
  Result := InputFm.ShowForm(Title, Prompt, Url, S, True, False, False, MinV, MaxV);
end;

function InputFloat(const Title, Prompt, Url: String; var S: String; MinV,
  MaxV: Extended): Boolean;
begin
  Result := InputFm.ShowForm(Title, Prompt, Url, S, True, True, False, MinV, MaxV);
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
        ErrMsg(rsInvalidNumber);
        CanClose := False;
      end;
    end
    else if FCheckName then
    begin
      if not CheckName(Edit1.Text) then CanClose := False;
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
  if ShowModal = mrOk then
  begin
    S := Edit1.Text;
    Result := True;
  end
  else Result := False;
end;

end.

