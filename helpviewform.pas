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
unit HelpViewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, Forms, Controls, Graphics,
  Dialogs, Menus, IpMsg, strconsts;

type

  { TMyHtmlProvider }

  TMyHtmlProvider = class(TIpCustomHtmlDataProvider)
  private
    FHtmlText: String;
  public
    function CheckURL(const URL: string; var ContentType: string): Boolean; override;
    function GetHtmlStream(const URL: string; PostData: TIpFormDataEntity): TStream;
      override;
    property HtmlText: String read FHtmlText write FHtmlText;
  end;

  { THelpViewFm }

  THelpViewFm = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
    FMyPvd: TMyHtmlProvider;
    IHP: TIpHtmlPanel;
  public
    { public declarations }
    procedure ShowForm(const aText: String);
  end;

var
  HelpViewFm: THelpViewFm;

implementation

{$R *.lfm}

{ THelpViewFm }

procedure THelpViewFm.FormCreate(Sender: TObject);
begin
  Caption := rsHelp;
  IHP := TIpHtmlPanel.Create(nil);
  IHP.PopupMenu := PopupMenu1;
  IHP.Parent := Self;
  IHP.Align := alClient;
  FMyPvd := TMyHtmlProvider.Create(nil);
  IHP.DataProvider := FMyPvd;
  MenuItem1.Caption:=rsCopy;
  MenuItem2.Caption := rsSelectAll;
end;

procedure THelpViewFm.FormDestroy(Sender: TObject);
begin
  IHP.Free;
  FMyPvd.Free;
end;

procedure THelpViewFm.MenuItem1Click(Sender: TObject);
begin
  IHP.CopyToClipboard;
end;

procedure THelpViewFm.MenuItem2Click(Sender: TObject);
begin
  IHP.SelectAll;
end;

procedure THelpViewFm.ShowForm(const aText: String);
begin
  FMyPvd.HtmlText:=aText;
  IHP.OpenURL('dummy');
  ShowModal;
end;

{ TMyHtmlProvider }

function TMyHtmlProvider.CheckURL(const URL: string; var ContentType: string
  ): Boolean;
begin
  Result:=True;
  ContentType := 'text/html';
end;

function TMyHtmlProvider.GetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
begin
  Result:=TStringStream.Create('<html><head><meta content="text/html;charset=UTF-8" ' +
    'http-equiv="Content-Type"></head>' + FHtmlText + '</html>');
end;

end.

