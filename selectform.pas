unit SelectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TSelectFm }

  TSelectFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
  private
    FUrl: String;
    function GetIndex: Integer;
    procedure SetControlState;
    { private declarations }
  public
    { public declarations }
    function ShowForm(const Title, Url: String; SL: TStrings; SelIndex: Integer = -1): Integer;
    property Index: Integer read GetIndex;
  end;

var
  SelectFm: TSelectFm;

function ShowSelectForm(const Title, Url: String; SL: TStrings; SelIndex: Integer = -1): Integer;

implementation

uses
  helpmanager;

function ShowSelectForm(const Title, Url: String; SL: TStrings;
  SelIndex: Integer): Integer;
begin
  if SelectFm = nil then
  	SelectFm := TSelectFm.Create(Application);
  Result := SelectFm.ShowForm(Title, Url, SL, SelIndex);
end;

{$R *.lfm}

{ TSelectFm }

procedure TSelectFm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TSelectFm.FormShow(Sender: TObject);
begin
  List.SetFocus;
  SetControlState;
end;

procedure TSelectFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FUrl);
end;

procedure TSelectFm.ListDblClick(Sender: TObject);
begin
  if List.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TSelectFm.ListSelectionChange(Sender: TObject; User: boolean);
begin
  SetControlState;
end;

function TSelectFm.GetIndex: Integer;
begin
  Result := List.ItemIndex;
end;

procedure TSelectFm.SetControlState;
begin
  ButtonPanel1.OkButton.Enabled:=List.ItemIndex >=0;
end;

function TSelectFm.ShowForm(const Title, Url: String; SL: TStrings;
  SelIndex: Integer): Integer;
begin
  FUrl := Url;
  Caption := Title;
  List.Clear;
  List.Items.AddStrings(SL);
  List.ItemIndex := SelIndex;
  ButtonPanel1.HelpButton.Visible := Url > '';
  Result := ShowModal;
end;

end.

