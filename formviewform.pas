unit FormViewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls, strconsts, dxctrls;

type

  { TFormViewFm }

  TFormViewFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Items: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Fm: TdxForm): Integer;
  end;

var
  FormViewFm: TFormViewFm;

implementation

{$R *.lfm}

{ TFormViewFm }

procedure TFormViewFm.FormCreate(Sender: TObject);
begin
  Caption := rsViewType;
  Items.Items.AddStrings([rsGridTop, rsGridBottom, rsGridLeft, rsGridRight,
    rsGridOnly, rsWithoutGrid]);
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TFormViewFm.FormShow(Sender: TObject);
begin
  Items.SetFocus;
end;

function TFormViewFm.ShowForm(Fm: TdxForm): Integer;
begin
  Items.ItemIndex:=Integer(Fm.ViewType);
  Result := ShowModal;
  if Result = mrOk then Fm.ViewType := TViewType(Items.ItemIndex);
end;

end.

