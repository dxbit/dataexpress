unit FormLayoutsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  DialogGrid, dxctrls, formlayouts, strconsts, Grids;

type

  { TFormLayoutsFm }

  TFormLayoutsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    GridBns: TDialogGridButtons;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
    procedure GridDblClick(Sender: TObject);
    procedure GridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure HelpButtonClick(Sender: TObject);
  private
    FpFm: PFormLayoutForm;
    FpLay: PFormLayout;
    FForm: TdxForm;
    procedure FillGrid;
  public
    function ShowForm(AForm: TdxForm): Integer;
  end;

var
  FormLayoutsFm: TFormLayoutsFm;

function ShowFormLayoutsForm(AForm: TdxForm): Integer;

implementation

uses
  formmanager, formlayoutform, apputils, helpmanager;

function ShowFormLayoutsForm(AForm: TdxForm): Integer;
begin
  if FormLayoutsFm = nil then
    FormLayoutsFm := TFormLayoutsFm.Create(Application);
  Result := FormLayoutsFm.ShowForm(AForm);
end;

{$R *.lfm}

{ TFormLayoutsFm }

procedure TFormLayoutsFm.GridCommand(Sender: TObject; Cmd: TDialogGridCommand);
var
  LayoutName: String;
  r, i: Integer;
begin
  case Cmd of
    dgcAppend:
      begin
        LayoutName := '';
        if ShowFormLayoutForm(FForm, LayoutName, FpFm) = mrOk then
        begin
          r := Grid.RowCount;
          Grid.RowCount := r + 1;
          Grid.Cells[0, r] := FormLayoutFm.NameEdt.Text;
          Grid.Cells[1, r] := Bool2Str(FormLayoutFm.AppTypeGrp.Checked[0]);
          Grid.Cells[2, r] := Bool2Str(FormLayoutFm.AppTypeGrp.Checked[1]);
          Grid.Cells[3, r] := FormLayoutFm.MinWidthSpn.Text;
          Grid.Cells[4, r] := Format('%d x %d', [FForm.Width, FForm.Height]);
          Grid.Cells[5, r] := Bool2Str(FormLayoutFm.FixedHeightChk.Checked);
          if FormLayoutFm.DisabledChk.Checked then Grid.Objects[1, r] := TObject(PtrInt(1));
          Grid.Row := r;
        end;
      end;
    dgcEdit:
      begin
        r := Grid.Row;
        LayoutName := Grid.Cells[0, r];
        if ShowFormLayoutForm(FForm, LayoutName, FpFm) = mrOk then
        begin
          Grid.Cells[0, r] := FormLayoutFm.NameEdt.Text;
          Grid.Cells[1, r] := Bool2Str(FormLayoutFm.AppTypeGrp.Checked[0]);
          Grid.Cells[2, r] := Bool2Str(FormLayoutFm.AppTypeGrp.Checked[1]);
          Grid.Cells[3, r] := FormLayoutFm.MinWidthSpn.Text;
          Grid.Cells[5, r] := Bool2Str(FormLayoutFm.FixedHeightChk.Checked);
          if FormLayoutFm.DisabledChk.Checked then Grid.Objects[1, r] := TObject(PtrInt(1))
          else Grid.Objects[1, r] := nil;
          Grid.InvalidateRow(r);
          //Grid.Cells[4, r] := Format('%d x %d', [FForm.Width, FForm.Height]);
        end;
      end;
    dgcDelete:
      begin
        if ConfirmDelete then
        begin
          r := Grid.Row;
          FpFm^.Layouts.DeleteLayout(Grid.Cells[0, r]);
          if Grid.Objects[0, r] <> nil then FpLay := nil;
          Grid.DeleteRow(r);
        end;
      end;
    dgcMoveDown:
      begin
        i := Grid.Row - 1;
        FpFm^.Layouts.Exchange(i, i-1);
      end;
    dgcMoveUp:
      begin
        i := Grid.Row - 1;
        FpFm^.Layouts.Exchange(i, i+1);
      end;
  end;
end;

procedure TFormLayoutsFm.GridDblClick(Sender: TObject);
begin
  Grid.PopupMenu.Items[1].Click;
end;

procedure TFormLayoutsFm.GridPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then Exit;
  if Grid.Objects[0, aRow] <> nil then Grid.Canvas.Font.Style := [fsBold];
  if Grid.Objects[1, aRow] <> nil then Grid.Canvas.Font.StrikeThrough := True;
end;

procedure TFormLayoutsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('layouts');
end;

procedure TFormLayoutsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TFormLayoutsFm.FormCreate(Sender: TObject);
begin
  Caption := rsLayoutsManagement;
  Grid.Columns[0].Title.Caption := rsLayoutName;
  Grid.Columns[1].Title.Caption := rsWeb;
  Grid.Columns[2].Title.Caption := rsDesktop;
  Grid.Columns[3].Title.Caption := rsBreak;
  Grid.Columns[4].Title.Caption := rsFormSize;
  Grid.Columns[5].Title.Caption := rsFixedH;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TFormLayoutsFm.FillGrid;
var
  i, r: Integer;
  pLay: PFormLayout;
begin
  Grid.RowCount := FpFm^.Layouts.Count + 1;
  for i := 0 to FpFm^.Layouts.Count - 1 do
  begin
    pLay := FpFm^.Layouts[i];
    r := i + 1;
    Grid.Cells[0, r] := pLay^.Name;
    Grid.Cells[1, r] := Bool2Str(pLay^.Web);
    Grid.Cells[2, r] := Bool2Str(pLay^.Desktop);
    Grid.Cells[3, r] := IntToStr(pLay^.MinWidth);
    Grid.Cells[4, r] := Format('%d x %d', [pLay^.Width, pLay^.Height]);
    Grid.Cells[5, r] := Bool2Str(pLay^.FixedHeight);
    if pLay = FpLay then Grid.Objects[0, r] := TObject(PtrInt(1))
    else Grid.Objects[0, r] := nil;
    if pLay^.Disabled then Grid.Objects[1, r] := TObject(PtrInt(1))
    else Grid.Objects[1, r] := nil;
  end;
end;

function TFormLayoutsFm.ShowForm(AForm: TdxForm): Integer;
var
  pFm: PFormLayoutForm;
  MS: TMemoryStream;
begin
  FForm := AForm;
  MS := TMemoryStream.Create;

  FpFm := FormMan.Layouts.NewForm(FForm);

  pFm := FormMan.Layouts.FindForm(AForm.Id);
  if pFm = nil then
    pFm := FormMan.Layouts.AddForm(FForm);

  FormMan.Layouts.SaveToStream(pFm, MS);
  MS.Position := 0;
  FormMan.Layouts.LoadFromStream(FpFm, MS);
  FpLay := FpFm^.Layouts.FindLayout(FForm.LayoutName);

  FillGrid;
  Result := ShowModal;

  if Result = mrOk then
  begin
    MS.Size := 0;
    FormMan.Layouts.SaveToStream(FpFm, MS);
    MS.Position := 0;
    FormMan.Layouts.LoadFromStream(pFm, MS);

    if FpLay = nil then
    begin
      if Grid.Row > 0 then
        FForm.LayoutName := pFm^.Layouts[Grid.Row-1]^.Name;
    end
    else if FpLay^.Name <> FForm.LayoutName then
      FForm.LayoutName := FpLay^.Name;
  end;

  FormMan.Layouts.DisposeForm(FpFm);
  MS.Free;
end;

end.

