unit NewGridForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, ExtCtrls, StdCtrls, CheckLst, Spin, Buttons,
  dxctrls, strconsts, lists, myctrls, dxreports, CtrlUtils, LclIntf;

type

  { TGrdFm }

  TGrdFm = class(TForm)
    AltCellColor: TColorButtonEx;
    CellColor: TColorButtonEx;
    CellFont: TComboBoxAdv;
    CellFontColor: TColorButtonEx;
    CellFontS: TToggleBox;
    GridTitleFont: TComboBoxAdv;
    GridTitleFontS: TToggleBox;
    GridColor: TColorButtonEx;
    GridTitleColor: TColorButtonEx;
    FlatStyle: TToggleBox;
    CellFontSize: TSpinEdit;
    GridTitleFontColor: TColorButtonEx;
    GridTitleFontSize: TSpinEdit;
    CellFontB: TToggleBox;
    CellFontI: TToggleBox;
    CellFontU: TToggleBox;
    GridTitleFontB: TToggleBox;
    GridTitleFontI: TToggleBox;
    GridTitleFontU: TToggleBox;
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    InSelColor: TColorButtonEx;
    InSelTextColor: TColorButtonEx;
    ColorBnMnu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    SelColor: TColorButtonEx;
    SelTextColor: TColorButtonEx;
    ResetCellFontBn: TSpeedButton;
    ResetTitleFontBn: TSpeedButton;
    VertLines: TCheckBox;
    HorzLines: TCheckBox;
    GridOptions: TCheckListBox;
    GridLineStyle: TComboBox;
    GridGrp: TGroupBox;
    DataGrp: TGroupBox;
    TitlesGrp: TGroupBox;
    OptionsGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ColumnMnu: TPopupMenu;
    Panel1: TPanel;
    DefRowHeight: TSpinEdit;
    VisibleCols: TButton;
    procedure CellFontSelect(Sender: TObject);
    procedure CellFontSizeChange(Sender: TObject);
    procedure CellFontStyleChange(Sender: TObject);
    procedure ColorBnMnuPopup(Sender: TObject);
    procedure ColorChange(Sender: TObject);
    procedure ColumnMnuPopup(Sender: TObject);
    procedure DefRowHeightChange(Sender: TObject);
    procedure FlatStyleChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure GridLineStyleChange(Sender: TObject);
    procedure GridOptionsClickCheck(Sender: TObject);
    procedure GridTitleFontSelect(Sender: TObject);
    procedure GridTitleFontSizeChange(Sender: TObject);
    procedure LinesChange(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure ResetCellFontBnClick(Sender: TObject);
    procedure ResetTitleFontBnClick(Sender: TObject);
    procedure TitleFontStyleChange(Sender: TObject);
    procedure VisibleColsClick(Sender: TObject);
  private
    FForm: TdxForm;
    //FRD: TReportData;
    Grid: TMyGrid;
    FCols: TList;
    FRowSelect, FReadOnly, FAllowChangeSort, FColMove, FShowRowDeleteButton: Boolean;
    FModified: Boolean;
    FPopupColorBn: TColorButtonEx;
    FSampleBmp: TBitmap;
    FStopChangeEvents: Boolean;
    { private declarations }
    function IsCellSelected(ACol: Integer): Boolean;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    function GetCellAlignment(IsHeader: Boolean): Integer;
    function GetCellLayout(IsHeader: Boolean): Integer;
    procedure SetCellAlignment(IsHeader: Boolean; Al: TAlignment; AA: Boolean);
    procedure SetCellLayout(IsHeader: Boolean; TL: TTextLayout; AL: Boolean);
    procedure FillTestText;
    procedure GridCanSort(Sender: TObject; Index: Integer; var Cancel: Boolean);
    procedure GridGetCellText(Sender: TObject; aCol, aRow: Integer;
      var aText: String);
    procedure GridHeaderSizing(sender: TObject; const IsColumn: boolean;
      const aIndex, aSize: Integer);
  public
    { public declarations }
    function ShowForm(aForm: TdxForm): Integer;
    function ShowRpForm(QGrid: TdxQueryGrid; RD: TReportData): Integer;
  end;

var
  GrdFm: TGrdFm;

function ShowGridForm(aForm: TdxForm): Integer;
function ShowRpGridForm(QGrid: TdxQueryGrid; RD: TReportData): Integer;

implementation

uses
  colorform, fontform, gridcolsform, DBGrids, LazUtf8, helpmanager, apputils,
  dximages, formmanager;

const
  EDIT_OPT_IDX = 10;
  {SM_CXSIZEFRAME = 32;
  SM_CXVSCROLL = 2;}

type
  { TColumnWidthDlg }

  TColumnWidthDlg = class(TForm)
  private
    FAdjustWidth: Boolean;
    FColWidth: Integer;
    FEdit: TSpinEdit;
    FChk: TCheckBox;
    FIsForm: Boolean;
    procedure ChkChange(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm: Integer;
    function CloseQuery: boolean; override;
    property ColWidth: Integer read FColWidth write FColWidth;
    property AdjustWidth: Boolean read FAdjustWidth;
    property IsForm: Boolean read FIsForm write FIsForm;
  end;

  { TColumnTitleDlg }

  TColumnTitleDlg = class(TForm)
  private
    FEdit: TEdit;
    FChk: TCheckBox;
    FFieldName, FOldText: String;
    procedure ChkChange(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function ShowForm(ACol: TGridColumn; AForm: TdxForm): Integer;
  end;

{ TColumnTitleDlg }

procedure TColumnTitleDlg.ChkChange(Sender: TObject);
begin
  if FChk.Checked then
  begin
    FEdit.Enabled := False;
    FOldText := FEdit.Text;
    FEdit.Text := FFieldName;
  end
  else
  begin
    FEdit.Enabled := True;
    FEdit.Text := FOldText;
  end;
end;

constructor TColumnTitleDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsTitle;
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterColumnTitle;
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Parent := Self;
    AnchorSideTop.Control := L;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 2;
    Left := 8;
    Width := 354;
  end;
  FChk := TCheckBox.Create(Self);
  with FChk do
  begin
    Parent := Self;
    AnchorSideTop.Control := FEdit;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 8;
    Left := 8;
    Caption := rsDefaultTitle;
    OnChange:=@ChkChange;
  end;
  with TButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
  end;
  ClientWidth := 370;
  ClientHeight := 124;
end;

function TColumnTitleDlg.ShowForm(ACol: TGridColumn; AForm: TdxForm): Integer;
var
  C: TComponent;
begin
  FEdit.Text := ACol.Title.Caption;
  FOldText := FEdit.Text;
  C := FindById(AForm, ACol.Tag);
  FFieldName := GetFieldName(C);
  FChk.Checked := FFieldName = FEdit.Text;
  Result := ShowModal;
  if Result = mrOk then
  begin
    ACol.Title.Caption := FEdit.Text;
  end;
end;

{ TColumnWidthDlg }

procedure TColumnWidthDlg.ChkChange(Sender: TObject);
begin
  FEdit.Enabled := not FChk.Checked;
end;

constructor TColumnWidthDlg.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Caption := rsColumnWidth;
  BorderStyle:=bsDialog;
  Position := poOwnerFormCenter;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.Left := 8; L.Top := 8;
  L.Caption := rsEnterValue;
  FEdit := TSpinEdit.Create(Self);
  with FEdit do
  begin
  	Parent := Self;
    AnchorSideTop.Control := L;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 2;
    Left := 8;
    Width := 254;
    MaxValue := 10000;
  end;
  FChk := TCheckBox.Create(Self);
  with FChk do
  begin
    Parent := Self;
    AnchorSideTop.Control := FEdit;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 8;
    Left := 8;
    Caption := rsAdjustByCompWidth;
    OnChange := @ChkChange;
  end;
  with TButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OkButton.Caption := rsOk;
    CancelButton.Caption := rsCancel;
  end;
  ClientWidth := 270;
  ClientHeight := 124;
end;

function TColumnWidthDlg.ShowForm: Integer;
begin
  FEdit.Value := FColWidth;
  if not FIsForm then
  begin
    FChk.Visible := False;
    ClientHeight := ScaleToScreen(104);
  end;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FColWidth := FEdit.Value;
    FAdjustWidth := FChk.Checked;
  end;
end;

function TColumnWidthDlg.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
end;

////////////////////////////////////////////////////////////////////////////////

function ShowGridForm(aForm: TdxForm): Integer;
begin
  if GrdFm = nil then
    GrdFm := TGrdFm.Create(Application);
  Result := GrdFm.ShowForm(aForm);
end;

function ShowRpGridForm(QGrid: TdxQueryGrid; RD: TReportData): Integer;
begin
  if GrdFm = nil then
    GrdFm := TGrdFm.Create(Application);
  Result := GrdFm.ShowRpForm(QGrid, RD);
end;

procedure CopySortColsToMyGrid(G1: TMyDBGrid; G2: TMyGrid);
var
  i: Integer;
  L1, L2: TSortColumns;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(G2.Columns[L1[i].Col.Index], L1[i].Desc);
end;

function ShowColumnWidthDlg(var AWidth: Integer; out AAdjustWidth: Boolean; AIsForm: Boolean): Boolean;
begin
  with TColumnWidthDlg.CreateNew(nil) do
  try
    IsForm := AIsForm;
    ColWidth := AWidth;
    Result := ShowForm = mrOk;
    if Result then
    begin
      AWidth := ColWidth;
      AAdjustWidth := AdjustWidth;
    end;
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TGrdFm }

procedure TGrdFm.VisibleColsClick(Sender: TObject);
begin
  if ShowGridColsForm(Grid.Columns, FForm) = mrOk then
    FModified := True;
end;

function TGrdFm.IsCellSelected(ACol: Integer): Boolean;
begin
  Result := Grid.IsCellSelected[ACol + Grid.FixedCols, Grid.Row];
end;

procedure TGrdFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const
  IMG_SIZE = 24;
var
  Col: TMyGridColumn;
  al: TAlignment;
  lay: TTextLayout;
  x, y: Integer;
begin
  if Grid.Cells[aCol, aRow] = '~' then
  begin
    Col := Grid.Columns[aCol - Grid.FixedCols];
    al := Col.Alignment;
    //if Col.AutoAlignment then al := taCenter;
    lay := Col.Layout;
    //if Col.AutoLayout then lay := tlCenter;
    case al of
      taLeftJustify: x := aRect.Left + ScaleToScreen(2);
      taCenter: x := aRect.Left + aRect.Width div 2 - IMG_SIZE div 2;
      taRightJustify: x := aRect.Right - IMG_SIZE - ScaleToScreen(2);
    end;
    case lay of
      tlTop: y := aRect.Top + ScaleToScreen(2);
      tlCenter: y := aRect.Top + aRect.Height div 2 - IMG_SIZE div 2;
      tlBottom: y:= aRect.Bottom - IMG_SIZE - ScaleToScreen(2);
    end;
    Grid.Canvas.Draw(x, y, FSampleBmp);
  end;
end;

{function StrToStrF(const S: String): String;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator:=',';
  Result := FormatFloat(',0.00', StrToFloat(S, FS));
end;  }

function StrToStrD(const S: String): String;
begin
  Result := DateToStr(Str2Date(S));
end;

procedure TGrdFm.FillTestText;

  procedure FillColumn(n: Integer; arr: array of String);
  var
    i: Integer;
  begin
    for i := 0 to High(arr) do
      Grid.Cells[n + Grid.FixedCols, i+1] := arr[i];
  end;

var
  i: Integer;
begin
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    case Grid.Columns[i].DataType of
      cdtNumber: FillColumn(i, ['345670', '2198', '795038', '905172']);
      cdtDate: FillColumn(i, [StrToStrD('1983-04-25'), StrToStrD('2006-10-17'),
        StrToStrD('2020-03-21'), StrToStrD('1999-11-22')]);
      cdtTime: FillColumn(i, ['18:23:09', '23:54:19', '08:46:29', '00:37:09']);
      cdtBool: FillColumn(i, ['1', '0', '1', '0']);
      cdtImage: FillColumn(i, ['~', '~', '~', '~'])
      else
        FillColumn(i, [rsPangram1, rsPangram2, rsPangram3, rsPangram4]);
    end;
  end;
end;

procedure TGrdFm.GridCanSort(Sender: TObject; Index: Integer;
  var Cancel: Boolean);
begin
  FModified := True;
end;

procedure TGrdFm.GridGetCellText(Sender: TObject; aCol, aRow: Integer;
  var aText: String);
var
  Cmp: TComponent;
  Col: TGridColumn;
begin
  if FForm = nil then Exit;

  if (aRow = 0) and (aCol > 0) then
  begin
    Col := Grid.Columns[aCol-1];
    Cmp := FindById(FForm, Col.Tag);
    if GetFieldName(Cmp) <> Col.Title.Caption then
      aText := Format('%s (%s)', [Col.Title.Caption, GetFieldName(Cmp)]);
  end;
end;

procedure TGrdFm.GridHeaderSizing(sender: TObject; const IsColumn: boolean;
  const aIndex, aSize: Integer);
var
  R: TGridRect;
  i: LongInt;
begin
  FModified := True;
  R := Grid.Selection;
  if not IsColumn or not (aIndex in [R.Left..R.Right]) then Exit;
  for i := R.Left to R.Right do
    if Grid.Columns[i - Grid.FixedCols].Visible then
      Grid.ColWidths[i] := aSize;
end;

procedure TGrdFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TGrdFm.FormDestroy(Sender: TObject);
begin
  FSampleBmp.Free;
end;

procedure TGrdFm.GridLineStyleChange(Sender: TObject);
begin
  case GridLineStyle.ItemIndex of
    0: Grid.GridLineStyle:=psSolid;
    1: Grid.GridLineStyle:=psDash;
    2: Grid.GridLineStyle:=psDot;
  end;
  FModified := True;
end;

procedure TGrdFm.GridOptionsClickCheck(Sender: TObject);
var
  Opt: TGridOptions;
begin
  Opt := Grid.Options;
  with GridOptions do
  begin
    if FForm <> nil then
    begin
      if (ItemIndex = EDIT_OPT_IDX) and Checked[EDIT_OPT_IDX] then
        Checked[0] := False;
    end;

    if (ItemIndex = 0) and Checked[0] then
    begin
      Checked[1] := False;
      if FForm <> nil then Checked[EDIT_OPT_IDX] := False;
    end;
    if (ItemIndex = 1) and Checked[1] then
      Checked[0] := False;
    FRowSelect := Checked[0];
    if Checked[1] then Include(Opt, goRowHighlight)
    else Exclude(Opt, goRowHighlight);
    if Checked[2] then Include(Opt, goCellEllipsis)
    else Exclude(Opt, goCellEllipsis);
    if Checked[3] then Include(Opt, goTruncCellHints)
    else Exclude(Opt, goTruncCellHints);
    Grid.WordWrap := Checked[4];
    if Checked[5] then Include(Opt, goThumbTracking)
    else Exclude(Opt, goThumbTracking);
    FColMove := Checked[6];
    FAllowChangeSort := Checked[7];
    Grid.Indicator := Checked[8];
    FShowRowDeleteButton := Checked[9];
    if FForm <> nil then FReadOnly := not Checked[EDIT_OPT_IDX];
  end;
  Grid.Options := Opt;
  FModified := True;
end;

procedure TGrdFm.GridTitleFontSelect(Sender: TObject);
begin
  //if FStopChangeEvents then Exit;
  with GridTitleFont do
    if ItemIndex >= 0 then
    begin
      Grid.TitleFont.Name := Text;
      Hint := Text;
      FModified := True;
    end;
end;

procedure TGrdFm.GridTitleFontSizeChange(Sender: TObject);
begin
  if FStopChangeEvents then Exit;
  Grid.TitleFont.Size := GridTitleFontSize.Value;
  FModified := True;
end;

procedure TGrdFm.LinesChange(Sender: TObject);
var
  Chk: TCheckBox;
  b: Boolean;
begin
  Chk := TCheckBox(Sender);
  b := Chk.Checked;
  case Chk.Tag of
    0:
      if b then Grid.Options := Grid.Options + [goVertLine]
      else Grid.Options := Grid.Options - [goVertLine];
    1:
      if b then Grid.Options := Grid.Options + [goHorzLine]
      else Grid.Options := Grid.Options - [goHorzLine];
  end;
  FModified := True;
end;

procedure TGrdFm.MenuItem10Click(Sender: TObject);
var
  W, i: Integer;
  Col: TGridColumn;
  AW: Boolean;
  C: TControl;
begin
  Col := Grid.SelectedColumn;
  if Col <> nil then W := Col.Width;
  if ShowColumnWidthDlg(W, AW, FForm <> nil) then
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
      begin
        Col := Grid.Columns[i];
        if AW then
        begin
          C := TControl(FindById(FForm, Col.Tag));
          if C is TdxDBImage then
            with TdxDBImage(C) do
            begin
              if ThumbSize > 0 then
                W := ThumbSize + ScaleToScreen(20);
            end
          else if C is TdxCheckBox then
            W := ScaleToScreen(60)
          else
            W := C.Width;
        end;
        Col.Width := W;
      end;
    end;
end;

procedure TGrdFm.SetCellAlignment(IsHeader: Boolean; Al: TAlignment; AA: Boolean
  );
var
  i: Integer;
begin
  if IsHeader then
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
        Grid.Columns[i].Title.Alignment := Al;
    end
  else
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
        with Grid.Columns[i] do
        begin
          Alignment := Al;
          AutoAlignment:=AA;
        end;
    end;
end;

procedure TGrdFm.SetCellLayout(IsHeader: Boolean; TL: TTextLayout; AL: Boolean);
var
  i: Integer;
begin
  if IsHeader then
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
        Grid.Columns[i].Title.Layout := TL;
    end
  else
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
        with Grid.Columns[i] do
        begin
          Layout := TL;
          AutoLayout := AL;
        end;
    end;
end;

procedure TGrdFm.MenuItem15Click(Sender: TObject);
begin
  SetCellAlignment(ColumnMnu.Tag = 1, taLeftJustify, False);
end;

procedure TGrdFm.MenuItem16Click(Sender: TObject);
begin
  SetCellAlignment(ColumnMnu.Tag = 1, taCenter, False);
end;

procedure TGrdFm.MenuItem17Click(Sender: TObject);
begin
  SetCellAlignment(ColumnMnu.Tag = 1, taRightJustify, False);
end;

procedure TGrdFm.MenuItem18Click(Sender: TObject);
begin
  SetCellLayout(ColumnMnu.Tag = 1, tlTop, False);
end;

procedure TGrdFm.MenuItem19Click(Sender: TObject);
begin
  SetCellLayout(ColumnMnu.Tag = 1, tlCenter, False);
end;

procedure TGrdFm.MenuItem20Click(Sender: TObject);
begin
  SetCellLayout(ColumnMnu.Tag = 1, tlBottom, False);
end;

procedure TGrdFm.MenuItem26Click(Sender: TObject);
var
  Col: TGridColumn;
begin
  Col := Grid.SelectedColumn;
  with TColumnTitleDlg.CreateNew(nil) do
  try
    if ShowForm(Col, FForm) = mrOk then FModified := True;
  finally
    Free;
  end;
end;

procedure TGrdFm.MenuItem29Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    if IsCellSelected(i) then
      Grid.Columns[i].Visible := False;
  end;
  Grid.Selection := Rect(-1, -1, -1, -1);
  FModified := True;
end;

procedure TGrdFm.ColorChange(Sender: TObject);
var
  ColorBn: TColorButtonEx;
  C: TColor;
  //R, G, B: Byte;
  //S: String;
begin
  if FStopChangeEvents then Exit;

  ColorBn := TColorButtonEx(Sender);
  C := ColorBn.ButtonColor;
  case ColorBn.Tag of
    0: Grid.GridLineColor:=C;
    1:
      begin
        if Grid.Color = Grid.AlternateColor then
          AltCellColor.ButtonColor:=C;
        Grid.Color:=C;
      end;
    2: Grid.SelectedColor:=C;
    3: Grid.SelectedTextColor:=C;
    4: Grid.AlternateColor:=C;
    5: Grid.InactiveSelectedColor:=C;
    6: Grid.InactiveSelectedTextColor:=C;
    7: Grid.Font.Color := C;
    8: Grid.FixedColor:=C;
    9:
      Grid.TitleFont.Color := C;
  end;
  {RedGreenBlue(C, R, G, B);
  if ColorToIdent(C, S) then
    S := Format('%s (rgb: %d, %d, %d)', [Copy(S, 3, 255), R, G, B])
  else
    S := Format('rgb: %d, %d, %d', [R, G, B]);
  ColorBn.Hint := S;     }
  FModified:=True;
end;

procedure TGrdFm.ColorBnMnuPopup(Sender: TObject);
begin
  FPopupColorBn := TColorButtonEx(ColorBnMnu.PopupComponent);
end;

procedure TGrdFm.CellFontSelect(Sender: TObject);
begin
  //if FStopChangeEvents then Exit;

  with CellFont do
  begin
    if ItemIndex >= 0 then
    begin
      Grid.Font.Name := Text;
      Hint := Text;
      FModified := True;
    end;
  end;
end;

procedure TGrdFm.CellFontSizeChange(Sender: TObject);
begin
  if FStopChangeEvents then Exit;
  Grid.Font.Size := CellFontSize.Value;
  FModified := True;
end;

procedure TGrdFm.CellFontStyleChange(Sender: TObject);
var
  FS: TFontStyles;
begin
  if FStopChangeEvents then Exit;

  FS := [];
  if CellFontB.Checked then Include(FS, fsBold);
  if CellFontI.Checked then Include(FS, fsItalic);
  if CellFontU.Checked then Include(FS, fsUnderline);
  if CellFontS.Checked then Include(FS, fsStrikeOut);
  Grid.Font.Style := FS;
  FModified := True;
end;

function TGrdFm.GetCellAlignment(IsHeader: Boolean): Integer;
var
  i: Integer;
  AlSet: set of TAlignment;
  b: Boolean;
begin
  AlSet := [taLeftJustify, taCenter, taRightJustify];
  if IsHeader then
  begin
    b := False;
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
    	  AlSet := AlSet * [Grid.Columns[i].Title.Alignment];
    end
  end
  else
  begin
    b := True;
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
      begin
  	    AlSet := AlSet * [Grid.Columns[i].Alignment];
        b := b and Grid.Columns[i].AutoAlignment;
        if b then AlSet := [];
      end;
    end;
  end;
  if b then Result := 3
  else if AlSet = [] then Result := -1
  else if taLeftJustify in AlSet then Result := 0
  else if taCenter in AlSet then Result := 1
  else if taRightJustify in AlSet then Result := 2
end;

function TGrdFm.GetCellLayout(IsHeader: Boolean): Integer;
var
  i: Integer;
  TlSet: set of TTextLayout;
  b: Boolean;
begin
  TlSet := [tlTop, tlCenter, tlBottom];
  if IsHeader then
  begin
    b := False;
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
    	  TlSet := TlSet * [Grid.Columns[i].Title.Layout];
    end
  end
  else
  begin
    b := True;
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if IsCellSelected(i) then
      begin
  	    TlSet := TlSet * [Grid.Columns[i].Layout];
        b := b and Grid.Columns[i].AutoLayout;
        if b then TlSet := [];
      end;
    end;
  end;
  if b then Result := 3
  else if TlSet = [] then Result := -1
  else if tlTop in TlSet then Result := 0
  else if tlCenter in TlSet then Result := 1
  else if tlBottom in TlSet then Result := 2;
end;

procedure TGrdFm.ColumnMnuPopup(Sender: TObject);
var
  P: TPoint;
  b, IsHeader, bb: Boolean;
  n: Integer;
begin
  P := ColumnMnu.PopupPoint;
  P := Grid.ScreenToClient(P);
  IsHeader := P.y <= Grid.DefaultRowHeight;
  if IsHeader then
    ColumnMnu.Tag := 1
  else
    ColumnMnu.Tag := 0;

  b := (Grid.Columns.Count > 0) and Grid.SelectedColumn.Visible;
  bb := FForm <> nil;

  MenuItem8.Enabled := b;
  MenuItem9.Enabled := b;
  MenuItem26.Visible := bb;
  MenuItem26.Enabled := b and (Grid.Selection.Width = 0);
  MenuItem27.Visible := bb;
  MenuItem29.Enabled := b;
  MenuItem3.Enabled := b;
  MenuItem4.Enabled := b;
  MenuItem10.Enabled := b;

  n := GetCellAlignment(IsHeader);
  MenuItem5.Visible := not IsHeader;
  MenuItem5.Checked := n = 3;
  MenuItem15.Checked := n = 0;
  MenuItem16.Checked := n = 1;
  MenuItem17.Checked := n = 2;
  n := GetCellLayout(IsHeader);
  MenuItem7.Visible := not IsHeader;
  MenuItem7.Checked := n = 3;
  MenuItem18.Checked := n = 0;
  MenuItem19.Checked := n = 1;
  MenuItem20.Checked := n = 2;

  if IsHeader then
  begin
    MenuItem8.Caption := rsHeaderColor;
    MenuItem9.Caption := rsHeaderFont;
  end
  else
  begin
    MenuItem8.Caption := rsColumnColor;
    MenuItem9.Caption := rsColumnFont;
  end;
end;

procedure TGrdFm.DefRowHeightChange(Sender: TObject);
begin
  Grid.DefaultRowHeight := DefRowHeight.Value;
  FModified := True;
end;

procedure TGrdFm.FlatStyleChange(Sender: TObject);
begin
  Grid.Flat:=FlatStyle.Checked;
  FModified := True;
end;

procedure TGrdFm.MenuItem2Click(Sender: TObject);
var
  i: Integer;
  C: TMyGridColumn;
begin
  if Confirm(rsWarning, rsResetSettingsMsg) = mrNo then Exit;

  FStopChangeEvents := True;
  // Сетка
  VertLines.Checked := True;
  HorzLines.Checked := True;
  GridColor.ButtonColor := clSilver;
  with GridLineStyle do
    ItemIndex := 0;
  DefRowHeight.Value := 20;
  // Область данных
  CellColor.ButtonColor := clWindow;
  AltCellColor.ButtonColor := clWindow;
  SelColor.ButtonColor := clHighlight;
  InSelColor.ButtonColor := clSilver;
  SelTextColor.ButtonColor := clHighlightText;
  InSelTextColor.ButtonColor := clBlack;
  CellFont.Text := 'Verdana';
  CellFontB.Checked := False;
  CellFontI.Checked := False;
  CellFontU.Checked := False;
  CellFontS.Checked := False;
  CellFontSize.Value := 10;
  CellFontColor.ButtonColor := clDefault;
  // Заголовки
  GridTitleColor.ButtonColor := clBtnFace;
  FlatStyle.Checked := False;
  GridTitleFont.Text := 'Verdana';
  GridTitleFontB.Checked := False;
  GridTitleFontI.Checked := False;
  GridTitleFontU.Checked := False;
  GridTitleFontS.Checked := False;
  GridTitleFontSize.Value := 10;
  GridTitleFontColor.ButtonColor := clDefault;
  // Опции
  with GridOptions do
  begin
    Checked[0] := False;
    Checked[1] := False;
    Checked[2] := False;
    Checked[3] := True;
    Checked[4] := False;
    Checked[5] := True;
    Checked[6] := False;
    Checked[7] := True;
    Checked[8] := True;
    Checked[9] := False;
    if FForm <> nil then Checked[EDIT_OPT_IDX] := False;
  end;
  FStopChangeEvents := False;

	Grid.GridLineColor:=clSilver;
  Grid.GridLineStyle := psSolid;
  Grid.DefaultRowHeight := 20;
  Grid.Color := clWindow;
  Grid.AlternateColor := clWindow;
  Grid.SelectedColor := clHighlight;
  Grid.SelectedTextColor := clHighlightText;
  Grid.InactiveSelectedColor := clSilver;
  Grid.InactiveSelectedTextColor := clBlack;
	SetDefaultFont(Grid.Font);
  Grid.FixedColor := clBtnFace;
  Grid.TitleFont := Grid.Font;
  Grid.Flat := False;
  Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
  	goHeaderHotTracking, goHeaderPushedLook, goDrawFocusSelected, goRangeSelect,
    goSelectionActive, goColMoving, goColSizing, goThumbTracking, goTruncCellHints];
  FRowSelect := False;
  FReadOnly := True;
  FColMove := False;
  FAllowChangeSort := True;
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    C := Grid.Columns[i];
    C.FillDefaultFont;
    //C.Font := Grid.Font;
    C.Color := Grid.Color;
    C.Title.FillTitleDefaultFont;
    //C.Title.Font := Grid.TitleFont;
    C.Title.Color := Grid.FixedColor;
    C.Title.Alignment := taLeftJustify;
    C.Title.Layout := tlCenter;
    C.AutoAlignment := True;
    C.AutoLayout := True;
  end;
  FModified := True;
end;

procedure TGrdFm.MenuItem30Click(Sender: TObject);
var
  C: TColor;
begin
  case FPopupColorBn.Tag of
    0: C := clSilver;
    1: C := clWindow;
    2: C := clHighlight;
    3: C := clHighlightText;
    4: C := Grid.Color;
    5: C := clSilver;
    6: C := clBlack;
    7: C := clDefault;
    8: C := clBtnFace;
    9: C := Grid.Font.Color;
  end;

  FPopupColorBn.ButtonColor:=C;

  case FPopupColorBn.Tag of
    0: Grid.GridLineColor:=C;
    1:
      begin
        if Grid.Color = Grid.AlternateColor then
          AltCellColor.ButtonColor:=C;
        Grid.Color:=C;
      end;
    2: Grid.SelectedColor:=C;
    3: Grid.SelectedTextColor:=C;
    4: Grid.AlternateColor:=C;
    5: Grid.InactiveSelectedColor:=C;
    6: Grid.InactiveSelectedTextColor:=C;
    7: Grid.Font.Color:=C;
    8: Grid.FixedColor:=C;
    9: Grid.TitleFont.Color:=C;
  end;

  FModified := True;
end;

procedure TGrdFm.FormCreate(Sender: TObject);
begin
  Caption := rsTable;

  GridGrp.Caption := rsGrid;
  VertLines.Caption := rsVert;
  HorzLines.Caption := rsHorz;
  DefRowHeight.Hint := rsRowHeight;
  GridLineStyle.Hint := rsLineStyle;
  GridLineStyle.Items.AddStrings([rsSolid, rsDash, rsDot]);

  DataGrp.Caption := rsDataArea;
  Label1.Caption := rsCellBkgnd;
  Label8.Caption := rsAltCellBkgnd;
  Label2.Caption := rsSelectedColor;
  Label6.Caption := rsSelectedText;
  Label4.Caption := rsInactiveSelected;
  Label7.Caption := rsInactiveSelectedText;
  CellFontB.Caption := rsFontBold;
  CellFontI.Caption := rsFontItalic;
  CellFontU.Caption := rsFontUnderline;
  CellFontS.Caption := rsFontStrikeOut;
  CellFontSize.Hint := rsFontSize;

  TitlesGrp.Caption := rsHeaders;
  FlatStyle.Caption := rsFlat;
  VisibleCols.Caption := rsColumns;
  VisibleCols.Hint := rsOrderVisibleColumns;
  GridTitleFontB.Caption := rsFontBold;
  GridTitleFontI.Caption := rsFontItalic;
  GridTitleFontU.Caption := rsFontUnderline;
  GridTitleFontS.Caption := rsFontStrikeOut;
  GridTitleFontSize.Hint := rsFontSize;

  OptionsGrp.Caption := rsOptions;
  {with GridOptions do
  begin
    Clear;
    Items.AddStrings([rsRowSelect, rsHighlightCurLine, rsEllipsisInCell, rsShowHints,
      rsWordWrap, rsMoveWhenScroll, rsAllowColumnsMove, rsAllowChangeSort,
      rsIndicator, rsEditing, rsShowRowDeleteButton]);
  end;}

  MenuItem30.Caption := rsReset;
  MenuItem8.Caption := rsColor;
  MenuItem9.Caption := rsFont;
  MenuItem26.Caption := rsTitle;
  MenuItem10.Caption := rsWidth;
  MenuItem29.Caption := rsHideColumn;
  MenuItem2.Caption := rsResetSettings;
  MenuItem3.Caption := rsAlignment;
  MenuItem5.Caption := rsAuto;
  MenuItem15.Caption := rsLeftJustify;
  MenuItem16.Caption := rsCenter;
  MenuItem17.Caption := rsRightJustify;
  MenuItem4.Caption := rsLayout;
  MenuItem5.Caption := rsAuto;
  MenuItem18.Caption := rsLayoutTop;
  MenuItem19.Caption := rsCenter;
  MenuItem20.Caption := rsLayoutBottom;

  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.OkButton.Default := False;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  Grid := TMyGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
  Grid.OnHeaderSizing:=@GridHeaderSizing;
  Grid.OnGetCellText:=@GridGetCellText;
  Grid.OnCanSort:=@GridCanSort;
  Grid.OnDrawCell:=@GridDrawCell;

  CellFont.Items := Screen.Fonts;
  GridTitleFont.Items := Screen.Fonts;

  ResetCellFontBn.Hint := rsResetFontToDefault;
  ResetCellFontBn.LoadGlyphFromLazarusResource('delete16');
  ResetTitleFontBn.Hint := rsResetFontToDefault;
  ResetTitleFontBn.LoadGlyphFromLazarusResource('delete16');

  FSampleBmp := LoadBitmapFromLazarusResource('smile24');
end;

procedure TGrdFm.FormShow(Sender: TObject);
begin
  with Grid do
  begin
    SetFocus;
    Selection := Rect(-1, -1, -1, -1);
  end;
end;

procedure TGrdFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('formgrid');
end;

procedure TGrdFm.MenuItem5Click(Sender: TObject);
begin
  SetCellAlignment(False, taLeftJustify, True);
end;

procedure TGrdFm.MenuItem7Click(Sender: TObject);
begin
  SetCellLayout(False, tlCenter, True);
end;

procedure TGrdFm.MenuItem8Click(Sender: TObject);
var
  IsHeader: Boolean;
  Clr: TColor;
  i: Integer;
begin
  IsHeader := ColumnMnu.Tag = 1;
  if IsHeader then
  begin
    Clr := Grid.SelectedColumn.Title.Color;
    if ShowSelectColor(Clr, Grid.FixedColor) = mrOk then
      for i := 0 to Grid.Columns.Count - 1 do
      begin
        if IsCellSelected(i) then
      	  Grid.Columns[i].Title.Color := Clr;
      end;
  end
  else
  begin
    Clr := Grid.SelectedColumn.Color;
    if ShowSelectColor(Clr, Grid.Color) = mrOk then
      for i := 0 to Grid.Columns.Count - 1 do
      begin
        if IsCellSelected(i) then
          Grid.Columns[i].Color := Clr;
      end;
  end;
  FModified := True;
end;

procedure TGrdFm.MenuItem9Click(Sender: TObject);
var
  Fnt: TFont;
  i: Integer;
  IsHeader: Boolean;
begin
  IsHeader := ColumnMnu.Tag = 1;
  if IsHeader then
  begin
    Fnt := Grid.SelectedColumn.Title.Font;
    if ShowFontForm(Fnt, Grid.TitleFont) = mrOk then
      for i := 0 to Grid.Columns.Count - 1 do
      begin
        if IsCellSelected(i) then
        begin
          if Fnt.IsEqual(Grid.TitleFont) then
            Grid.Columns[i].Title.FillTitleDefaultFont
          else
      	    Grid.Columns[i].Title.Font := Fnt;
        end;
      end;
  end
  else
  begin
    Fnt := Grid.SelectedColumn.Font;
    if ShowFontForm(Fnt, Grid.Font) = mrOk then
      for i := 0 to Grid.Columns.Count - 1 do
      begin
        if IsCellSelected(i) then
        begin
          if Fnt.IsEqual(Grid.Font) then
            Grid.Columns[i].FillDefaultFont
          else
        	  Grid.Columns[i].Font := Fnt;
        end;
      end;
  end;
  FModified := True;
end;

procedure TGrdFm.ResetCellFontBnClick(Sender: TObject);
var
 DefFont: TFont;
begin
  if Confirm(rsWarning, rsResetFontMsg) = mrNo then Exit;

  DefFont := TFont.Create;
  DefFont.Name := 'Verdana';
  DefFont.Size := 10;
  DefFont.Color := clDefault;

  Grid.Font := DefFont;

  FStopChangeEvents := True;
  CellFont.Text := Grid.Font.Name;
  CellFont.Hint := Grid.Font.Name;
  CellFontB.Checked := False;
  CellFontI.Checked := False;
  CellFontU.Checked := False;
  CellFontS.Checked := False;
  CellFontSize.Value := Grid.Font.Size;
  CellFontColor.ButtonColor := Grid.Font.Color;
  FStopChangeEvents := False;

  FModified := True;

  DefFont.Free;
end;

procedure TGrdFm.ResetTitleFontBnClick(Sender: TObject);
begin
  if Confirm(rsWarning, rsResetFontMsg) = mrNo then Exit;

  Grid.TitleFont := Grid.Font;
  FStopChangeEvents := True;
  with Grid.TitleFont do
  begin
    GridTitleFont.Text := Name;
    GridTitleFont.Hint := Name;
    GridTitleFontB.Checked := fsBold in Style;
    GridTitleFontI.Checked := fsItalic in Style;
    GridTitleFontU.Checked := fsUnderline in Style;
    GridTitleFontS.Checked := fsStrikeOut in Style;
    GridTitleFontSize.Value := Size;
    GridTitleFontColor.ButtonColor := Color;
  end;
  FStopChangeEvents := False;
  FModified := True;
end;

procedure TGrdFm.TitleFontStyleChange(Sender: TObject);
var
  FS: TFontStyles;
begin
  if FStopChangeEvents then Exit;

  FS := [];
  if GridTitleFontB.Checked then Include(FS, fsBold);
  if GridTitleFontI.Checked then Include(FS, fsItalic);
  if GridTitleFontU.Checked then Include(FS, fsUnderline);
  if GridTitleFontS.Checked then Include(FS, fsStrikeOut);
  Grid.TitleFont.Style := FS;
  FModified := True;
end;

procedure CopySortColsToDxGrid(G1: TMyGrid; G2: TdxGrid);
var
  i: Integer;
  L1, L2: TSortColumns;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(G2.Columns[L1[i].Col.Index], L1[i].Desc);
end;

function TGrdFm.ShowForm(aForm: TdxForm): Integer;

  function ComponentToDataType(C: TComponent): TMyGridColumnDataType;
  begin
    if C is TdxObjectField then
      C := LookupObjectField(TdxObjectField(C), True);
    if (C is TdxCalcEdit) or (C is TdxCounter) or (C is TdxRecordId) then
      Result := cdtNumber
    else if C is TdxDateEdit then
      Result := cdtDate
    else if C is TdxTimeEdit then
      Result := cdtTime
    else if C is TdxCheckBox then
      Result := cdtBool
    else if C is TdxDBImage then
      Result := cdtImage
    else
      Result := cdtText;
  end;

var
  i: Integer;
  C: TMyGridColumn;
  C2: TMyDBGridColumn;
  Cmp: TComponent;
  FGrid: TdxGrid;
begin
  MenuItem26.Visible := True;
  MenuItem27.Visible := True;
  with GridOptions do
  begin
    Clear;
    Items.AddStrings([rsRowSelect, rsHighlightCurLine, rsEllipsisInCell, rsShowHints,
      rsWordWrap, rsMoveWhenScroll, rsAllowColumnsMove, rsAllowChangeSort,
      rsIndicator, rsShowRowDeleteButton, rsEditing]);
  end;
  //if GridOptions.Count = EDIT_OPT_IDX then GridOptions.Items.AddStrings([rsEditing, rsShowRowDeleteButton]);

  Caption := rsTable + ': ' + aForm.FormCaption;
  FForm := aForm;
  FGrid := aForm.Grid;

  FStopChangeEvents := True;
  // Сетка
  VertLines.Checked := dgColLines in FGrid.Options;
  HorzLines.Checked := dgRowLines in FGrid.Options;
  GridColor.ButtonColor := FGrid.GridLineColor;
  with GridLineStyle do
    case FGrid.GridLineStyle of
      psSolid: ItemIndex := 0;
      psDash: ItemIndex := 1;
      psDot: ItemIndex := 2;
    end;
  DefRowHeight.Value := FGrid.DefaultRowHeight;
  // Область данных
  CellColor.ButtonColor := FGrid.Color;
  AltCellColor.ButtonColor := FGrid.AlternateColor;
  SelColor.ButtonColor := FGrid.SelectedColor;
  InSelColor.ButtonColor := FGrid.InactiveSelectedColor;
  SelTextColor.ButtonColor := FGrid.SelectedTextColor;
  InSelTextColor.ButtonColor := FGrid.InactiveSelectedTextColor;
  CellFont.Text := FGrid.Font.Name;
  CellFont.Hint := FGrid.Font.Name;
  CellFontB.Checked := fsBold in FGrid.Font.Style;
  CellFontI.Checked := fsItalic in FGrid.Font.Style;
  CellFontU.Checked := fsUnderline in FGrid.Font.Style;
  CellFontS.Checked := fsStrikeOut in FGrid.Font.Style;
  CellFontSize.Value := FGrid.Font.Size;
  CellFontColor.ButtonColor := FGrid.Font.Color;
  // Заголовки
  GridTitleColor.ButtonColor := FGrid.FixedColor;
  FlatStyle.Checked := FGrid.Flat;
  GridTitleFont.Text := FGrid.TitleFont.Name;
  GridTitleFont.Hint := FGrid.TitleFont.Name;
  GridTitleFontB.Checked := fsBold in FGrid.TitleFont.Style;
  GridTitleFontI.Checked := fsItalic in FGrid.TitleFont.Style;
  GridTitleFontU.Checked := fsUnderline in FGrid.TitleFont.Style;
  GridTitleFontS.Checked := fsStrikeOut in FGrid.TitleFont.Style;
  GridTitleFontSize.Value := FGrid.TitleFont.Size;
  GridTitleFontColor.ButtonColor := FGrid.TitleFont.Color;
  // Опции
  with GridOptions do
  begin
    Checked[0] := dgRowSelect in FGrid.Options;
    Checked[1] := dgRowHighlight in FGrid.Options;
    Checked[2] := dgCellEllipsis in FGrid.Options;;
    Checked[3] := dgTruncCellHints in FGrid.Options;
    Checked[4] := FGrid.WordWrap;
    Checked[5] := dgThumbTracking in FGrid.Options;
    Checked[6] := dgColumnMove in FGrid.Options;
    Checked[7] := FGrid.AllowChangeSort;
    Checked[8] := dgIndicator in FGrid.Options;
    Checked[9] := FGrid.ShowRowDeleteButton;
    Checked[EDIT_OPT_IDX] := not FGrid.ReadOnly;
  end;
  FStopChangeEvents := False;

  if aForm.PId > 0 then
    Grid.BorderLinePos := FindGridById(FormMan.FindForm(aForm.PId), aForm.Id).ClientWidth
  else
    Grid.BorderLinePos := 0;

  Grid.GridLineColor:=FGrid.GridLineColor;
  Grid.GridLineStyle := FGrid.GridLineStyle;
  Grid.DefaultRowHeight:=FGrid.DefaultRowHeight;
  Grid.Color:=FGrid.Color;
  Grid.AlternateColor:=FGrid.AlternateColor;
  Grid.SelectedColor:=FGrid.SelectedColor;
  Grid.SelectedTextColor:=FGrid.SelectedTextColor;
  Grid.InactiveSelectedColor := FGrid.InactiveSelectedColor;
  Grid.InactiveSelectedTextColor := FGrid.InactiveSelectedTextColor;
  Grid.FixedColor:=FGrid.FixedColor;
  //Grid.TitleFont.SetDefault; // Сбрасываем флаг FTitleFontIsDefault
  Grid.Font := FGrid.Font;
  Grid.TitleFont := FGrid.TitleFont;
  Grid.Flat:=FGrid.Flat;
  Grid.PopupMenu := ColumnMnu;

  FReadOnly := FGrid.ReadOnly;
  FRowSelect := dgRowSelect in FGrid.Options;
  if dgRowHighlight in FGrid.Options then Grid.Options := Grid.Options + [goRowHighlight]
  else Grid.Options := Grid.Options - [goRowHighlight];
  if dgCellEllipsis in FGrid.Options then Grid.Options := Grid.Options + [goCellEllipsis]
  else Grid.Options := Grid.Options - [goCellEllipsis];
  if dgTruncCellHints in FGrid.Options then Grid.Options := Grid.Options + [goTruncCellHints]
  else Grid.Options := Grid.Options - [goTruncCellHints];
  if dgThumbTracking in FGrid.Options then Grid.Options := Grid.Options + [goThumbTracking]
  else Grid.Options := Grid.Options - [goThumbTracking];
  FColMove := dgColumnMove in FGrid.Options;
  Grid.Indicator := dgIndicator in FGrid.Options;

  if dgRowLines in FGrid.Options then Grid.Options := Grid.Options + [goHorzLine]
  else Grid.Options := Grid.Options - [goHorzLine];
  if dgColLines in FGrid.Options then Grid.Options := Grid.Options + [goVertLine]
  else Grid.Options := Grid.Options - [goVertLine];
  Grid.Columns.Clear;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    C := Grid.Columns.Add;
    C2 := FGrid.Columns[i];
    Cmp := FindById(FForm, C2.Tag);
    C.DataType:=ComponentToDataType(Cmp);
    C.Visible:=C2.Visible;
    C.Font := C2.Font;
    C.Color := C2.Color;
    C.Alignment := C2.Alignment;
    C.Layout := C2.Layout;
    C.AutoAlignment := C2.AutoAlignment;
    C.AutoLayout := C2.AutoLayout;
    C.Title.Font := C2.Title.Font;
    C.Title.Color := C2.Title.Color;
    C.Title.Alignment := C2.Title.Alignment;
    C.Title.Layout := C2.Title.Layout;
    if C2.Title.Caption = ' ' then
      C.Title.Caption := GetFieldName(Cmp)
    else
      C.Title.Caption := C2.Title.Caption;
    C.Width:=C2.Width;
    C.Index:=C2.Index;
    C.Tag := C2.Tag;
    if Cmp is TdxCheckBox then
      C.ButtonStyle:=cbsCheckboxColumn;
  end;
  CopySortColsToMyGrid(FGrid, Grid);
  Grid.WordWrap:=FGrid.WordWrap;
  FAllowChangeSort:=FGrid.AllowChangeSort;
  FShowRowDeleteButton:=FGrid.ShowRowDeleteButton;

  FillTestText;
  FModified := False;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  FGrid.GridLineColor:=Grid.GridLineColor;
  FGrid.GridLineStyle := Grid.GridLineStyle;
  FGrid.DefaultRowHeight:=Grid.DefaultRowHeight;
  FGrid.Color:=Grid.Color;
  FGrid.AlternateColor:=Grid.AlternateColor;
  FGrid.SelectedColor:=Grid.SelectedColor;
  FGrid.SelectedTextColor:=Grid.SelectedTextColor;
  FGrid.InactiveSelectedColor:=Grid.InactiveSelectedColor;
  FGrid.InactiveSelectedTextColor:=Grid.InactiveSelectedTextColor;
  FGrid.FixedColor:=Grid.FixedColor;
  FGrid.Font := Grid.Font;
  //FGrid.TitleFont.SetDefault;
  FGrid.TitleFont := Grid.TitleFont;
  FGrid.Flat:=Grid.Flat;

  if FRowSelect then FGrid.Options := FGrid.Options + [dgRowSelect]
  else FGrid.Options := FGrid.Options - [dgRowSelect];
  FGrid.ReadOnly := FReadOnly;
  if FGrid.ReadOnly then FGrid.Options := FGrid.Options - [dgEditing]
  else FGrid.Options := FGrid.Options + [dgEditing];
  if goRowHighlight in Grid.Options then FGrid.Options := FGrid.Options + [dgRowHighlight]
  else FGrid.Options := FGrid.Options - [dgRowHighlight];
  if goCellEllipsis in Grid.Options then FGrid.Options := FGrid.Options + [dgCellEllipsis]
  else FGrid.Options := FGrid.Options - [dgCellEllipsis];
  if goTruncCellHints in Grid.Options then FGrid.Options := FGrid.Options + [dgTruncCellHints]
  else FGrid.Options := FGrid.Options - [dgTruncCellHints];
  if goThumbTracking in Grid.Options then FGrid.Options := FGrid.Options + [dgThumbTracking]
  else FGrid.Options := FGrid.Options - [dgThumbTracking];
  if FColMove then FGrid.Options := FGrid.Options + [dgColumnMove]
  else FGrid.Options := FGrid.Options - [dgColumnMove];
  if Grid.Indicator then FGrid.Options := FGrid.Options + [dgIndicator]
  else FGrid.Options := FGrid.Options - [dgIndicator];

  if goHorzLine in Grid.Options then FGrid.Options := FGrid.Options + [dgRowLines]
  else FGrid.Options := FGrid.Options - [dgRowLines];
  if goVertLine in Grid.Options then FGrid.Options := FGrid.Options + [dgColLines]
  else FGrid.Options := FGrid.Options - [dgColLines];
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    C := Grid.Columns[i];
    C2 := TMyDBGridColumn(FindColumnByTag(FGrid, C.Tag));
    C2.Visible:=C.Visible;
    C2.FillDefaultFont;
    C2.Font := C.Font;
    C2.Color := C.Color;
    C2.AutoAlignment := C.AutoAlignment;
    C2.Alignment := C.Alignment;
    C2.AutoLayout := C.AutoLayout;
    C2.Layout := C.Layout;
    C2.Title.FillTitleDefaultFont;
    C2.Title.Font := C.Title.Font;
    C2.Title.Color := C.Title.Color;
    C2.Title.Alignment := C.Title.Alignment;
    C2.Title.Layout := C.Title.Layout;
    Cmp := FindById(FForm, C2.Tag);
    if GetFieldName(Cmp) = C.Title.Caption then
      C2.Title.Caption := ' '
    else
      C2.Title.Caption := C.Title.Caption;
    C2.Width:=C.Width;
    // Ширина колонки по умолчанию равна 64 пиксела. Данное значение не
    // сохраняется в базе. Из-за этого ширина колонки будет случайной.
    if C.Width = C.DefaultWidth then C2.Width := C.Width - 1;
    C2.Index:=C.Index;
  end;
  CopySortColsToDxGrid(Grid, FGrid);
  FGrid.WordWrap:=Grid.WordWrap;
  FGrid.AllowChangeSort:=FAllowChangeSort;
  FGrid.ShowRowDeleteButton:=FShowRowDeleteButton;
end;

procedure CopySortColsToMyGrid(G1: TRpGrid; G2: TMyGrid);
var
  i, n: Integer;
  L2: TSortColumns;
  L1: TRpGridSortList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
  begin
    n := L1[i].Col.Index;
    if (n >= 0) and (n < G2.Columns.Count) then
      L2.AddCol(G2.Columns[n], L1[i].Desc);
  end;
end;

procedure CopySortColsToRpGrid(aCols: TList; G1: TMyGrid; G2: TRpGrid);
var
  i: Integer;
  L1: TSortColumns;
  L2: TRpGridSortList;
begin
  L1 := G1.SortCols;
  L2 := G2.SortCols;
  L2.Clear;
  for i := 0 to L1.Count - 1 do
    L2.AddCol(TRpGridColumn(aCols[L1[i].Col.Tag]), L1[i].Desc);
end;

function TGrdFm.ShowRpForm(QGrid: TdxQueryGrid; RD: TReportData): Integer;

  function RpFieldToDataType(Col: TRpGridColumn): TMyGridColumnDataType;
  var
    Tp: TRpFieldType;
    i: Integer;
  begin
    i := RD.IndexOfNameDS(Col.FieldNameDS);
    Tp := RD.GetFieldType(i);

    case Tp of
      flNumber, flCounter, flRecId: Result := cdtNumber;
      flDate: Result := cdtDate;
      flTime: Result := cdtTime;
      flBool:
        begin
          if RpFieldIsCheckBox(RD, Col.FieldNameDS) then Result := cdtBool
          else Result := cdtNumber;
        end
      else
        Result := cdtText;
    end;
  end;

var
  G: TRpGrid;
  i, x: Integer;
  C: TRpGridColumn;
  Col: TMyGridColumn;
  L: TList;
begin
  MenuItem26.Visible := True;
  MenuItem27.Visible := True;
  with GridOptions do
  begin
    Clear;
    Items.AddStrings([rsRowSelect, rsHighlightCurLine, rsEllipsisInCell, rsShowHints,
      rsWordWrap, rsMoveWhenScroll, rsAllowColumnsMove, rsAllowChangeSort,
      rsIndicator, rsShowRowDeleteButton]);
  end;
  {if GridOptions.Count > EDIT_OPT_IDX then
  begin
    GridOptions.Items.Delete(EDIT_OPT_IDX+1);
    GridOptions.Items.Delete(EDIT_OPT_IDX);
  end;   }

  FForm := nil;

  Caption := rsTable + ': ' + RD.Name;
  Grid.Columns.Clear;
  G := RD.Grid;

  FStopChangeEvents := True;
  // Сетка
  VertLines.Checked := G.VertLines;
  HorzLines.Checked := G.HorzLines;
  GridColor.ButtonColor := G.GridLineColor;
  with GridLineStyle do
    case G.GridLineStyle of
      psSolid: ItemIndex := 0;
      psDash: ItemIndex := 1;
      psDot: ItemIndex := 2;
    end;
  DefRowHeight.Value := G.DefaultRowHeight;
  // Область данных
  CellColor.ButtonColor := G.Color;
  AltCellColor.ButtonColor := G.AlternateColor;
  SelColor.ButtonColor := G.SelectedColor;
  InSelColor.ButtonColor := G.InactiveSelectedColor;
  SelTextColor.ButtonColor := G.SelectedTextColor;
  InSelTextColor.ButtonColor := G.InactiveSelectedTextColor;
  CellFont.Text := G.Font.Name;
  CellFontB.Checked := fsBold in G.Font.Style;
  CellFontI.Checked := fsItalic in G.Font.Style;
  CellFontU.Checked := fsUnderline in G.Font.Style;
  CellFontSize.Value := G.Font.Size;
  CellFontColor.ButtonColor := G.Font.Color;
  // Заголовки
  GridTitleColor.ButtonColor := G.FixedColor;
  FlatStyle.Checked := G.Flat;
  GridTitleFont.Text := G.TitleFont.Name;
  GridTitleFontB.Checked := fsBold in G.TitleFont.Style;
  GridTitleFontI.Checked := fsItalic in G.TitleFont.Style;
  GridTitleFontU.Checked := fsUnderline in G.TitleFont.Style;
  GridTitleFontSize.Value := G.TitleFont.Size;
  GridTitleFontColor.ButtonColor := G.TitleFont.Color;
  // Опции
  with GridOptions do
  begin
    Checked[0] := G.RowSelect;
    Checked[1] := G.RowHighlight;
    Checked[2] := G.CellEllipsis;
    Checked[3] := G.ShowHints;
    Checked[4] := G.WordWrap;
    Checked[5] := G.ThumbTracking;
    Checked[6] := G.ColMove;
    Checked[7] := G.AllowChangeSort;
    Checked[8] := G.Indicator;
    Checked[9] := G.ShowRowDeleteButton;
  end;
  FStopChangeEvents := False;

  if RD.Kind = rkQuery then
    Grid.BorderLinePos := QGrid.ClientWidth
  else
    Grid.BorderLinePos := 0;
  {else
  begin
    x := ScaleToScreen(900);
    if Win32MajorVersion = 10 then Dec(x, 2)
    else Dec(x, GetSystemMetrics(SM_CXSIZEFRAME) * 2);
    Dec(x, GetSystemMetrics(SM_CXVSCROLL));
    if G.Flat then Dec(x, 2)
    else Dec(x, 4);
    Grid.BorderLinePos := x;
  end;}

  Grid.Color:=G.Color;
  Grid.AlternateColor:=G.AlternateColor;
  Grid.SelectedColor:=G.SelectedColor;
  Grid.SelectedTextColor:=G.SelectedTextColor;
  Grid.InactiveSelectedColor:=G.InactiveSelectedColor;
  Grid.InactiveSelectedTextColor:=G.InactiveSelectedTextColor;
  Grid.FixedColor:=G.FixedColor;
  //Grid.FixedHotColor := G.FixedHotColor;
  Grid.GridLineColor:=G.GridLineColor;
  Grid.GridLineStyle:=G.GridLineStyle;
  Grid.Flat:=G.Flat;
  if G.VertLines then Grid.Options:=Grid.Options + [goVertLine]
  else Grid.Options:=Grid.Options - [goVertLine];
  if G.HorzLines then Grid.Options:=Grid.Options + [goHorzLine]
  else Grid.Options:=Grid.Options - [goHorzLine];
  Grid.DefaultRowHeight:=G.DefaultRowHeight;
  //Grid.TitleFont.SetDefault;
  Grid.Font := G.Font;
  Grid.TitleFont := G.TitleFont;
  L := TList.Create;
  G.SortColumns(L);
  FCols := L;
  for i := 0 to L.Count - 1 do
  begin
    C := TRpGridColumn(L[i]);
    Col := Grid.Columns.Add;
    Col.DataType:=RpFieldToDataType(C);
    Col.Color:=C.Color;
    Col.Font := C.Font;
    Col.Title.Color:=C.FixedColor;
    Col.Title.Font := C.TitleFont;
    Col.Title.Caption:=C.Caption;
    Col.Tag := i;
    Col.Width:=C.Width;
    Col.Visible:=C.Visible;
    Col.Alignment:=C.Alignment;
    Col.Layout:=C.Layout;
    Col.AutoAlignment:=C.AutoAlignment;
    Col.AutoLayout:=C.AutoLayout;
    Col.Title.Alignment:=C.TitleAlignment;
    Col.Title.Layout:=C.TitleLayout;
    if Col.DataType = cdtBool then
      Col.ButtonStyle:=cbsCheckboxColumn;
  end;
  CopySortColsToMyGrid(G, Grid);
  Grid.WordWrap:=G.WordWrap;
  FRowSelect := G.RowSelect;
  if G.RowHighlight then Grid.Options := Grid.Options + [goRowHighlight]
  else Grid.Options := Grid.Options - [goRowHighlight];
  if G.CellEllipsis then Grid.Options := Grid.Options + [goCellEllipsis]
  else Grid.Options := Grid.Options - [goCellEllipsis];
  if G.ShowHints then Grid.Options := Grid.Options + [goTruncCellHints]
  else Grid.Options := Grid.Options - [goTruncCellHints];
  if G.ThumbTracking then Grid.Options := Grid.Options + [goThumbTracking]
  else Grid.Options := Grid.Options - [goThumbTracking];
  FColMove := G.ColMove;
  FAllowChangeSort:=G.AllowChangeSort;
  Grid.Indicator := G.Indicator;
  FShowRowDeleteButton := G.ShowRowDeleteButton;
  Grid.PopupMenu := ColumnMnu;

  FillTestText;
  FModified := False;

  Result := ShowModal;
  if Result <> mrOk then Exit;

  G.Color:=Grid.Color;
  G.AlternateColor:=Grid.AlternateColor;
  G.SelectedColor:=Grid.SelectedColor;
  G.SelectedTextColor:=Grid.SelectedTextColor;
  G.InactiveSelectedColor:=Grid.InactiveSelectedColor;
  G.InactiveSelectedTextColor:=Grid.InactiveSelectedTextColor;
  G.FixedColor:=Grid.FixedColor;
  G.GridLineColor:=Grid.GridLineColor;
  G.GridLineStyle:=Grid.GridLineStyle;
  G.Flat:=Grid.Flat;
  G.VertLines := goVertLine in Grid.Options;
  G.HorzLines := goHorzLine in Grid.Options;
  G.DefaultRowHeight:=Grid.DefaultRowHeight;
  G.Font.Assign(Grid.Font);
  G.TitleFont.Assign(Grid.TitleFont);
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    Col := Grid.Columns[i];
    C := TRpGridColumn(L[Col.Tag]);
    C.Color:=Col.Color;
    C.Font.Assign(Col.Font);
    C.FixedColor:=Col.Title.Color;
    C.TitleFont.Assign(Col.Title.Font);
    C.Caption:=Col.Title.Caption;
    C.Index:=Col.Index;
    C.Width := Col.Width;
    // Ширина колонки по умолчанию равна 64 пиксела. Данное значение не
    // сохраняется в базе. Из-за этого ширина колонки будет случайной.
    if Col.Width = Col.DefaultWidth then C.Width := Col.Width - 1;

    C.Visible:=Col.Visible;
    C.Alignment:=Col.Alignment;
    C.Layout:=Col.Layout;
    C.AutoAlignment:=Col.AutoAlignment;
    C.AutoLayout:=Col.AutoLayout;
    C.TitleAlignment:=Col.Title.Alignment;
    C.TitleLayout:=Col.Title.Layout;
  end;
  CopySortColsToRpGrid(L, Grid, G);
  G.WordWrap := Grid.WordWrap;
  G.RowSelect := FRowSelect;
  G.RowHighlight := goRowHighlight in Grid.Options;
  G.CellEllipsis := goCellEllipsis in Grid.Options;
  G.ShowHints := goTruncCellHints in Grid.Options;
  G.ThumbTracking := goThumbTracking in Grid.Options;
  G.ColMove := FColMove;
  G.AllowChangeSort := FAllowChangeSort;
  G.Indicator := Grid.Indicator;
  G.ShowRowDeleteButton := FShowRowDeleteButton;
  L.Free;
end;

end.

