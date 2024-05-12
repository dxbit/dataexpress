unit TimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, dbctrlsex, EditBtn, Controls, Forms, Grids, LclType,
  StdCtrls, Db, Menus, strconsts;

type

  TTimeForm = class;

  { TDBTimeEdit }

  TDBTimeEdit = class(TCustomDBEditButton)
    procedure FormHide(Sender: TObject);
  private
    FForm: TTimeForm;
  protected
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TTimeEditEx }

  TTimeEditEx = class(TCustomEditButton)
  private
    FForm: TTimeForm;
    FOldText: String;
    procedure FormHide(Sender: TObject);
    procedure MenuHandler(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure DoEnter; override;
    procedure ButtonClick; override;
    procedure EditEditingDone; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Undo; override;
    function Validate: Boolean;
    property Button;
  end;

  { TTimeForm }

  TTimeForm = class(TForm)
  private
    FAccept: Boolean;
    FHrs, FMins: TStringGrid;
    FTimeControl: TWinControl;
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetTime: TDateTime;
    procedure SetTime(AValue: TDateTime);
    procedure HideClose;
  protected
    procedure Deactivate; override;
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    procedure ShowForm;
    property TimeControl: TWinControl read FTimeControl write FTimeControl;
    property Time: TDateTime read GetTime write SetTime;
    property Accept: Boolean read FAccept write FAccept;
  end;

implementation

uses
  DateUtils, LazUtf8, apputils, appimagelists;

{ TTimeForm }

procedure TTimeForm.GridDblClick(Sender: TObject);
begin
  FAccept := True;
  HideClose;
end;

procedure TTimeForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_SPACE] then
  begin
    FAccept := True;
    HideClose
  end
  else if Key = VK_ESCAPE then
    HideClose;
end;

function TTimeForm.GetTime: TDateTime;
begin
  Result := EncodeTime(StrToInt(FHrs.Cells[FHrs.Col, FHrs.Row]),
    StrToInt(FMins.Cells[FMins.Col, FMins.Row]), 0, 0);
end;

procedure TTimeForm.SetTime(AValue: TDateTime);
var
  H, M: Word;
begin
  H := HourOf(AValue);
  M := MinuteOf(AValue);
  FHrs.Row:= H div 12;
  FHrs.Col := H - ((H div 12) * 12);
  //FMins.Row := M div 12;
  //FMins.Col := M - ((M div 12) * 12);
  FMins.Row := 0;
  FMins.Col := Round(M / 5);
end;

procedure TTimeForm.HideClose;
begin
  {$ifdef windows}
  Hide;
  {$else}
  if FAccept then ModalResult := mrOk
  else ModalResult := mrCancel;
  {$endif}
end;

procedure TTimeForm.Deactivate;
var
  BnCaptured: Boolean;
begin
  inherited Deactivate;
  {$ifdef windows}
  if FTimeControl is TTimeEditEx then
    BnCaptured := TTimeEditEx(FTimeControl).Button.MouseInClient
  else if FTimeControl is TDBTimeEdit then
    BnCaptured := TDBTimeEdit(FTimeControl).Button.MouseInClient
  else
    BnCaptured := False;
  if not BnCaptured then Hide;
  {$endif}
end;

procedure TTimeForm.DoShow;
begin
  inherited DoShow;
  FAccept := False;
end;

constructor TTimeForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  BorderStyle:=bsNone;
  ShowInTaskBar := stNever;
  FHrs := TStringGrid.Create(Self);
  FHrs.Parent := Self;
  FHrs.DefaultRowHeight:=22;
  FHrs.DefaultColWidth:=22;
  FHrs.FixedCols:=0;
  FHrs.FixedRows:=0;
  FHrs.RowCount:=2;
  FHrs.ColCount:=12;
  FHrs.AutoFillColumns := True;
  FHrs.Flat := True;
  FHrs.Font.Name:='Tahoma';
  {$ifdef windows}
  FHrs.Font.Size := 10;
  {$else}
  FHrs.Font.Size := 8;
  {$endif}
  FHrs.Width := 265;
  FHrs.Height := 44;
  FHrs.ScrollBars := ssNone;
  FHrs.Options:=FHrs.Options - [goRangeSelect] + [goDrawFocusSelected];
  FHrs.FocusColor:=FHrs.SelectedColor;
  FHrs.Rows[0].AddStrings(['00', '01', '02', '03', '04', '05', '06', '07', '08',
    '09', '10', '11']);
  FHrs.Rows[1].AddStrings(['12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23']);
  FHrs.OnDblClick:=@GridDblClick;
  FHrs.OnKeyDown:=@GridKeyDown;

  FMins := TStringGrid.Create(Self);
  FMins.Parent := Self;
  FMins.DefaultRowHeight:=22;
  FMins.DefaultColWidth:=22;
  FMins.FixedCols:=0;
  FMins.FixedRows:=0;
  FMins.RowCount:=1;
  FMins.ColCount:=12;
  FMins.AutoFillColumns := True;
  FMins.Flat := True;
  FMins.Font.Name:='Tahoma';
  {$ifdef windows}
  FMins.Font.Size := 10;
  {$else}
  FMins.Font.Size := 8;
  {$endif}
  FMins.Top := 48;
  FMins.Width := 265;
  FMins.Height := 22; //111;
  FMins.ScrollBars := ssNone;
  FMins.Options:=FMins.Options - [goRangeSelect] + [goDrawFocusSelected];
  FMins.FocusColor:=FMins.SelectedColor;
  FMins.Rows[0].AddStrings(['00', '05', '10', '15', '20', '25', '30', '35', '40',
    '45', '50', '55']);
  {FMins.Rows[0].AddStrings(['00', '01', '02', '03', '04', '05', '06', '07', '08',
    '09', '10', '11']);
  FMins.Rows[1].AddStrings(['12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23']);
  FMins.Rows[2].AddStrings(['24', '25', '26', '27', '28', '29', '30', '31', '32',
    '33', '34', '35']);
  FMins.Rows[3].AddStrings(['36', '37', '38', '39', '40', '41', '42', '43', '44',
    '45', '46', '47']);
  FMins.Rows[4].AddStrings(['48', '49', '50', '51', '52', '53', '54', '55', '56',
    '57', '58', '59']);  }
  FMins.OnDblClick:=@GridDblClick;
  FMins.OnKeyDown:=@GridKeyDown;
  Width := 265;
  Height := 70; //159;
end;

procedure TTimeForm.ShowForm;
begin
  {$ifdef windows}
  Show;
  {$else}
  ShowModal;
  {$endif}
end;

{ TTimeEditEx }

procedure TTimeEditEx.FormHide(Sender: TObject);
begin
  if FForm.Accept then
    Text := TimeToStr(FForm.Time);
end;

procedure TTimeEditEx.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
  end;
end;

procedure TTimeEditEx.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
  PopupMenu.Items[1].Enabled := SelText <> '';
end;

procedure TTimeEditEx.DoEnter;
begin
  FOldText := Text;
  inherited DoEnter;
end;

procedure TTimeEditEx.ButtonClick;
var
  P: TPoint;
  T: TDateTime;
begin
  inherited ButtonClick;
  if CanFocus then SetFocus;
  if FForm = nil then
  begin
    FForm := TTimeForm.CreateNew(nil);
    FForm.TimeControl := Self;
    FForm.OnHide:=@FormHide;
  end;

  if FForm.Visible then
  begin
    FForm.Hide;
    Exit;
  end;

  P := ClientToScreen(Point(0, 0));
  FForm.Top := P.y + Height;
  FForm.Left := P.x;
  CorrectFormPos(Self, FForm);

  T := 0;
  TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), T);
  FForm.Time:=T;
  FForm.ShowForm;
end;

constructor TTimeEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Flat:=True;
  Button.Width := 23;
  Button.Height := 23;
  Button.LoadGlyphFromLazarusResource('clock16');
  // Блокируем системное меню
  //Button.PopupMenu := TPopupMenu.Create(Self);

  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuHandler, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuHandler, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuHandler, IMG16_PASTE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

destructor TTimeEditEx.Destroy;
begin
  FreeAndNil(FForm);
  inherited Destroy;
end;

procedure TTimeEditEx.Undo;
begin
  Text := FOldText;
end;

function TTimeEditEx.Validate: Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), D);
  if Result then
    Text := FormatDateTime('hh:nn:ss', D);
end;

procedure TTimeEditEx.EditEditingDone;
begin
  if (Text <> '') and not Validate then Undo;
  inherited EditEditingDone;
end;

{ TDBTimeEdit }

procedure TDBTimeEdit.FormHide(Sender: TObject);
begin
  if FForm.Accept then
  begin
    //if CanFocus then SetFocus;
    //Text := TimeToStr(FForm.Time);
    if (Field <> nil) then
    begin
      {if not (Field.DataSet.State in [dsInsert, dsEdit]) then
        Field.DataSet.Edit;   }
      if Field.DataSet.State in [dsInsert, dsEdit] then
        Field.AsDateTime:=FForm.Time;
    end;
  end;
end;

function TDBTimeEdit.GetDefaultGlyphName: String;
begin
  Result:='clock16';
end;

procedure TDBTimeEdit.DoButtonClick(Sender: TObject);
var
  P: types.TPoint;
  T: TDateTime;
begin
  inherited DoButtonClick(Sender);
  if FForm = nil then
  begin
    FForm := TTimeForm.CreateNew(nil);
    FForm.TimeControl := Self;
    FForm.OnHide:=@FormHide;
  end;

  if FForm.Visible then
  begin
    FForm.Hide;
    Exit;
  end;

  P := ClientToScreen(Point(0, 0));
  FForm.Top := P.y + Height;
  FForm.Left := P.x;
  CorrectFormPos(Self, FForm);

  T := 0;
  TryStrToTime(StringReplace(Text, ' ', ':', [rfReplaceAll]), T);
  FForm.Time:=T;
  FForm.ShowForm;
end;

procedure TDBTimeEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then
  begin
    Button.Images := Images48;
    Button.ImageIndex := IMG48_CLOCK;
  end
  else if AHeight > 36 then
  begin
    Button.Images := Images32;
    Button.ImageIndex := IMG32_CLOCK;
  end
  else if AHeight > 28 then
  begin
    Button.Images := Images24;
    Button.ImageIndex := IMG24_CLOCK;
  end
  else
  begin
    Button.Images := Images16;
    Button.ImageIndex := IMG16_CLOCK;
  end
  {if AHeight > 52 then S := 'clock48'
  else if AHeight > 36 then S := 'clock32'
  else if AHeight > 28 then S := 'clock24'
  else S := 'clock16';
  Button.LoadGlyphFromLazarusResource(S);  }
end;

constructor TDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Flat:=True;
  Button.Width := 23;
  Button.Height := 23;
  Button.Images := Images16;
  Button.ImageIndex := IMG16_CLOCK;
end;

destructor TDBTimeEdit.Destroy;
begin
  FreeAndNil(FForm);
  inherited Destroy;
end;

end.

