unit PropsMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Menus, strconsts, dxctrls, dxreports,
  Graphics;

type

  { TDateEditMenu }

  TDateEditMenu = class(TPopupMenu)
  private
    FCmp: TdxDateEdit;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxDateEdit);
  end;

  { TCalcEditMenu }

  {TCalcEditMenu = class(TPopupMenu)
  private
    FCmp: TdxCalcEdit;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxCalcEdit);
  end;   }

  { TFormMenu }

  TFormMenu = class(TPopupMenu)
  private
    FCmp: TdxForm;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxForm);
  end;

  { TFormMoreMenu }

  TFormMoreMenu = class(TPopupMenu)
  private
    FCmp: TdxForm;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxForm);
  end;

  { TTimeEditMenu }

  TTimeEditMenu = class(TPopupMenu)
  private
    FCmp: TdxTimeEdit;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxTimeEdit);
    procedure PopupMenu2(C: TdxTimeEdit);
  end;

  { TRequiredMenu }

  TRequiredMenu = class(TPopupMenu)
  private
    FCmp: TComponent;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TComponent);
  end;

  { TCounterMenu }

  TCounterMenu = class(TPopupMenu)
  private
    FCmp: TdxCounter;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxCounter);
  end;

  { TComboBoxMenu }

  {TComboBoxMenu = class(TPopupMenu)
  private
    FCmp: TCustomComboBox;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TCustomComboBox);
  end;    }

  { TInsertValuesMenu }

  TInsertValuesMenu = class(TPopupMenu)
  private
    FCmp: TdxLookupComboBox;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxLookupComboBox);
  end;

  { TExprMenu }

  TExprMenu = class(TPopupMenu)
  private
    FCmp: TComponent;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TComponent);
  end;

  { TButtonGlyphMenu }

  TButtonGlyphMenu = class(TPopupMenu)
  private
    FCmp: TdxButton;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxButton);
  end;

  { TShowScrollBarsMenu }

  {TShowScrollBarsMenu = class(TPopupMenu)
  private
    FCmp: TdxForm;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxForm);
  end;  }

  { TQueryGridMoreMenu }

  TQueryGridMoreMenu = class(TPopupMenu)
  private
    FCmp: TdxQueryGrid;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxQueryGrid);
  end;

  { TFormActionsMenu }

  {TFormActionsMenu = class(TPopupMenu)
  private
    FCmp: TdxForm;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxForm);
  end;}

  { TCalcEditMoreMenu }

  TCalcEditMoreMenu = class(TPopupMenu)
  private
    FCmp: TdxCalcEdit;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxCalcEdit);
  end;

var
  DateEditMnu: TDateEditMenu;
  //CalcEditMnu: TCalcEditMenu;
  FormMnu: TFormMenu;
  FormMoreMnu: TFormMoreMenu;
  TimeEditMnu: TTimeEditMenu;
  RequiredMnu: TRequiredMenu;
  CounterMnu: TCounterMenu;
  InsValMnu: TInsertValuesMenu;
  ExprMnu: TExprMenu;
  BtnGlyphMnu: TButtonGlyphMenu;
  QGridMoreMenu: TQueryGridMoreMenu;
  CalcEditMoreMnu: TCalcEditMoreMenu;

implementation

uses
  apputils, insertvaluesform, filltableform, propdialogs, BGRABitmap,
  LazUtf8, designerframe, hintform, imagesform, imagemanager, appimagelists;

{ TCalcEditMoreMenu }

procedure TCalcEditMoreMenu.MenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: FCmp.NullToZero:=not FCmp.NullToZero;
  end;
end;

procedure TCalcEditMoreMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.NullToZero;
end;

constructor TCalcEditMoreMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsNullToZero, 0, 0, @MenuClick) );
end;

procedure TCalcEditMoreMenu.PopupMenu(C: TdxCalcEdit);
begin
  FCmp := C;
  Popup;
end;

{ TFormActionsMenu }

(*procedure TFormActionsMenu.MenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
	  0: FCmp.ActionOnCreate := ShowActionsEditForm(rsCreateForm, FCmp.ActionOnCreate, FCmp);
    1: FCmp.ActionOnDestroy := ShowActionsEditForm(rsDestroyForm, FCmp.ActionOnDestroy, FCmp);
    3: FCmp.ActionOnFieldChange := ShowActionsEditForm(rsFieldChange, FCmp.ActionOnFieldChange, FCmp);
    4: FCmp.ActionOnValidate := ShowActionsEditForm(rsValidateRecord, FCmp.ActionOnValidate, FCmp);

    6: FCmp.ActionOnAfterOpen := ShowActionsEditForm(rsAfterOpenData, FCmp.ActionOnAfterOpen, FCmp);
    7: FCmp.ActionOnAfterScroll := ShowActionsEditForm(rsAfterGoToRecord, FCmp.ActionOnAfterScroll, FCmp);
    8: FCmp.ActionOnAfterInsert := ShowActionsEditForm(rsAfterInsertRecord, FCmp.ActionOnAfterInsert, FCmp);
    9: FCmp.ActionOnAfterEdit := ShowActionsEditForm(rsAfterEditRecord, FCmp.ActionOnAfterEdit, FCmp);
    10: FCmp.ActionOnAfterPost := ShowActionsEditForm(rsAfterSaveRecord, FCmp.ActionOnAfterPost, FCmp);
    11: FCmp.ActionOnAfterCancel := ShowActionsEditForm(rsAfterCancelChanges, FCmp.ActionOnAfterCancel, FCmp);
    12: FCmp.ActionOnAfterDelete := ShowActionsEditForm(rsAfterDeleteRecord, FCmp.ActionOnAfterDelete, FCmp);
    13: FCmp.ActionOnAfterClose := ShowActionsEditForm(rsAfterCloseData, FCmp.ActionOnAfterClose, FCmp);

    15: FCmp.ActionOnBeforeOpen := ShowActionsEditForm(rsBeforeOpenData, FCmp.ActionOnBeforeOpen, FCmp);
    16: FCmp.ActionOnBeforeScroll := ShowActionsEditForm(rsBeforeGoToRecord, FCmp.ActionOnBeforeScroll, FCmp);
    17: FCmp.ActionOnBeforeInsert := ShowActionsEditForm(rsBeforeInsertRecord, FCmp.ActionOnBeforeInsert, FCmp);
    18: FCmp.ActionOnBeforeEdit := ShowActionsEditForm(rsBeforeEditRecord, FCmp.ActionOnBeforeEdit, FCmp);
    19: FCmp.ActionOnBeforePost := ShowActionsEditForm(rsBeforeSaveRecord, FCmp.ActionOnBeforePost, FCmp);
    20: FCmp.ActionOnBeforeCancel := ShowActionsEditForm(rsBeforeCancelChanges, FCmp.ActionOnBeforeCancel, FCmp);
    21: FCmp.ActionOnBeforeDelete := ShowActionsEditForm(rsBeforeDeleteRecord, FCmp.ActionOnBeforeDelete, FCmp);
    22: FCmp.ActionOnBeforeClose := ShowActionsEditForm(rsBeforeCloseData, FCmp.ActionOnBeforeClose, FCmp);

    24: FCmp.ActionOnShowEditWindow := ShowActionsEditForm(rsShowEditWindow, FCmp.ActionOnShowEditWindow, FCmp);
    25: FCmp.ActionOnCloseEditWindow := ShowActionsEditForm(rsCloseEditWindow, FCmp.ActionOnCloseEditWindow, FCmp);
  end;
end;

procedure TFormActionsMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Visible := FCmp.PId = 0;
  Items[1].Visible := FCmp.PId = 0;
  Items[2].Visible := FCmp.PId = 0;
  Items[0].Checked := FCmp.ActionOnCreate <> '';
  Items[1].Checked := FCmp.ActionOnDestroy <> '';

  Items[3].Checked := FCmp.ActionOnFieldChange <> '';
  Items[4].Checked := FCmp.ActionOnValidate <> '';

  Items[6].Checked := FCmp.ActionOnAfterOpen <> '';
  Items[7].Checked := FCmp.ActionOnAfterScroll <> '';
  Items[8].Checked := FCmp.ActionOnAfterInsert <> '';
  Items[9].Checked := FCmp.ActionOnAfterEdit <> '';
  Items[10].Checked := FCmp.ActionOnAfterPost <> '';
  Items[11].Checked := FCmp.ActionOnAfterCancel <> '';
  Items[12].Checked := FCmp.ActionOnAfterDelete <> '';
  Items[13].Checked := FCmp.ActionOnAfterClose <> '';

  Items[15].Checked := FCmp.ActionOnBeforeOpen <> '';
  Items[16].Checked := FCmp.ActionOnBeforeScroll <> '';
  Items[17].Checked := FCmp.ActionOnBeforeInsert <> '';
  Items[18].Checked := FCmp.ActionOnBeforeEdit <> '';
  Items[19].Checked := FCmp.ActionOnBeforePost <> '';
  Items[20].Checked := FCmp.ActionOnBeforeCancel <> '';
  Items[21].Checked := FCmp.ActionOnBeforeDelete <> '';
  Items[22].Checked := FCmp.ActionOnBeforeClose <> '';

  Items[24].Checked := FCmp.ActionOnShowEditWindow <> '';
  Items[25].Checked := FCmp.ActionOnCloseEditWindow <> '';
end;

constructor TFormActionsMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsCreateForm, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsDestroyForm, 1, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 2, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsFieldChange, 3, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsValidateRecord, 4, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 5, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterOpenData, 6, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterGoToRecord, 7, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterInsertRecord, 8, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterEditRecord, 9, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterSaveRecord, 10, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterCancelChanges, 11, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterDeleteRecord, 12, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsAfterCloseData, 13, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 14, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeOpenData, 15, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeGoToRecord, 16, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeInsertRecord, 17, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeEditRecord, 18, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeSaveRecord, 19, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeCancelChanges, 20, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeDeleteRecord, 21, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsBeforeCloseData, 22, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 23, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsShowEditWindow, 24, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsCloseEditWindow, 25, 0, @MenuClick, '') );
end;

procedure TFormActionsMenu.PopupMenu(C: TdxForm);
begin
  FCmp := C;
  Popup;
end;  *)

{ TQueryGridMoreMenu }

procedure TQueryGridMoreMenu.MenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: FCmp.ManualRefresh:=not FCmp.ManualRefresh;
  end;
end;

procedure TQueryGridMoreMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.ManualRefresh;
end;

constructor TQueryGridMoreMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsManualRefresh, 0, 0, @MenuClick) );
end;

procedure TQueryGridMoreMenu.PopupMenu(C: TdxQueryGrid);
begin
  FCmp := C;
  Popup;
end;

{ TFormMoreMenu }

procedure TFormMoreMenu.MenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: FCmp.ConfirmSaveRecord := not FCmp.ConfirmSaveRecord;
    1: FCmp.ConfirmAutoSaveRecord := not FCmp.ConfirmAutoSaveRecord;
    2: FCmp.ConfirmCancelEditing := not FCmp.ConfirmCancelEditing;
    4: FCmp.ShowScrollBars := not FCmp.ShowScrollBars;
    6:
      begin
        FCmp.SoftCheck := not FCmp.SoftCheck;
        if FCmp.SoftCheck then
          Info(rsSoftCheckEnableMsg);
      end;
  end;
end;

procedure TFormMoreMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.ConfirmSaveRecord;
  Items[1].Checked:=FCmp.ConfirmAutoSaveRecord;
  Items[2].Checked:=FCmp.ConfirmCancelEditing;
  Items[4].Checked:=FCmp.ShowScrollBars;
  Items[6].Checked:=FCmp.SoftCheck;
end;

constructor TFormMoreMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsConfirmSaveRec, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsConfirmAutosaveRec, 1, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsConfirmCancelEditing, 2, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, '-', 3, 0, nil) );
  Items.Add( CreateMenuItem(Self, rsShowScrollbars, 4, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, '-', 5, 0, nil) );
  Items.Add( CreateMenuItem(Self, rsSoftCheckValues, 6, 0, @MenuClick) );
end;

procedure TFormMoreMenu.PopupMenu(C: TdxForm);
begin
  FCmp := C;
  Popup;
end;

{ TShowScrollBarsMenu }

{procedure TShowScrollBarsMenu.MenuClick(Sender: TObject);
begin
  FCmp.ShowScrollBars:=not FCmp.ShowScrollBars;
end;

procedure TShowScrollBarsMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.ShowScrollBars;
end;

constructor TShowScrollBarsMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsShowScrollbars, 0, 0, @MenuClick, '') );
end;

procedure TShowScrollBarsMenu.PopupMenu(C: TdxForm);
begin
  FCmp := C;
  Popup;
end;    }

{ TButtonGlyphMenu }

procedure TButtonGlyphMenu.MenuClick(Sender: TObject);
var
  i: PtrInt;
  S: String;
  Pic: TPicture;
begin
  i := TComponent(Sender).Tag;
  case i of
    0:
      begin
        S := OpenPictureDialog(True);
        if S <> '' then
        begin
          FCmp.ImageName:='';
          Pic := TPicture.Create;
          try try
            Pic.LoadFromFile(S);
            FCmp.Glyph := Pic.Bitmap;
          except
            on E: Exception do
              ErrMsg(rsFailedToLoadImage + Spaces + E.Message);
          end;
          finally
            Pic.Free;
          end;
        end;
      end;
    1:
      begin
        S := SavePictureDialog('', True);
        if S <> '' then
        begin
          Pic := TPicture.Create;
          try try
            Pic.Bitmap := FCmp.Glyph;
            Pic.SaveToFile(S);
          except
            on E: Exception do
              ErrMsg(rsFailedToSaveImage + Spaces + E.Message);
          end;
          finally
            Pic.Free;
          end;
        end;
      end;
    2:
      begin
        FCmp.Glyph.Clear;
        FCmp.ImageName := '';
      end;
    3:
      begin
        if ShowImagesForm(True, FCmp.ImageName, False) = mrOk then
          FCmp.ImageName := ImagesFm.SelectedImageName;
      end;
  end;
end;

procedure TButtonGlyphMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[2].Enabled := not FCmp.Glyph.Empty;
  Items[3].Enabled := (not FCmp.Glyph.Empty) or (FCmp.ImageName <> '');
end;

constructor TButtonGlyphMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Images := Images16;
  Items.Add( CreateMenuItem(Self, rsSelectFromGallery, 3, 0, @MenuClick, IMG16_IMAGE) );
  Items.Add( CreateMenuItem(Self, rsLoadImage, 0, 0, @MenuClick, IMG16_DB) );
  Items.Add( CreateMenuItem(Self, rsSaveImage, 1, 0, @MenuClick, IMG16_SAVE) );
  Items.Add( CreateMenuItem(Self, rsClear, 2, 0, @MenuClick, IMG16_DELETE) );
end;

procedure TButtonGlyphMenu.PopupMenu(C: TdxButton);
begin
  FCmp := C;
  Popup;
end;

{ TExprMenu }

procedure TExprMenu.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: ShowExprDlg(FCmp);
    1: SetEditable(FCmp, not GetEditable(FCmp));
  end;
end;

procedure TExprMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[1].Checked:=GetEditable(FCmp);
end;

constructor TExprMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsExpression, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsEditable, 1, 0, @MenuClick) );
end;

procedure TExprMenu.PopupMenu(C: TComponent);
begin
  FCmp := C;
  Popup;
end;

{ TInsertValuesMenu }

procedure TInsertValuesMenu.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: ShowInsertValuesForm(FCmp);
    1: ShowFillTableForm(FCmp);
  end;
end;

procedure TInsertValuesMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
end;

constructor TInsertValuesMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsInsertValues, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsFillTable, 1, 0, @MenuClick) );
end;

procedure TInsertValuesMenu.PopupMenu(C: TdxLookupComboBox);
begin
  FCmp := C;
  PopUp;
end;

{ TComboBoxMenu }

{procedure TComboBoxMenu.MenuClick(Sender: TObject);
begin
  if FCmp.Style = csDropDown then
    FCmp.Style:=csDropDownList
  else
    FCmp.Style := csDropDown;
end;

procedure TComboBoxMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.Style = csDropDown;
end;

constructor TComboBoxMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsEditing, 0, 0, @MenuClick, '') );
end;

procedure TComboBoxMenu.PopupMenu(C: TCustomComboBox);
begin
  FCmp := C;
  Popup;
end;   }

{ TCounterMenu }

procedure TCounterMenu.MenuClick(Sender: TObject);
begin
  FCmp.ReadOnly:=not FCmp.ReadOnly;
end;

procedure TCounterMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked := FCmp.ReadOnly;
end;

constructor TCounterMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsReadOnly, 0, 0, @MenuClick) );
end;

procedure TCounterMenu.PopupMenu(C: TdxCounter);
begin
  FCmp := C;
  Popup;
end;

{function HasHierarchyRequired(Cmp: TdxLookupComboBox{; var FullFieldName: String}): Boolean;
var
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
	Result := Cmp.Required and IsHierarchyObj(Cmp);
  if Result then
  begin
    //FullFieldName := TdxForm(Cmp.Owner).FormCaption + '|' + Cmp.FieldName;
    Exit;
  end;
  Fm := FormMan.FindForm(Cmp.SourceTId);
  if Fm <> nil then
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if (C is TdxLookupComboBox) and (Cmp.SourceTId <> GetSourceTId(C)) then
      begin
        Result := HasHierarchyRequired(TdxLookupComboBox(C){, FullFieldName});
        if Result then Exit;
      end;
    end;
  end;
end;

procedure CheckEmptyObjects(Cmp: TdxLookupComboBox);
var
  SQL: String;
  Fm: TdxForm;
begin
  if (Cmp.SourceTId = 0) or (Cmp.SourceFId = 0) or (not Cmp.Required) or
  	(not HasHierarchyRequired(Cmp)) then Exit;

  Fm := TdxForm(Cmp.Owner);

	SQL := 'select first 1 id from ' + TableStr(Fm.Id) + ' where ' +
  	FieldStr(Cmp) + ' is null';

  try
    with DBase.OpenDataSet(SQL) do
	  begin
      if RecordCount > 0 then
			  Info(rsCheckEmptyObjMsg);
      Free;
    end;
  except
    // Форма может и не существовать пока. Глушим ошибку.
  end;
end;   }

{ TRequiredMenu }

procedure TRequiredMenu.MenuClick(Sender: TObject);
var
  i: PtrInt;
begin
  i := TComponent(Sender).Tag;
  case i of
    0:
      begin
        SetRequired(FCmp, not GetRequired(FCmp));
        DesignFr.SummaryTree.UpdateTree;
      end;
    1: ShowCheckExprDlg(FCmp);
  end;
end;

procedure TRequiredMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=GetRequired(FCmp);
end;

constructor TRequiredMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsRequired, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsCheckValue, 1, 0, @MenuClick) );
end;

procedure TRequiredMenu.PopupMenu(C: TComponent);
begin
  FCmp := C;
  Popup;
end;

{ TTimeEditMenu }

procedure TTimeEditMenu.MenuClick(Sender: TObject);
begin
  if Sender = Items[0] then
    FCmp.CurTime:=not FCmp.CurTime
  else
  begin
    FCmp.TimeFormat:=TdxTimeFormat(TComponent(Sender).Tag - 1);
  end;
end;

procedure TTimeEditMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  if Items[0].Visible then
    Items[0].Checked := FCmp.CurTime
  else
  begin
    Items[1].Checked := FCmp.TimeFormat = ttHH;
    Items[2].Checked := FCmp.TimeFormat = ttHHMM;
    Items[3].Checked := FCmp.TimeFormat = ttHHMMSS;
  end;
end;

constructor TTimeEditMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsCurrentTime, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsHH, 1, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsHHMM, 2, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsHHMMSS, 3, 0, @MenuClick) );
end;

procedure TTimeEditMenu.PopupMenu(C: TdxTimeEdit);
begin
  FCmp := C;
  Items[0].Visible := True;
  Items[1].Visible := False;
  Items[2].Visible := False;
  Items[3].Visible := False;
  Popup;
end;

procedure TTimeEditMenu.PopupMenu2(C: TdxTimeEdit);
begin
  FCmp := C;
  Items[0].Visible := False;
  Items[1].Visible := True;
  Items[2].Visible := True;
  Items[3].Visible := True;
  Popup;
end;

{ TFormMenu }

procedure TFormMenu.MenuClick(Sender: TObject);
begin
  FCmp.ViewType := TViewType(TComponent(Sender).Tag);
end;

procedure TFormMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.ViewType = vtGridTop;
  Items[1].Checked:=FCmp.ViewType = vtGridBottom;
  Items[2].Checked:=FCmp.ViewType = vtGridLeft;
  Items[3].Checked:=FCmp.ViewType = vtGridRight;
  Items[4].Checked:=FCmp.ViewType = vtGridOnly;
  Items[5].Checked:=FCmp.ViewType = vtWithoutGrid;
  Items[6].Checked:=FCmp.ViewType = vtSimpleForm;
end;

constructor TFormMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsGridTop, 0, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsGridBottom, 1, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsGridLeft, 2, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsGridRight, 3, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsGridOnly, 4, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsWithoutGrid, 5, 0, @MenuClick) );
  Items.Add( CreateMenuItem(Self, rsSimpleForm, 6, 0, @MenuClick) );
end;

procedure TFormMenu.PopupMenu(C: TdxForm);
begin
  FCmp := C;
  Popup;
end;

{ TCalcEditMenu }

(*procedure TCalcEditMenu.MenuClick(Sender: TObject);
begin
  //FCmp.AutoNum:=not FCmp.AutoNum;
end;

procedure TCalcEditMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  //Items[0].Checked:=FCmp.AutoNum;
end;

constructor TCalcEditMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Items.Add( CreateMenuItem(Self, rsAutoNum, 0, 0, @MenuClick, '') );
end;

procedure TCalcEditMenu.PopupMenu(C: TdxCalcEdit);
begin
  FCmp := C;
  Popup;
end;   *)

{ TDateEditMenu }

procedure TDateEditMenu.MenuClick(Sender: TObject);
begin
  FCmp.DateNow:=not FCmp.DateNow;
end;

procedure TDateEditMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.DateNow;
end;

constructor TDateEditMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsNow, 0, 0, @MenuClick) );
end;

procedure TDateEditMenu.PopupMenu(C: TdxDateEdit);
begin
  FCmp := C;
  Popup;
end;

end.

