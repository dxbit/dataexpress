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
unit PropsMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, strconsts, dxctrls, Graphics;

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

  TCalcEditMenu = class(TPopupMenu)
  private
    FCmp: TdxCalcEdit;
    procedure MenuClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopupMenu(C: TdxCalcEdit);
  end;

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

var
  DateEditMnu: TDateEditMenu;
  CalcEditMnu: TCalcEditMenu;
  FormMnu: TFormMenu;
  FormMoreMnu: TFormMoreMenu;
  TimeEditMnu: TTimeEditMenu;
  RequiredMnu: TRequiredMenu;
  CounterMnu: TCounterMenu;
  InsValMnu: TInsertValuesMenu;
  ExprMnu: TExprMenu;
  BtnGlyphMnu: TButtonGlyphMenu;
  //ShowSBMnu: TShowScrollBarsMenu;
  //CbxMnu: TComboBoxMenu;

implementation

uses
  apputils, insertvaluesform, filltableform, propdialogs, BGRABitmap,
  LazUtf8, dxactions;

{ TFormMoreMenu }

procedure TFormMoreMenu.MenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: FCmp.ConfirmSaveRecord:=not FCmp.ConfirmSaveRecord;
    1: FCmp.ConfirmAutoSaveRecord:=not FCmp.ConfirmAutoSaveRecord;
    2: FCmp.ConfirmCancelEditing:=not FCmp.ConfirmCancelEditing;
    4: FCmp.ShowScrollBars:=not FCmp.ShowScrollBars;
  end;
end;

procedure TFormMoreMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[0].Checked:=FCmp.ConfirmSaveRecord;
  Items[1].Checked:=FCmp.ConfirmAutoSaveRecord;
  Items[2].Checked:=FCmp.ConfirmCancelEditing;
  Items[4].Checked:=FCmp.ShowScrollBars;
end;

constructor TFormMoreMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsConfirmSaveRec, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsConfirmAutosaveRec, 1, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsConfirmCancelEditing, 2, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 3, 0, nil, '') );
  Items.Add( CreateMenuItem(Self, rsShowScrollbars, 4, 0, @MenuClick, '') );
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
          Pic := TPicture.Create;
          try try
            Pic.LoadFromFile(S);
            FCmp.Glyph := Pic.Bitmap;
            FCmp.ResName:='';
          except
            on E: Exception do
              ErrMsg(Format(rsErrorLoadingImg, [LineEnding + LineEnding + E.Message]));
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
              ErrMsg(Format(rsErrorSavingImg, [LineEnding + LineEnding +
                E.Message]));
          end;
          finally
            Pic.Free;
          end;
        end;
      end;
    2:
      begin
        FCmp.Glyph.Clear;
        FCmp.ResName := '';
      end;
    4: FCmp.SetDefaultGlyph;
  end;
end;

procedure TButtonGlyphMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
  Items[1].Enabled := not FCmp.Glyph.Empty;
  Items[2].Enabled := not FCmp.Glyph.Empty;
  Items[4].Enabled := FCmp.ActionType <> actNone;
end;

constructor TButtonGlyphMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsLoadImage, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsSaveImage, 1, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsClear, 2, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, '-', 3, 0, nil, '') );
  Items.Add( CreateMenuItem(Self, rsDefaultImage, 4, 0, @MenuClick, '') );
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
  Items.Add( CreateMenuItem(Self, rsExpression, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsEditable, 1, 0, @MenuClick, '') );
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
    0: InsertValuesFm.ShowForm(FCmp);
    1: FillTableFm.ShowForm(FCmp);
  end;
end;

procedure TInsertValuesMenu.DoPopup(Sender: TObject);
begin
  inherited DoPopup(Sender);
end;

constructor TInsertValuesMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.Add( CreateMenuItem(Self, rsInsertValues, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsFillTable, 1, 0, @MenuClick, '') );
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
  Items.Add( CreateMenuItem(Self, rsReadOnly, 0, 0, @MenuClick, '') );
end;

procedure TCounterMenu.PopupMenu(C: TdxCounter);
begin
  FCmp := C;
  Popup;
end;

{ TRequiredMenu }

procedure TRequiredMenu.MenuClick(Sender: TObject);
var
  i: PtrInt;
//  S: String;
begin
  i := TComponent(Sender).Tag;
  case i of
    0: SetRequired(FCmp, not GetRequired(FCmp));
    1: ShowCheckExprDlg(FCmp);
      {begin
        S := GetCheckExpression(FCmp);
        if ExprFm.ShowForm(rsCheckValue, FCmp, S, TdxForm(FCmp.Owner), nil,
          nil, nil, 'checkvalue') then SetCheckExpression(FCmp, S);
      end;}
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
  Items.Add( CreateMenuItem(Self, rsRequired, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsCheckValue, 1, 0, @MenuClick, '') );
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
  Items.Add( CreateMenuItem(Self, rsCurrentTime, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsHH, 1, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsHHMM, 2, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsHHMMSS, 3, 0, @MenuClick, '') );
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
  Items.Add( CreateMenuItem(Self, rsGridTop, 0, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsGridBottom, 1, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsGridLeft, 2, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsGridRight, 3, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsGridOnly, 4, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsWithoutGrid, 5, 0, @MenuClick, '') );
  Items.Add( CreateMenuItem(Self, rsSimpleForm, 6, 0, @MenuClick, '') );
end;

procedure TFormMenu.PopupMenu(C: TdxForm);
begin
  FCmp := C;
  Popup;
end;

{ TCalcEditMenu }

procedure TCalcEditMenu.MenuClick(Sender: TObject);
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
end;

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
  Items.Add( CreateMenuItem(Self, rsNow, 0, 0, @MenuClick, '') );
end;

procedure TDateEditMenu.PopupMenu(C: TdxDateEdit);
begin
  FCmp := C;
  Popup;
end;

end.

