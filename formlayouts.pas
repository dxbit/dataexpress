unit FormLayouts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, dxctrls;

type

  PControlLayoutItem = ^TControlLayoutItem;
  TControlLayoutItem = record
    Name: String;
    Left, Top, Width, Height: Integer;
    Anchors: TAnchors;
  end;

  { TControlLayoutList }

  TControlLayoutList = class(TList)
  private
    function GetControlItems(Index: Integer): PControlLayoutItem;
  public
    function AddControl(AControl: TControl): PControlLayoutItem;
    function FindControl(AControlName: String): PControlLayoutItem;
    procedure DeleteControl(AControlName: String);
    procedure Clear; override;
    property ControlItems[Index: Integer]: PControlLayoutItem read GetControlItems; default;
  end;

  PFormLayout = ^TFormLayout;
  TFormLayout = record
    Name: String;
    MinWidth: Integer;
    Width, Height: Integer;
    Web, Desktop: Boolean;
    FixedHeight: Boolean;
    Disabled: Boolean;
    Controls: TControlLayoutList;
  end;

  { TFormLayoutList }

  TFormLayoutList = class(TList)
  private
    function GetLayouts(Index: Integer): PFormLayout;
  public
    function AddLayout(const AName: String; AForm: TdxForm): PFormLayout;
    procedure UpdateLayout(const AName: String; AForm: TdxForm);
    procedure ApplyLayout(const AName: String; AForm: TdxForm);
    function FindLayout(const AName: String): PFormLayout;
    function FindLayoutWidth(const AWidth: Integer): PFormLayout;
    procedure DeleteLayout(const AName: String);
    procedure Clear; override;
    procedure AddControl(Ctrl: TControl);
    procedure DeleteControl(Ctrl: TControl);
    procedure RenameControl(Ctrl: TControl; const OldName, NewName: String);
    property Layouts[Index: Integer]: PFormLayout read GetLayouts; default;
  end;

  PFormLayoutForm = ^TFormLayoutForm;
  TFormLayoutForm = record
    Id: Integer;
    Layouts: TFormLayoutList;
  end;

  { TFormLayoutFormList }

  TFormLayoutFormList = class(TList)
  private
    function GetForms(Index: Integer): PFormLayoutForm;
  public
    function NewForm(AForm: TdxForm): PFormLayoutForm;
    procedure DisposeForm(pFm: PFormLayoutForm);
    function AddForm(AForm: TdxForm): PFormLayoutForm;
    function FindForm(FmId: Integer): PFormLayoutForm;
    procedure DeleteForm(Fm: TdxForm);
    procedure Clear; override;
    procedure LoadFromStream(AFm: PFormLayoutForm; St: TStream);
    procedure SaveToStream(pFm: PFormLayoutForm; St: TStream);
    procedure SaveToFile(pFm: PFormLayoutForm; const FileName: String);
    procedure LoadFromFile(pFm: PFormLayoutForm; const FileName: String);
    procedure CopyForm(pSrc, pDest: PFormLayoutForm);
    property Forms[Index: Integer]: PFormLayoutForm read GetForms; default;
  end;

implementation

uses
  SAX, saxbasereader, Buttons, myctrls, apputils, formmanager, designerframe;

type

  { TFormLayoutsReader }

  TFormLayoutsReader = class(TSAXBaseReader)
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    pLay: PFormLayout;
    pFm: PFormLayoutForm;
  end;

{ TFormLayoutsReader }

procedure TFormLayoutsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  pCLI: PControlLayoutItem;
  S: String;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'layout' then
  begin
    pLay := pFm^.Layouts.AddLayout(GetStr(Atts, 'name'), nil);
    with pLay^ do
    begin
      Web := GetBool(Atts, 'web');
      Desktop := GetBool(Atts, 'desktop');
      S := GetStr(Atts, 'size');
      Width := StrToInt(CutStr(S, ';'));
      Height := StrToInt(CutStr(S, ';'));
      MinWidth := GetInt(Atts, 'minwidth', Width);
      FixedHeight := GetBool(Atts, 'fixedheight');
      Disabled := GetBool(Atts, 'disabled');
    end;
  end
  else if LocalName = 'control' then
  begin
    pCLI := pLay^.Controls.AddControl(nil);
    with pCLI^ do
    begin
      Name := GetStr(Atts, 'name');
      S := GetStr(Atts, 'bounds');
      Left := StrToInt(CutStr(S, ';'));
      Top := StrToInt(CutStr(S, ';'));
      Width := StrToInt(CutStr(S, ';'));
      Height := StrToInt(CutStr(S, ';'));
      Anchors := TAnchors(GetInt(Atts, 'anchors'));
    end;
  end;
end;

{ TControlLayoutList }

function TControlLayoutList.GetControlItems(Index: Integer): PControlLayoutItem;
begin
  Result := PControlLayoutItem(Items[Index]);
end;

function TControlLayoutList.AddControl(AControl: TControl): PControlLayoutItem;
begin
  New(Result);
  FillChar(Result^, SizeOf(TControlLayoutItem), 0);
  if AControl <> nil then
    with Result^ do
    begin
      //Debug(AControl.Name + ' - ' + AControl.ClassName);
      Name := AControl.Name;
      Left := AControl.Left;
      Top := AControl.Top;
      Width := AControl.Width;
      Height := AControl.Height;
      Anchors := AControl.Anchors;
    end;
  Add(Result);
end;

function TControlLayoutList.FindControl(AControlName: String): PControlLayoutItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ControlItems[i]^.Name = AControlName then
      Exit(ControlItems[i]);
end;

procedure TControlLayoutList.DeleteControl(AControlName: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if ControlItems[i]^.Name = AControlName then
    begin
      //Debug(AControlName);
      Dispose(ControlItems[i]);
      Delete(i);
      Exit;
    end;
end;

procedure TControlLayoutList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(ControlItems[i]);
  inherited Clear;
end;

{ TFormLayoutList }

function TFormLayoutList.GetLayouts(Index: Integer): PFormLayout;
begin
  Result := PFormLayout(Items[Index]);
end;

function TFormLayoutList.AddLayout(const AName: String; AForm: TdxForm
  ): PFormLayout;
var
  i: Integer;
  C: TComponent;
  L: TControlLayoutList;
begin
  New(Result);
  FillChar(Result^, SizeOf(TFormLayout), 0);
  Result^.Name := AName;
  L := TControlLayoutList.Create;
  Result^.Controls := L;
  if AForm <> nil then
  begin
    Result^.Width := AForm.Width;
    Result^.Height := AForm.Height;
    for i := 0 to AForm.ComponentCount - 1 do
    begin
      C := AForm.Components[i];
      if (C is TControl) and not (C is TSpeedButton) and not (C is TGridButtons)
        and not (C is TdxTabSheet) then
        L.AddControl(TControl(C));
    end;
  end;
  Add(Result);
end;

procedure TFormLayoutList.UpdateLayout(const AName: String; AForm: TdxForm);
var
  pLay: PFormLayout;
  i: Integer;
  C: TComponent;
begin
  pLay := FindLayout(AName);
  if pLay = nil then Exit;

  pLay^.Width := AForm.Width;
  pLay^.Height := AForm.Height;
  pLay^.Controls.Clear;
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    C := AForm.Components[i];
    if (C is TControl) and not (C is TSpeedButton) and not (C is TGridButtons)
      and not (C is TdxTabSheet) then
      pLay^.Controls.AddControl(TControl(C));
  end;
end;

procedure TFormLayoutList.ApplyLayout(const AName: String; AForm: TdxForm);
var
  OnResizeArr, OnChangeBoundsArr: array of TNotifyEvent;
  Ctrls: TControlLayoutList;
  C: TControl;

  procedure DisableResizing;
  var
    j: Integer;
  begin
    SetLength(OnResizeArr, Ctrls.Count + 1);
    SetLength(OnChangeBoundsArr, Ctrls.Count + 1);
    OnResizeArr[0] := AForm.OnResize;
    OnChangeBoundsArr[0] := AForm.OnChangeBounds;
    AForm.OnResize := nil;
    AForm.OnChangeBounds := nil;
    for j := 1 to Ctrls.Count - 1 do
    begin
      //Debug(Ctrls[j]^.Name);
      C := TControl(AForm.FindComponent(Ctrls[j]^.Name));
      OnResizeArr[j] := C.OnResize;
      OnChangeBoundsArr[j] := C.OnChangeBounds;
      C.OnResize := nil;
      C.OnChangeBounds := nil;
      C.Anchors := [akLeft, akTop];
    end;
  end;

  procedure EnableResizing;
  var
    j: Integer;
  begin
    AForm.OnResize := OnResizeArr[0];
    AForm.OnChangeBounds := OnChangeBoundsArr[0];
    for j := 1 to Ctrls.Count - 1 do
    begin
      C := TControl(AForm.FindComponent(Ctrls[j]^.Name));
      C.OnResize := OnResizeArr[j];
      C.OnChangeBounds := OnChangeBoundsArr[j];
    end;
    SetLength(OnResizeArr, 0);
    SetLength(OnChangeBoundsArr, 0);
  end;

var
  i: Integer;
  pLay: PFormLayout;
  CLI: TControlLayoutItem;
begin
  pLay := FindLayout(AName);
  Ctrls := pLay^.Controls;

  DisableResizing;

  with AForm do
    SetBounds(Left, Top, pLay^.Width, pLay^.Height);

  for i := 0 to Ctrls.Count - 1 do
  begin
    CLI := Ctrls[i]^;
    C := TControl(AForm.FindComponent(CLI.Name));
    C.SetBounds(CLI.Left, CLI.Top, CLI.Width, CLI.Height);
    C.Anchors := CLI.Anchors;
    //Debug(C.Name + ': ' + IntToStr(Cardinal(CLI.Anchors)));
  end;

  if AForm.ComponentState * [csDesigning] = [] then
    SetFormFixedHeight(AForm, pLay^.FixedHeight)
  {$ifdef linux}
  else
  begin
    DesignFr.DesignBox.DesignFm.Width := AForm.Width;
    DesignFr.DesignBox.DesignFm.Height := AForm.Height;
  end{$endif};

  EnableResizing;
end;

function TFormLayoutList.FindLayout(const AName: String): PFormLayout;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Layouts[i]^.Name = AName then Exit(Layouts[i]);
end;

// Ищем наиболее подходящий по ширине макет. Ищем макет с наибольшей шириной,
// но меньше текущей ширины формы. Если такого нет, то выбираем минимальную
// ширину.
function TFormLayoutList.FindLayoutWidth(const AWidth: Integer): PFormLayout;
var
  i: Integer;
  pLay, pLayMax, pLayMin: PFormLayout;
begin
  pLayMax := nil;
  pLayMin := nil;
  for i := 0 to Count - 1 do
  begin
    pLay := Layouts[i];
    if not pLay^.Desktop or pLay^.Disabled then Continue;

    if pLayMin = nil then pLayMin := pLay
    else if pLay^.MinWidth < pLayMin^.MinWidth then pLayMin := pLay;

    if pLayMax = nil then
    begin
      if pLay^.MinWidth <= AWidth then pLayMax := pLay;
    end
    else if (pLay^.MinWidth > pLayMax^.MinWidth) and (pLay^.MinWidth <= AWidth) then pLayMax := pLay;
  end;

  if pLayMax <> nil then Result := pLayMax
  else Result := pLayMin;
end;

procedure TFormLayoutList.DeleteLayout(const AName: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Layouts[i]^.Name = AName then
    begin
      Layouts[i]^.Controls.Free;
      Dispose(Layouts[i]);
      Delete(i);
      Exit;
    end;
end;

procedure TFormLayoutList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Layouts[i]^.Controls.Free;
    Dispose(Layouts[i]);
  end;
  inherited Clear;
end;

procedure TFormLayoutList.AddControl(Ctrl: TControl);
var
  i, j, z: Integer;
  WC: TWinControl;
  Pages: TdxPageControl;
begin
  if (Ctrl is TSpeedButton) or (Ctrl is TGridButtons) then Exit;

  for i := 0 to Count - 1 do
    Layouts[i]^.Controls.AddControl(Ctrl);

  if (Ctrl is TdxGroupBox) or (Ctrl is TdxPanel) then
  begin
    WC := TWinControl(Ctrl);
    for j := 0 to WC.ControlCount - 1 do
      AddControl(WC.Controls[j]);
  end
  else if Ctrl is TdxPageControl then
  begin
    Pages := TdxPageControl(Ctrl);
    for j := 0 to Pages.PageCount - 1 do
      for z := 0 to Pages.Pages[j].ControlCount - 1 do
        AddControl(Pages.Pages[j].Controls[z]);
  end;
end;

procedure TFormLayoutList.DeleteControl(Ctrl: TControl);
var
  i, j, z: Integer;
  WC: TWinControl;
  Pages: TdxPageControl;
begin
  if not (Ctrl is TdxTabSheet) then
    for i := 0 to Count - 1 do
      Layouts[i]^.Controls.DeleteControl(Ctrl.Name);

  if (Ctrl is TdxGroupBox) or (Ctrl is TdxPanel) or (Ctrl is TdxTabSheet) then
  begin
    WC := TWinControl(Ctrl);
    for j := 0 to WC.ControlCount - 1 do
      DeleteControl(WC.Controls[j]);
  end
  else if Ctrl is TdxPageControl then
  begin
    Pages := TdxPageControl(Ctrl);
    for j := 0 to Pages.PageCount - 1 do
      for z := 0 to Pages.Pages[j].ControlCount - 1 do
        DeleteControl(Pages.Pages[j].Controls[z]);
  end;
end;

procedure TFormLayoutList.RenameControl(Ctrl: TControl; const OldName,
  NewName: String);
var
  i: Integer;
  CLI: PControlLayoutItem;
begin
  if Ctrl is TdxTabSheet then Exit;
  for i := 0 to Count - 1 do
  begin
    CLI := Layouts[i]^.Controls.FindControl(OldName);
    if CLI <> nil then
    begin
      //Debug(OldName + ' -> ' + NewName);
      CLI^.Name := NewName;
    end;
  end;
end;

{ TFormLayoutFormList }

function TFormLayoutFormList.GetForms(Index: Integer): PFormLayoutForm;
begin
  Result := PFormLayoutForm(Items[Index]);
end;

function TFormLayoutFormList.NewForm(AForm: TdxForm): PFormLayoutForm;
begin
  New(Result);
  Result^.Id := AForm.Id;
  Result^.Layouts := TFormLayoutList.Create;
end;

procedure TFormLayoutFormList.DisposeForm(pFm: PFormLayoutForm);
begin
  pFm^.Layouts.Free;
  Dispose(pFm);
end;

function TFormLayoutFormList.AddForm(AForm: TdxForm): PFormLayoutForm;
begin
  Result := NewForm(AForm);
  Add(Result);
end;

function TFormLayoutFormList.FindForm(FmId: Integer): PFormLayoutForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Forms[i]^.Id = FmId then Exit(Forms[i]);
end;

procedure TFormLayoutFormList.DeleteForm(Fm: TdxForm);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Forms[i]^.Id = Fm.Id then
    begin
      Forms[i]^.Layouts.Free;
      Dispose(Forms[i]);
      Delete(i);
      Exit;
    end;

  if Fm.PId = 0 then
    for i := 0 to FormMan.FormCount - 1 do
      if FormMan.Forms[i].PId = Fm.Id then
        DeleteForm(FormMan.Forms[i]);
end;

procedure TFormLayoutFormList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Forms[i]^.Layouts.Free;
    Dispose(Forms[i]);
  end;
  inherited Clear;
end;

procedure TFormLayoutFormList.LoadFromStream(AFm: PFormLayoutForm; St: TStream);
begin
  AFm^.Layouts.Clear;
  with TFormLayoutsReader.Create do
  try
    pFm := AFm;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TFormLayoutFormList.SaveToStream(pFm: PFormLayoutForm; St: TStream);

  procedure WrStr(const S: String);
  begin
    St.Write(Pointer(S)^, Length(S));
  end;

  procedure WrControl(C: TControlLayoutItem);
  begin
    WrStr('<control name="' + C.Name +
      '" bounds="' + Format('%d;%d;%d;%d', [C.Left, C.Top, C.Width, C.Height]) +
      '" anchors="' + IntToStr(Cardinal(C.Anchors)) + '"/>');
  end;

  procedure WrLayout(Lay: TFormLayout);
  var
    i: Integer;
  begin
    WrStr('<layout name="' + StrToXml(Lay.Name) + '" web="' + Bool2Str(Lay.Web) +
      '" desktop="' + Bool2Str(Lay.Desktop) + '" minwidth="' + IntToStr(Lay.MinWidth) +
      '" fixedheight="' + Bool2Str(Lay.FixedHeight) +
      '" size="' + Format('%d;%d', [Lay.Width, Lay.Height]) +
      '" disabled="' + Bool2Str(Lay.Disabled) + '"><controls>');
    for i := 0 to Lay.Controls.Count - 1 do
      WrControl(Lay.Controls[i]^);
    WrStr('</controls></layout>');
  end;

var
  i: Integer;
begin
  WrStr('<layouts>');
  for i := 0 to pFm^.Layouts.Count - 1 do
    WrLayout(pFm^.Layouts[i]^);
  WrStr('</layouts>');
end;

procedure TFormLayoutFormList.SaveToFile(pFm: PFormLayoutForm;
  const FileName: String);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(pFm, FS);
  finally
    FS.Free;
  end;
end;

procedure TFormLayoutFormList.LoadFromFile(pFm: PFormLayoutForm;
  const FileName: String);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(pFm, FS);
  finally
    FS.Free;
  end;
end;

procedure TFormLayoutFormList.CopyForm(pSrc, pDest: PFormLayoutForm);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  SaveToStream(pSrc, MS);
  MS.Position := 0;
  LoadFromStream(pDest, MS);
  MS.Free;
end;

end.

