{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit FormDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignSurface, Buttons, Controls,
  Forms, Grids, dxctrls, LMessages, strconsts, JvDesignImp, Graphics;

type
  TFormChangesItem = class
  public
    Id, Cnt: Integer;
  end;

  { TFormChangesList }

  { Хранит число изменений форм для контроля и предотвращения исключения
  "Таблица БД была изменена 255 раз" }

  TFormChangesList = class(TList)
  private
    function GetFCItems(Index: Integer): TFormChangesItem;
  public
    procedure AddForm(Id, Cnt: Integer);
    procedure DeleteForm(Id: Integer);
    procedure Clear; override;
    procedure GetFormChanges;
    function FindForm(Id: Integer): TFormChangesItem;
    function Clone: TFormChangesList;
    property FCItems[Index: Integer]: TFormChangesItem read GetFCItems; default;
  end;

  TDesignStatus = (dstNew, dstDelete, dstFieldSize, dstCounter);

  { TDesignCacheItem }

  TDesignCacheItem = class
  public
    ClsName: String;
    Id, FmId, PId: Integer;
    FieldSize, StartWith: Integer;
    OldSize: Integer;
    InitField: Boolean;
    Status: TDesignStatus;
    function IsForm: Boolean;
  end;

  { TDesignCache }

  { Хранит сведения о добавленных и удаленных компонентах и формах и используется
  для генерация скрипта изменения структуры БД }

  TDesignCache = class(TList)
  private
    function GetCacheItems(Index: Integer): TDesignCacheItem;
    procedure DeleteCacheItem(DCI: TDesignCacheItem);
    function GetStructChanged: Boolean;
  public
    procedure AddForm(Fm: TdxForm);
    procedure AddFormWithComponents(Fm: TdxForm);
    procedure AddComponent(C: TComponent);
    procedure DeleteForm(Fm: TdxForm);
    procedure DeleteComponent(C: TComponent);
    procedure SetFieldSize(C: TComponent; OldSize: Integer);
    procedure SetCounter(C: TComponent; StartWith: Integer);
    function FindFieldSize(FmId, Id: Integer): TDesignCacheItem;
    function FormExists(FmId: Integer): Boolean;
    procedure Clear; override;
    property CacheItems[Index: Integer]: TDesignCacheItem read GetCacheItems; default;
    property StructChanged: Boolean read GetStructChanged;
  end;

  TUndoControlItem = class
  public
    Control: TControl;
    Bounds: TRect;
    ParentControl: TWinControl;
  end;

  { TUndoControls }

  TUndoControls = class(TList)
  private
    function GetControls(Index: Integer): TUndoControlItem;
  public
    function AddControl(C: TControl): TUndoControlItem;
    function FindControl(C: TControl): TUndoControlItem;
    procedure Clear; override;
    property Controls[Index: Integer]: TUndoControlItem read GetControls; default;
  end;

  TUndoAction = (uaChange);

  { TUndoCacheItem }

  TUndoCacheItem = class
  private
    procedure UndoChanges;
    procedure SelectComponents;
  public
    Controls: TUndoControls;
    UndoAction: TUndoAction;
    constructor Create;
    destructor Destroy; override;
    procedure Undo;
  end;

  TFormDesigner = class;

  TUndoCache = class(TList)
  private
    FForm: TdxForm;
    FCurIndex: Integer;
    function GetCacheItems(Index: Integer): TUndoCacheItem;
    procedure ChangingComponents(Sender: TObject);
    procedure UndoBounds(Item: TUndoCacheItem);
  public
    function AddItem: TUndoCacheItem;
    procedure Clear; override;
    property CacheItems[Index: Integer]: TUndoCacheItem read GetCacheItems; default;
    constructor Create(AForm: TdxForm);
    procedure AttachSurface;
    procedure DetachSurface;
    procedure StoreProps;
    procedure Undo;
    procedure Redo;
    procedure DeleteComponent(C: TComponent);
  end;

  { TUndoManager }

  TUndoManager = class(TList)
  private
    FCurrentCache: TUndoCache;
    function GetCaches(Index: Integer): TUndoCache;
  public
    function AddCache(Fm: TdxForm): TUndoCache;
    procedure SelectCache(Fm: TdxForm);
    function FindCache(Fm: TdxForm): TUndoCache;
    procedure DeleteCache(Fm: TdxForm);
    procedure Clear; override;
    property Caches[Index: Integer]: TUndoCache read GetCaches; default;
    property CurrentCache: TUndoCache read FCurrentCache;
  end;

  { TMyController }

  TMyController = class(TJvDesignController)
  private
    FLeftButtonPressed, FKeyPressed: Boolean;
  protected
    function KeyDown(AKeyCode: Cardinal): Boolean; override;
    function KeyUp(AKeyCode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
      ): Boolean; override;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
      ): Boolean; override;
  public
    property LeftButtonPressed: Boolean read FLeftButtonPressed;
  end;

  { TMyMessenger }

  TMyMessenger = class(TJvDesignWinControlHookMessenger)
  public
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage): Boolean;
      override;
  end;

  { TMySelector }

  TMySelector = class(TJvDesignSelector)
  public
    constructor Create(ASurface: TJvDesignSurface); override;
    procedure AddToSelection(AValue: TControl); override;
  end;

  TIdObjList = class;

  { TFormDesigner }

  TFormDesigner = class(TJvDesignSurface)
  private
    FActive: Boolean;
    FControlClass: String;
    FNewControl: Boolean;
    FOnKeyDown: TKeyEvent;
    FParent: TWinControl;
    FMoves: TList;
    FIdObjList: TIdObjList;
    //FStructChanged: Boolean;
    function GetControl: TControl;
    function GetForm: TdxForm;
    procedure SetActive(AValue: Boolean);
    function IsParentSelect(C: TControl): Boolean;
    function IsParent(C, aParent: TControl): Boolean;
    function GetSelectionRect: TRect;
    procedure AdjustPastedComponents;
    function CheckDeleteComponents: Boolean;
    //procedure SaveIdObjList;
    procedure AdjustIdObjList;
  protected
    procedure SurfaceSelectionChange(Sender: TObject);
    procedure SurfaceGetComponentName(Sender: TObject; aComponent: TComponent);
    procedure SurfaceAddComponent(Sender: TObject; aComponent: TComponent);
    procedure SurfaceDeleteComponent(Sender: TObject; aComponent: TComponent);
    procedure SurfaceGetAddClass(Sender: TObject; var ioClass: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DesignForm(Fm: TWinControl);
    function GetLeftMostEdge: Integer;
    procedure SetLeftMostEdge;
    function GetTopMostEdge: Integer;
    procedure SetTopMostEdge;
    function GetRightMostEdge: Integer;
    procedure SetRightMostEdge;
    function GetBottomMostEdge: Integer;
    procedure SetBottomMostEdge;
    procedure SetVertCenter;
    procedure SetHorzCenter;
    procedure SetMaxWidth;
    procedure SetMinWidth;
    procedure SetMaxHeight;
    procedure SetMinHeight;
    procedure ClearClipboard;
    procedure MoveComponents;
    function CanMoveComponents: Boolean;
    procedure PasteComponents;
    function CanPasteComponents: Boolean;
    procedure DeleteComponents;
    procedure UserDeleteComponents;
    procedure CopyComponents;
    procedure ChangeParent;
    procedure FindLost;
    procedure BringToFront;
    procedure SendToBack;
    property Active: Boolean read FActive write SetActive;
    property ControlClass: String read FControlClass write FControlClass;
    property Control: TControl read GetControl;
    property Parent: TWinControl read FParent write FParent;
    property Form: TdxForm read GetForm;
    //property StructChanged: Boolean read FStructChanged write FStructChanged;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

  { TDesignerBox }

  TDesignerBox = class(TScrollBox)
  private
    FIsWheel: Boolean;
  protected
    procedure WMHScroll(var Message: TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure DoEnter; override;
    procedure DoOnResize; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TIdObjItem = class
  public
    Id: Integer;
    Obj: TComponent;
  end;

  { TIdObjList }

  TIdObjList = class(TList)
  private
    function GetObjects(Index: Integer): TIdObjItem;
  public
    procedure AddObj(Obj: TComponent);
    function FindById(Id: Integer): TIdObjItem;
    procedure Clear; override;
    property Objects[Index: Integer]: TIdObjItem read GetObjects; default;
  end;

procedure MakeUniqueName(Owner, C: TComponent);

var
  FormDesign: TFormDesigner;
  Cache: TDesignCache;
  FormChanges: TFormChangesList;
  UndoMan: TUndoManager;

implementation

uses
  designerframe, dbengine, sqlgen, formmanager, propsform,
  comctrls, LCLType, dximages, dxfiles, dbctrlsex, apputils, DXReports,
  reportmanager, dialogs, myctrls, scriptform, scriptmanager, JvDesignClip,
  templatefieldsform, appsettings, findactionsform, findexprform;

{ TFormDesigner }


procedure AssignDesignMenu(C: TComponent);
var
  Bn: TSpeedButton;
begin
  {$ifndef windows}
  if C is TControl then
  begin
  	TControl(C).PopupMenu := DesignFr.PopupMenu1;
    Bn := GetEditButton(C);
    if Bn <> nil then Bn.PopupMenu := DesignFr.PopupMenu1;
  end;
  {$endif}
end;

procedure MakeUniqueName(Owner, C: TComponent);
var
  S: String;
  Num: Integer;
begin
  if C.Name = '' then
  begin
    S := Copy(C.ClassName, 2, 1024);
    Num := 0;
  end
  else
  begin
    // Не меняем имя, если компонента с таким именем нет (вставили компонент в
    // другую форму)
    if Owner.FindComponent(C.Name) = nil then Exit;
    SplitComponentName(C.Name, S, Num);
  end;

  repeat
    Inc(Num);
  until Owner.FindComponent(S + IntToStr(Num)) = nil;
  C.Name := S + IntToStr(Num);
end;

procedure MakeUniqueFieldName(Fm: TdxForm; aComponent: TComponent);
var
  S, Nm: String;
  Num: Integer;
begin
  S := GetFieldName(aComponent);
  if S = '' then
  begin
    if aComponent is TdxEdit then
      S := rsText
    else if aComponent is TdxCalcEdit then
      S := rsNumber
    else if aComponent is TdxDateEdit then
      S := rsDate
    else if aComponent is TdxMemo then
      S := rsMemo
    else if aComponent is TdxCheckBox then
      S := rsCheckBox
    else if aComponent is TdxComboBox then
      S := rsList
    else if aComponent is TdxLookupComboBox then
      S := rsObject
    else if aComponent is TdxDBImage then
      S := rsImage
    else if aComponent is TdxFile then
      S := rsDsgnFile
    else if aComponent is TdxObjectField then
      S := rsObjField
    else if aComponent is TdxTimeEdit then
      S := rsTime
    else if aComponent is TdxCounter then
      S := rsCounter
    else if aComponent is TdxRecordId then
      S := rsRecordID
  end
  else
    S := GetFieldName(aComponent);

  if FindComponentByFieldName(Fm, S, aComponent) = nil then
  begin
    SetFieldName(aComponent, S);
    Exit;
  end;

  SplitComponentName(S, Nm, Num);

  repeat
    Inc(Num);
  until FindComponentByFieldName(Fm, Nm + IntToStr(Num), aComponent) = nil;

  SetFieldName(aComponent, Nm + IntToStr(Num));
end;

{ TUndoManager }

function TUndoManager.GetCaches(Index: Integer): TUndoCache;
begin
  Result := TUndoCache(Items[Index]);
end;

function TUndoManager.AddCache(Fm: TdxForm): TUndoCache;
begin
  Result := TUndoCache.Create(Fm);
  Add(Result);
end;

procedure TUndoManager.SelectCache(Fm: TdxForm);
begin
  if FCurrentCache <> nil then FCurrentCache.DetachSurface;
  FCurrentCache := FindCache(Fm);
  if FCurrentCache = nil then FCurrentCache := AddCache(Fm);
  FCurrentCache.AttachSurface;
end;

function TUndoManager.FindCache(Fm: TdxForm): TUndoCache;
var
  i: Integer;
  C: TUndoCache;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    C := Caches[i];
    if C.FForm = Fm then Exit(C);
  end;
end;

procedure TUndoManager.DeleteCache(Fm: TdxForm);
var
  C: TUndoCache;
begin
  C := FindCache(Fm);
  if C <> nil then
  begin
    C.Free;
    Remove(C);
  end;
end;

procedure TUndoManager.Clear;
var
  i: Integer;
begin
  FCurrentCache := nil;
  for i := 0 to Count - 1 do
    Caches[i].Free;
  inherited Clear;
end;

{ TUndoCache }

function TUndoCache.GetCacheItems(Index: Integer): TUndoCacheItem;
begin
  Result := TUndoCacheItem(Items[Index]);
end;

procedure TUndoCache.ChangingComponents(Sender: TObject);
begin
  StoreProps;
end;

procedure TUndoCache.UndoBounds(Item: TUndoCacheItem);
var
  i: Integer;
  CI: TUndoControlItem;
begin
  for i := 0 to Item.Controls.Count - 1 do
  begin
    CI := Item.Controls[i];
    CI.Control.BoundsRect := CI.Bounds;
  end;
  FormDesign.UpdateDesigner;
end;

function TUndoCache.AddItem: TUndoCacheItem;
var
  i: Integer;
begin
  for i := Count - 1 downto FCurIndex + 1 do
  begin
    CacheItems[i].Free;
    Delete(i);
  end;
  Result := TUndoCacheItem.Create;
  FCurIndex := Add(Result);
end;

procedure TUndoCache.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    CacheItems[i].Free;
  inherited Clear;
end;

constructor TUndoCache.Create(AForm: TdxForm);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TUndoCache.AttachSurface;
begin
  FormDesign.OnChangingComponents:=@ChangingComponents;
end;

procedure TUndoCache.DetachSurface;
begin
  FormDesign.OnChangingComponents:=nil;
end;

procedure TUndoCache.StoreProps;
var
  i: Integer;
  Item: TUndoCacheItem;
begin
  if Length(FormDesign.Selected) = 0 then Exit;
  Item := AddItem;
  for i := 0 to High(FormDesign.Selected) do
    with Item.Controls.AddControl(FormDesign.Selection[i]) do
    begin
      Bounds := Control.BoundsRect;
      ParentControl := Control.Parent;
    end;
end;

procedure TUndoCache.Undo;
begin
  if (Count = 0) or (FCurIndex < 0) then Exit;
  CacheItems[FCurIndex].Undo;
  Dec(FCurIndex);
  FormDesign.UpdateDesigner;
  if IsPropsFormVisible then UpdatePropsForm;
end;

procedure TUndoCache.Redo;
begin
  if (Count = 0) or (FCurIndex = Count - 1) then Exit;
  Inc(FCurIndex);
  CacheItems[FCurIndex].Undo;
  FormDesign.UpdateDesigner;
  if IsPropsFormVisible then UpdatePropsForm;
end;

procedure TUndoCache.DeleteComponent(C: TComponent);
var
  i, j: Integer;
  Item: TUndoCacheItem;
begin
  for i := Count - 1 downto 0 do
  begin
    Item := CacheItems[i];
    for j := Item.Controls.Count - 1 downto 0 do
    begin
      if (Item.Controls[j].Control = C) or (Item.Controls[j].ParentControl = C) then
      begin
        Item.Controls.Delete(j);
        //Break;
      end;
    end;
    if Item.Controls.Count = 0 then
    begin
      Delete(i);
      if FCurIndex >= i then Dec(FCurIndex);
    end;
  end;
end;

{ TUndoCacheItem }

procedure TUndoCacheItem.UndoChanges;
var
  i: Integer;
  C: TUndoControlItem;
  OldBounds: TRect;
  OldParent: TWinControl;
begin
  for i := 0 to Controls.Count - 1 do
  begin
    C := Controls[i];
    OldBounds := C.Control.BoundsRect;
    OldParent := C.Control.Parent;
    C.Control.BoundsRect := C.Bounds;
    C.Control.Parent := C.ParentControl;
    C.Bounds := OldBounds;
    C.ParentControl := OldParent;
  end;
  SelectComponents;
  DesignFr.SelectControl(FormDesign.Selection[0], True);
  DesignFr.CompTree.SelectComponents(FormDesign.Selected);
end;

procedure TUndoCacheItem.SelectComponents;
var
  i: Integer;
  C: TControl;
begin
  for i := High(FormDesign.Selected) downto 0 do
  begin
    C := FormDesign.Selection[i];
    if Controls.FindControl(C) = nil then
      FormDesign.Selector.RemoveFromSelection(C);
  end;
  for i := 0 to Controls.Count - 1 do
  begin
    C := Controls[i].Control;
    if not FormDesign.Selector.IsSelected(C) then
      FormDesign.Selector.AddToSelection(C);
  end;
end;

constructor TUndoCacheItem.Create;
begin
  Controls := TUndoControls.Create;
end;

destructor TUndoCacheItem.Destroy;
begin
  Controls.Free;
  inherited Destroy;
end;

procedure TUndoCacheItem.Undo;
begin
  case UndoAction of
    uaChange: UndoChanges;
  end;
end;

{ TUndoControls }

function TUndoControls.GetControls(Index: Integer): TUndoControlItem;
begin
  Result := TUndoControlItem(Items[Index]);
end;

function TUndoControls.AddControl(C: TControl): TUndoControlItem;
begin
  Result := TUndoControlItem.Create;
  Result.Control := C;
  Add(Result);
end;

function TUndoControls.FindControl(C: TControl): TUndoControlItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Controls[i].Control = C then Exit(Controls[i]);
end;

procedure TUndoControls.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Controls[i].Free;
  inherited Clear;
end;

{ TIdObjList }

function TIdObjList.GetObjects(Index: Integer): TIdObjItem;
begin
  Result := TIdObjItem(Items[Index]);
end;

procedure TIdObjList.AddObj(Obj: TComponent);
var
  Item: TIdObjItem;
begin
  Item := TIdObjItem.Create;
  Item.Id := GetId(Obj);
  Item.Obj := Obj;
  Add(Item);
end;

function TIdObjList.FindById(Id: Integer): TIdObjItem;
var
  i: Integer;
  Item: TIdObjItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Item := Objects[i];
    if Item.Id = Id then Exit(Item);
  end;
end;

procedure TIdObjList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;
  inherited Clear;
end;

{ TFormChangesList }

function TFormChangesList.GetFCItems(Index: Integer): TFormChangesItem;
begin
  Result := TFormChangesItem(Items[Index]);
end;

procedure TFormChangesList.AddForm(Id, Cnt: Integer);
var
  FCI: TFormChangesItem;
begin
  FCI := TFormChangesItem.Create;
  FCI.Id := Id;
  FCI.Cnt := Cnt;
  Add(FCI);
end;

procedure TFormChangesList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FCItems[i].Free;
  inherited Clear;
end;

procedure TFormChangesList.GetFormChanges;
var
  SQL, S: String;
  N: Longint;
begin
  SQL := 'select r.rdb$relation_name, max(t.rdb$format) ' +
    'from rdb$formats t inner join rdb$relations r on t.rdb$relation_id = r.rdb$relation_id ' +
    'where LEFT(r.rdb$relation_name, 1) = ''T'' group by 1';
  try
    with DBase.OpenDataSet(SQL) do
    begin
      while not Eof do
      begin
        S := Trim(Fields[0].AsString);
        System.Delete(S, 1, 1);
        if TryStrToInt(S, N) then
          AddForm(N, Fields[1].AsInteger);
        Next;
      end;
      Free;
    end;
  except
    on E: Exception do
      ErrMsg(rsGetChangesCountError + ExceptionToString(E, True, False));
  end;
end;

function TFormChangesList.FindForm(Id: Integer): TFormChangesItem;
var
  i: Integer;
  FCI: TFormChangesItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    FCI := FCItems[i];
    if FCI.Id = Id then Exit(FCI);
  end;
end;

procedure TFormChangesList.DeleteForm(Id: Integer);
var
  FCI: TFormChangesItem;
begin
  FCI := FindForm(Id);
  if FCI <> nil then
  begin
    Remove(FCI);
    FCI.Free;
  end;
end;

function TFormChangesList.Clone: TFormChangesList;
var
  i: Integer;
begin
  Result := TFormChangesList.Create;
  for i := 0 to Count - 1 do
    Result.AddForm(FCItems[i].Id, FCItems[i].Cnt);
end;

{ TDesignCacheItem }

function TDesignCacheItem.IsForm: Boolean;
begin
  Result := FmId = 0;
end;

{ TDesignCache }

function TDesignCache.GetCacheItems(Index: Integer): TDesignCacheItem;
begin
  Result := TDesignCacheItem(Items[Index]);
end;

procedure TDesignCache.DeleteCacheItem(DCI: TDesignCacheItem);
begin
  Self.Remove(DCI);
  DCI.Free;
end;

function TDesignCache.GetStructChanged: Boolean;
begin
  Result := Count > 0;
end;

function TDesignCache.FormExists(FmId: Integer): Boolean;
var
  i: Integer;
  DCI: TDesignCacheItem;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    DCI := CacheItems[i];
    if DCI.IsForm and (DCI.Id = FmId) then Exit(True);
  end;
end;

function TDesignCache.FindFieldSize(FmId, Id: Integer): TDesignCacheItem;
var
  i: Integer;
  DCI: TDesignCacheItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    DCI := CacheItems[i];
    if (DCI.Status = dstFieldSize) and (DCI.FmId = FmId) and (DCI.Id = Id) then
      Exit(DCI);
  end;
end;

procedure TDesignCache.AddForm(Fm: TdxForm);
var
  DCI: TDesignCacheItem;
begin
  DCI := TDesignCacheItem.Create;
  DCI.Id := Fm.Id;
  DCI.PId := Fm.PId;
  DCI.ClsName := Fm.ClassName;
  DCI.Status := dstNew;
  Add(DCI);
end;

procedure TDesignCache.AddFormWithComponents(Fm: TdxForm);
var
  i: Integer;
begin
  AddForm(Fm);
  for i := 0 to Fm.ComponentCount - 1 do
    AddComponent(Fm.Components[i]);
end;

procedure TDesignCache.AddComponent(C: TComponent);
var
  DCI: TDesignCacheItem;
  FmId: Integer;
begin
  if not IsField(C) then Exit;

  FmId := GetId(C.Owner);
  DCI := TDesignCacheItem.Create;
  DCI.Id := GetId(C);
  DCI.FmId := FmId;
  DCI.ClsName := C.ClassName;
  DCI.FieldSize := GetFieldSize(C);
  DCI.InitField := ((C is TdxCalcEdit) or (C is TdxCheckBox)) and not FormExists(FmId);
  DCI.Status := dstNew;
  Add(DCI);
end;

procedure TDesignCache.DeleteForm(Fm: TdxForm);
var
  IsNew: Boolean;
  i, FmId: Integer;
  DCI: TDesignCacheItem;
begin
  IsNew := False;

  // Добавляем на удаление компоненты формы
  for i := 0 to Fm.ComponentCount - 1 do
    DeleteComponent(Fm.Components[i]);

  FmId := Fm.Id;
  for i := Count - 1 downto 0 do
  begin
    DCI := CacheItems[i];
    if DCI.IsForm and (DCI.Id = FmId) then
    begin
      if DCI.Status = dstNew then IsNew := True;
      DeleteCacheItem(DCI);
    end;
  end;

  // Добавляем форму на удаление только если она есть в базе.
  if not IsNew then
  begin
    DCI := TDesignCacheItem.Create;
    DCI.Id := FmId;
    DCI.ClsName := Fm.ClassName;
    DCI.Status := dstDelete;
    Add(DCI);
  end;
end;

procedure TDesignCache.DeleteComponent(C: TComponent);
var
  i, FmId, Id: Integer;
  DCI: TDesignCacheItem;
  IsNew: Boolean;
begin
  if not IsField(C) then Exit;

  IsNew := False;
  Id := GetId(C);
  FmId := GetId(C.Owner);
  // Удаляем все записи о компоненте из кэша
  for i := Count - 1 downto 0 do
  begin
    DCI := CacheItems[i];
    if (DCI.Id = Id) and (DCI.FmId = FmId) then
    begin
      if DCI.Status = dstNew then IsNew := True;
      DeleteCacheItem(DCI);
    end;
  end;
  // Добавляем элемент, только если компонент удаляется из существующей в базе формы
  if not IsNew then
  begin
    DCI := TDesignCacheItem.Create;
    DCI.Id := Id;
    DCI.FmId := FmId;
    DCI.ClsName := C.ClassName;
    DCI.Status := dstDelete;
    Add(DCI);
  end;
end;

procedure TDesignCache.SetFieldSize(C: TComponent; OldSize: Integer);
var
  Id, FmId, i, FieldSize: Integer;
  DCI: TDesignCacheItem;
begin
  Id := GetId(C);
  FmId := GetId(C.Owner);
  FieldSize := GetFieldSize(C);
  //OldSize := GetOldSize(C);
  for i := 0 to Count - 1 do
  begin
    DCI := CacheItems[i];
    if (DCI.Id = Id) and (DCI.FmId = FmId) then
    begin
      // dstFieldSize
      if DCI.Status <> dstNew then
      begin
        // Если ранее поменяли размер поля, а затем передумали, вернув старое значение,
        // то удаляем запись из кэша.
        if FieldSize = DCI.OldSize then
          DeleteCacheItem(DCI)
        else
          DCI.FieldSize := FieldSize;
      end
      else
        DCI.FieldSize := FieldSize;
      Exit;
    end;
  end;
  DCI := TDesignCacheItem.Create;
  DCI.Id := Id;
  DCI.FmId := FmId;
  DCI.ClsName := C.ClassName;
  DCI.Status := dstFieldSize;
  DCI.FieldSize := FieldSize;
  DCI.OldSize := OldSize;
  Add(DCI);
end;

procedure TDesignCache.SetCounter(C: TComponent; StartWith: Integer);
var
  i, Id, FmId: Integer;
  DCI: TDesignCacheItem;
begin
  Id := GetId(C);
  FmId := GetId(C.Owner);
  for i := 0 to Count - 1 do
  begin
    DCI := CacheItems[i];
    if (DCI.Id = Id) and (DCI.FmId = FmId) then
    begin
      if DCI.Status <> dstNew then
      begin
        DCI.StartWith := StartWith;
        Exit;
      end;
    end;
  end;
  DCI := TDesignCacheItem.Create;
  DCI.Id := Id;
  DCI.FmId := FmId;
  DCI.ClsName := C.ClassName;
  DCI.Status := dstCounter;
  DCI.StartWith := StartWith;
  Add(DCI);
end;

procedure TDesignCache.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    CacheItems[i].Free;
  inherited Clear;
end;

{ TDesignerBox }

procedure TDesignerBox.WMHScroll(var Message: TLMHScroll);
begin
  inherited WMHScroll(Message);
  if FormDesign.Active and (Length(FormDesign.Selected) <> 1) then Exit;
  if Message.ScrollCode <> 8 then HidePropsForm
  else UpdatePropsForm;
  if FIsWheel then UpdatePropsForm;
end;

procedure TDesignerBox.WMVScroll(var Message: TLMVScroll);
begin
  inherited WMVScroll(Message);
  if FormDesign.Active and (Length(FormDesign.Selected) <> 1) then Exit;
  if Message.ScrollCode <> 8 then HidePropsForm
  else UpdatePropsForm;
  if FIsWheel then UpdatePropsForm;
end;

procedure TDesignerBox.WMMouseWheel(var Message: TLMMouseEvent);
begin
  FIsWheel := True;
  inherited WMMouseWheel(Message);
  FIsWheel := False;
end;

procedure TDesignerBox.DoEnter;
begin
  inherited DoEnter;
  if FormDesign.Active then
    FormDesign.Container.SetFocus;
end;

procedure TDesignerBox.DoOnResize;
begin
  inherited DoOnResize;
  if FormDesign.Active then
    FormDesign.UpdateDesigner;
end;

procedure TDesignerBox.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if not HandleAllocated then Exit;
  if FormDesign.Active and (Length(FormDesign.Selected) <> 1) then Exit;
  UpdatePropsForm;
end;

procedure TDesignerBox.Click;
begin
  inherited Click;
  HidePropsForm;
end;

constructor TDesignerBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clSilver;
  HorzScrollBar.Smooth:=True;
  HorzScrollBar.Tracking:=True;
  VertScrollBar.Smooth:=True;
  VertScrollBar.Tracking:=True;
end;

{ TMySelector }

constructor TMySelector.Create(ASurface: TJvDesignSurface);
begin
  inherited Create(ASurface);
  HandleWidth := ScaleToScreen(HandleWidth);
end;

procedure TMySelector.AddToSelection(AValue: TControl);
begin
  inherited AddToSelection(AValue);
  if AppConfig.IsWine then AValue.Invalidate;
end;

{ TMyMessenger }

// Только для TJvDesignerWinControlMessenger
function TMyMessenger.IsDesignMessage(ASender: TControl; var AMessage: TLMessage
  ): Boolean;
begin
  Result := inherited IsDesignMessage(ASender, AMessage);
	  case AMessage.Msg of
  	  LM_SETFOCUS, LM_KEYDOWN: Container.SetFocus;
  	end;
end;

{ TMyController }

function TMyController.KeyDown(AKeyCode: Cardinal): Boolean;
var
  SState: TShiftState;
  Key: Word;
begin
  SState := GetKeyShiftState;
  if (ssCtrl in SState) and (AKeyCode in [VK_C, VK_X, VK_V]) then
  begin
    case AKeyCode of
      VK_C: TFormDesigner(Surface).CopyComponents;
      VK_X: TFormDesigner(Surface).MoveComponents;
      VK_V: TFormDesigner(Surface).PasteComponents;
    end;
    Exit;
  end
  else if AKeyCode = VK_ESCAPE then Action(daSelectParent)
  else
  begin
    Key := AKeyCode;
    with TFormDesigner(Surface) do
      if OnKeyDown <> nil then OnKeyDown(Surface, Key, SState);
  end;

  if (AKeyCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) and
    ([ssCtrl, ssShift] * SState <> []) and not FKeyPressed and
    (Surface.OnChangingComponents <> nil) then
  begin
    Surface.DoChangingComponents;
    FKeyPressed := True;
  end;

  Result:=inherited KeyDown(AKeyCode);
end;

function TMyController.KeyUp(AKeyCode: Cardinal): Boolean;
begin
  Result := True;
  FKeyPressed := False;
  case AKeyCode of
    VK_DELETE: TFormDesigner(Surface).UserDeleteComponents;
    VK_PRIOR: TFormDesigner(Surface).BringToFront;
    VK_NEXT: TFormDesigner(Surface).SendToBack;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: DesignFr.UpdateStatusBar;
    else Result := False;
  end;
  //Result:=inherited KeyUp(AKeyCode);
end;

function TMyController.MouseDown(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseDown(Button, X, Y, TheMessage);
  FLeftButtonPressed := True;
end;

function TMyController.MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseMove(X, Y, TheMessage);
  if FLeftButtonPressed then HidePropsForm;
end;

function TMyController.MouseUp(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result:=inherited MouseUp(Button, X, Y, TheMessage);
  FLeftButtonPressed := False;
end;

procedure TFormDesigner.SurfaceAddComponent(Sender: TObject;
  aComponent: TComponent);

  procedure AddComponent(C: TComponent);
  {var
    S: String; }
  begin
    if not FNewControl then
    begin
      if HasFId(C) then FIdObjList.AddObj(C);
    end;
    SetId(C, DBase.GenId('gen_fid'));
    MakeUniqueFieldName(GetForm, C);
    {S := GetFieldName(C);
    if S <> '' then
    begin
      if FindComponentByFieldName(GetForm, S, C) <> nil then
        SetFieldName(C, S + IntToStr(GetId(C)))
    end
    else
      SetFieldName( C, GetUniqueFieldName(C) ); }

    with TdxForm(Container).Grid.Columns.Add do
    begin
      Tag:=GetId(C);
      FieldName:=FieldStr(C);
      if C is TdxLookupComboBox then
      begin
        FieldName := FieldName + 'l';
        with TdxLookupComboBox(C) do
        begin
          //InsertedValues.Clear;
          ClearInsertTableProps;
        end;
      end
      else if C is TdxCheckBox then
        ButtonStyle:=cbsCheckBoxColumn
      else if C is TdxDBImage then
        FieldName := FieldName + 'thumb'
      else if C is TdxFile then
        FieldName := FieldName + 'd';
      {else if C is TdxObjectField then
        with TdxObjectField(C) do
        begin
          ObjId:=0;
          FieldId:=0;
        end; }
      Title.Caption := ' ';
      Width := ScaleToScreen(100);
    end;

    if FNewControl then DoInitControl(C);

    Cache.AddComponent(C);
    //if IsField(C) then FStructChanged := True;
  end;

  procedure AddGrid(C: TComponent);
  var
    Fm: TdxForm;
  begin
    Fm := FormMan.CreateNewChildForm(TdxForm(Container).Id);
    //Fm.PId:=TdxForm(Container).Id;
    SetId(C, Fm.Id);
    DesignFr.FormsTreeView.AddForm(Fm, True);
    Cache.AddForm(Fm);
    FormChanges.AddForm(Fm.Id, 0);
    //FStructChanged := True;
  end;

  procedure AddQueryGrid(C: TComponent);
  var
    RD, SrcRD: TReportData;
    QG: TdxQueryGrid;
    OldId: Integer;
    MS: TMemoryStream;
  begin
    QG :=  TdxQueryGrid(C);
    RD := ReportMan.CreateNewReport;
    RD.Kind:=rkQuery;
    if QG.Id = 0 then
      RD.Name := ReportMan.MakeUniqueName(rsQuery)  //rsQuery + IntToStr(RD.Id)
    else
    begin
      OldId := RD.Id;
      SrcRD := ReportMan.FindReport(QG.Id);
      if SrcRD <> nil then
      begin
        MS := TMemoryStream.Create;
        SrcRD.SaveToStream(MS);
        MS.Position:=0;
        RD.LoadFromStream(MS);
        MS.Free;
        RD.Name := ReportMan.MakeUniqueName(RD.Name); //RD.Name + IntToStr(OldId);
        RD.Id := OldId;
      end
      else
        RD.Name := ReportMan.MakeUniqueName(rsQuery) //rsQuery + IntToStr(RD.Id)
    end;
    QG.Id:=RD.Id;
  end;

  procedure ChangeOwner(Ctrl: TWinControl);
  var
    //i: Integer;
    C: TComponent;
    Bn: TSpeedButton;
  begin
    //for i := Ctrl.ComponentCount - 1 downto 0 do
    while Ctrl.ComponentCount > 0 do
    begin
      C := Ctrl.Components[0];
      Ctrl.RemoveComponent(C);
      //C.Name := DesignUniqueName(Container, C.ClassName);
      MakeUniqueName(Container, C);
      Container.InsertComponent(C);
      Messenger.DesignComponent(C, Active);
      if C is TCustomDBEditButton then
      begin
        Bn := GetEditButton(C);
        Bn.AnchorToCompanion(akLeft,0,TControl(C));
      end
      else if C is TdxLookupComboBox then
      begin
      	TdxLookupComboBox(C).DoPositionButton;
        TdxLookupComboBox(C).SetButtonState;
      end
      else if C is TMyDBGrid then
      	TMyDBGrid(C).PositionButtons;
      if HasFId(C) then AddComponent(C)
      else if C is TdxGrid then
      begin
        if Form.PId = 0 then AddGrid(C)
        else C.Free;
      end
      else if C is TdxQueryGrid then
      begin
        AddQueryGrid(C);
      end
    end;
  end;

begin
  AssignDesignMenu(aComponent);

  if (FNewControl) and (aComponent is TControl) then
    with TControl(aComponent) do
    begin
      //Left := (Left div GridSizeX) * GridSizeX;
      //Top := (Top div GridSizeY) * GridSizeY;
    end;
  if HasFId(aComponent) then
    AddComponent(aComponent)
  else if aComponent is TdxGrid then
  begin
    if Form.PId = 0 then
      AddGrid(AComponent)
    else
    begin
      Selector.RemoveFromSelection(TControl(AComponent));         // 29.05.15
      AComponent.Free;
    end;
  end
  else if aComponent is TdxQueryGrid then
  begin
    AddQueryGrid(aComponent);
  end
  else if aComponent is TTabSheet then
    DeleteComponents
  else if (aComponent is TdxGroupBox) or (aComponent is TdxPageControl) then
    ChangeOwner(TWinControl(aComponent))
  else if (aComponent is TdxLabel) and (Trim(TdxLabel(aComponent).Expression) <> '') then
  begin
    if CalcLabelWithSameCaptionExists(TdxLabel(aComponent)) then
    	with TdxLabel(aComponent) do
	    begin
    	  Caption := Caption + IntToStr(ComponentIndex);
        FieldName := Caption;
  	  end;
  end;
  if FNewControl then
  begin
    DesignFr.CompTree.UpdateTree;
    UpdateTemplateFieldsForm;
  end;
  FNewControl := False;
end;

procedure TFormDesigner.SurfaceGetAddClass(Sender: TObject; var ioClass: string
  );
begin
  ioClass := FControlClass;
  TDesignFr(Owner).CursorBn.Down := True;
  FControlClass := '';
  FNewControl := ioClass <> '';
end;

procedure TFormDesigner.SurfaceDeleteComponent(Sender: TObject;
  aComponent: TComponent);

  procedure DeleteGridColumn(C: TComponent);
  var
    i, CId: Integer;
  begin
    CId := GetId(C);
    DeleteReferences(C);
    UndoMan.CurrentCache.DeleteComponent(C);
    Cache.DeleteComponent(C);
    if FindExprFm <> nil then FindExprFm.DeleteComponent(C);
    with TdxForm(Container).Grid do
      for i := Columns.Count - 1 downto 0 do
      begin
        if Columns[i].Tag = CId then
        begin
          Columns.Delete(i);
          Exit;
        end;
      end;
  end;

  procedure DeleteGrid(C: TComponent);
  var
    Fm: TdxForm;
  begin
    Fm := FormMan.FindForm(GetId(C));
    DeleteQueries(Fm);
 		DeleteReferences(Fm);
    if ScriptFm <> nil then ScriptFm.DeleteForm(Fm.Id)
    else
    begin
      ScriptMan.DeleteFormModule(Fm.Id, skForm);
      ScriptMan.DeleteFormModule(Fm.Id, skWebForm);
    end;
    if FindActionsFm <> nil then FindActionsFm.DeleteForm(Fm);
    if FindExprFm <> nil then FindExprFm.DeleteForm(Fm);
    DesignFr.FormsTreeView.DeleteForm(Fm);
    Cache.DeleteForm(Fm);
    FormChanges.DeleteForm(Fm.Id);
    UndoMan.CurrentCache.DeleteComponent(C);
    UndoMan.DeleteCache(Fm);
    FormMan.DeleteForm(Fm.Id);
    UpdateTemplateFieldsForm;
  end;

  procedure DeleteQueryGrid(C: TComponent);
  var
    RD: TReportData;
  begin
    UndoMan.CurrentCache.DeleteComponent(C);
    if FindExprFm <> nil then FindExprFm.DeleteQuery(C);
    RD := ReportMan.FindReport(TdxQueryGrid(C).Id);
    ReportMan.DeleteReport(RD);
  end;

  procedure DeleteContainer(Cont: TWinControl);
  var
    i: Integer;
    C: TControl;
  begin
    UndoMan.CurrentCache.DeleteComponent(Cont);
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if (C is TdxGroupBox) or (C is TdxPageControl) or (C is TTabSheet) then
        DeleteContainer(TWinControl(C))
      else if C is TdxGrid then
        DeleteGrid(C)
      else if C is TdxQueryGrid then
        DeleteQueryGrid(C)
      else if HasFId(C) then
        DeleteGridColumn(C)
      else
        UndoMan.CurrentCache.DeleteComponent(C);
    end;
  end;

begin
  if aComponent is TdxGrid then
    DeleteGrid(aComponent)
  else if aComponent is TdxQueryGrid then
    DeleteQueryGrid(aComponent)
  else if HasFId(aComponent) then
    DeleteGridColumn(aComponent)
  else if (aComponent is TdxGroupBox) or (aComponent is TTabSheet) or
    (aComponent is TdxPageControl) then DeleteContainer(TWinControl(aComponent))
  else
  begin
    if aComponent is TdxButton then
    begin
      if FindActionsFm <> nil then FindActionsFm.DeleteButton(aComponent);
    end
    else if aComponent is TdxLabel then
    begin
      if FindExprFm <> nil then FindExprFm.DeleteComponent(aComponent);
    end;
    UndoMan.CurrentCache.DeleteComponent(aComponent);
  end;
end;

function TFormDesigner.GetControl: TControl;
begin
  Result := nil;
  if Length(Selected) > 0 then
    Result := Selection[0];
end;

function TFormDesigner.GetForm: TdxForm;
begin
  Result := TdxForm(Container);
end;

procedure TFormDesigner.SetActive(AValue: Boolean);
begin
  FMoves.Clear;
  FActive := AValue;
  inherited SetActive(AValue);
end;

function TFormDesigner.IsParentSelect(C: TControl): Boolean;
begin
  Result := False;
  if C is TdxForm then Exit;
  Result := Selector.IsSelected(C.Parent);
  if not Result then Result := IsParentSelect(C.Parent);
end;

function TFormDesigner.IsParent(C, aParent: TControl): Boolean;
begin
  Result := False;
  if C is TdxForm then Exit;
  Result := C.Parent = aParent;
  if not Result then Result := IsParent(C.Parent, aParent);
end;

function TFormDesigner.GetSelectionRect: TRect;
var
  i, L, x, y, r, b: Integer;
  C: TControl;
begin
  L := Length(Selected);
  x := 20000;
  for i := 0 to L - 1 do
    if Selection[i].Left < x then
      x := Selection[i].Left;
  y := 20000;
  for i := 0 to L - 1 do
    if Selection[i].Top < y then
      y := Selection[i].Top;
  r := 0;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    if C.Left + C.Width > r then
      r := C.Left + C.Width;
  end;
  b := 0;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    if C.Top + C.Height > b then
      b := C.Top + C.Height;
  end;
  Result := Rect(x, y, r, b);
end;

procedure TFormDesigner.AdjustPastedComponents;
var
  L, dx, dy, pw, ph: Integer;
  R: TRect;
  C: TControl;
  i: Integer;
  Par: TWinControl;
begin
  L := Length(Selected);
  if L = 0 then Exit;
  Par := Selection[0].Parent;
  pw := Par.Width;
  ph := Par.Height;
  R := GetSelectionRect;

  // Если область выделения вписывается в контейнер, то оставляем все как есть,
  // иначе перемещаем все в левый верхний угол.
  if (pw >= R.Right) and (ph >= R.Bottom) then Exit;
  dx := R.Right - pw; dy := R.Bottom - ph;
  if dx < 0 then dx := 0;
  if dy < 0 then dy := 0;
  if R.Left - dx < 0 then dx := R.Left;
  if R.Top - dy < 0 then dy := R.Top;
  for i := 0 to L - 1 do
  begin
    C := Selection[i];
    C.Left:=C.Left - dx;
    C.Top := C.Top - dy;
  end;
  UpdateDesigner;
end;

procedure TFormDesigner.SurfaceSelectionChange(Sender: TObject);
var
  i: Integer;
  C: TControl;
begin
  if Length(Selected) = 0 then
  begin
    ShowPropsForm(Container);
    DesignFr.CompTree.SelectRoot;
  end
  else
  begin
    OnSelectionChange:=nil;
    for i := Length(Selected) - 1 downto 0 do
    begin
      C := Selection[i];
      if C is TSpeedButton then
        Selector.RemoveFromSelection(C)
      else if IsParentSelect(C) then
      	Selector.RemoveFromSelection(C);
    end;
    OnSelectionChange:=@SurfaceSelectionChange;
    if Length(Selected) = 1 then
      ShowPropsForm(Selection[0])
    else
      HidePropsForm;
    DesignFr.CompTree.SelectComponents(Selected);
  end;
  DesignFr.UpdateStatusBar;
end;

constructor TFormDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMoves := TList.Create;
  FIdObjList := TIdObjList.Create;
  OnSelectionChange:=@SurfaceSelectionChange;
  OnGetAddClass:=@SurfaceGetAddClass;
  OnGetComponentName:=@SurfaceGetComponentName;
  OnAddComponent:=@SurfaceAddComponent;
  OnDeleteComponent:=@SurfaceDeleteComponent;
  ControllerClass:=TMyController;
  SelectorClass:=TMySelector;
  {$ifndef windows}
  MessengerClass := TMyMessenger;
  {$endif}
end;

destructor TFormDesigner.Destroy;
begin
  FIdObjList.Free;
  FMoves.Free;
  inherited Destroy;
end;

procedure TFormDesigner.DesignForm(Fm: TWinControl);
var
  i: Integer;
begin
  Container := Fm;
  if Fm = nil then
    Active := False;
  if Fm <> nil then
  begin
    Active := True;
    {$ifndef windows}
    for i := 0 to Fm.ComponentCount - 1 do
      AssignDesignMenu(Fm.Components[i]);
    {$endif}
  end;
end;

function TFormDesigner.GetLeftMostEdge: Integer;
var
  i, X: Integer;
begin
  Result := 100000;
  for i := 0 to Length(Selected) - 1 do
  begin
    X := Selection[i].Left;
    if X < Result then
      Result := X;
  end;
end;

procedure TFormDesigner.SetLeftMostEdge;
var
  i, MinX: Integer;
begin
  DoChangingComponents;
  MinX := GetLeftMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Left := MinX;
  UpdateDesigner;
end;

function TFormDesigner.GetTopMostEdge: Integer;
var
  i, Y: Integer;
begin
  Result := 100000;
  for i := 0 to Length(Selected) - 1 do
  begin
    Y := Selection[i].Top;
    if Y < Result then
      Result := Y;
  end;
end;

procedure TFormDesigner.SetTopMostEdge;
var
  i, MinY: Integer;
begin
  DoChangingComponents;
  MinY := GetTopMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Top := MinY;
  UpdateDesigner;
end;

function TFormDesigner.GetRightMostEdge: Integer;
var
  i, X: Integer;
begin
  Result := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    X := Selection[i].Left + Selection[i].Width;
    if X > Result then
      Result := X;
  end;
end;

procedure TFormDesigner.SetRightMostEdge;
var
  i, MaxX: Integer;
begin
  DoChangingComponents;
  MaxX := GetRightMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Left := MaxX - Selection[i].Width;
  UpdateDesigner;
end;

function TFormDesigner.GetBottomMostEdge: Integer;
var
  i, Y: Integer;
begin
  Result := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    Y := Selection[i].Top + Selection[i].Height;
    if Y > Result then
      Result := Y;
  end;
end;

procedure TFormDesigner.SetBottomMostEdge;
var
  i, MaxY: Integer;
begin
  DoChangingComponents;
  MaxY := GetBottomMostEdge;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Top := MaxY - Selection[i].Height;
  UpdateDesigner;
end;

procedure TFormDesigner.SetVertCenter;
var
  MinX, MaxX, i: Integer;
begin
  DoChangingComponents;
  MinX := GetLeftMostEdge;
  MaxX := GetRightMostEdge;
  for i := 0 to Length(Selected) - 1 do
    with Selection[i] do
      Left := (MaxX - MinX) div 2 + MinX - Width div 2;
  UpdateDesigner;
end;

procedure TFormDesigner.SetHorzCenter;
var
  MinY, MaxY, i: Integer;
begin
  DoChangingComponents;
  MinY := GetTopMostEdge;
  MaxY := GetBottomMostEdge;
  for i := 0 to Length(Selected) - 1 do
    with Selection[i] do
      Top := (MaxY - MinY) div 2 + MinY - Height div 2;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMaxWidth;
var
  MaxN, N, i: Integer;
begin
  DoChangingComponents;
  MaxN := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Width;
    if N > MaxN then
      MaxN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Width:=MaxN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMinWidth;
var
  MinN, N, i: Integer;
begin
  DoChangingComponents;
  MinN := 10000;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Width;
    if N < MinN then
      MinN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Width:=MinN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMaxHeight;
var
  MaxN, N, i: Integer;
begin
  DoChangingComponents;
  MaxN := 0;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Height;
    if N > MaxN then
      MaxN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Height:=MaxN;
  UpdateDesigner;
end;

procedure TFormDesigner.SetMinHeight;
var
  MinN, N, i: Integer;
begin
  DoChangingComponents;
  MinN := 10000;
  for i := 0 to Length(Selected) - 1 do
  begin
    N := Selection[i].Height;
    if N < MinN then
      MinN := N;
  end;
  for i := 0 to Length(Selected) - 1 do
    Selection[i].Height:=MinN;
  UpdateDesigner;
end;

procedure TFormDesigner.ClearClipboard;
begin
  with TJvDesignComponentClipboard.Create(Container) do
  try
    OpenWrite;
    CloseWrite;
  finally
    Free;
  end;
end;

procedure TFormDesigner.MoveComponents;
var
  i: Integer;
  C: TControl;
begin
  ClearClipboard;
  FMoves.Clear;
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    if (not (C is TdxTabSheet)) and (not IsParentSelect(C)) then
      FMoves.Add(Selection[i]);
  end;
end;

function TFormDesigner.CanMoveComponents: Boolean;
begin
  Result := FMoves.Count > 0;
end;

procedure TFormDesigner.PasteComponents;
begin
  if FMoves.Count > 0 then
  begin
    ChangeParent;
    FMoves.Clear;
  end
  else
  begin
    FIdObjList.Clear;
    inherited PasteComponents;
    // Добавление в список происходит в SurfaceAddComponent
    AdjustIdObjList;
  end;
  AdjustPastedComponents;

  DesignFr.CompTree.UpdateTree;
  DesignFr.CompTree.SelectComponents(Selected);
  DesignFr.SummaryTree.UpdateTree;
  UpdateTemplateFieldsForm;
end;

function TFormDesigner.CanPasteComponents: Boolean;
begin
  Result := False;
  with TJvDesignComponentClipboard.Create(Container) do
  try
    OpenRead;
    if GetComponent <> nil then Exit(True);
  finally
    CloseRead;
    Free;
  end;
end;

procedure TFormDesigner.DeleteComponents;

  procedure CheckMoves(Ctrl: TControl);
  var
    i: Integer;
  begin
    FMoves.Remove(Ctrl);
    if Ctrl is TWinControl then
      with TWinControl(Ctrl) do
        for i := 0 to ControlCount - 1 do
          CheckMoves(Controls[i]);
  end;

var
  i: Integer;
  C: TControl;
  Fm: TdxForm;
begin
  // Если есть таблицы, то проверяем возможность удаления
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    if C is TdxGrid then
    begin
      Fm := FormMan.FindForm(TdxGrid(C).Id);
    	if not DesignFr.CheckDeleteForm(Fm) then Exit;
    end;
  end;
  for i := 0 to Length(Selected) - 1 do
    CheckMoves(Selection[i]);
  inherited DeleteComponents;

  DesignFr.CompTree.UpdateTree;
  DesignFr.SummaryTree.UpdateTree;
  UpdateTemplateFieldsForm;
end;

procedure TFormDesigner.UserDeleteComponents;
begin
  if (Length(Selected) > 0) and ConfirmDelete and CheckDeleteComponents then
    DeleteComponents;
end;

procedure TFormDesigner.CopyComponents;
var
  i: Integer;
  C: TControl;
begin
  FMoves.Clear;
  for i := Length(Selected) - 1 downto 0 do
  begin
    C := Selection[i];
    if (C is TdxTabSheet) or (IsParentSelect(C)) then
      Selector.RemoveFromSelection(C);
  end;
  inherited CopyComponents;
end;

procedure TFormDesigner.ChangeParent;
var
  C: TWinControl;
  i: Integer;
  MovC: TControl;
begin
  if Length(Selected) = 0 then
    C := Container
  else
    C := TWinControl(Selection[0]);
  if (C is TdxForm) or (C is TdxGroupBox) or (C is TdxTabSheet) then
  begin
    ClearSelection;
    for i := 0 to FMoves.Count - 1 do
    begin
      MovC := TControl(FMoves[i]);
      if (MovC = C) or IsParent(C, MovC) then Continue;
      //MovC.Parent := C;
      Selector.AddToSelection(MovC);
    end;
    DoChangingComponents;
    for i := 0 to High(Selected) do
      Selection[i].Parent := C;
    UpdateDesigner;
  end;
end;

procedure TFormDesigner.FindLost;
var
  Fm: TdxForm;
  C: TComponent;
  i: Integer;
begin
  Selector.ClearSelection;
  Fm := GetForm;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TControl) then
      with TControl(C) do
        if (Left + Width < 0) or (Top + Height < 0) then
        begin
          //Left := 0; Top := 0;
          Selector.AddToSelection(TControl(C));
        end;
  end;
  DoChangingComponents;
  for i := 0 to High(Selected) do
    with Selection[i] do
    begin
      Left := 0; Top := 0;
    end;
  UpdateDesigner;
end;

procedure TFormDesigner.BringToFront;
var
  i: Integer;
  C: TControl;
begin
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    C.BringToFront;
  end;
end;

procedure TFormDesigner.SendToBack;
var
  i: Integer;
  C: TControl;
begin
  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    C.SendToBack;
  end;
end;

function TFormDesigner.CheckDeleteComponents: Boolean;
var
  C: TControl;
  i, fid, j, z: Integer;
  RD: TReportData;
  Sc: TRpSource;
  L: TList;
  RptMsg, QryMsg: String;
  Fm: TdxForm;

  function CheckField(const Fl: TRpField): Boolean;
  begin
    Result := True;
    if Fl.Fid = 0 then Exit;
    if Fl.FId = fid then
      Exit(False);
    if Fl.Src <> nil then
      Result := CheckField(Fl.Src^);
  end;

  function CheckFields(L: TRpFieldList): Boolean;
  var
    m: Integer;
    Fl: TRpField;
    Fm: TdxForm;
  begin
    Result := True;
    for m := 0 to L.Count - 1 do
    begin
      Fl := L[m]^;
      if not CheckField(Fl) then
      begin
        if RD.Kind = rkReport then
          RptMsg := RptMsg + '    ' + RD.Name + LineEnding
	        //ErrMsg(Format(rsCantDeleteFieldReport, [GetFieldName(C), RD.Name]))
        else if RD.Kind = rkQuery then
        begin
          Fm := FindFormByRDId(RD.Id);
          QryMsg := QryMsg + Format('    %s (%s)', [RD.Name, Fm.FormCaption]) + LineEnding;
          //ErrMsg(Format(rsCantDeleteFieldQuery, [GetFieldName(C), RD.Name, Fm.FormCaption]))
        end;
        Exit(False);
      end;
    end;
  end;

  procedure AddControls(WC: TWinControl);
  var
    j: Integer;
    C: TControl;
  begin
    for j := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[j];
      L.Add(C);
      if C is TWinControl then AddControls(TWinControl(C));
    end;
  end;

begin
  Result := True;
  RptMsg := ''; QryMsg := '';

  L := TList.Create;
  try

  for i := 0 to Length(Selected) - 1 do
  begin
    C := Selection[i];
    L.Add(C);
    if C is TWinControl then AddControls(TWinControl(C));
  end;

  Fm := TdxForm(Container);
  for i := 0 to L.Count - 1 do
  begin
    C := TControl(L[i]);

    if HasFId(C) and CheckExistsInActions(Fm, renField, GetFieldName(C)) then Exit(False);
    if (C is TdxQueryGrid) and CheckExistsInActions(Fm, renQuery, TdxQueryGrid(C).QueryName) then
      Exit(False);
    if CheckExistsInActions(Fm, renComponent, C.Name) then Exit(False);

    if C is TdxGrid then
    begin
      Result := DesignFr.CheckDeleteForm(FormMan.FindForm(TdxGrid(C).Id));
      if not Result then Exit;
    end
    else if C is TdxQueryGrid then
      CheckDeleteQuery(GetForm, TdxQueryGrid(C).Id);

    if not IsField(C) and not (C is TdxRecordId) then Continue;
    fid := GetId(C);

    for j := 0 to ReportMan.ReportCount - 1 do
    begin
      RD := ReportMan.Reports[j];
      for z := 0 to RD.Sources.Count - 1 do
      begin
        Sc := RD.Sources[z]^;
        if not CheckFields(Sc.Fields) then Break;
      end;
    end;

    if (RptMsg <> '') or (QryMsg <> '') then
    begin
      if RptMsg <> '' then RptMsg := Format(rsCantDelFormMsgRpt, [LineEnding + SortStr(RptMsg)]);
      if QryMsg <> '' then QryMsg := Format(rsCantDelFormMsgQry, [LineEnding + SortStr(QryMsg)]);
      ErrMsgFmt(rsCantDelFieldMsg, [GetFieldName(C), RptMsg + QryMsg]);
      Exit(False);
    end;
  end;

  finally
    L.Free;
  end;
end;

procedure TFormDesigner.AdjustIdObjList;

  procedure AdjustObject(C: TdxLookupComboBox);
  var
    IV: TInsertedValues;
    i: Integer;
    Item: TIdObjItem;
  begin
    IV := C.InsertedValues;
    for i := IV.Count - 1 downto 0 do
    begin
      Item := FIdObjList.FindById(IV[i].DestField);
      if Item = nil then IV.Delete(i)
      else IV[i].DestField := GetId(Item.Obj);
    end;
  end;

  procedure AdjustObjectField(C: TdxObjectField);
  var
    Item: TIdObjItem;
  begin
    Item := FIdObjList.FindById(C.ObjId);
    if Item = nil then
    begin
      C.ObjId := 0;
      C.FieldId := 0;
    end
    else
      C.ObjId := GetId(Item.Obj);
  end;

var
  i: Integer;
  Item: TIdObjItem;
  C: TComponent;
begin
  for i := 0 to FIdObjList.Count - 1 do
  begin
    Item := FIdObjList[i];
    C := Item.Obj;
    if C is TdxLookupComboBox then AdjustObject(TdxLookupComboBox(C))
    else if C is TdxObjectField then AdjustObjectField(TdxObjectField(C));
  end;
end;

procedure TFormDesigner.SurfaceGetComponentName(Sender: TObject;
  aComponent: TComponent);
begin
  MakeUniqueName(Container, aComponent);
end;

end.

