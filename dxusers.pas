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

unit DXUsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls;

type

  { TdxControlRight }

  TdxControlRight = class
  private
    FEditing: Boolean;
    FName: String;
    FVisible: Boolean;
  public
    property Name: String read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Editing: Boolean read FEditing write FEditing;
  end;

  { TdxControlRightList }

  TdxControlRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxControlRight;
  public
    function AddRight: TdxControlRight;
    function FindRight(const aName: String): TdxControlRight;
    procedure Clear; override;
    procedure DeleteRight(aCR: TdxControlRight);
    property Rights[Index: Integer]: TdxControlRight read GetRights; default;
  end;

  { TdxFormRight }

  TdxFormRight = class
  private
    FAdding: Boolean;
    FApplySelCondToObj: Boolean;
    FControls: TdxControlRightList;
    FDelCond: String;
    FEditCond: String;
    FSelCond: String;
    FVisible: Boolean;
    FDeleting: Boolean;
    FEditing: Boolean;
    FFormId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAccess: Integer;
    procedure SetAccess(Value: Integer);
    property FormId: Integer read FFormId write FFormId;
    property Visible: Boolean read FVisible write FVisible;
    property Adding: Boolean read FAdding write FAdding;
    property Editing: Boolean read FEditing write FEditing;
    property Deleting: Boolean read FDeleting write FDeleting;
    property Controls: TdxControlRightList read FControls;
    property SelCond: String read FSelCond write FSelCond;
    property EditCond: String read FEditCond write FEditCond;
    property DelCond: String read FDelCond write FDelCond;
    property ApplySelCondToObj: Boolean read FApplySelCondToObj write FApplySelCondToObj;
  end;

  { TdxFormRightList }

  TdxFormRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxFormRight;
  public
    function AddRight: TdxFormRight;
    function FindRight(FmId: Integer): TdxFormRight;
    procedure DeleteRight(aFR: TdxFormRight);
    procedure Clear; override;
    property Rights[Index: Integer]: TdxFormRight read GetRights; default;
  end;

  { TdxReportRight }

  TdxReportRight = class
  private
    FReportId: Integer;
    FVisible: Boolean;
  public
    property ReportId: Integer read FReportId write FReportId;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { TdxReportRightList }

  TdxReportRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxReportRight;
  public
    function AddRight: TdxReportRight;
    function FindRight(RpId: Integer): TdxReportRight;
    procedure DeleteRight(aRR: TdxReportRight);
    procedure Clear; override;
    property Rights[Index: Integer]: TdxReportRight read GetRights; default;
  end;

  TdxIntf = class;

  { TdxRole }

  TdxRole = class
  private
    FFormRights: TdxFormRightList;
    FId: Integer;
    FIntfId: Integer;
    FName: String;
    FReportRights: TdxReportRightList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIntf: TdxIntf;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property FormRights: TdxFormRightList read FFormRights;
    property ReportRights: TdxReportRightList read FReportRights;
    property IntfId: Integer read FIntfId write FIntfId;
  end;

  { TdxRoleList }

  TdxRoleList = class(TList)
  private
    function GetRoles(Index: Integer): TdxRole;
    function GetUniqueId: Integer;
  public
    function AddRole: TdxRole;
    function FindRole(aId: Integer): TdxRole;
    function FindRoleByName(aName: String): TdxRole;
    procedure DeleteRole(aR: TdxRole);
    procedure Clear; override;
    procedure SaveToStream(St: TStream);
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(L: TStrings);
    property Roles[Index: Integer]: TdxRole read GetRoles; default;
  end;

  { TdxUser }

  TdxUser = class
  private
    FId: Integer;
    FAllowMultiAuth: Boolean;
    FName: String;
    FPassword: String;
    FRoleId: Integer;
    FAllowSingleMode: Boolean;
    FHidden: Boolean;
    FWasDeveloper: Boolean;
  public
    function GetRole: TdxRole;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Password: String read FPassword write FPassword;
    property RoleId: Integer read FRoleId write FRoleId;
    property AllowSingleMode: Boolean read FAllowSingleMode write FAllowSingleMode;
    property AllowMultiAuth: Boolean read FAllowMultiAuth write FAllowMultiAuth;
    property Hidden: Boolean read FHidden write FHidden;
    property WasDeveloper: Boolean read FWasDeveloper write FWasDeveloper;
  end;

  { TdxUserList }

  TdxUserList = class(TList)
  private
    function GetUsers(Index: Integer): TdxUser;
    function GetUniqueId: Integer;
  public
    function AddUser: TdxUser;
    function FindUser(aId: Integer): TdxUser;
    function FindUserByName(const aName: String): TdxUser;
    procedure Clear; override;
    procedure DeleteUser(aU: TdxUser);
    procedure SaveToStream(St: TStream);
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(L: TStrings);
    property Users[Index: Integer]: TdxUser read GetUsers; default;
  end;

  TdxMenuItemKind = (miMenu, miDiv, miForm, miReport);

  TdxMenuItemList = class;

  { TdxMenuItem }

  TdxMenuItem = class
  private
    FCaption: String;
    FId: Integer;
    FItems: TdxMenuItemList;
    FKind: TdxMenuItemKind;
  public
    constructor Create;
    destructor Destroy; override;
    property Caption: String read FCaption write FCaption;
    property Kind: TdxMenuItemKind read FKind write FKind;
    property Id: Integer read FId write FId;
    property Items: TdxMenuItemList read FItems;
  end;

  { TdxMenuItemList }

  TdxMenuItemList = class(TList)
  private
    function GetMenuItems(Index: Integer): TdxMenuItem;
  public
    function AddItem: TdxMenuItem;
    procedure DeleteItem(Item: TdxMenuItem);
    procedure Clear; override;
    property MenuItems[Index: Integer]: TdxMenuItem read GetMenuItems; default;
  end;

  { TdxTabList }

  TdxTabList = class(TList)
  private
    function GetTabs(Index: Integer): Integer;
  public
    function AddTab(FmId: Integer): Integer;
    property Tabs[Index: Integer]: Integer read GetTabs; default;
  end;

  { TdxIntf }

  TdxIntf = class
  private
    FId: Integer;
    FIsDefault: Boolean;
    FMenu: TdxMenuItemList;
    FName: String;
    FTabs: TdxTabList;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Tabs: TdxTabList read FTabs;
    property Menu: TdxMenuItemList read FMenu;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  end;

  { TdxIntfList }

  TdxIntfList = class(TList)
  private
    function GetIntfs(Index: Integer): TdxIntf;
    function GetUniqueId: Integer;
  public
    function AddIntf: TdxIntf;
    function FindIntf(aId: Integer): TdxIntf;
    function FindIntfByName(const aName: String): TdxIntf;
    function FindDefaultIntf: TdxIntf;
    procedure DeleteIntf(Intf: TdxIntf);
    procedure Clear; override;
    procedure SaveToStream(St: TStream);
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(SL: TStrings);
    property Intfs[Index: Integer]: TdxIntf read GetIntfs; default;
  end;

  { TdxUserManager }

  TdxUserManager = class
  private
    FConnId: Integer;
    FCurUserId: Integer;
    FIntfsChanged: Boolean;
    FIntfs: TdxIntfList;
    FLastModified: TDateTime;
    FRoles: TdxRoleList;
    FRolesChanged: Boolean;
    FSingleMode: Boolean;
    FUsers: TdxUserList;
    FUsersChanged: Boolean;
    FRecordNotExists: Boolean;
    function GetConnectionId: Integer;
    function GetFormRight(FmId: Integer): TdxFormRight;
    function GetReportRight(RpId: Integer): TdxReportRight;
    procedure RemoveBrokenConn;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure SaveToDir(const aDir: String);
    procedure LoadFromDir(const aDir: String);
    //procedure LoadFromCache(const aDir: String);
    function CurrentUser: TdxUser;
    function CurrentUserName: String;
    function CheckFmVisible(FmId: Integer): Boolean;
    function CheckFmAdding(FmId: Integer): Boolean;
    function CheckFmEditing(FmId: Integer): Boolean;
    function CheckFmDeleting(FmId: Integer): Boolean;
    //function CheckFmExporting(FmId: Integer): Boolean;
    function CheckRpVisible(RpId: Integer): Boolean;
    function CheckControlVisible(FmId: Integer; const aName: String): Boolean;
    function CheckControlEditing(FmId: Integer; const aName: String): Boolean;
    function GetSelCond(FmId: Integer): String;
    function GetEditCond(FmId: Integer): String;
    function GetDelCond(FmId: Integer): String;
    function GetApplySelCondToObj(FmId: Integer): Boolean;
    function GetIntf: TdxIntf;
    function RegisterUser: Integer;
    procedure UnRegisterUser;
    function CloneManager: TdxUserManager;
    function IsUser: Boolean;
    function CanInput(C: TComponent): Boolean;
    procedure SetUsersChanged;
    procedure SetRolesChanged;
    procedure SetIntfsChanged;
    property Users: TdxUserList read FUsers;
    property Roles: TdxRoleList read FRoles;
    property Intfs: TdxIntfList read FIntfs;
    property CurrentUserId: Integer read FCurUserId write FCurUserId;
    property ConnId: Integer read FConnId write FConnId;
    property SingleMode: Boolean read FSingleMode write FSingleMode;
    property UsersChanged: Boolean read FUsersChanged write FUsersChanged;
    property RolesChanged: Boolean read FRolesChanged write FRolesChanged;
    property IntfsChanged: Boolean read FIntfsChanged write FIntfsChanged;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

procedure InitRole(R: TdxRole);
procedure InitFormRights(FR: TdxFormRight; Fm: TdxForm);
procedure CopyRole(Src, Dst: TdxRole);
procedure CopyFormRights(Src, Dst: TdxFormRight);
procedure CopyIntf(Src, Dst: TdxIntf);

var
  UserMan: TdxUserManager;

implementation

uses
  LazUtf8, sqldb, dbengine, apputils, SAX, saxbasereader, Db, mytypes,
  formmanager, dxreports, reportmanager, Buttons, myctrls, JvDesignImp,
  TACustomSeries;

type

  { TdxRolesReader }

  TdxRolesReader = class(TSAXBaseReader)
  private
    FRoles: TdxRoleList;
    FRole: TdxRole;
    FFR: TdxFormRight;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Roles: TdxRoleList read FRoles write FRoles;
  end;

  { TdxUsersReader }

  TdxUsersReader = class(TSAXBaseReader)
  private
    FUsers: TdxUserList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Users: TdxUserList read FUsers write FUsers;
  end;

  { TdxIntfsReader }

  TdxIntfsReader = class(TSAXBaseReader)
  private
    FIntfs: TdxIntfList;
    FIntf: TdxIntf;
    FParentMenu: TdxMenuItemList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    property Intfs: TdxIntfList read FIntfs write FIntfs;
  end;

procedure CopyReportRights(Src, Dst: TdxReportRight);
begin
  Dst.ReportId:=Src.ReportId;
  Dst.Visible:=Src.Visible;
end;

procedure CopyControlRights(Src, Dst: TdxControlRight);
begin
  Dst.Name:=Src.Name;
  Dst.Visible:=Src.Visible;
  Dst.Editing:=Src.Editing;
end;

procedure CopyFormRights(Src, Dst: TdxFormRight);
var
  i: Integer;
  CR: TdxControlRight;
begin
  Dst.FormId:=Src.FormId;
  Dst.Visible:=Src.Visible;
  Dst.Adding:=Src.Adding;
  Dst.Editing:=Src.Editing;
  Dst.Deleting:=Src.Deleting;
  Dst.SelCond:=Src.SelCond;
  Dst.EditCond:=Src.EditCond;
  Dst.DelCond:=Src.DelCond;
  Dst.ApplySelCondToObj:=Src.ApplySelCondToObj;
  Dst.Controls.Clear;
  for i := 0 to Src.Controls.Count - 1 do
  begin
    CR := Dst.Controls.AddRight;
    CopyControlRights(Src.Controls[i], CR);
  end;
end;

procedure CopyIntf(Src, Dst: TdxIntf);

  procedure CopyMenu(SL, DL: TdxMenuItemList);
  var
    i: Integer;
    MI, DMI: TdxMenuItem;
  begin
    for i := 0 to SL.Count - 1 do
    begin
      MI := SL[i];
      DMI := DL.AddItem;
      DMI.Kind := MI.Kind;
      DMI.Caption:=MI.Caption;
      DMI.Id := MI.Id;
      CopyMenu(MI.Items, DMI.Items)
    end;
  end;

  procedure CopyTabs(SL, DL: TdxTabList);
  var
    i: Integer;
  begin
    for i := 0 to SL.Count - 1 do
      DL.AddTab(SL[i]);
  end;

begin
  Dst.Menu.Clear;
  Dst.Tabs.Clear;
  CopyMenu(Src.Menu, Dst.Menu);
  CopyTabs(Src.Tabs, Dst.Tabs);
  Dst.Id := Src.Id;
  Dst.Name := Src.Name;
  Dst.IsDefault := Src.IsDefault;
end;

// Программа не отслеживает добавление/удаление форм и отчетов. Поэтому в списке могут
// присутствовать удаленные и отсутствовать новые. Приводим список в порядок.
procedure InitRole(R: TdxRole);
var
  i: Integer;
  FR: TdxFormRight;
  Fm: TdxForm;
  RD: TReportData;
  RR: TdxReportRight;
begin
  // Удаляем несуществующие формы
  for i := R.FormRights.Count - 1 downto 0 do
  begin
    FR := R.FormRights[i];
    Fm := FormMan.FindForm(FR.FormId);
    if Fm = nil then
      R.FormRights.DeleteRight(FR);
  end;
  // Добавляем новые
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    FR := R.FormRights.FindRight(Fm.Id);
    if FR = nil then
    begin
      FR := R.FormRights.AddRight;
      FR.FormId:=Fm.Id;
      FR.Visible:=True;
      FR.Adding:=True;
      FR.Editing:=True;
      FR.Deleting:=True;
    end;
  end;

  // Удаляем несуществующие отчеты
  for i := R.ReportRights.Count - 1 downto 0 do
  begin
    RR := R.ReportRights[i];
    RD := ReportMan.FindReport(RR.ReportId);
    if RD = nil then
      R.ReportRights.DeleteRight(RR);
  end;
  // Добавляем новые
  for i := 0 to ReportMan.ReportCount - 1 do
  begin
    RD := ReportMan.Reports[i];
    if RD.Kind <> rkReport then Continue;
    RR := R.ReportRights.FindRight(RD.Id);
    if RR = nil then
    begin
      RR := R.ReportRights.AddRight;
      RR.ReportId:=RD.Id;
      RR.Visible:=True;
    end;
  end;
end;

procedure InitFormRights(FR: TdxFormRight; Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
  CR: TdxControlRight;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxLabel) or (C is TSpeedButton) or (C is TdxShape) or (C is TGridButtons) or
      (C is TJvDesignHandle) or (C is TChartSeries) or (C.Name = '') then Continue;
    CR := FR.Controls.FindRight(C.Name);
    if (CR = nil) {and (C.Name <> '')} then
    begin
      CR := FR.Controls.AddRight;
      CR.Name := C.Name;
      CR.Visible := True;
      CR.Editing := True;
      if not HasFId(C) then CR.Editing:=False;
    end;
  end;
end;

procedure CopyRole(Src, Dst: TdxRole);
var
  i: Integer;
  FR: TdxFormRight;
  RR: TdxReportRight;
begin
  Dst.Id := Src.Id;
  Dst.Name := Src.Name;
  Dst.IntfId:=Src.IntfId;
  Dst.FormRights.Clear;
  for i := 0 to Src.FormRights.Count - 1 do
  begin
    FR := Dst.FormRights.AddRight;
    CopyFormRights(Src.FormRights[i], FR);
  end;
  Dst.ReportRights.Clear;
  for i := 0 to Src.ReportRights.Count - 1 do
  begin
    RR := Dst.ReportRights.AddRight;
    CopyReportRights(Src.ReportRights[i], RR);
  end;
end;

{ TdxIntfsReader }

procedure TdxIntfsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  MI: TdxMenuItem;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'interface' then
  begin
    FIntf := FIntfs.AddIntf;
    FIntf.Id := GetInt(Atts, 'id');
    FIntf.Name := XmlToStr(Atts.GetValue('', 'name'));
    FIntf.IsDefault:=GetBool(Atts, 'default');
    FParentMenu := FIntf.Menu;
  end
  else if LocalName = 'tab' then
  begin
    FIntf.Tabs.AddTab(GetInt(Atts, 'fmid'));
  end
  else if LocalName = 'menuitem' then
  begin
    MI := FParentMenu.AddItem;
    MI.Kind := TdxMenuItemKind(GetInt(Atts, 'kind'));
    MI.Caption := XmlToStr(Atts.GetValue('', 'caption'));
    MI.Id:=GetInt(Atts, 'id');
    FParentMenu := MI.Items;
  end;
end;

function FindParentMenu(Menu, Cur: TdxMenuItemList): TdxMenuItemList;
var
  i: Integer;
  MI: TdxMenuItem;
begin
  Result := nil;
  if Menu = Cur then Exit(Cur);
  for i := 0 to Menu.Count - 1 do;
  begin
    MI := Menu[i];
    if MI.Items = Cur then Exit(Menu)
    else Result := FindParentMenu(MI.Items, Cur)
  end;
end;

procedure TdxIntfsReader.DoEndElement(const NamespaceURI, LocalName,
  QName: SAXString);
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  if LocalName = 'menuitem' then
    FParentMenu := FindParentMenu(FIntf.Menu, FParentMenu);
end;

{ TdxTabList }

function TdxTabList.GetTabs(Index: Integer): Integer;
begin
  Result := PtrInt(Items[Index]);
end;

function TdxTabList.AddTab(FmId: Integer): Integer;
begin
  Result := Add(Pointer(FmId));
end;

{ TdxIntfList }

function TdxIntfList.GetIntfs(Index: Integer): TdxIntf;
begin
  Result := TdxIntf(Items[Index]);
end;

function TdxIntfList.GetUniqueId: Integer;
var
  i, n: Integer;
  Intf: TdxIntf;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    Intf := Intfs[i];
    if Intf.Id > n then n := Intf.Id;
  end;
  Result := n + 1;
end;

function TdxIntfList.AddIntf: TdxIntf;
begin
  Result := TdxIntf.Create;
  Result.Id := GetUniqueId;
  Add(Result);
end;

function TdxIntfList.FindIntf(aId: Integer): TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Intfs[i].Id = aId then Exit(Intfs[i]);
end;

function TdxIntfList.FindIntfByName(const aName: String): TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if MyUtf8CompareText(Intfs[i].Name, aName) = 0 then Exit(Intfs[i]);
end;

function TdxIntfList.FindDefaultIntf: TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Intfs[i].IsDefault then Exit(Intfs[i]);
end;

procedure TdxIntfList.DeleteIntf(Intf: TdxIntf);
begin
  Remove(Intf);
  FreeAndNil(Intf);
end;

procedure TdxIntfList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Intfs[i].Free;
  inherited Clear;
end;

procedure WrTabs(var S: String; Tabs: TdxTabList);
var
  i: Integer;
begin
  S := S + '<tabs>';
  for i := 0 to Tabs.Count - 1 do
    S := S + '<tab fmid="' + IntToStr(Tabs[i]) + '"/>';
  S := S + '</tabs>'
end;

procedure WrMenuItem(var S: String; MI: TdxMenuItem);
var
  i: Integer;
begin
  S := S + '<menuitem caption="' + StrToXml(MI.Caption) + '" kind="' + IntToStr(Ord(MI.Kind)) +
    '" id="' + IntToStr(MI.Id) + '"><items>';
  for i := 0 to MI.Items.Count - 1 do
    WrMenuItem(S, MI.Items[i]);
  S := S + '</items></menuitem>';
end;

procedure WrMenu(var S: String; Menu: TdxMenuItemList);
var
  i: Integer;
begin
  S := S + '<menu>';
  for i := 0 to Menu.Count - 1 do
    WrMenuItem(S, Menu[i]);
  S := S + '</menu>';
end;

procedure WrIntf(var S: String; Intf: TdxIntf);
begin
  S := S + '<interface id="' + IntToStr(Intf.Id) + '" name="' + StrToXml(Intf.Name) +
    '" default="' + Bool2Str(Intf.IsDefault) + '">';
  WrTabs(S, Intf.Tabs);
  WrMenu(S, Intf.Menu);
  S := S + '</interface>';
end;

procedure TdxIntfList.SaveToStream(St: TStream);
var
  S: String;
  i: Integer;
begin
  S := '<interfaces>';
  for i := 0 to Count - 1 do
    WrIntf(S, Intfs[i]);
  S := S + '</interfaces>';
  St.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TdxIntfList.LoadFromStream(St: TStream);
var
  R: TdxIntfsReader;
begin
  Clear;
  if St = nil then Exit;
  St.Position:=0;
  R := TdxIntfsReader.Create;
  try
    R.Intfs := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxIntfList.FillStrings(SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to Count - 1 do
    SL.AddObject(Intfs[i].Name, Intfs[i]);
end;

{ TdxIntf }

constructor TdxIntf.Create;
begin
  FTabs := TdxTabList.Create;
  FMenu := TdxMenuItemList.Create;
end;

destructor TdxIntf.Destroy;
begin
  FMenu.Free;
  FTabs.Free;
  inherited Destroy;
end;

{ TdxMenuItem }

constructor TdxMenuItem.Create;
begin
  FItems := TdxMenuItemList.Create;
end;

destructor TdxMenuItem.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TdxMenuItemList }

function TdxMenuItemList.GetMenuItems(Index: Integer): TdxMenuItem;
begin
  Result := TdxMenuItem(Items[Index]);
end;

function TdxMenuItemList.AddItem: TdxMenuItem;
begin
  Result := TdxMenuItem.Create;
  Add(Result);
end;

procedure TdxMenuItemList.DeleteItem(Item: TdxMenuItem);
begin
  Remove(Item);
  Item.Free;
end;

procedure TdxMenuItemList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    MenuItems[i].Free;
  inherited Clear;
end;

{ TdxFormRight }

constructor TdxFormRight.Create;
begin
  FControls := TdxControlRightList.Create;
  //FApplySelCondToObj := True;
end;

destructor TdxFormRight.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;

function TdxFormRight.GetAccess: Integer;
begin
  if Visible and Adding and Editing and Deleting then
    Result := 3
  else if Visible and Editing and not Deleting and not Adding then
    Result := 2
  else if Visible and not Editing and not Deleting and not Adding then
    Result := 1
  else if not Visible and not Editing and not Deleting and not Adding then
    Result := 0
  else
    Result := -1;
end;

procedure TdxFormRight.SetAccess(Value: Integer);
begin
  case Value of
    0:
      begin
        Visible:=False;
        Editing:=False;
        Adding:=False;
        Deleting:=False;
      end;
    1:
      begin
        Visible := True;
        Editing:=False;
        Adding:=False;
        Deleting:=False;
      end;
    2:
      begin
        Visible := True;
        Editing:=True;
        Adding:=False;
        Deleting:=False;
      end;
    3:
      begin
        Visible := True;
        Editing:=True;
        Adding:=True;
        Deleting:=True;
      end;
  end;
end;

{ TdxControlRightList }

function TdxControlRightList.GetRights(Index: Integer): TdxControlRight;
begin
  Result := TdxControlRight(Items[Index]);
end;

function TdxControlRightList.AddRight: TdxControlRight;
begin
  Result := TdxControlRight.Create;
  Add(Result);
end;

function TdxControlRightList.FindRight(const aName: String): TdxControlRight;
var
  i: Integer;
  R: TdxControlRight;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.Name = aName then Exit(R);
  end;
end;

procedure TdxControlRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

procedure TdxControlRightList.DeleteRight(aCR: TdxControlRight);
begin
  Remove(aCR);
  aCR.Free;
end;

function TdxUser.GetRole: TdxRole;
begin
  Result := UserMan.Roles.FindRole(FRoleId);
end;

{ TdxUsersReader }

procedure TdxUsersReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  U: TdxUser;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'user' then
  begin
    U := FUsers.AddUser;
    U.Id := GetInt(Atts, 'id');
    U.Name := XmlToStr(GetStr(Atts, 'name'));
    U.Password := GetStr(Atts, 'password');
    U.RoleId := GetInt(Atts, 'roleid');
    U.AllowSingleMode := GetBool(Atts, 'singlemode');
    U.AllowMultiAuth := GetBool(Atts, 'multiauth');
    U.Hidden := GetBool(Atts, 'visible');
  end;
end;

{ TdxRolesReader }

procedure TdxRolesReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  CR: TdxControlRight;
  RR: TdxReportRight;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'role' then
  begin
    FRole := FRoles.AddRole;
    FRole.Id := GetInt(Atts, 'id');
    FRole.Name := XmlToStr(Atts.GetValue('', 'name'));
    FRole.IntfId:=GetInt(Atts, 'intfid');
  end
  else if LocalName = 'fright' then
  begin
    FFR := FRole.FormRights.AddRight;
    FFR.FormId:=GetInt(Atts, 'form');
    FFR.Visible:=GetBool(Atts, 'visible');
    FFR.Adding:=GetBool(Atts, 'adding');
    FFR.Editing:=GetBool(Atts, 'editing');
    FFR.Deleting:=GetBool(Atts, 'deleting');
    //FFR.Exporting:=GetBool(Atts, 'exporting');
    FFR.SelCond:=XmlToStr(Atts.GetValue('', 'selcond'));
    FFR.EditCond:=XmlToStr(Atts.GetValue('', 'editcond'));
    FFR.DelCond := XmlToStr(Atts.GetValue('', 'delcond'));
    FFR.ApplySelCondToObj:=GetBool(Atts, 'applyselcondtoobj');
  end
  else if LocalName = 'cright' then
  begin
    CR := FFR.Controls.AddRight;
    CR.Name:=Atts.GetValue('', 'name');
    CR.Visible:=GetBool(Atts, 'visible');
    CR.Editing:=GetBool(Atts, 'editing');
  end
  else if LocalName = 'rright' then
  begin
    RR := FRole.ReportRights.AddRight;
    RR.ReportId:=GetInt(Atts, 'report');
    RR.Visible:=GetBool(Atts, 'visible');
  end;
end;

{ TdxUserManager }

constructor TdxUserManager.Create;
begin
  FUsers := TdxUserList.Create;
  FRoles := TdxRoleList.Create;
  FIntfs := TdxIntfList.Create;
  FCurUserId := -1;
end;

destructor TdxUserManager.Destroy;
begin
  FIntfs.Free;
  FRoles.Free;
  FUsers.Free;
  inherited Destroy;
end;

procedure TdxUserManager.Clear;
begin
  FIntfs.Clear;
  FRoles.Clear;
  FUsers.Clear;
  FCurUserId:=-1;
  FConnId := 0;
  FSingleMode:=False;
end;

function TdxUserManager.GetConnectionId: Integer;
begin
  with DBase.OpenDataSet('select current_connection from rdb$database') do
  begin
    Result := Fields[0].AsInteger;
    Free;
  end;
end;

function TdxUserManager.GetFormRight(FmId: Integer): TdxFormRight;
var
  U: TdxUser;
  R: TdxRole;
begin
  Result := nil;
  U := CurrentUser;
  if U = nil then Exit;
  R := U.GetRole;
  if R = nil then Exit;
  Result := R.FormRights.FindRight(FmId);
end;

function TdxUserManager.GetReportRight(RpId: Integer): TdxReportRight;
var
  U: TdxUser;
  R: TdxRole;
begin
  Result := nil;
  U := CurrentUser;
  if U = nil then Exit;
  R := U.GetRole;
  if R = nil then Exit;
  Result := R.ReportRights.FindRight(RpId);
end;

procedure TdxUserManager.LoadFromDb;
var
  DS: TSQLQuery;
  St: TStream;
begin
  Clear;
  FRecordNotExists := False;
  St := nil;
  DS := DBase.OpenDataSet('select id, users, roles, intfs, lastmodified from dx_users');
  try
    if DS.RecordCount = 0 then
    begin
      FRecordNotExists := True;
      Exit;
    end;
    St := nil;
    St := DS.CreateBlobStream(DS.Fields[1], bmRead);
    FUsers.LoadFromStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[2], bmRead);
    FRoles.LoadFromStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[3], bmRead);
    FIntfs.LoadFromStream(St);
    FLastModified := DS.Fields[4].AsDateTime;
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

procedure TdxUserManager.SaveToDb;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
begin
  if FRecordNotExists then
  begin
    //Debug('Новая запись dx_users');

    MS := TMemoryStream.Create;
    DS := DBase.CreateQuery('insert into dx_users (id, users, roles, intfs, lastmodified) ' +
      'values (:id, :users, :roles, :intfs, :lastmodified)');
    try
      DS.Prepare;
      DS.Params[0].AsInteger := 1;

      MS.Size := 0;
      FUsers.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[1].LoadFromStream(MS, ftMemo);

      MS.Size := 0;
      FRoles.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[2].LoadFromStream(MS, ftMemo);

      MS.Size := 0;
      FIntfs.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[3].LoadFromStream(MS, ftMemo);

      DS.Params[4].AsDateTime := FLastModified;

      DBase.ExecuteQuery(DS);

      FRecordNotExists:=False;
    finally
      DS.Free;
      MS.Free;
    end;

    Exit;
  end;

  if FUsersChanged then
  begin
    //Debug('Изменены пользователи');

    MS := TMemoryStream.Create;
    DS := DBase.CreateQuery('update dx_users set users=:users, lastmodified=:lastmodified where id=1');
    try
      DS.Prepare;

      FUsers.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[0].LoadFromStream(MS, ftMemo);

      DS.Params[1].AsDateTime := FLastModified;

      DBase.ExecuteQuery(DS);
    finally
      DS.Free;
      MS.Free;
    end;
  end;

  if FRolesChanged then
  begin
    //Debug('Изменены роли');

    MS := TMemoryStream.Create;
    DS := DBase.CreateQuery('update dx_users set roles=:roles, lastmodified=:lastmodified where id=1');
    try
      DS.Prepare;

      FRoles.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[0].LoadFromStream(MS, ftMemo);

      DS.Params[1].AsDateTime := FLastModified;

      DBase.ExecuteQuery(DS);
    finally
      DS.Free;
      MS.Free;
    end;
  end;

  if FIntfsChanged then
  begin
    //Debug('Изменен интерфейс');

    MS := TMemoryStream.Create;
    DS := DBase.CreateQuery('update dx_users set intfs=:intfs, lastmodified=:lastmodified where id=1');
    try
      DS.Prepare;

      FIntfs.SaveToStream(MS);
      MS.Position := 0;
      DS.Params[0].LoadFromStream(MS, ftMemo);

      DS.Params[1].AsDateTime := FLastModified;

      DBase.ExecuteQuery(DS);
    finally
      DS.Free;
      MS.Free;
    end;
  end;
  //Debug('');

  FUsersChanged := False;
  FRolesChanged := False;
  FIntfsChanged := False;
end;

{procedure TdxUserManager.SaveToDb;
var
  DS: TSQLQuery;
  St: TStream;
begin
  St := nil;
  DS := DBase.OpenDataSet('select id, users, roles, intfs from dx_users');
  try
    while not DS.Eof do
    	DS.Delete;

    DS.Append;
    DS.Fields[0].AsInteger := 1;
    St := DS.CreateBlobStream(DS.Fields[1], bmWrite);
    FUsers.SaveToStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[2], bmWrite);
    FRoles.SaveToStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[3], bmWrite);
    FIntfs.SaveToStream(St);
    DS.Post;
    DBase.ApplyDataSet(DS);
    //DBase.Commit;
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end; }

procedure TdxUserManager.SaveToDir(const aDir: String);
var
  FS: TFileStream;
begin
  FS := nil;
  try
    FS := TFileStream.Create(aDir + 'users', fmCreate + fmOpenWrite);
    FUsers.SaveToStream(FS);
    SetFileDateTime(FS.Handle, FLastModified);
    FS.Free;

    FS := TFileStream.Create(aDir + 'roles', fmCreate + fmOpenWrite);
    FRoles.SaveToStream(FS);
    SetFileDateTime(FS.Handle, FLastModified);
    FS.Free;

    FS := TFileStream.Create(aDir + 'intfs', fmCreate + fmOpenWrite);
    FIntfs.SaveToStream(FS);
    SetFileDateTime(FS.Handle, FLastModified);
  finally
    FreeAndNil(FS);
  end;
end;

procedure TdxUserManager.LoadFromDir(const aDir: String);
var
  FS: TFileStream;
  S: String;
begin
  Clear;
  FS := nil;
  try
    FS := TFileStream.Create(aDir + 'users', fmOpenRead + fmShareDenyNone);
    FUsers.LoadFromStream(FS);
    FreeAndNil(FS);
    FS := TFileStream.Create(aDir + 'roles', fmOpenRead + fmShareDenyNone);
    FRoles.LoadFromStream(FS);
    FreeAndNil(FS);
    S := aDir + 'intfs';
    if FileExists(S) then
    begin
      FS := TFileStream.Create(S, fmOpenRead + fmShareDenyNone);
      FIntfs.LoadFromStream(FS);
    end;

    FLastModified := GetFileDateTime(aDir + 'users');
  finally
    FreeAndNil(FS);
  end;
end;

{procedure TdxUserManager.LoadFromCache(const aDir: String);
var
  DS: TSQLQuery;
begin
  DS := DBase.OpenDataSet('select lastmodified from dx_users where id=1');
  if DS.Fields[0].AsDateTime <> FileDateToDateTime(FileAge(aDir + 'users')) then
  begin
    LoadFromDb;
    SaveToDir(aDir);
  end
  else
    LoadFromDir(aDir);
end;  }

function TdxUserManager.CurrentUser: TdxUser;
begin
  Result := FUsers.FindUser(FCurUserId);
end;

function TdxUserManager.CurrentUserName: String;
begin
  Result := '';
  if CurrentUser <> nil then Result := CurrentUser.Name;
end;

function TdxUserManager.CheckFmVisible(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR <> nil then
    Result := FR.Visible;
end;

function TdxUserManager.CheckFmAdding(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR <> nil then
    Result := FR.Adding;
end;

function TdxUserManager.CheckFmEditing(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR <> nil then
    Result := FR.Editing;
end;

function TdxUserManager.CheckFmDeleting(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR <> nil then
    Result := FR.Deleting;
end;

function TdxUserManager.CheckRpVisible(RpId: Integer): Boolean;
var
  RR: TdxReportRight;
begin
  Result := True;
  RR := GetReportRight(RpId);
  if RR <> nil then
    Result := RR.Visible;
end;

function TdxUserManager.CheckControlVisible(FmId: Integer; const aName: String
  ): Boolean;
var
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  Result := True;
  if aName = '' then Exit;
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  CR := FR.Controls.FindRight(aName);
  if CR = nil then Exit;
  Result := CR.Visible;
end;

function TdxUserManager.CheckControlEditing(FmId: Integer; const aName: String
  ): Boolean;
var
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  CR := FR.Controls.FindRight(aName);
  if CR = nil then Exit;
  Result := CR.Editing;
end;

function TdxUserManager.GetSelCond(FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  Result := FR.SelCond;
end;

function TdxUserManager.GetEditCond(FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  Result := FR.EditCond;
end;

function TdxUserManager.GetDelCond(FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  Result := FR.DelCond;
end;

function TdxUserManager.GetApplySelCondToObj(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := False;
  FR := GetFormRight(FmId);
  if FR = nil then Exit;
  Result := FR.ApplySelCondToObj;
end;

function TdxUserManager.GetIntf: TdxIntf;
var
  U: TdxUser;
  R: TdxRole;
begin
  Result := nil;
  U := CurrentUser;
  if U <> nil then
  begin
    R := U.GetRole;
    if R <> nil then
      Result := R.GetIntf;
  end;
  if Result = nil then
    Result := UserMan.Intfs.FindDefaultIntf;
end;

function SIdExists(DS: TDataSet; SId: Integer): Boolean;
begin
  Result := False;
  DS.First;
  while not DS.Eof do
  begin
    if DS.Fields[0].AsInteger = SId then Exit(True);
    DS.Next;
  end;
end;

procedure TdxUserManager.RemoveBrokenConn;
var
  MonDS, ConDS: TSQLQuery;
  SQL: String;
  cid: Integer;
begin
  SQL := '';
  MonDS := DBase.OpenDataSet('select mon$attachment_id from mon$attachments');
  ConDS := DBase.OpenDataSet('select id from dx_conn');
  while not ConDS.Eof do
  begin
    cid := ConDS.Fields[0].AsInteger;
    if not SIdExists(MonDS, cid) then
    begin
      SQL := SQL + 'delete from dx_lock where cid is null or cid=' + IntToStr(cid) + ';' +
        'delete from dx_conn where id=' + IntToStr(cid) + ';';
    end;
    ConDS.Next;
  end;
  MonDS.Free;
  ConDS.Free;
  DBase.Execute(SQL);
end;

function UserExists(DS: TDataSet; UId: Integer): Boolean;
begin
  Result := False;
  DS.First;
  while not DS.Eof do
  begin
    if DS.Fields[1].AsInteger = UId then
      Exit(True);
    DS.Next;
  end;
end;

function IsSingleMode(DS: TDataSet): Boolean;
begin
  Result := False;
  DS.First;
  while not DS.Eof do
  begin
    if DS.Fields[2].AsInteger = 1 then
      Exit(True);
    DS.Next;
  end;
end;

function TdxUserManager.RegisterUser: Integer;
var
  DS: TSQLQuery;
  cid: Integer;
  S, mode: String;
begin
  RemoveBrokenConn;
  Result := 0;
  S := 'select id, uid, mode, dtime from dx_conn';
  DS := DBase.OpenDataSet(S);
  try
    if CurrentUser.RoleId >= 0 then
    begin
      if IsSingleMode(DS) then Exit(1);
      if (CurrentUser.AllowMultiAuth = False) and UserExists(DS, CurrentUserId) then Exit(2);
      if FSingleMode and (DS.RecordCount > 0) then Exit(3);
    end
    // Для разработчика при монопольном входе, если есть пользователи...
    else if FSingleMode and (DS.RecordCount > 0) then Result := 10;
    cid := GetConnectionId;//GetMaxId(DS);
  finally
    //if Result in [1..3] then DBase.SafeCommit;
    DS.Free;
  end;
  mode := '0';
  if FSingleMode then mode := '1';
  S := 'insert into dx_conn (id, uid, mode, dtime) values (' + IntToStr(cid) +
    ',' + IntToStr(CurrentUserId) + ',' + mode + ',CURRENT_TIMESTAMP);';
  DBase.Execute(S);
  FConnId:=cid;
end;

procedure TdxUserManager.UnRegisterUser;
var
  S: String;
begin
  S := 'delete from dx_conn where id=' + IntToStr(FConnId) + ';';
  DBase.Execute(S);
end;

function TdxUserManager.CloneManager: TdxUserManager;
var
  MS: TMemoryStream;
begin
  Result := TdxUserManager.Create;
  MS := TMemoryStream.Create;

  UserMan.Users.SaveToStream(MS);
  Result.Users.LoadFromStream(MS);
  MS.Size := 0;

  UserMan.Roles.SaveToStream(MS);
  Result.Roles.LoadFromStream(MS);
  MS.Size := 0;

  UserMan.Intfs.SaveToStream(MS);
  Result.Intfs.LoadFromStream(MS);
  MS.Free;

  Result.CurrentUserId:=UserMan.CurrentUserId;
  Result.ConnId:=UserMan.ConnId;
  Result.SingleMode:=UserMan.SingleMode;
end;

function TdxUserManager.IsUser: Boolean;
begin
  Result := (CurrentUser <> nil) and (CurrentUser.RoleId >= 0);
end;

function TdxUserManager.CanInput(C: TComponent): Boolean;
var
  Fm: TdxForm;
begin
  Fm := TdxForm(C.Owner);
  if Fm.SoftCheck then
    Result := CheckControlVisible(Fm.Id, C.Name) and CheckControlEditing(Fm.Id, C.Name)
  else
    Result := True;
end;

procedure TdxUserManager.SetUsersChanged;
begin
  FUsersChanged:=True;
  FLastModified:=Now;
end;

procedure TdxUserManager.SetRolesChanged;
begin
  FRolesChanged:=True;
  FLastModified:=Now;
end;

procedure TdxUserManager.SetIntfsChanged;
begin
  FIntfsChanged:=True;
  FLastModified:=Now;
end;

{ TdxUserList }

function TdxUserList.GetUsers(Index: Integer): TdxUser;
begin
  Result := TdxUser(Items[Index]);
end;

function TdxUserList.GetUniqueId: Integer;
var
  i, n: Integer;
  U: TdxUser;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if U.Id > n then n := U.Id;
  end;
  Result := n + 1;
end;

function TdxUserList.AddUser: TdxUser;
begin
  Result := TdxUser.Create;
  Result.Id := GetUniqueId;
  Add(Result);
end;

function TdxUserList.FindUser(aId: Integer): TdxUser;
var
  U: TdxUser;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if U.Id = aId then Exit(U);
  end;
end;

function TdxUserList.FindUserByName(const aName: String): TdxUser;
var
  U: TdxUser;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if MyUtf8CompareText(aName, U.Name) = 0 then Exit(U);
  end;
end;

procedure TdxUserList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Users[i].Free;
  inherited Clear;
end;

procedure TdxUserList.DeleteUser(aU: TdxUser);
begin
  Remove(aU);
  aU.Free;
end;

procedure TdxUserList.SaveToStream(St: TStream);
var
  i: Integer;
  S: String;
  U: TdxUser;
begin
  S := '<users>';
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    S := S + '<user id="' + IntToStr(U.Id) + '" name="' + StrToXml(U.Name) +
      '" password="' + U.Password + '" roleid="' + IntToStr(U.RoleId) +
      '" singlemode="' + Bool2Str(U.AllowSingleMode) +
      '" multiauth="' + Bool2Str(U.AllowMultiAuth) +
      '" visible="' + Bool2Str(U.Hidden) + '"/>';
  end;
  S := S + '</users>';
  St.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TdxUserList.LoadFromStream(St: TStream);
var
  R: TdxUsersReader;
begin
  Clear;
  if St = nil then Exit;
  St.Position:=0;
  R := TdxUsersReader.Create;
  try
    R.Users := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxUserList.FillStrings(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
  U: TdxUser;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to UserMan.Users.Count - 1 do
  begin
    U := UserMan.Users[i];
    if not U.Hidden then
    	SL.Add(U.Name);
	    //SL.AddObject(U.Name, U);
  end;
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

{ TdxRoleList }

function TdxRoleList.GetRoles(Index: Integer): TdxRole;
begin
  Result := TdxRole(Items[Index]);
end;

function TdxRoleList.GetUniqueId: Integer;
var
  i, n: Integer;
  R: TdxRole;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    if R.Id > n then n := R.Id;
  end;
  Result := n + 1;
end;

function TdxRoleList.AddRole: TdxRole;
begin
  Result := TdxRole.Create;
  Result.Id := GetUniqueId;
  Result.IntfId:=-1;
  Add(Result);
end;

function TdxRoleList.FindRole(aId: Integer): TdxRole;
var
  R: TdxRole;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    if R.Id = aId then Exit(R);
  end;
end;

function TdxRoleList.FindRoleByName(aName: String): TdxRole;
var
  R: TdxRole;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    if MyUtf8CompareText(aName, R.Name) = 0 then Exit(R);
  end;
end;

procedure TdxRoleList.DeleteRole(aR: TdxRole);
begin
  Remove(aR);
  aR.Free;
end;

procedure TdxRoleList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Roles[i].Free;
  inherited Clear;
end;

procedure TdxRoleList.SaveToStream(St: TStream);
var
  S: String;
  i, j, z: Integer;
  R: TdxRole;
  FR: TdxFormRight;
  RR: TdxReportRight;
  CR: TdxControlRight;
begin
  S := '<roles>';
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    S := S + '<role id="' + IntToStr(R.Id) + '" name="' + StrToXml(R.Name) +
      '" intfid="' + IntToStr(R.IntfId) + '">';
    S := S + '<formrights>';
    for j := 0 to R.FormRights.Count - 1 do
    begin
      FR := R.FormRights[j];
      S := S + '<fright form="' + IntToStr(FR.FormId) +
        '" visible="' + Bool2Str(FR.Visible) +
        '" adding="' + Bool2Str(FR.Adding) + '" editing="' + Bool2Str(FR.Editing) +
        '" deleting="' + Bool2Str(FR.Deleting) + {'" exporting="' + Bool2Str(FR.Exporting) +}
        '" selcond="' + StrToXml(FR.SelCond) + '" editcond="' + StrToXml(FR.EditCond) +
        '" delcond="' + StrToXml(FR.DelCond) +
        '" applyselcondtoobj="' + Bool2Str(FR.ApplySelCondToObj) + '">';
      S := S + '<controlrights>';
      for z := 0 to FR.Controls.Count - 1 do
      begin
        CR := FR.Controls[z];
        S := S + '<cright name="' + CR.Name +
          '" visible="' + Bool2Str(CR.Visible) +
          '" editing="' + Bool2Str(CR.Editing) + '"/>';
      end;
      S := S + '</controlrights></fright>';
    end;
    S := S + '</formrights>';
    S := S + '<reportrights>';
    for j := 0 to R.ReportRights.Count - 1 do
    begin
      RR := R.ReportRights[j];
      S := S + '<rright report="' + IntToStr(RR.ReportId) +
        '" visible="' + Bool2Str(RR.Visible) + '"/>';
    end;
    S := S + '</reportrights>';
    S := S + '</role>';
  end;
  S := S + '</roles>';
  St.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TdxRoleList.LoadFromStream(St: TStream);
var
  R: TdxRolesReader;
begin
  Clear;
  if St = nil then Exit;
  St.Position:=0;
  R := TdxRolesReader.Create;
  try
    R.Roles := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxRoleList.FillStrings(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
  R: TdxRole;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    SL.AddObject(R.Name, R);
  end;
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

{ TdxReportRightList }

function TdxReportRightList.GetRights(Index: Integer): TdxReportRight;
begin
  Result := TdxReportRight(Items[Index]);
end;

function TdxReportRightList.AddRight: TdxReportRight;
begin
  Result := TdxreportRight.Create;
  Add(Result);
end;

function TdxReportRightList.FindRight(RpId: Integer): TdxReportRight;
var
  R: TdxReportRight;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.ReportId = RpId then Exit(R);
  end;
end;

procedure TdxReportRightList.DeleteRight(aRR: TdxReportRight);
begin
  Remove(aRR);
  aRR.Free;
end;

procedure TdxReportRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

{ TdxFormRightList }

function TdxFormRightList.GetRights(Index: Integer): TdxFormRight;
begin
  Result := TdxFormRight(Items[Index]);
end;

function TdxFormRightList.AddRight: TdxFormRight;
begin
  Result := TdxFormRight.Create;
  Add(Result);
end;

function TdxFormRightList.FindRight(FmId: Integer): TdxFormRight;
var
  i: Integer;
  R: TdxFormRight;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.FormId = FmId then Exit(R);
  end;
end;

procedure TdxFormRightList.DeleteRight(aFR: TdxFormRight);
begin
  Remove(aFR);
  aFR.Free;
end;

procedure TdxFormRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

{ TdxRole }

constructor TdxRole.Create;
begin
  FFormRights := TdxFormRightList.Create;
  FReportRights := TdxReportRightList.Create;
  FIntfId := -1;
end;

destructor TdxRole.Destroy;
begin
  FReportRights.Free;
  FFormRights.Free;
  inherited Destroy;
end;

function TdxRole.GetIntf: TdxIntf;
begin
  //if FIntfId >= 0 then
    Result := UserMan.Intfs.FindIntf(FIntfId)
  //else
  //  Result := UserMan.Intfs.FindDefaultIntf;
end;

end.

