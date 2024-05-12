unit MonitorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, Grids, Menus, ExtCtrls, EditBtn, sqldb, strconsts, DxCtrls,
  dbengine;

type

  TRecLock = class
    CId, UId, FmId, RecId: Integer;
  end;

  { TMonitorFm }

  TMonitorFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    MenuItem2: TMenuItem;
    PopupMenu2: TPopupMenu;
    UGrid: TStringGrid;
    RGrid: TStringGrid;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    PopupMenu1: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
    FMon: TDBEngine;
    FRecs: TList;
    procedure DeleteFromRGrid(CId: Integer);
    procedure FillUsers;
    procedure FillRecs;
    procedure SetControlState;
  public
    { public declarations }
    procedure ShowForm;
  end;

var
  MonitorFm: TMonitorFm;

procedure ShowMonitorForm;

implementation

uses
  dxusers, DateUtils, apputils, formmanager, helpmanager;

procedure ShowMonitorForm;
begin
  if MonitorFm = nil then
  	MonitorFm := TMonitorFm.Create(Application);
  MonitorFm.ShowForm;
end;

{$R *.lfm}

{ TMonitorFm }

procedure TMonitorFm.MenuItem1Click(Sender: TObject);
var
  id: Integer;
  S: String;
begin
  //FillUsers;
  if not ConfirmDelete then Exit;
  id := StrToInt(UGrid.Cells[0, UGrid.Row]);
  if id = UserMan.ConnId then
  begin
    ErrMsg(rsYouCantDelCurUser);
    Exit;
  end;
  S := 'delete from dx_conn where id=' + IntToStr(id) + ';' +
    'delete from dx_lock where cid=' + IntToStr(id) + ';';
  DBase.Execute(S);
  UGrid.DeleteRow(UGrid.Row);
  DeleteFromRGrid(id);
  SetControlState;
end;

procedure TMonitorFm.FormCreate(Sender: TObject);
begin
  Caption := rsUserMonitor;
  TabSheet1.Caption := rsActiveUsers;
  TabSheet2.Caption := rsEditableRecs;
  UGrid.Columns[0].Title.Caption := rsCID;
  UGrid.Columns[1].Title.Caption := rsUsername;
  UGrid.Columns[2].Title.Caption := rsIP;
  UGrid.Columns[3].Title.Caption := rsSingle;
  UGrid.Columns[4].Title.Caption := rsLoginTime;
  UGrid.Columns[5].Title.Caption := rsElapsedTime;
  RGrid.Columns[0].Title.Caption := rsCID;
  RGrid.Columns[1].Title.Caption := rsUsername;
  RGrid.Columns[2].Title.Caption := rsFormName;
  RGrid.Columns[3].Title.Caption := rsID;
  RGrid.Columns[4].Title.Caption := rsStarted;
  RGrid.Columns[5].Title.Caption := rsElapsedTime;
  MenuItem1.Caption := rsDelete;
  SetMenuItemImage(MenuItem1, 'delete16');
  MenuItem2.Caption := rsDelete;
  SetMenuItemImage(MenuItem2, 'delete16');
  ButtonPanel1.HelpButton.Caption := rsHelp;
  ButtonPanel1.CloseButton.Caption := rsClose;;
  FRecs := TList.Create;
end;

procedure TMonitorFm.FormDestroy(Sender: TObject);
begin
  ClearList(FRecs);
  FRecs.Free;
end;

procedure TMonitorFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('monitor');
end;

procedure TMonitorFm.MenuItem2Click(Sender: TObject);
var
  r: Integer;
  Rec: TRecLock;
  S: String;
begin
  //FillRecs;
  if not ConfirmDelete then Exit;
  r := RGrid.Row;
  Rec := TRecLock(FRecs[r - 1]);
  S := 'delete from dx_lock where cid=' + IntToStr(Rec.CId) + ' and ' +
    'uid=' + IntToStr(Rec.UId) + ' and fmid=' +
    IntToStr(Rec.FmId) + ' and recid=' + IntToStr(Rec.RecId) + ';';
  DBase.Execute(S);
  Rec.Free;
  FRecs.Delete(r - 1);
  RGrid.DeleteRow(r);
  SetControlState;
end;

procedure TMonitorFm.DeleteFromRGrid(CId: Integer);
var
  i: Integer;
  R: TRecLock;
begin
  for i := FRecs.Count - 1 downto 0 do
  begin
    R := TRecLock(FRecs[i]);
    if R.CId = CId then
    begin
      R.Free;
      FRecs.Delete(i);
      RGrid.DeleteRow(i + 1);
    end;
  end;
end;

procedure TMonitorFm.FillUsers;
var
  S: String;
  DS: TSQLQuery;
  r, mins, h, m: Integer;
  U: TdxUser;
begin
  UGrid.RowCount := 1;
  r := 1;
  S := 'select c.id, c.uid, m.mon$remote_address, c.mode, c.dtime, CURRENT_TIMESTAMP as ctime ' +
    'from dx_conn c left join mon$attachments m on c.id=m.mon$attachment_id order by c.id';
  DS := FMon.OpenDataSet(S);
  try
    while DS.Eof = False do
    begin
      UGrid.RowCount := r + 1;
      UGrid.Cells[0, r] := DS.Fields[0].AsString;
      U := UserMan.Users.FindUser(DS.Fields[1].AsInteger);
      if U <> nil then
        UGrid.Cells[1, r] := U.Name;
      UGrid.Cells[2, r] := DS.Fields[2].AsString;
      UGrid.Cells[3, r] := DS.Fields[3].AsString;
      UGrid.Cells[4, r] := DS.Fields[4].AsString;
      mins := MinutesBetween(DS.Fields[5].AsDateTime, DS.Fields[4].AsDateTime);
      h := mins div 60;
      m := mins - h * 60;
      UGrid.Cells[5, r] := Format('%s h %s m', [IntToStr(h), IntToStr(m)]);
      Inc(r);
      DS.Next;
    end;
  finally
    DS.Free;
    //FMon.ReadCommit;
  end;
end;

procedure TMonitorFm.FillRecs;
var
  S: String;
  DS: TSQLQuery;
  r, mins, h, m: Integer;
  U: TdxUser;
  Fm: TdxForm;
  Rec: TRecLock;
begin
  RGrid.RowCount := 1;
  r := 1;
  S := 'select cid, uid, fmid, recid, dtime, CURRENT_TIMESTAMP as ctime from dx_lock order by cid';
  DS := FMon.OpenDataSet(S);
  try
    while DS.Eof = False do
    begin
      RGrid.RowCount := r + 1;
      Rec := TRecLock.Create;
      Rec.CId := DS.Fields[0].AsInteger;
      Rec.UId:=DS.Fields[1].AsInteger;
      Rec.FmId := DS.Fields[2].AsInteger;
      Rec.RecId := DS.Fields[3].AsInteger;
      FRecs.Add(Rec);
      RGrid.Cells[0, r] := DS.Fields[0].AsString;
      U := UserMan.Users.FindUser(DS.Fields[1].AsInteger);
      if U <> nil then
        RGrid.Cells[1, r] := U.Name;
      Fm := FormMan.FindForm(DS.Fields[2].AsInteger);
      if Fm <> nil then
        RGrid.Cells[2, r] := GetFullCaption(Fm);
      RGrid.Cells[3, r] := DS.Fields[3].AsString;
      RGrid.Cells[4, r] := DS.Fields[4].AsString;
      mins := MinutesBetween(DS.Fields[5].AsDateTime, DS.Fields[4].AsDateTime);
      h := mins div 60;
      m := mins - h * 60;
      RGrid.Cells[5, r] := Format('%s h %s m', [IntToStr(h), IntToStr(m)]);
      Inc(r);
      DS.Next;
    end;
  finally
    DS.Free;
    //FMon.ReadCommit;
  end;
end;

procedure TMonitorFm.SetControlState;
begin
  MenuItem1.Enabled:=UGrid.Row > 0;
  MenuItem2.Enabled := RGrid.Row > 0;
end;

procedure TMonitorFm.ShowForm;
begin
  FMon := TDBEngine.Create;
  FMon.Pwd:=DBase.Pwd;
  FMon.Database:=DBase.Database;

  try
    FMon.Connect;

    ClearList(FRecs);
    FillUsers;
    FillRecs;
    SetControlState;
    ShowModal;
    FMon.Disconnect;

  finally
    FMon.Free;
  end;
end;

end.

