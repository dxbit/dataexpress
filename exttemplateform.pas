unit ExtTemplateForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, {$IFDEF WINDOWS}windows, {$ENDIF}SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts;

type

  { TExtTemplateFm }

  TExtTemplateFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ExtKindCbx: TComboBox;
    NameEd: TEdit;
    GroupEd: TEdit;
    FuncNameEd: TEdit;
    TypeNameEd: TEdit;
    VarNameEd: TEdit;
    ArrNameEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure ExtKindCbxChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSourceCode: String;
    procedure SetControlState;
    procedure GenerateFunction;
    function GenerateAction: String;
    procedure GenerateButtonAction;
    procedure GenerateFormAction;
    procedure GenerateStartupAction;
  public
    function ShowForm: Integer;
    property SourceCode: String read FSourceCode;
  end;

var
  ExtTemplateFm: TExtTemplateFm;


function ShowExtTemplateForm: Integer;

implementation

uses
  apputils;

function ShowExtTemplateForm: Integer;
begin
  if ExtTemplateFm = nil then
    ExtTemplateFm := TExtTemplateFm.Create(Application);
  Result := ExtTemplateFm.ShowForm;
end;

function CheckIdentName(const Ident: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(Ident) do
    case Ident[i] of
      '0'..'9':
        if i = 1 then
        begin
          Result := False;
          Break;
        end;
      'A'..'Z', 'a'..'z', '_': ;
      else
      begin
        Result := False;
        Break;
      end;
    end;
  if not Result then
    ErrMsg(rsIdentValidNameMsg);
end;

{$R *.lfm}

{ TExtTemplateFm }

procedure TExtTemplateFm.FormCreate(Sender: TObject);
begin
  Caption := rsPasteExtTemplate;
  Label1.Caption := rsExtensionKind;
  ExtKindCbx.Items.AddStrings([rsFunction, rsButtonAction, rsFormAction,
    rsStartupAction]);
  Label2.Caption := rsExtensionName;
  NameEd.TextHint := rsMyActionOrMyFunc;
  Label3.Caption := rsGroupName;
  GroupEd.TextHint := rsMyGroupMySubGroup;
  Label4.Caption := rsFunctionName;
  FuncNameEd.TextHint := 'MyFunction';
  Label5.Caption := rsTypeName;
  TypeNameEd.TextHint := 'TMyData';
  Label6.Caption := rsVariableName;
  VarNameEd.TextHint := 'MyData';
  Label7.Caption := rsArrayName;
  ArrNameEd.TextHint := 'MyArr';
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TExtTemplateFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then Exit;

  CanClose := False;
  if Trim(NameEd.Text) = '' then
  begin
    ErrMsg(rsEnterExtName);
    NameEd.SetFocus;
  end
  else if (ExtKindCbx.ItemIndex = 0) and not CheckIdentName(NameEd.Text) then
    NameEd.SetFocus
  else if Trim(GroupEd.Text) = '' then
  begin
    ErrMsg(rsEnterGroupName);
    GroupEd.SetFocus;
  end
  else if Trim(FuncNameEd.Text) = '' then
  begin
    ErrMsg(rsEnterFuncName);
    FuncNameEd.SetFocus;
  end
  else if not CheckIdentName(FuncNameEd.Text) then
    FuncNameEd.SetFocus
  else if ExtKindCbx.ItemIndex in [2, 3] then
  begin
    if Trim(TypeNameEd.Text) = '' then
    begin
      ErrMsg(rsEnterTypeName);
      TypeNameEd.SetFocus;
    end
    else if not CheckIdentName(TypeNameEd.Text) then
      TypeNameEd.SetFocus
    else if Trim(VarNameEd.Text) = '' then
    begin
      ErrMsg(rsEnterVarName);
      VarNameEd.SetFocus;
    end
    else if not CheckIdentName(VarNameEd.Text) then
      VarNameEd.SetFocus
    else if Trim(ArrNameEd.Text) = '' then
    begin
      ErrMsg(rsEnterArrayName);
      ArrNameEd.SetFocus;
    end
    else if not CheckIdentName(ArrNameEd.Text) then
      ArrNameEd.SetFocus
    else if CompareText(FuncNameEd.Text, TypeNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      TypeNameEd.SetFocus;
    end
    else if CompareText(FuncNameEd.Text, VarNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      VarNameEd.SetFocus;
    end
    else if CompareText(FuncNameEd.Text, ArrNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      ArrNameEd.SetFocus;
    end
    else if CompareText(TypeNameEd.Text, VarNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      VarNameEd.SetFocus;
    end
    else if CompareText(TypeNameEd.Text, ArrNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      ArrNameEd.SetFocus;
    end
    else if CompareText(VarNameEd.Text, ArrNameEd.Text) = 0 then
    begin
      ErrMsg(rsIdentNotSameNames);
      ArrNameEd.SetFocus;
    end
    else
      CanClose := True;
  end
  else
    CanClose := True;
end;

procedure TExtTemplateFm.ExtKindCbxChange(Sender: TObject);
begin
  SetControlState;
end;

procedure TExtTemplateFm.FormShow(Sender: TObject);
begin
  NameEd.SetFocus;
end;

procedure TExtTemplateFm.SetControlState;
var
  b: Boolean;
begin
  b := ExtKindCbx.ItemIndex in [2, 3];
  TypeNameEd.Enabled := b;
  VarNameEd.Enabled := b;
  ArrNameEd.Enabled := b;
end;

procedure TExtTemplateFm.GenerateFunction;
begin
  FSourceCode := LineEnding +
    '{@function' + LineEnding +
    'OrigName=' + FuncNameEd.Text + LineEnding +
    'Name=' + NameEd.Text + LineEnding +
    'Args=' + LineEnding +
    'Result=' + LineEnding +
    'Group=' + GroupEd.Text + LineEnding +
    'Description=' + LineEnding +
    '@}' + LineEnding +
    'function ' + FuncNameEd.Text + ': Variant;' + LineEnding +
    'begin' + LineEnding +
    LineEnding +
    'end;' + LineEnding;
end;

function TExtTemplateFm.GenerateAction: String;
var
  ExtKind, GuidStr: String;
  GUID: TGUID;
begin
  if CreateGUID(GUID) = 0 then
  begin
    GuidStr := GUIDToString(GUID);
    GuidStr := Copy(GuidStr, 2, Length(GuidStr) - 2)
  end
  else
    GuidStr := '';

  case ExtKindCbx.ItemIndex of
    1: ExtKind := 'button';
    2: ExtKind := 'form';
    3: ExtKind := 'main';
    4: ExtKind := 'all';
    else ExtKind := '';
  end;

  Result := LineEnding +
    '{@action' + LineEnding +
    'Id=' + GuidStr + LineEnding +
    'Target=' + ExtKind + LineEnding +
    'OrigName=' + FuncNameEd.Text + LineEnding +
    'Name=' + NameEd.Text + LineEnding +
    'Group=' + GroupEd.Text + LineEnding +
    'UI=<ui>' + LineEnding +
    '</ui>' + LineEnding +
    'Description=' + LineEnding +
    '@}' + LineEnding +
    LineEnding;
end;

procedure TExtTemplateFm.GenerateButtonAction;
begin
  FSourceCode := GenerateAction +
    'function ' + FuncNameEd.Text + ': Variant;' + LineEnding +
    'begin' + LineEnding +
    '  Result := True;' + LineEnding +
    'end;' + LineEnding; ;
end;

procedure TExtTemplateFm.GenerateFormAction;
var
  Vr, Arr: String;
begin
  Vr := VarNameEd.Text;
  Arr := ArrNameEd.Text;
  FSourceCode := GenerateAction +
    'type' + LineEnding +
    '  ' + TypeNameEd.Text + ' = record' + LineEnding +
    '    Form: TdxForm;' + LineEnding +
    '    OldFormDestroy: TNotifyEvent;' + LineEnding +
    '    Skip: Boolean;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    'var' + LineEnding +
    '  ' + Arr + ': array of ' + TypeNameEd.Text + ';' + LineEnding +
    LineEnding +
    'function Find' + Vr + '(AForm: TObject): Integer;' + LineEnding +
    'var' + LineEnding +
    '  i: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  for i := High(' + Arr + ') downto 0 do' + LineEnding +
    '    with ' + Arr + '[i] do' + LineEnding +
    '      if (Form = AForm) and not Skip then' + LineEnding +
    '      begin' + LineEnding +
    '        Result := i;' + LineEnding +
    '        Exit;' + LineEnding +
    '      end;' + LineEnding +
    '  RaiseException(erCustomError, ''' + rsFormNotFoundStr + ''');' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'procedure FormDestroy(Sender: TObject);' + LineEnding +
    'var' + LineEnding +
    '  n: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  n := Find' + Vr + '(Sender);' + LineEnding +
    '  with ' + Arr + '[n] do' + LineEnding +
    '  begin' + LineEnding +
    '    // Destroy any objects and other finishing actions...' + LineEnding +
    '    Form := nil;' + LineEnding +
    '    // Calling the old event handler' + LineEnding +
    '    if OldFormDestroy <> nil then' + LineEnding +
    '    try' + LineEnding +
    '      Skip := True;' + LineEnding +
    '      OldFormDestroy(Sender);' + LineEnding +
    '    finally' + LineEnding +
    '      Skip := False;' + LineEnding +
    '    end;' + LineEnding +
    '  end;' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'function ' + FuncNameEd.Text + ': Variant;' + LineEnding +
    'var' + LineEnding +
    '  ' + Vr + ': ' + TypeNameEd.Text + ';' + LineEnding +
    '  n: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  with ' + Vr + ' do' + LineEnding +
    '  begin' + LineEnding +
    '    Form := Self;' + LineEnding +
    '    OldFormDestroy := Self.OnDestroy;' + LineEnding +
    '    Self.OnDestroy := @FormDestroy;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    '  n := Length(' + Arr + ');' + LineEnding +
    '  SetLength(' + Arr + ', n + 1);' + LineEnding +
    '  ' + Arr + '[n] := ' + Vr + ';' + LineEnding +
    LineEnding +
    '  Result := True;' + LineEnding +
    'end;' + LineEnding
end;

procedure TExtTemplateFm.GenerateStartupAction;
var
  Vr, Arr: String;
begin
  Vr := VarNameEd.Text;
  Arr := ArrNameEd.Text;
  FSourceCode := GenerateAction +
    'type' + LineEnding +
    '  ' + TypeNameEd.Text + ' = record' + LineEnding +
    '    OldDatabaseClose: TNotifyEvent;' + LineEnding +
    '    Skip: Boolean;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    'var' + LineEnding +
    '  ' + Arr + ': array of ' + TypeNameEd.Text + ';' + LineEnding +
    LineEnding +
    'function Find' + Vr + ': Integer;' + LineEnding +
    'var' + LineEnding +
    '  i: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  for i := High(' + Arr + ') downto 0 do' + LineEnding +
    '    with ' + Arr + '[i] do' + LineEnding +
    '      if not Skip then' + LineEnding +
    '      begin' + LineEnding +
    '        Result := i;' + LineEnding +
    '        Exit;' + LineEnding +
    '      end;' + LineEnding +
    '  RaiseException(erCustomError, ''' + rsElemNotFoundStr + ''');' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'procedure DatabaseClose(Sender: TObject);' + LineEnding +
    'var' + LineEnding +
    '  n: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  n := Find' + Vr + ';' + LineEnding +
    '  with ' + Arr + '[n] do' + LineEnding +
    '  begin' + LineEnding +
    '    // Destroy any objects and other finishing actions...' + LineEnding +
    LineEnding +
    '    // Calling the old event handler' + LineEnding +
    '    if OldDatabaseClose <> nil then' + LineEnding +
    '    try' + LineEnding +
    '      Skip := True;' + LineEnding +
    '      OldDatabaseClose(Sender);' + LineEnding +
    '    finally' + LineEnding +
    '      Skip := False;' + LineEnding +
    '    end;' + LineEnding +
    '  end;' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'function ' + FuncNameEd.Text + ': Variant;' + LineEnding +
    'var' + LineEnding +
    '  ' + Vr + ': ' + TypeNameEd.Text + ';' + LineEnding +
    '  n: Integer;' + LineEnding +
    'begin' + LineEnding +
    '  with ' + Vr + ' do' + LineEnding +
    '  begin' + LineEnding +
    '    OldDatabaseClose := MainWindow.OnDatabaseClose;' + LineEnding +
    '    MainWindow.OnDatabaseClose := @DatabaseClose;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    '  n := Length(' + Arr + ');' + LineEnding +
    '  SetLength(' + Arr + ', n + 1);' + LineEnding +
    '  ' + Arr + '[n] := ' + Vr + ';' + LineEnding +
    LineEnding +
    '  Result := True;' + LineEnding +
    'end;' + LineEnding
end;

function TExtTemplateFm.ShowForm: Integer;
begin
  ExtKindCbx.ItemIndex := 0;
  NameEd.Text := '';
  GroupEd.Text := '';
  FuncNameEd.Text := '';
  TypeNameEd.Text := '';
  VarNameEd.Text := '';
  ArrNameEd.Text := '';
  SetControlState;

  Result := ShowModal;
  if Result = mrOk then
    case ExtKindCbx.ItemIndex of
      0: GenerateFunction;
      1, 4: GenerateButtonAction;
      2: GenerateFormAction;
      3: GenerateStartupAction;
    end;
end;

end.

