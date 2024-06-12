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

unit DebugScriptForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, Types, FileUtil, SynEdit, SynHighlighterPas,
  Forms, Controls, Graphics, Dialogs, ComCtrls, strconsts, scriptmanager,
  SynEditMiscClasses, uPSDebugger, uPSUtils, uPSRuntime, scriptedit, LclType,
  Menus, crossapi;

{ TDebugScriptFm }

type
  TDebugScriptFm = class(TForm)
    ImageList2: TImageList;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure EditShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { private declarations }
    FExec: TPSDebugExec;
    FSD, FOldSD: TScriptData;
    Edit: TScriptEdit;
    FFormsDisabled: TList;
    procedure EditBreakpointChanged(Sender: TObject; aLine: Integer);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function GetPaused: Boolean;
    function GetVarContents(const AName: tbtstring; AdvInfo: Boolean): tbtstring;
    procedure SetFormCaption(const aCaption: String);
    procedure SetControlState;
    //procedure UpdateBreakpoints;
    procedure UpdateStatusBar;
    function DebugGetValue(MousePos: TPoint; AdvInfo: Boolean): String;
  protected
    procedure SetVisible(Value: boolean); override;
  public
    { public declarations }
    procedure ShowForm(Exec: TPSDebugExec; SD: TScriptData; aLine: Integer);
    property Paused: Boolean read GetPaused;
  end;

var
  DebugScriptFm: TDebugScriptFm;

procedure ShowDebugForm(Exec: TPSDebugExec; SD: TScriptData; aLine: Integer);

implementation

uses
  appsettings, dxusers, apputils;

{$R *.lfm}

{ TDebugScriptFm }

procedure ExecIdleCall(Sender: TPSDebugExec);
begin
  Application.ProcessMessages;
end;

procedure ShowDebugForm(Exec: TPSDebugExec; SD: TScriptData; aLine: Integer);
var
  R: Types.TRect;
begin
  if (UserMan.CurrentUser <> nil) and (UserMan.CurrentUser.GetRole <> nil) then Exit;

  FillChar(R, SizeOf(R), 0);

  // Если при отладке открывается новое модальное окно, то окно отладки
  // становится неактивным. Чтобы получить управление окном, нужно его
  // пересоздать. Не нашел ничего лучше как проверять свойство активности окна.
  // Окно может быть неактивно и по другим причинам, но, думаю, лишнее
  // пересоздание не скажется на комфорте.
  if (DebugScriptFm <> nil) and (DebugScriptFm.Active = False) then
  begin
    R := DebugScriptFm.BoundsRect;
  	FreeAndNil(DebugScriptFm);
  end;

  if DebugScriptFm = nil then
  	DebugScriptFm := TDebugScriptFm.Create(nil);
  DebugScriptFm.ShowForm(Exec, SD, aLine);
  if R.Width > 0 then DebugScriptFm.BoundsRect := R;
end;

// Копипаст с uPsRuntime
(*function PropertyToString(Instance: TObject; PName: tbtString): tbtString;
var
  s: tbtString;
  i: Longint;
  PP: PPropInfo;
begin
  if PName = '' then
  begin
    Result := tbtString(Instance.ClassName);
    exit;
  end;
  while Length(PName) > 0 do
  begin
    i := pos(tbtChar('.'), pname);
    if i = 0 then
    begin
      s := Trim(PNAme);
      pname := '';
    end else begin
      s := trim(Copy(PName, 1, i-1));
      Delete(PName, 1, i);
    end;
    pp := GetPropInfo(PTypeInfo(Instance.ClassInfo), string(s));
    if pp = nil then begin Result := tbtstring('Unknown Identifier'); exit; end;


    case pp^.PropType^.Kind of
      tkInteger: begin Result := IntToStr(GetOrdProp(Instance, pp)); exit; end;
      tkChar: begin Result := '#'+IntToStr(GetOrdProp(Instance, pp)); exit; end;
      tkEnumeration: begin Result := tbtstring(GetEnumName(pp^.PropType{$IFNDEF FPC}{$IFDEF DELPHI3UP}^{$ENDIF}{$ENDIF}, GetOrdProp(Instance, pp))); exit; end;
      tkFloat: begin Result := FloatToStr(GetFloatProp(Instance, PP)); exit; end;
      tkString, tkAString, tkLString: begin Result := ''''+tbtString(GetStrProp(Instance, PP))+''''; exit; end;
      tkSet: begin Result := '[Set]'; exit; end;
      tkClass: begin Instance := TObject(Pointer(GetOrdProp(Instance, pp))); end;
      tkMethod: begin Result := '[Method]'; exit; end;
      tkVariant: begin Result := '[Variant]'; exit; end;
	  {$IFDEF DELPHI6UP}
	  {$IFNDEF PS_NOWIDESTRING}
      tkWString: begin Result := ''''+tbtString(GetWideStrProp(Instance, pp))+''; end;
	  {$IFDEF DELPHI2009UP}
      tkUString: begin Result := ''''+tbtString(GetUnicodeStrProp(Instance, pp))+''; end;
	  {$ENDIF}
      {$ENDIF}
	  {$ENDIF}
      else begin Result := '[Unknown]'; exit; end;
    end;
    if Instance = nil then begin result := 'nil'; exit; end;
  end;
  Result := tbtstring(Instance.ClassName);
end;   *)

(*function _GetObjectPropertyValue(Obj: TObject; const ClassProps: String): String;
var
  S, S1: String;
  p: SizeInt;
  V: Variant;
begin
  Result := '[Unknown]';
  if ClassProps = '' then
  begin
    if Obj <> nil then Exit(Obj.ClassName)
    else Exit('nil');
  end;

  S := ClassProps;
  S1 := S;
  p := Pos('.', S);
  if p > 0 then
  begin
    S1 := Copy(S, 1, p - 1);
    Delete(S, 1, p);
  end
  else S := '';

  {p := Pos('[', S1);
  if p > 0 then
  begin
    if not TryStrToInt(Copy(S1, p + 1, Pos(']', S1) - p - 1), Idx) then Exit;
    Delete(S1, p, 1024);
  end
  else Idx := -1; }

  Result := PropertyToString(Obj, S1);
  if (Result <> '[Unknown]') and (Result <> 'Unknown Identifier') then Exit;

  V := '[Unknown]';

  if Obj is TControl then
  	with TControl(Obj) do
			case S1 of
  	  	'CAPTION': V := QStr(Caption);
	  	end;
  if Obj is TComponent then
  	with TComponent(Obj) do
      case S1 of
        'COMPONENTCOUNT': V := ComponentCount;
        'COMPONENTINDEX': V := ComponentIndex;
        //'COMPONENTS': Result := GetObjectPropertyValue(Components[Idx], S);
        'OWNER': V := _GetObjectPropertyValue(Owner, S);
      end;
 { if Obj is TStringList then
  	with TStringList(Obj) do
      case S1 of
        'DUPLICATES': V := GetEnumName(GetTypeData();
      end;    }
  if Obj is TFileStream then
  	with TFileStream(Obj) do
    	case S1 of
        'HANDLE': V := Handle;
      end;
  if Obj is TStream then
  	with TStream(Obj) do
    	case S1 of
        'POSITION': V := Position;
        'SIZE': V := Size;
      end;
  if Obj is TCollectionItem then
  	with TCollectionItem(Obj) do
    	case S1 of
        'INDEX': V := Index;
        'COLLECTION': V := _GetObjectPropertyValue(Collection, S);
      end;
  if Obj is TCollection then
  	with TCollection(Obj) do
      case S1 of
        'COUNT': V := Count;
      end;
  if Obj is TObject then
  	case S1 of
      'CLASSNAME': V := QStr(Obj.ClassName);
    end;

  Result := VarToStr(V);
end;        *)

function GetObjectPropertyValueSource(Obj: TObject; const Source: String;
	out Value: String): Boolean;
var
  Comp: TEvalCompiler;
  RS: TRunScript;
  SD: TScriptData;
begin
  Result := False;
  SD := TScriptData.Create;
  SD.Source := Source;
  Comp := TEvalCompiler.Create;
  Comp.SD := SD;
  Comp.Obj := Obj;
  if Comp.CompileSource then
  begin
    RS := TRunScript.Create;
    try try
      RS.SD := SD;
      RS.LoadBin;
      SetVariantToClass(RS.Exec.GetVar2('OBJ'), Obj);
      if RS.Exec.RunScript then
      begin
	      Value := VGetString(RS.Exec.GetVar2('V'));
        Result := True;
      end;
    except
      ;
    end;

    finally
      RS.Free;
    end;
  end;

	Comp.Free;
  SD.Free;
end;

function GetObjectPropertyValue(Obj: TObject; const Props: String): String;
begin
  if GetObjectPropertyValueSource(Obj, Format('begin V := OBJ.%s; end.', [Props]), Result) then
  else if GetObjectPropertyValueSource(Obj, Format('begin if OBJ.%0:s <> nil then V := OBJ.%0:s.CLASSNAME else V := ''nil''; end.', [Props]), Result) then
  else if GetObjectPropertyValueSource(Obj, Format('begin if OBJ.%s = nil then V := ''nil'' else V := ''[Method]''; end.', [Props]), Result) then
  else Result := rsCanNotGetValue;
end;

function myPSVariantToString(const p: TPSVariantIFC; const ClassProperties: tbtstring): tbtstring;
var
  n: TbtDouble;
begin
  if p.dta <> nil then
  begin
    case p.aType.BaseType of
      btSingle: Exit(FloatToStr(tbtsingle(p.dta^)));
      btDouble:
        begin
          n := tbtdouble(p.dta^);
          if p.aType.ExportName = 'TDATETIME' then
            Exit(DateTimeToStr(n))
          else
            Exit(FloatToStr(n));
        end;
      btExtended: Exit(FloatToStr(tbtextended(p.dta^)));
    end;
  end;
  Result := PSVariantToString(p, ClassProperties);
end;

// Копипаст с компонента TPSScriptDebugger
function TDebugScriptFm.GetVarContents(const AName: tbtstring; AdvInfo: Boolean
  ): tbtstring;
var
  i: Longint;
  pv: PIFVariant;
  s1, s: tbtstring;
  Tmp: TPSVariantIFC;
  E: Extended;
begin
  s := Uppercase(AName);
  if pos('.', s) > 0 then
  begin
    s1 := copy(s,1,pos('.', s) -1);
    delete(s,1,pos('.', AName));
  end else begin
    s1 := s;
    s := '';
  end;
  pv := nil;
  for i := 0 to FExec.CurrentProcVars.Count -1 do
  begin
    if Uppercase(FExec.CurrentProcVars[i]) =  s1 then
    begin
      pv := FExec.GetProcVar(i);
      break;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to FExec.CurrentProcParams.Count -1 do
    begin
      if Uppercase(FExec.CurrentProcParams[i]) =  s1 then
      begin
        pv := FExec.GetProcParam(i);
        break;
      end;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to FExec.GlobalVarNames.Count -1 do
    begin
      if Uppercase(FExec.GlobalVarNames[i]) =  s1 then
      begin
        pv := FExec.GetGlobalVar(i);
        break;
      end;
    end;
  end;
  if pv = nil then
  begin
    Result := rsCanNotGetValue;// rsUnknownIdentifier //RPS_UnknownIdentifier
  end
  else
  begin
    Tmp := NewTPSVariantIFC(pv, False);
    Result := myPSVariantToString(Tmp, s);
  	if (Result = '[Unknown]') or (Result = 'Unknown Identifier') or (Result = '[Method]') then
    begin
      if AdvInfo and (Tmp.aType.BaseType = btClass) then
        Result := GetObjectPropertyValue( PSGetObject(Tmp.Dta, Tmp.aType), s )
      else Result := rsCanNotGetValue;
    end;
  end;
end;

procedure TDebugScriptFm.EditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  UpdateStatusBar;
end;

function TDebugScriptFm.GetPaused: Boolean;
begin
  Result := FExec.DebugMode = dmPaused;
end;

procedure TDebugScriptFm.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  	VK_F9: ToolButton1.Click;
    VK_F7: ToolButton2.Click;
    VK_F8: ToolButton3.Click;
  end;
end;

procedure TDebugScriptFm.EditBreakpointChanged(Sender: TObject; aLine: Integer);
var
  MainSD: TScriptData;
  i: Integer;
begin
  Edit.SaveState;
  MainSD := TRunScript(FExec.Id).SD;
  for i := 0 to MainSD.LineCount - 1 do
    with MainSD.Lines[i] do
    	if (SD = FSD) and (Line = aLine) then
      begin
        Breakpoint := SD.SourceData.Marks.FindBreakpoint(aLine) <> nil;
        Exit;
      end;
end;

procedure TDebugScriptFm.SetFormCaption(const aCaption: String);
begin
  Caption := Format(rsDebugFormCaption, [FSD.GetModuleName, aCaption]);
end;

procedure TDebugScriptFm.SetControlState;
begin
  ToolButton1.Enabled:=FExec.DebugMode = dmPaused;
  ToolButton2.Enabled:=FExec.DebugMode = dmPaused;
  ToolButton3.Enabled:=FExec.DebugMode = dmPaused;
end;

{procedure TDebugScriptFm.UpdateBreakpoints;
var
  i: Integer;
  MainSD: TScriptData;
  LD: TLineData;
begin
  Edit.SaveState;
  if Edit.BreakPointsChanged then
  begin
    MainSD := TRunScript(FExec.Id).SD;
    for i := 0 to MainSD.LineCount - 1 do
    begin
      LD := MainSD.Lines[i];
      if LD.SD = FSD then
	      LD.Breakpoint:=LD.SD.SourceData.Marks.FindBreakpoint(LD.Row) <> nil;
    end;
  end;
end; }

procedure TDebugScriptFm.UpdateStatusBar;
begin
  StatusBar.Panels[0].Text:=Format('%d: %d', [Edit.CaretY, Edit.CaretX]);
end;

function GetIdentifier(const S: String; P: Integer): String;
var
  p1, p2, Len: Integer;
begin
  Result := '';
	Len := Length(S);
  if Len = 0 then Exit;
  p2 := P;
  while (P >= 1) and (S[P] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
		Dec(P);
  p1 := P+1;
  while (p2 <= Len) and (S[p2] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
		Inc(p2);
  Result := Copy(S, p1, p2 - p1);
end;

function TDebugScriptFm.DebugGetValue(MousePos: TPoint; AdvInfo: Boolean
  ): String;
var
  P: Classes.TPoint;
  S, V: String;
begin
  Result := '';
  try
    P := Edit.PixelsToLogicalPos(MousePos);
    // S := Edit.GetWordAtRowCol(P);
    S := GetIdentifier(Edit.Lines[P.Y - 1], P.X);
    if S <> '' then
    begin
	    V := GetVarContents(S, AdvInfo);
  	  if V <> rsCanNotGetValue then
		 	  Result := S + ' = ' + V;
    end;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

procedure TDebugScriptFm.SetVisible(Value: boolean);
begin
  try
    inherited SetVisible(Value);
  except
    on E: EInvalidOperation do
      if Pos('TControl.EnableAutoSizing', E.Message) = 1 then
      else raise;
  end;
end;

procedure TDebugScriptFm.FormCreate(Sender: TObject);
begin
  Edit := TScriptEdit.Create(Self);
  Edit.Parent := Self;
  Edit.ShowHint := True;
  Edit.Align := alClient;
  Edit.ReadOnly:=True;
  Edit.OnShowHint:=@EditShowHint;
  Edit.OnStatusChange:=@EditStatusChange;
  Edit.OnKeyDown:=@EditKeyDown;
  Edit.OnBreakpointChanged:=@EditBreakpointChanged;
  Edit.PopupMenu := PopupMenu1;
  with ImageList2 do
	begin
    AddLazarusResource('compile24');
    AddLazarusResource('stepinto24');
    AddLazarusResource('stepover24');
  end;
  ToolButton1.Hint := rsContinueRunBn;
  ToolButton2.Hint := rsStepIntoBn;
  ToolButton3.Hint := rsStepOverBn;
  MenuItem1.Caption := rsGetValue;

  Left := AppConfig.DebugFormLeft;
  Top := AppConfig.DebugFormTop;
  Width := AppConfig.DebugFormWidth;
  Height := AppConfig.DebugFormHeight;

  SetFormPropPosition(Self);
end;

procedure TDebugScriptFm.FormDestroy(Sender: TObject);
var
  FormBounds: TRect;
begin
  if FFormsDisabled <> nil then Screen.EnableForms(FFormsDisabled);

  FormBounds := ScaleRectTo96(GetFormRealBounds(Self));
  AppConfig.DebugFormLeft:=FormBounds.Left;
  AppConfig.DebugFormTop:=FormBounds.Top;
  AppConfig.DebugFormWidth:=FormBounds.Width;
  AppConfig.DebugFormHeight:=FormBounds.Height;
end;

procedure TDebugScriptFm.FormShow(Sender: TObject);
begin
  if AppConfig.DebugFormPosCorrected = False then
  begin
    CorrectFormPos(Self, Self);
    AppConfig.DebugFormPosCorrected := True;
  end;
  Edit.TopLine := Edit.CaretY - Edit.LinesInWindow div 2;
end;

procedure TDebugScriptFm.MenuItem1Click(Sender: TObject);
begin
  MessageDlg(rsGetValue,
  	DebugGetValue(Edit.ScreenToClient(PopupMenu1.PopupPoint), True),
  	mtInformation, [mbOk], 0);
end;

procedure TDebugScriptFm.ToolButton1Click(Sender: TObject);
begin
  Screen.EnableForms(FFormsDisabled);
  Edit.CurLine := 0;
  Edit.Refresh;
  FExec.Run;
  SetFormCaption(rsRunning);
  SetControlState;
end;

procedure TDebugScriptFm.ToolButton2Click(Sender: TObject);
begin
  Screen.EnableForms(FFormsDisabled);
  Edit.CurLine := 0;
  Edit.Refresh;
  FExec.StepInto;
  SetFormCaption(rsStepInto);
  SetControlState;
end;

procedure TDebugScriptFm.ToolButton3Click(Sender: TObject);
begin
  Screen.EnableForms(FFormsDisabled);
  Edit.CurLine := 0;
  Edit.Refresh;
  FExec.StepOver;
  SetFormCaption(rsStepOver);
  SetControlState;
end;

procedure TDebugScriptFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AppConfig.DebugFormWidth := ScaleTo96(Width);
  AppConfig.DebugFormHeight := ScaleTo96(Height);

  FOldSD := FSD;
  FExec.Run;
end;

procedure TDebugScriptFm.EditShowHint(Sender: TObject; HintInfo: PHintInfo);
begin
  if FExec.DebugMode <> dmPaused then Exit;

  HintInfo^.HintStr := DebugGetValue(HIntInfo^.CursorPos, False);
  {P := Edit.PixelsToLogicalPos(HintInfo^.CursorPos);
  // S := Edit.GetWordAtRowCol(P);
  S := GetIdentifier(Edit.Lines[P.Y - 1], P.X);
  if S <> '' then
  begin
	  V := GetVarContents(S, False);
  	if V <> rsUnknownIdentifier then
	  	HintInfo^.HintStr := S + ' = ' + V;
  end;   }
end;

procedure TDebugScriptFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Edit.CurLine = 0;
end;

procedure TDebugScriptFm.ShowForm(Exec: TPSDebugExec; SD: TScriptData;
  aLine: Integer);
begin
  FExec := Exec;
  FExec.OnIdleCall:=@ExecIdleCall;
  FSD := SD;
  if FSD <> FOldSD then
  begin
	  Edit.LoadData(SD);
  	Edit.RestoreState;
  end;

  FExec.Pause;
  Edit.CurLine := aLine;
  Edit.CaretY:=aLine;
  Edit.CaretX := 1;
  Edit.TopLine := aLine - Edit.LinesInWindow div 2;

  SetFormCaption(rsPaused);
  SetControlState;
  Show;
  FFormsDisabled := Screen.DisableForms(Self);
end;

end.

