program DataExpress;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, MainForm, ProgressForm, ModulesTree, treepanel, MetaManager,
  UpdateManForm, MsgForm, UpdateOptionsForm, UpdateManager, MyZipper, AppUtils,
  MyLogger, PascalScriptFCL, LCbxListSourceMoreForm;

{$R *.res}

begin
  {$IFDEF HeapTrc}
  if FileExists(AppPath + 'heap.trc') then
    DeleteFile(AppPath + 'heap.trc');
  SetHeapTraceOutput(AppPath + 'heap.trc');
  {$ENDIF}

  RequireDerivedFormResource:=True;
  Application.Title:='DataExpress';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.CreateForm(TProgressFm, ProgressFm);
  Application.CreateForm(TMsgFm, MsgFm);
  Application.Run;
end.

