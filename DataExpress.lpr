program DataExpress;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem, clocale,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, DBEngine, AppUtils, StrConsts, DxCtrls, FormManager,
  jvRuntimeDesign, bgrabitmappack, DesignerFrame, AppSettings, FormResizer,
  SqlGen, FormDesigner, MainFrame, FormView, PropsForm, DatasetProcessor,
  ListSourceForm, FilterForm, FindForm, StringsForm, XmlReport, ColorForm,
  FontForm, TabOrderForm, SettingsForm, ShapeForm, StorageTypeForm, ExportForm,
  ImportForm, ImportErrorsForm, ProcessForm, dxImages, dxFiles, PropDialogs,
  ProcessMiniForm, AddFieldsForm, CheckPrintOutForm, ExprForm, ExprFuncs,
  FuncsForm, CalcFieldsForm, ReportManager, ReportsForm, ReportExportForm,
  SelectForm, VisibleFormsForm, StyleForm, NewGridForm, GridColsForm,
  RpGridForm, RpStyleForm, ToWordsFuncs, TotalForm, Expressions, ErrorsForm,
  Lists, FilterControl, MyCtrls, FormFiltersForm, ObjectFieldsForm, TimeEdit,
  PropsMenus, QuickSearchForm, ColoringForm, HelpForm, InputForm, CounterForm,
  InsertValuesForm, DSProcLists, LabelTextForm, RangeForm, FixedListForm,
  ShoppingForm, CalcErrsForm, RecalcForm, DXReports, ReportForm, DXUsers,
  UsersForm, UserForm, LoginForm, CtrlRightsForm, WarningForm, QueryCalcForm,
  TotalsForm, NewRoleForm, HintForm, HelpTextForm, HelpViewForm,
  ParentFieldForm, TreeForm, DeleteRecsForm, SetValueForm, GridButtonsForm,
  QueryColoringForm, IntfForm, MonitorForm, FillTableForm, ConnectionForm,
  crypt, ActionForm, ActionPans, DxActions, PivotGrid, PivotGridForm,
  PivotFieldForm, MergeProjectsForm, ScriptForm, ScriptManager, CompilerDecls,
  RunDecls, OutputForm, ScriptFuncs, FieldNameForm, uPSR_MyControls,
  uPSC_MyControls, uPSC_MyStdCtrls, uPSR_MyStdCtrls, uPSC_MyStd, uPSR_MyStd,
  uPSC_MyExtctrls, uPSR_MyExtctrls, uPSC_MyButtons, uPSR_MyButtons, uPSR_forms,
  uPSC_forms, uPSR_classes, uPSC_classes, uPSC_menus, uPSR_menus, uPSC_graphics,
  uPSR_graphics, PascalScriptLCL, SearchReplaceForm, uPSR_mydateutils,
  uPSC_mydateutils, ListForm, EditForm, ReportWindow, MySqlDbLib, IBConnection,
  sqldb, DxSQLQuery, SqlForm, myfpsqlparser, myfpsqltree, myfpsqlscanner,
  MyClasses, MyDialogs, AnchorsForm, ModulesForm, SummaryTree, padeg,
  EditMaskForm, ComponentTree, SAXBaseReader, ErrorIcon, DebugScriptForm, 
ScriptEdit, BreakpointsForm, MyTypes, LangManager;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.CreateForm(TListSourceFrm, ListSourceFrm);
  Application.CreateForm(TFindFrm, FindFrm);
  Application.CreateForm(TStringsFm, StringsFm);
  Application.CreateForm(TColorFm, ColorFm);
  Application.CreateForm(TFontFm, FontFm);
  Application.CreateForm(TTabOrderFm, TabOrderFm);
  Application.CreateForm(TSettingsFm, SettingsFm);
  Application.CreateForm(TShapeFm, ShapeFm);
  Application.CreateForm(TStorageTypeFm, StorageTypeFm);
  Application.CreateForm(TExportFm, ExportFm);
  Application.CreateForm(TImportFm, ImportFm);
  Application.CreateForm(TImportErrorsFm, ImportErrorsFm);
  Application.CreateForm(TProcessFm, ProcessFm);
  Application.CreateForm(TProcessMiniFm, ProcessMiniFm);
  Application.CreateForm(TAddFieldsFm, AddFieldsFm);
  Application.CreateForm(TCheckPrintOutFm, CheckPrintOutFm);
  Application.CreateForm(TExprFm, ExprFm);
  Application.CreateForm(TFuncsFm, FuncsFm);
  Application.CreateForm(TCalcFieldsFm, CalcFieldsFm);
  Application.CreateForm(TReportsFm, ReportsFm);
  Application.CreateForm(TReportExportFm, ReportExportFm);
  Application.CreateForm(TSelectFm, SelectFm);
  Application.CreateForm(TVisibleFormsFm, VisibleFormsFm);
  Application.CreateForm(TStyleFm, StyleFm);
  Application.CreateForm(TGrdFm, GrdFm);
  Application.CreateForm(TGridColsFm, GridColsFm);
  Application.CreateForm(TRpGridFm, RpGridFm);
  Application.CreateForm(TRpStyleFm, RpStyleFm);
  Application.CreateForm(TTotalFm, TotalFm);
  Application.CreateForm(TErrorsFm, ErrorsFm);
  Application.CreateForm(TFormFiltersFm, FormFiltersFm);
  Application.CreateForm(TObjFieldsFm, ObjFieldsFm);
  Application.CreateForm(TFilterFm, FilterFm);
  Application.CreateForm(TQuickSearchFrm, QuickSearchFrm);
  Application.CreateForm(TColoringFm, ColoringFm);
  Application.CreateForm(TInputFm, InputFm);
  Application.CreateForm(TCounterFm, CounterFm);
  Application.CreateForm(TInsertValuesFm, InsertValuesFm);
  Application.CreateForm(TLabelTextFm, LabelTextFm);
  Application.CreateForm(TRangeFm, RangeFm);
  Application.CreateForm(TFixedListFm, FixedListFm);
  Application.CreateForm(TShoppingFm, ShoppingFm);
  Application.CreateForm(TCalcErrsFm, CalcErrsFm);
  Application.CreateForm(TRecalcFm, RecalcFm);
  Application.CreateForm(TReportFm, ReportFm);
  Application.CreateForm(TUsersFm, UsersFm);
  Application.CreateForm(TUserFm, UserFm);
  Application.CreateForm(TLoginFm, LoginFm);
  Application.CreateForm(TCtrlRightsFm, CtrlRightsFm);
  Application.CreateForm(TWarnFm, WarnFm);
  Application.CreateForm(TCalcFm, CalcFm);
  Application.CreateForm(TTotalsFm, TotalsFm);
  Application.CreateForm(TNewRoleFm, NewRoleFm);
  Application.CreateForm(THintFm, HintFm);
  Application.CreateForm(THelpTextFm, HelpTextFm);
  Application.CreateForm(THelpViewFm, HelpViewFm);
  Application.CreateForm(TParentFieldFm, ParentFieldFm);
  Application.CreateForm(TTreeFm, TreeFm);
  Application.CreateForm(TDeleteRecsFm, DeleteRecsFm);
  Application.CreateForm(TSetValueFm, SetValueFm);
  Application.CreateForm(TGridButtonsFm, GridButtonsFm);
  Application.CreateForm(TQueryColoringFm, QueryColoringFm);
  Application.CreateForm(TIntfFm, IntfFm);
  Application.CreateForm(TMonitorFm, MonitorFm);
  Application.CreateForm(TFillTableFm, FillTableFm);
  Application.CreateForm(TConnectionFm, ConnectionFm);
  Application.CreateForm(TActionFm, ActionFm);
  Application.CreateForm(TPivotGridFm, PivotGridFm);
  Application.CreateForm(TPivotFieldFm, PivotFieldFm);
  Application.CreateForm(TMergeProjectsFm, MergeProjectsFm);
  Application.CreateForm(TFieldNameFm, FieldNameFm);
  Application.CreateForm(TSearchReplaceFm, SearchReplaceFm);
  Application.CreateForm(TSqlFm, SqlFm);
  Application.CreateForm(TAnchorsFm, AnchorsFm);
  Application.CreateForm(TModulesFm, ModulesFm);
  Application.Run;
end.

