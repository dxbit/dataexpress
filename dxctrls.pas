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
unit DxCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DBCtrls, StdCtrls, ExtCtrls, DBGrids, DbCtrlsEx,
  ComCtrls, Db, Grids, Graphics, strconsts, Menus, Buttons, Forms, timeedit,
  LazUtf8, LclType, lists, myctrls, LMessages, DxActions, myclasses, KGrids;

const
  StorageTypeDB = 0;
  StorageTypeFolder = 1;
  StorageTypeLink = 2;

type
  TdxForm = class;

  TCreateFormEvent = procedure (Sender: TObject; Form: TdxForm) of object;
  TCreateListWindowEvent = procedure (Sender: TObject; aWindow: TForm) of object;
  TCreateReportWindowEvent = procedure (Sender: TObject; aWindow: TForm) of object;
  TValidateEvent = procedure (Sender: TObject; var Ok: Boolean) of object;
  TPrintFieldEvent = procedure (Sender: TObject; const FieldName: String; var FieldValue: String; var Ok: Boolean) of object;
  TFieldChangeEvent = procedure (Sender, Control: TObject; const FieldName: String) of object;
  TParamNotifyEvent = procedure (Sender: TObject; const ParamName: String) of object;
  TMyUtf8KeyPressEvent = procedure (Sender: TObject; var Utf8Key: String) of object;

  { TParamData }

  TParamData = class
  private
    FName: String;
    FObj: TObject;
    FValue: Variant;
  public
    property Name: String read FName write FName;
    property Value: Variant read FValue write FValue;
    property Obj: TObject read FObj write FObj;
  end;

  { TParamList }

  TParamList = class(TList)
  private
    FOnGetParam: TParamNotifyEvent;
    FOnSetParam: TParamNotifyEvent;
    function GetNames(Index: Integer): String;
    function GetObjectFromIndex(Index: Integer): TObject;
    function GetObjects(Name: String): TObject;
    function GetValueFromIndex(Index: Integer): Variant;
    function GetValues(Name: String): Variant;
    procedure SetObjectFromIndex(Index: Integer; AValue: TObject);
    procedure SetObjects(Name: String; AValue: TObject);
    procedure SetValueFromIndex(Index: Integer; AValue: Variant);
    procedure SetValues(Name: String; AValue: Variant);
    function GetParams(Index: Integer): TParamData;
    function Find(const aName: String): TParamData;
    procedure DoSetParam(const Name: String);
    procedure DoGetParam(const Name: String);
  public
    function AddParam(const aName: String; aValue: Variant; aObj: TObject): TParamData;
    procedure Clear; override;
    function ParamExists(const aName: String): Boolean;
    property Values[Name: String]: Variant read GetValues write SetValues;
    property Objects[Name: String]: TObject read GetObjects write SetObjects;
    property Names[Index: Integer]: String read GetNames;
    property ValueFromIndex[Index: Integer]: Variant read GetValueFromIndex write
      SetValueFromIndex;
    property ObjectFromIndex[Index: Integer]: TObject read GetObjectFromIndex write
      SetObjectFromIndex;
    property OnSetParam: TParamNotifyEvent read FOnSetParam write FOnSetParam;
    property OnGetParam: TParamNotifyEvent read FOnGetParam write FOnGetParam;
  end;

  { TdxLabel }

  TdxLabel = class(TLabel)
  private
    FExpr: String;
    FFieldName: String;
    FValue: Variant;
  protected
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
  public
    property Value: Variant read FValue write FValue;
    property FieldName: String read FFieldName write FFieldName;
  published
    property PopupMenu stored False;
    property Expression: String read FExpr write FExpr;
  end;

  { TdxEdit }

  TdxEdit = class(TDBEdit)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FOldSize: Integer;
    FFieldSize: Integer;
    FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FRequired: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetDataSetEdit;
  protected
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; override;
    function ValidateText: Boolean;
    function MaskTextEmpty: Boolean;
    procedure EditingDone; override;
    property OldSize: Integer read FOldSize write FOldSize;
    property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Required: Boolean read FRequired write FRequired;
    property Expression: String read FExpression write FExpression;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property PopupMenu stored False;
    property CustomEditMask stored False;
  end;


  { TdxCalcEdit }

  //TTotalFunc = (fnSum, fnAvg, fnCount, fnMin, fnMax);

  TdxCalcEdit = class(TDBCalcEdit)
  private
    FAutoNum: Boolean;
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FMaxValue: Double;
    FMinValue: Double;
    FPrecission: Integer;
    FRequired: Boolean;
    procedure MenuPopup(Sender: TObject);
    procedure MenuClick(Sender: TObject);
  protected
    function GetDefaultGlyphName: String; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    function PrecStr: String;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property Precission: Integer read FPrecission write FPrecission;
    property Expression: String read FExpression write FExpression;
    property Required: Boolean read FRequired write FRequired;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;

    property PopupMenu stored False;
    property AutoNum: Boolean read FAutoNum write FAutoNum stored False;
  end;

  { TdxDateEdit }

  TdxDateEdit = class(TDBDateEditEx)
  private
    FCheckExpression: String;
    FDateNow: Boolean;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FRequired: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    function GetDefaultGlyphName: String; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property DateNow: Boolean read FDateNow write FDateNow;
    property PopupMenu stored False;
    property Expression: String read FExpression write FExpression;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  { TdxMemo }

  TdxMemo = class(TDBMemo)
  private
    FButton: TSpeedButton;
    FCheckExpression: String;
    FDefaultValue: String;
    FDelimiter: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FFieldSize: Integer;
    FFilter: String;
    FId: Integer;
    FOldSize: Integer;
    FOnButtonClick: TNotifyEvent;
    FOnCreateListWindow: TCreateListWindowEvent;
    FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTId: Integer;
    procedure DoPositionButton;
    procedure SetSourceFId(AValue: Integer);
    procedure DoButtonClick(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OldSize: Integer read FOldSize write FOldSize;
    property Button: TSpeedButton read FButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnCreateListWindow: TCreateListWindowEvent read FOnCreateListWindow
      write FOnCreateListWindow;
    property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property PopupMenu stored False;
    property Required: Boolean read FRequired write FRequired;
    property SourceTId: Integer read FSourceTId write FSourceTId;
    property SourceFId: Integer read FSourceFId write SetSourceFId;
    property Delimiter: String read FDelimiter write FDelimiter;
    property Filter: String read FFilter write FFilter;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  { TdxCheckBox }

  TdxCheckBox = class(TDBCheckBox)
  private
    FCheckedText: String;
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FUnCheckedText: String;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property CheckedText: String read FCheckedText write FCheckedText;
    property UnCheckedText: String read FUnCheckedText write FUnCheckedText;
    property PopupMenu stored False;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  { TdxComboBox }

  TdxComboBox = class(TDBComboBox)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FFieldSize: Integer;
    FFilter: String;
    FId: Integer;
    FOldSize: Integer;
    FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTId: Integer;
  protected
    procedure Loaded; override;
    procedure UpdateData(Sender: TObject); override;
    procedure Select; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    property OldSize: Integer read FOldSize write FOldSize;
    property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property SourceTId: Integer read FSourceTId write FSourceTId;
    property SourceFId: Integer read FSourceFId write FSourceFId;
    property Filter: String read FFilter write FFilter;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property PopupMenu stored False;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;
  end;

  { TdxLookupComboBox }

  TdxLookupComboBox = class(TDBComboBox)
    procedure ButtonChangeBounds(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
  private
    FCheckExpression: String;
    FClearTableBeforeFill: Boolean;
    FDefaultValue: String;
    FDestTable: Integer;
    FEditable: Boolean;
    FExpression: String;
    FFieldsTables: TStrings;
    FFillFilter: String;
    FInsertedValues: String;
    FButton: TSpeedButton;
    FFieldName: String;
    FFilter: String;
    FId: Integer;
    FKeyField: String;
    FListField: String;
    FListFieldIndex: Integer;
    FListSource: TDataSource;
    FLookupCache: Boolean;
    FOnButtonClick: TNotifyEvent;
    //FOnCbxChange: TNotifyEvent;
    //FOnCbxDropDown: TNotifyEvent;
    FOnCreateForm: TCreateFormEvent;
    FOnCreateListWindow: TCreateListWindowEvent;
    FOnCtrlClick: TNotifyEvent;
    FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FParams: String;
    FPromptFillTable: Boolean;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTable: Integer;
    FSourceTId: Integer;
    FOldItemIndex: Integer;
    //FOldText: String;
    procedure DoPositionButton;
    function GetKeyValue: Variant;
    procedure SetFieldsTables(AValue: TStrings);
    procedure SetKeyValue(AValue: Variant);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetVisible(Value: Boolean); override;
    procedure UpdateData(Sender: TObject); override;
    procedure DropDown; override;
    procedure Change; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    procedure Clear;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property OnCtrlClick: TNotifyEvent read FOnCtrlClick write FOnCtrlClick;
    property Button: TSpeedButton read FButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    //property OnCbxDropDown: TNotifyEvent read FOnCbxDropDown write FOnCbxDropDown;
    //property OnCbxChange: TNotifyEvent read FOnCbxChange write FOnCbxChange;
    property OnCreateListWindow: TCreateListWindowEvent read FOnCreateListWindow
      write FOnCreateListWindow;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
    property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property SourceTId: Integer read FSourceTId write FSourceTId;
    property SourceFId: Integer read FSourceFId write FSourceFId;
    property Filter: String read FFilter write FFilter;
    property InsertedValues: String read FInsertedValues write FInsertedValues;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property SourceTable: Integer read FSourceTable write FSourceTable;
    property DestTable: Integer read FDestTable write FDestTable;
    property FillFilter: String read FFillFilter write FFillFilter;
    property FieldsTables: TStrings read FFieldsTables write SetFieldsTables;
    property PromptFillTable: Boolean read FPromptFillTable write FPromptFillTable;
    property ClearTableBeforeFill: Boolean read FClearTableBeforeFill write
      FClearTableBeforeFill;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;

    property PopupMenu stored False;
    property ListSource: TDataSource read FListSource write FListSource stored False;
    property KeyField: String read FKeyField write FKeyField stored False;
    property ListField: String read FListField write FListField stored False;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex stored False;
    property LookupCache: Boolean read FLookupCache write FLookupCache stored False;
    property Params: String read FParams write FParams stored False;
  end;

  { TdxImage }

  TdxImage = class(TImage)
  published
    property PopupMenu stored False;
  end;

  TdxGridCanSortEvent = procedure (Sender: TObject; var Cancel: Boolean) of object;
  TdxGridValidateEvent = procedure (Sender: TObject; var Ok: Boolean) of object;

  { TdxGrid }

  TdxGrid = class(TMyDBGrid)
  private
    FForm: TdxForm;
    FId: Integer;
    FOnValidate: TdxGridValidateEvent;
    FSortAZ: Boolean;
    FSortColumn: Integer;
    FMaskEdit: TMaskCellEditor;
    function DoValidate: Boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
    procedure SelectEditor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnVaidate: TdxGridValidateEvent read FOnValidate
      write FOnValidate;
  public
    function GetFieldNameByColumn(aColumn: TColumn): String;
    property Form: TdxForm read FForm write FForm;
  published
    property Id: Integer read FId write FId;

    property SortColumn: Integer read FSortColumn write FSortColumn stored False;
    property SortAZ: Boolean read FSortAZ write FSortAZ stored False;
  end;

  { TdxGroupBox }

  TdxGroupBox = class(TGroupBox)
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PopupMenu stored False;
  end;

  { TdxPageControl }

  TdxPageControl = class(TPageControl)
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetPageClass: TCustomPageClass; override;
  published
    property PopupMenu stored False;
  end;

  { TdxTabSheet }

  TdxTabSheet = class(TTabSheet)
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PopupMenu stored False;
    property ClientWidth stored False;
    property ClientHeight stored False;
  end;

  { TdxShape }

  TdxShape = class(TShape)
  published
    property PopupMenu stored False;
  end;

  TViewType = (vtGridTop, vtGridBottom, vtGridLeft, vtGridRight, vtGridOnly,
    vtWithoutGrid, vtSimpleForm, vtDefault);

  TAccessStatus = (asOk, asCantAppend, asCantEdit, asCantDelete, asModified,
    asDeleted, asLocked);

  { TdxForm }

  TdxForm = class(TCustomPanel)
    procedure ReadShopData(Reader: TReader);
    procedure WriteShopData(Writer: TWriter);
  private
    FAutoOpen: Boolean;
    FCalcFields: TStrings;
    FColoring: TStrings;
    FDataSet: TDataSet;
    FFilters: TStrings;
    FGroupField: Integer;
    FHelpText: TStrings;
    FLayout: TAlign;
    FFormCaption: String;
    FGrid: TdxGrid;
    FId: Integer;
    FOnAfterCancel: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnAfterDelete: TNotifyEvent;
    FOnAfterEdit: TNotifyEvent;
    FOnAfterInsert: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterPost: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FOnBeforeCancel: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeDelete: TNotifyEvent;
    FOnBeforeEdit: TNotifyEvent;
    FOnBeforeInsert: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforePost: TNotifyEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnValidate: TValidateEvent;
    FParentField: Integer;
    FPId: Integer;
    FShopData: TShopData;
    FTemplates: TStringList;
    FTreeBackColor: TColor;
    FTreeFont: TFont;
    FTreeLineColor: TColor;
    FTreeSelectColor: TColor;
    FTreeWidth: Integer;
    FViewType: TViewType;
    function GetFields(Index: String): Variant;
    procedure SetCalcFields(AValue: TStrings);
    procedure SetColoring(AValue: TStrings);
    procedure SetFields(Index: String; AValue: Variant);
    procedure SetFilters(AValue: TStrings);
    procedure SetGrid(AValue: TdxGrid);
    procedure SetHelpText(AValue: TStrings);
    procedure SetTemplates(AValue: TStringList);
    procedure SetTreeFont(AValue: TFont);
  private
    FDSP: TObject;
    FDSR: Pointer;
    FExpr: TObject;
    FOldBeforeInsert, FOldBeforeEdit, FOldBeforeScroll, FOldBeforePost: TNotifyEvent;
    FOldAfterInsert, FOldAfterEdit, FOldAfterScroll, FOldAfterPost: TNotifyEvent;
    FOnAfterDuplicate: TNotifyEvent;
    FOnAfterPrint: TNotifyEvent;
    FOnAfterRecalculate: TNotifyEvent;
    FOnBeforeDuplicate: TNotifyEvent;
    FOnBeforePrint: TNotifyEvent;
    FOnBeforeRecalculate: TNotifyEvent;
    FOnFieldChange: TFieldChangeEvent;
    FOnPrintField: TPrintFieldEvent;
  private
    FConfirmAutoSaveRecord: Boolean;
    FConfirmCancelEditing: Boolean;
    FConfirmSaveRecord: Boolean;
    FDSRi: Integer;
    FParams: TParamList;
    FShowScrollBars: Boolean;
    //FFirstSearch: Boolean;
    {procedure BeginSearch;
    procedure EndSearch;
    function CompareExpr: Boolean; }
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetEditWindow: TObject;
    function GetFilter: TFilterObject;
    function GetFormByIndex(Index: Integer): TdxForm;
    function GetFormCount: Integer;
    function GetForms(Index: String): TdxForm;
    function GetFormGrid: TdxGrid;
    function GetModified: Boolean;
    function GetOldValues(Index: String): Variant;
    function GetParentForm: TdxForm;
    function GetQueries(Index: String): TObject;
    function GetQueryByIndex(Index: Integer): TObject;
    function GetQueryCount: Integer;
    function GetState: TDataSetState;
  protected
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure BeginDuplicate;
    procedure EndDuplicate;
    procedure BeginRecalc;
    procedure EndRecalc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasShop: Boolean;
    property ShopData: TShopData read FShopData;
    property Fields[Index: String]: Variant read GetFields write SetFields;
    property AsI[Index: String]: Integer read GetAsI;
    property AsF[Index: String]: Extended read GetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT;
    property AsS[Index: String]: String read GetAsS;
    property OldValues[Index: String]: Variant read GetOldValues;
    property ParentForm: TdxForm read GetParentForm;
    property DataSet: TDataSet read FDataSet write FDataSet;
  public
    procedure Append;
    procedure Edit;
    function Delete: Boolean;
    procedure Post;
    procedure Cancel;
    procedure Refresh;
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveBy(Distance: Integer);
    procedure MoveTo(aRecNo: Integer);
    function Bof: Boolean;
    function Eof: Boolean;
    function RecNo: Integer;
    function RecId: Integer;
    function RecordCount: Integer;
    procedure Print(const TemplateName, OutFileName: String; var Errs: String; aOpenFile: Boolean);
    {function FindFirst(const aExpr: String): Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;    }
    function Locate(const FieldName: String; FieldValue: Variant; Options: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    procedure DisableControls;
    procedure EnableControls;
    function ControlsDisabled: Boolean;
    function CanAppend: TAccessStatus;
    function CanEdit: TAccessStatus;
    function CanDelete: TAccessStatus;
    function Show: Boolean;
    //procedure AddUserFilter(const FieldName: String; aNot, aNull: Boolean; Values: array of Variant);
    //procedure ClearUserFilter;
    procedure Open;
    procedure Close;
    procedure OpenRecords(const aFilter: String; SelCond: Boolean);
    procedure OpenRecord(aRecId: Integer);
    function Opened: Boolean;
    function Validate: Boolean;
    property DSP: TObject read FDSP write FDSP;
    property DSR: Pointer read FDSR write FDSR;
    property DSRi: Integer read FDSRi write FDSRi;
    property Forms[Index: String]: TdxForm read GetForms;
    property FormByIndex[Index: Integer]: TdxForm read GetFormByIndex;
    property FormCount: Integer read GetFormCount;
    property Queries[Index: String]: TObject read GetQueries;
    property QueryByIndex[Index: Integer]: TObject read GetQueryByIndex;
    property QueryCount: Integer read GetQueryCount;
    property FormGrid: TdxGrid read GetFormGrid;
    property EditWindow: TObject read GetEditWindow;
    property Params: TParamList read FParams;
    property State: TDataSetState read GetState;
    property Filter: TFilterObject read GetFilter;
    property Modified: Boolean read GetModified;
  published
    property Id: Integer read FId write FId;
    property PId: Integer read FPId write FPId;
    property FormCaption: String read FFormCaption write FFormCaption;
    property Layout: TAlign read FLayout write FLayout stored False;       // устаревшее
    property Templates: TStringList read FTemplates write SetTemplates;
    property Color;
    property Font;
    property CalcFields: TStrings read FCalcFields write SetCalcFields;
    property PopupMenu stored False;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen;
    property ViewType: TViewType read FViewType write FViewType;      // вместо Layout
    property Filters: TStrings read FFilters write SetFilters;
    property Coloring: TStrings read FColoring write SetColoring;
    property HelpText: TStrings read FHelpText write SetHelpText;
    property ParentField: Integer read FParentField write FParentField;
    property GroupField: Integer read FGroupField write FGroupField;
    property TreeBackColor: TColor read FTreeBackColor write FTreeBackColor;
    property TreeLineColor: TColor read FTreeLineColor write FTreeLineColor;
    property TreeSelectColor: TColor read FTreeSelectColor write FTreeSelectColor;
    property TreeFont: TFont read FTreeFont write SetTreeFont;
    property TreeWidth: Integer read FTreeWidth write FTreeWidth;
    property Grid: TdxGrid read FGrid write SetGrid;
    property ShowScrollBars: Boolean read FShowScrollBars write FShowScrollBars;
    property ConfirmSaveRecord: Boolean read FConfirmSaveRecord write FConfirmSaveRecord;
    property ConfirmAutoSaveRecord: Boolean read FConfirmAutoSaveRecord write FConfirmAutoSaveRecord;
    property ConfirmCancelEditing: Boolean read FConfirmCancelEditing write FConfirmCancelEditing;

    property OnAfterCancel: TNotifyEvent read FOnAfterCancel write FOnAfterCancel;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterDelete: TNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property OnAfterEdit: TNotifyEvent read FOnAfterEdit write FOnAfterEdit;
    property OnAfterInsert: TNotifyEvent read FOnAfterInsert write FOnAfterInsert;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterPost: TNotifyEvent read FOnAfterPost write FOnAfterPost;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnAfterDuplicate: TNotifyEvent read FOnAfterDuplicate write FOnAfterDuplicate;
    property OnAfterPrint: TNotifyEvent read FOnAfterPrint write FOnAfterPrint;
    property OnAfterRecalculate: TNotifyEvent read FOnAfterRecalculate write FOnAfterRecalculate;

    property OnBeforeCancel: TNotifyEvent read FOnBeforeCancel write FOnBeforeCancel;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeDelete: TNotifyEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnBeforeEdit: TNotifyEvent read FOnBeforeEdit write FOnBeforeEdit;
    property OnBeforeInsert: TNotifyEvent read FOnBeforeInsert write FOnBeforeInsert;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforePost: TNotifyEvent read FOnBeforePost write FOnBeforePost;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnBeforeDuplicate: TNotifyEvent read FOnBeforeDuplicate write FOnBeforeDuplicate;
    property OnBeforePrint: TNotifyEvent read FOnBeforePrint write FOnBeforePrint;
    property OnBeforeRecalculate: TNotifyEvent read FOnBeforeRecalculate write FOnBeforeRecalculate;

    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnPrintField: TPrintFieldEvent read FOnPrintField write FOnPrintField;
    property OnFieldChange: TFieldChangeEvent read FOnFieldChange write FOnFieldChange;

    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  { TdxObjectField }

  TdxObjectField = class(TDBEdit)
  private
    FFieldId: Integer;
    FFieldName: String;
    FId: Integer;
    FObjId: Integer;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property ObjId: Integer read FObjId write FObjId;
    property FieldId: Integer read FFieldId write FFieldId;
  end;

  { TdxTimeEdit }

  TdxTimeFormat = (ttHH, ttHHMM, ttHHMMSS);

  TdxTimeEdit = class(TDBTimeEdit)
  private
    FCheckExpression: String;
    FCurTime: Boolean;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FRequired: Boolean;
    FTimeFormat: TdxTimeFormat;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function TimeFormatStr: String;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property CurTime: Boolean read FCurTime write FCurTime;
    property TimeFormat: TdxTimeFormat read FTimeFormat write FTimeFormat;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  { TdxCounter }

  TdxCounter = class(TDBEdit)
  private
    FCheckExpression: String;
    FFieldName: String;
    FId: Integer;
    FRequired: Boolean;
    FRestart: Boolean;
    FStartWith: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    property StartWith: Integer read FStartWith write FStartWith;
    property Restart: Boolean read FRestart write FRestart;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
  end;

  { TdxScrollBox }

  TdxScrollBox = class(TScrollBox)
  public
    procedure Paint; override;
  end;

  { TdxButton }

  TdxButton = class(TBitBtn)
  private
    FActionProps: String;
    FActionType: TdxActionType;
    FButtonName: String;
    FOnButtonClick: TNotifyEvent;
    FResName: String;
    function IsStoredGlyph: Boolean;
  protected
    procedure Loaded; override;
    procedure TextChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDefaultGlyph;
    procedure Click; override;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  published
    property ActionType: TdxActionType read FActionType write FActionType;
    property ActionProps: String read FActionProps write FActionProps;
    property ResName: String read FResName write FResName;
    property Glyph stored IsStoredGlyph;

    property PopupMenu stored False;
    property ButtonName: String read FButtonName write FButtonName stored False;
  end;

  { TUtf8StringList }

  {TUtf8StringList = class(TStringList)
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  public
    function QuickFind(const S: String): Integer;
  end; }

  { TWindow }

  TWindow = class(TForm)
  private
    FParams: TParamList;
  public
    constructor CreateWindow;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    destructor Destroy; override;
    property Params: TParamList read FParams;
  end;

function HasFId(C: TComponent): Boolean;
function IsField(C: TComponent): Boolean;
function GetId(C: TComponent): Integer;
function SetId(C: TComponent; Id: Integer): Boolean;
function FindById(Fm: TdxForm; Id: Integer): TComponent;
function GetFieldName(C: TComponent): String;
function SetFieldName(C: TComponent; const S: String): Boolean;
function SetDataSource(C: TComponent; DS: TDataSource): Boolean;
function GetDataSource(C: TComponent): TDataSource;
function SetDataField(C: TComponent; const DataField: String): Boolean;
function GetSourceTId(C: TComponent): Integer;
function SetSourceTId(C: TComponent; Id: Integer): Boolean;
function GetSourceFId(C: TComponent): Integer;
function SetSourceFId(C: TComponent; Id: Integer): Boolean;
function GetStorageType(C: TComponent): Integer;
function SetStorageType(C: TComponent; ST: Integer): Boolean;
function GetStorageFolder(C: TComponent): String;
function SetStorageFolder(C: TComponent; const SF: String): Boolean;
procedure GetFields(Fm: TdxForm; L: TStrings; Blobs: Boolean);
function FindComponentByFieldName(Fm: TdxForm; const FieldName: String): TComponent;
function FindComponentByFieldName(Fm: TdxForm; const FieldName: String; aExclude: TComponent): TComponent; overload;
//function GetParentColor(C: TComponent): Boolean;
//function SetParentColor(C: TComponent; Value: Boolean): Boolean;
//function GetParentFont(C: TComponent): Boolean;
function SetParentFont(C: TComponent; Value: Boolean): Boolean;
function GetEditButton(C: TComponent): TSpeedButton;
function GetComponentType(C: TComponent): String;
function GetComponentName(C: TComponent): String;
function GetComboFilter(C: TComponent): String;
function SetComboFilter(C: TComponent; const S: String): Boolean;
function SetExpression(C: TComponent; const S: String): Boolean;
function GetExpression(C: TComponent): String;
function HasExpression(C: TComponent): Boolean;
function FindLabelByFieldName(Fm: TdxForm; const S: String; All: Boolean): TdxLabel;
function LookupComponent(Fm: TdxForm; FlNm: String): TComponent;
function GetFieldSize(C: TComponent): Integer;
function SetFieldSize(C: TComponent; Value: Integer): Boolean;
function GetOldSize(C: TComponent): Integer;
procedure SetOldSize(C: TComponent; Value: Integer);
function GetPrecission(C: TComponent): Integer;
function SetPrecission(C: TComponent; Value: Integer): Boolean;
function GetPrecStr(C: TComponent): String;
function GetRequired(C: TComponent): Boolean;
function SetRequired(C: TComponent; Value: Boolean): Boolean;
function HasRequired(C: TComponent): Boolean;
function HasReadOnly(C: TComponent): Boolean;
function GetReadOnly(C: TComponent): Boolean;
function SetReadOnly(C: TComponent; Value: Boolean): Boolean;
function GetDefaultValue(C: TComponent): String;
function SetDefaultValue(C: TComponent; const Value: String): Boolean;
function HasDefaultValue(C: TComponent): Boolean;
function IsFieldExist(Fm: TdxForm): Boolean;
function HasCheckExpression(C: TComponent): Boolean;
function GetCheckExpression(C: TComponent): String;
function SetCheckExpression(C: TComponent; const Value: String): Boolean;
function GetEditable(C: TComponent): Boolean;
function SetEditable(C: TComponent; Value: Boolean): Boolean;
function HasEditable(C: TComponent): Boolean;
function HasMaxLength(C: TComponent): Boolean;
function SetMaxLength(C: TComponent; Value: Integer): Boolean;
function GetCtrlAnchors(C: TComponent): TAnchors;
procedure SetCtrlAnchors(C: TComponent; Anchors: TAnchors);
function HasAnchors(C: TComponent): Boolean;

implementation

uses
  TypInfo, formmanager, Dialogs, appsettings,
  FileUtil, BGRABitmap, BGRABitmapTypes, FPReadJpeg,
  FPImage, BGRAReadPNG, dximages, dxfiles, apputils, reportmanager,
  DXReports, StrUtils, DateUtils, pivotgrid, sqlgen, datasetprocessor,
  Variants, expressions, dxusers, LazFileUtils, scriptfuncs, MaskEdit, mytypes;

// Utils

function HasFId(C: TComponent): Boolean;
begin
  Result := (not (C is TDBGrid)) and (not (C is TdxPivotGrid)) and
    (GetPropInfo(C, 'Id') <> nil);
end;

function GetInt(C: TComponent; const PropName: String): Integer;
var
  pInf: PPropInfo;
begin
  Result := 0;
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    Result := GetOrdProp(C, pInf);
end;

function SetInt(C: TComponent; const PropName: String; aInt: Integer): Boolean;
var
  pInf: PPropInfo;
begin
  Result := True;
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    SetOrdProp(C, pInf, aInt)
  else
    Result := False;
end;

function GetStr(C: TComponent; const PropName: String): String;
var
  pInf: PPropInfo;
begin
  Result := '';
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    Result := GetStrProp(C, pInf);
end;

function SetStr(C: TComponent; const PropName, S: String): Boolean;
var
  pInf: PPropInfo;
begin
  Result := True;
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    SetStrProp(C, PInf, S)
  else
    Result := False;
end;

function SetObj(C: TComponent; const PropName: String; Obj: TObject): Boolean;
var
  pInf: PPropInfo;
begin
  Result := True;
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    SetObjectProp(C, pInf, Obj)
  else
    Result := False;
end;

function GetObj(C: TComponent; const PropName: String): TObject;
var
  pInf: PPropInfo;
begin
  Result := nil;
  pInf := GetPropInfo(C, PropName);
  if pInf <> nil then
    Result := GetObjectProp(C, pInf);
end;

function IsField(C: TComponent): Boolean;
begin
  Result := (GetId(C) > 0) and (not (C is TDBGrid)) and (not (C is TdxObjectField))
    and (not (C is TdxPivotGrid));
end;

function GetId(C: TComponent): Integer;
begin
  Result := GetInt(C, 'Id');
end;

function SetId(C: TComponent; Id: Integer): Boolean;
begin
  Result := SetInt(C, 'Id', Id);
end;

function FindById(Fm: TdxForm; Id: Integer): TComponent;
var
  i: Integer;
  C: TComponent;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then Continue;
    if GetId(C) = Id then
      Exit(C);
  end;
end;

function GetFieldName(C: TComponent): String;
begin
  Result := GetStr(C, 'FieldName');
end;

function SetFieldName(C: TComponent; const S: String): Boolean;
begin
  Result := SetStr(C, 'FieldName', S);
end;

function SetDataSource(C: TComponent; DS: TDataSource): Boolean;
begin
  Result := SetObj(C, 'DataSource', DS);
end;

function GetDataSource(C: TComponent): TDataSource;
begin
  Result := TDataSource(GetObj(C, 'DataSource'));
end;

function SetDataField(C: TComponent; const DataField: String): Boolean;
begin
  Result := SetStr(C, 'DataField', DataField);
end;

function GetSourceTId(C: TComponent): Integer;
begin
  Result := GetInt(C, 'SourceTId');
end;

function SetSourceTId(C: TComponent; Id: Integer): Boolean;
begin
  Result := SetInt(C, 'SourceTId', Id);
end;

function GetSourceFId(C: TComponent): Integer;
begin
  Result := GetInt(C, 'SourceFId');
end;

function SetSourceFId(C: TComponent; Id: Integer): Boolean;
begin
  Result := SetInt(C, 'SourceFId', Id);
end;

function GetStorageType(C: TComponent): Integer;
begin
  Result := GetInt(C, 'StorageType');
end;

function SetStorageType(C: TComponent; ST: Integer): Boolean;
begin
  Result := SetInt(C, 'StorageType', ST);
end;

function GetStorageFolder(C: TComponent): String;
begin
  Result := GetStr(C, 'StorageFolder');
end;

function SetStorageFolder(C: TComponent; const SF: String): Boolean;
begin
  Result := SetStr(C, 'StorageFolder', SF);
end;

procedure GetFields(Fm: TdxForm; L: TStrings; Blobs: Boolean);
var
  i: Integer;
  C: TComponent;
begin
  L.Clear;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    if (C is TdxDBImage) and (not Blobs) then Continue;
    L.AddObject(GetFieldName(C), C);
  end;
end;

function FindComponentByFieldName(Fm: TdxForm; const FieldName: String
  ): TComponent;
var
  C: TComponent;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then Continue;
    if Utf8CompareText(GetFieldName(C), FieldName) = 0 then
      Exit(C);
  end;
end;

function GetParentColor(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'ParentColor'));
end;

function SetParentColor(C: TComponent; Value: Boolean): Boolean;
begin
  Result := SetInt(C, 'ParentColor', Integer(Value));
end;

function GetParentFont(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'ParentFont'));
end;

function FindComponentByFieldName(Fm: TdxForm; const FieldName: String;
  aExclude: TComponent): TComponent;
var
  C: TComponent;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then Continue
    else if C = aExclude then Continue;
    if Utf8CompareText(GetFieldName(C), FieldName) = 0 then
      Exit(C);
  end;
end;

function SetParentFont(C: TComponent; Value: Boolean): Boolean;
begin
  Result := SetInt(C, 'ParentFont', Integer(Value));
end;

function GetEditButton(C: TComponent): TSpeedButton;
begin
  Result := nil;
  if C is TdxDateEdit then
    Result := TSpeedButton(TdxDateEdit(C).Button)
  else if C is TdxCalcEdit then
    Result := TSpeedButton(TdxCalcEdit(C).Button)
  else if C is TdxFile then
    Result := TSpeedButton(TdxFile(C).Button)
  else if C is TdxLookupComboBox then
    Result := TdxLookupComboBox(C).Button
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).Button
  else if C is TdxMemo then
    Result := TdxMemo(C).Button;
end;

function GetComponentType(C: TComponent): String;
const
  Cls: array [1..23] of String = ('TdxLabel', 'TdxEdit', 'TdxCalcEdit',
    'TdxDateEdit', 'TdxMemo', 'TdxCheckBox', 'TdxComboBox', 'TdxLookupComboBox',
    'TdxGrid', 'TdxGroupBox', 'TdxPageControl', 'TdxForm', 'TdxTabSheet', 'TdxShape',
    'TdxDBImage', 'TdxImage', 'TdxFile', 'TdxQueryGrid', 'TdxObjectField',
    'TdxTimeEdit', 'TdxCounter', 'TdxButton', 'TdxPivotGrid');
var
  i: Integer;
  Tps: array [1..23] of String;
begin
  Result := '';
  Tps[1] := rsLabel; Tps[2] := rsText; Tps[3] := rsNumber;
  Tps[4] := rsDate; Tps[5] := rsMemo; Tps[6] := rsCheckBox;
  Tps[7] := rsList; Tps[8] := rsObject; Tps[9] := rsTable;
  Tps[10] := rsGroup; Tps[11] := rsPages; Tps[12] := rsForm;
  Tps[13] := rsPage; Tps[14] := rsShape; Tps[15] := rsImage;
  Tps[16] := rsDsgnBackImage; Tps[17] := rsDsgnFile; Tps[18] := rsQuery;
  Tps[19] := rsObjField; Tps[20] := rsTime; Tps[21] := rsCounter;
  Tps[22] := rsButton; Tps[23] := rsPivotTable;
  for i := Low(Cls) to High(Cls) do
    if CompareText(C.ClassName, Cls[i]) = 0 then
      Exit(Tps[i]);
end;

function GetComponentName(C: TComponent): String;
begin
  if C is TdxForm then
    Result := TdxForm(C).FormCaption
  else if C is TdxGrid then
    Result := FormMan.FindForm(TdxGrid(C).Id).FormCaption
  else if HasFId(C) then
    Result := GetFieldName(C)
  else if (C is TdxGroupBox) or (C is TdxTabSheet) then
    Result := TWinControl(C).Caption
  else if C is TdxPageControl then
    Result := rsPages
  else if C is TdxShape then
    Result := rsShape
  else if C is TdxImage then
    Result := rsDsgnBackImage
  else if C is TdxQueryGrid then
    Result := ReportMan.FindReport(TdxQueryGrid(C).Id).Name
  else if C is TdxButton then
    Result := TdxButton(C).Caption
  else if C is TdxLabel then
  	Result := TdxLabel(C).Caption
  else
    Result := '';

  if Result <> '' then
  begin
    if AppConfig.ExpertMode then
      Result := Result + ' (' + C.Name + ')'
  end
  else
    Result := C.Name;
end;

function GetComboFilter(C: TComponent): String;
begin
  Result := GetStr(C, 'Filter');
end;

function SetComboFilter(C: TComponent; const S: String): Boolean;
begin
  Result := SetStr(C, 'Filter', S);
end;

function SetExpression(C: TComponent; const S: String): Boolean;
begin
  Result := SetStr(C, 'Expression', S);
end;

function GetExpression(C: TComponent): String;
begin
  Result := GetStr(C, 'Expression');
end;

function HasExpression(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'Expression') <> nil;
end;

function FindLabelByFieldName(Fm: TdxForm; const S: String; All: Boolean
  ): TdxLabel;
var
  i: Integer;
  C: TComponent;
begin
  Result := nil;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if C is TdxLabel then
      with TdxLabel(C) do
        if (All or (Expression <> '')) and (Utf8CompareText(FieldName, S) = 0) then
          Exit(TdxLabel(C));
  end;
end;

function LookupComponent(Fm: TdxForm; FlNm: String): TComponent;
var
  L: TStringList;
  i, TId: Integer;
  C: TComponent;
begin
  C := nil;
  Result := nil;

  L := TStringList.Create;
  try

  SplitStr(FlNm, '|', L);
  for i := 0 to L.Count - 1 do
  begin
    C := FindComponentByFieldName(Fm, L[i]);
    if C = nil then Exit;
    if C is TdxLookupComboBox then
    begin
      TId := GetSourceTId(C);
      Fm := FormMan.FindForm(TId);
      if Fm = nil then Exit;
    end;
  end;
  Result := C;

  finally
    L.Free;
  end;
end;

function GetFieldSize(C: TComponent): Integer;
begin
  Result := GetInt(C, 'FieldSize');
end;

function SetFieldSize(C: TComponent; Value: Integer): Boolean;
begin
  Result := SetInt(C, 'FieldSize', Value);
end;

function GetOldSize(C: TComponent): Integer;
begin
  Result := 0;
  if C is TdxEdit then
    Result := TdxEdit(C).OldSize
  else if C is TdxMemo then
    REsult := TdxMemo(C).OldSize
  else if C is TdxComboBox then
    Result := TdxComboBox(C).OldSize
  else if C is TdxFile then
    Result := TdxFile(C).OldSize;
end;

procedure SetOldSize(C: TComponent; Value: Integer);
begin
  if C is TdxEdit then
    TdxEdit(C).OldSize:= Value
  else if C is TdxMemo then
    TdxMemo(C).OldSize := Value
  else if C is TdxComboBox then
    TdxComboBox(C).OldSize := Value
  else if C is TdxFile then
    TdxFile(C).OldSize := Value
end;

function GetPrecission(C: TComponent): Integer;
begin
  Result := GetInt(C, 'Precission');
end;

function SetPrecission(C: TComponent; Value: Integer): Boolean;
begin
  Result := SetInt(C, 'Precission', Value);
end;

function GetPrecStr(C: TComponent): String;
begin
  Result := '';
  if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).PrecStr
end;

function GetRequired(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'Required'));
end;

function SetRequired(C: TComponent; Value: Boolean): Boolean;
begin
  Result := SetInt(C, 'Required', Ord(Value));
end;

function HasRequired(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'Required') <> nil;
end;

function HasReadOnly(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'ReadOnly') <> nil;
end;

function GetReadOnly(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'ReadOnly'));
end;

function SetReadOnly(C: TComponent; Value: Boolean): Boolean;
begin
  Result := SetInt(C, 'ReadOnly', Ord(Value));
end;

function GetDefaultValue(C: TComponent): String;
begin
  Result := GetStr(C, 'DefaultValue');
end;

function SetDefaultValue(C: TComponent; const Value: String): Boolean;
begin
  Result := SetStr(C, 'DefaultValue', Value);
end;

function HasDefaultValue(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'DefaultValue') <> nil;
end;

function IsFieldExist(Fm: TdxForm): Boolean;
var
  i: Integer;
  C: TComponent;
begin
  Result := False;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if IsField(C) then Exit(True);
  end;
end;

function HasCheckExpression(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'CheckExpression') <> nil;
end;

function GetCheckExpression(C: TComponent): String;
begin
  Result := GetStr(C, 'CheckExpression');
end;

function SetCheckExpression(C: TComponent; const Value: String): Boolean;
begin
  Result := SetStr(C, 'CheckExpression', Value);
end;

function GetEditable(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'Editable'));
end;

function SetEditable(C: TComponent; Value: Boolean): Boolean;
begin
  Result := SetInt(C, 'Editable', Ord(Value));
end;

function HasEditable(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'Editable') <> nil;
end;

function HasMaxLength(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'MaxLength') <> nil;
end;

function SetMaxLength(C: TComponent; Value: Integer): Boolean;
begin
  Result := SetInt(C, 'MaxLength', Value);
end;

function GetCtrlAnchors(C: TComponent): TAnchors;
var
  S: String;
  PInfo: PPropInfo;
begin
  Result := [];
  PInfo := GetPropInfo(C, 'Anchors');
  if PInfo = nil then Exit;
  S := GetSetProp(C, PInfo, False);
  if Pos('akLeft', S) > 0 then Include(Result, akLeft);
  if Pos('akTop', S) > 0 then Include(Result, akTop);
  if Pos('akRight', S) > 0 then Include(Result, akRight);
  if Pos('akBottom', S) > 0 then Include(Result, akBottom);
end;

procedure SetCtrlAnchors(C: TComponent; Anchors: TAnchors);
var
  PInfo: PPropInfo;
  S: String;
begin
  PInfo := GetPropInfo(C, 'Anchors');
  if PInfo = nil then Exit;
  S := '';
  if akLeft in Anchors then S := S + 'akLeft,';
  if akTop in Anchors then S := S + 'akTop,';
  if akRight in Anchors then S := S + 'akRight,';
  if akBottom in Anchors then S := S + 'akBottom,';
  S := Copy(S, 1, Length(S) - 1);
  SetSetProp(C, PInfo, S);
end;

function HasAnchors(C: TComponent): Boolean;
begin
  Result := GetPropInfo(C, 'Anchors') <> nil;
end;

{ TdxButton }

function TdxButton.IsStoredGlyph: Boolean;
begin
  Result := FResName = '';
end;

procedure TdxButton.Loaded;
begin
  inherited Loaded;
  try
    if FResName <> '' then LoadGlyphFromLazarusResource(FResName);
  except
    ;
  end;
end;

procedure TdxButton.TextChanged;
begin
  inherited TextChanged;
  if Caption = '' then Spacing:=0
  else Spacing := 4;
end;

procedure TdxButton.Click;
begin
  inherited Click;
  if FOnButtonClick <> nil then FOnButtonClick(Self);
end;

constructor TdxButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TdxButton.Destroy;
begin
  inherited Destroy;
end;

procedure TdxButton.SetDefaultGlyph;
var
  S: String;
begin
  S := '';
  case FActionType of
    actGotoForm: S := 'form24';
    actPrint: S := 'print24';
    actMassCalc: S := 'calc24';
    actOpenReport: S := 'grid24';
    actSaveChanges: S := 'save24';
    actUserMonitor: S := 'users24';
    actCallFunc: S := 'action24';
    actClearFields: S := 'delete16';
  end;
  FResName:=S;
  if S <> '' then LoadGlyphFromLazarusResource(S);
end;

{ TdxGrid }

function TdxGrid.DoValidate: Boolean;
var
  Ok: Boolean;
begin
  Ok := True;
  if (FOnValidate <> nil) and (DataSource.DataSet.State in [dsInsert, dsEdit]) then
    FOnValidate(Self, Ok);
  Result := Ok;
end;

procedure TdxGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR]) and (not DoValidate) then
  begin
    Key := 0;
    Exit;
  end
  else if Key = VK_ESCAPE then
  begin
    if OnKeyDown <> nil then OnKeyDown(Self, Key, Shift);
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TdxGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  C, R: Longint;
begin
  if DataSource = nil then Exit;

  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    MouseToCell(X, Y, C, R);
    if (R <> Row) and (not DoValidate) then Exit;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxGrid.WMVScroll(var Message: TLMVScroll);
begin
  //if DoValidate then
  if DataSource = nil then Exit;

  if DataSource.DataSet.State in [dsInsert, dsEdit] then Exit;
  inherited WMVScroll(Message);
end;

procedure TdxGrid.SelectEditor;
var
  C: TComponent;
begin
  inherited SelectEditor;
  if (DataLink <> nil) and (Datalink.Active) and (Editor <> nil) and
    (not (Editor is TPickListCellEditor)) and (SelectedField is TStringField) then
  begin
    C := FindById(Form, SelectedColumn.Tag);
    if C is TdxEdit then
    	with TdxEdit(C) do
        if EditMask <> '' then
        begin
          FMaskEdit.EditMask := EditMask;
          Editor := FMaskEdit;
        end;
  end;
end;

constructor TdxGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VisibleButtons := [gbnAppend, gbnEdit, gbnDelete, gbnDuplicate, gbnShopping,
    gbnMoveUp, gbnMoveDown];
  FMaskEdit := TMaskCellEditor.Create(nil);
end;

destructor TdxGrid.Destroy;
begin
  FMaskEdit.Free;
  inherited Destroy;
end;

function TdxGrid.GetFieldNameByColumn(aColumn: TColumn): String;
var
  C: TComponent;
begin
  Result := '';
  C := FindById(FForm, aColumn.Tag);
  if C <> nil then Result := GetFieldName(C);
end;

{ TdxCounter }

constructor TdxCounter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
end;

{ TdxTimeEdit }

procedure TdxTimeEdit.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4:
      begin
        SetDSEdit(Field.DataSet);
        Field.SetData(nil);
      end;
    6:
      begin
        SetDSEdit(Field.DataSet);
        Field.Value := TMenuItem(Sender).Caption;
      end;
  end;
end;

procedure TdxTimeEdit.MenuPopup(Sender: TObject);
var
  ae, rae: Boolean;
begin
  ae := DataSource.AutoEdit;
  rae := (not ReadOnly) and ae;
  PopupMenu.Items[0].Enabled := (SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := rae;
  PopupMenu.Items[4].Enabled := (Text <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[6].Enabled := rae;
  PopupMenu.Items[7].Enabled := rae;
  PopupMenu.Items[8].Enabled := rae;
  PopupMenu.Items[9].Enabled := rae;
  PopupMenu.Items[10].Enabled := rae;
  PopupMenu.Items[11].Enabled := rae;
  PopupMenu.Items[12].Enabled := rae;
  PopupMenu.Items[13].Enabled := rae;
  PopupMenu.Items[14].Enabled := rae;
  PopupMenu.Items[15].Enabled := rae;
end;

constructor TdxTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  TimeFormat := ttHHMM;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, 'cut16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, 'paste16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, ShortCut(VK_DELETE, [ssCtrl]), @MenuClick, 'delete16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '08:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '09:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '10:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '11:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '12:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '13:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '14:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '15:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '16:00', 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '17:00', 6, 0, @MenuClick, '') );
  PopupMenu.OnPopup:=@MenuPopup;
  Button.PopupMenu := PopupMenu;
end;

function TdxTimeEdit.TimeFormatStr: String;
const
  TimeFmt: array[0..2] of String = ('hh', 'hh:nn', 'hh:nn:ss');
begin
  Result := TimeFmt[Ord(FTimeFormat)];
end;

{ TdxObjectField }

procedure TdxObjectField.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CopyToClipboard;
  end;
end;

procedure TdxObjectField.MenuPopup(Sender: TObject);
begin
  PopupMenu.Items[0].Enabled := SelText <> '';
end;

constructor TdxObjectField.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 0, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.OnPopup:=@MenuPopup;
end;

{ TdxLabel }

procedure TdxLabel.Loaded;
begin
  inherited Loaded;
  FFieldName := Caption;
end;

procedure TdxLabel.SetName(const Value: TComponentName);
begin
  if (FieldName = '') or (CompareText(FFieldName, Name) = 0) then FFieldName := Value;
  inherited SetName(Value);
end;

{ TdxDateEdit }

procedure TdxDateEdit.MenuPopup(Sender: TObject);
var
  ae: Boolean;
begin
  ae := DataSource.AutoEdit;
  PopupMenu.Items[0].Enabled := (SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled := (Text <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[6].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[7].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[8].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[9].Enabled := (not ReadOnly) and ae;
end;

procedure TdxDateEdit.MenuClick(Sender: TObject);
var
  D: TDateTime;
begin
  D := SysUtils.Date;
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4:
      begin
        if not (Field.DataSet.State in [dsInsert, dsEdit]) then
          Field.DataSet.Edit;
        Field.SetData(nil);
      end;
    6: Self.Date := D;
    7: Self.Date := IncDay(D, -DayOfTheWeek(D) + 1);
    8: Self.Date := IncDay(D, -DayOf(D) + 1);
    9: Self.Date := EncodeDate(YearOf(D), 1, 1);
  end;
end;

function TdxDateEdit.GetDefaultGlyphName: String;
begin
  Result:='date16';
end;

procedure TdxDateEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
var
  S: String;
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then S := 'date48'
  else if AHeight > 36 then S := 'date32'
  else if AHeight > 28 then S := 'date24'
  else S := 'date16';
  Button.LoadGlyphFromLazarusResource(S);
end;

constructor TdxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, 'cut16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, 'paste16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, ShortCut(VK_DELETE, [ssCtrl]), @MenuClick, 'delete16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsToday, 6, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginWeek, 7, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginMonth, 8, 0, @MenuClick, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginYear, 9, 0, @MenuClick, '') );
  PopupMenu.OnPopup:=@MenuPopup;
  Button.PopupMenu := PopupMenu;
end;

{ TdxCalcEdit }

procedure TdxCalcEdit.MenuPopup(Sender: TObject);
var
  ae: Boolean;
begin
  ae := DataSource.AutoEdit;
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Text <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[5].Enabled := (not ReadOnly) and ae;
end;

procedure TdxCalcEdit.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4:
      begin
        SetDSEdit(Field.DataSet);
        Field.SetData(nil);
      end;
    5:
      begin
        SetDSEdit(Field.DataSet);
        Field.Value := 0;
      end;
  end;
end;

function TdxCalcEdit.GetDefaultGlyphName: String;
begin
  Result:='calc16';
end;

procedure TdxCalcEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
var
  S: String;
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then S := 'calc48'
  else if AHeight > 36 then S := 'calc32'
  else if AHeight > 28 then S := 'calc24'
  else S := 'calc16';
  Button.LoadGlyphFromLazarusResource(S);
end;

constructor TdxCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FDefaultValue := '0';
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, 'cut16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, 'paste16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, ShortCut(VK_DELETE, [ssCtrl]), @MenuClick, 'delete16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsSetToZero, 5, 0, @MenuClick, '') );
  PopupMenu.OnPopup:=@MenuPopup;
  Button.PopupMenu := PopupMenu;
end;

function TdxCalcEdit.PrecStr: String;
begin
  Result := '0';
  if Precission > 0 then
    Result := '0.' + DupeString('0', Precission);
end;

{ TdxScrollBox }

procedure TdxScrollBox.Paint;
begin
  inherited Paint;
  if Color = clDefault then Exit;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

{ TUtf8StringList }

{function TUtf8StringList.DoCompareText(const s1, s2: string): PtrInt;
begin
  Result:=Utf8CompareText(s1, s2);
end;

function TUtf8StringList.QuickFind(const S: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if S = Strings[i] then Exit(i);
end; }

{ TdxComboBox }

procedure TdxComboBox.Loaded;
begin
  inherited Loaded;
  FOldSize := FFieldSize;
end;

procedure TdxComboBox.UpdateData(Sender: TObject);
begin
  if (not ReadOnly) and (Field.DataSet.State in [dsEdit, dsInsert]) then
    inherited UpdateData(Sender)
  else Text := Field.Text;
end;

procedure TdxComboBox.Select;
begin
  inherited Select;
  if Style = csDropDownList then EditingDone;
end;

procedure TdxComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  inherited UTF8KeyPress(UTF8Key);
  if FOnMyUtf8KeyPress <> nil then
  begin
    S := Utf8Key;
    FOnMyUtf8KeyPress(Self, S);
    Utf8Key := S;
  end;
end;

constructor TdxComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoComplete:=True;
  ControlStyle := ControlStyle - [csSetCaption];
  FFieldSize := 150; FOldSize := 150;
end;

{ TdxLookupComboBox }

procedure TdxLookupComboBox.ButtonChangeBounds(Sender: TObject);
var
  S: String;
begin
  if Button = nil then Exit;
  Button.Width:=Height;
  Button.Height := Height;
  if Height > 52 then S := 'form48'
  else if Height > 36 then S := 'form32'
  else if Height > 28 then S := 'form24'
  else S := 'form16';
  Button.LoadGlyphFromLazarusResource(S);
end;

procedure TdxLookupComboBox.DoButtonClick(Sender: TObject);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Self);
end;

procedure TdxLookupComboBox.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

function TdxLookupComboBox.GetKeyValue: Variant;
begin
  Result := Field.DataSet.FieldByName(FKeyField).Value;
end;

procedure TdxLookupComboBox.SetFieldsTables(AValue: TStrings);
begin
  FieldsTables.Assign(AValue);
end;

procedure TdxLookupComboBox.SetKeyValue(AValue: Variant);
begin
  if {(not ReadOnly) and }(Field.DataSet.State in [dsInsert, dsEdit]) then
    Field.DataSet.FieldByName(FKeyField).Value := AValue;
end;

procedure TdxLookupComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ssCtrl in Shift then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TdxLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (ssCtrl in Shift) then
    if FOnCtrlClick <> nil then FOnCtrlClick(Self);
end;

procedure TdxLookupComboBox.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  DoPositionButton;
end;

procedure TdxLookupComboBox.Loaded;
begin
  inherited Loaded;
  DoPositionButton;
end;

procedure TdxLookupComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TdxLookupComboBox.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FButton.Visible := Value;
end;

procedure TdxLookupComboBox.UpdateData(Sender: TObject);
begin
  if (not ReadOnly) and (Field.DataSet.State in [dsEdit, dsInsert]) then
    inherited UpdateData(Sender)
  else Text := Field.Text;
end;

procedure TdxLookupComboBox.DropDown;
begin
  inherited DropDown;
  //if FOnCbxDropDown <> nil then FOnCbxDropDown(Self);
end;

procedure TdxLookupComboBox.Change;
begin
  inherited Change;
  //if FOnCbxChange <> nil then FOnCbxChange(Self);
end;

procedure TdxLookupComboBox.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  Button.Enabled:=Value;
end;

procedure TdxLookupComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  inherited UTF8KeyPress(UTF8Key);
  if FOnMyUtf8KeyPress <> nil then
  begin
    S := Utf8Key;
    FOnMyUtf8KeyPress(Self, S);
    Utf8Key := S;
  end;
end;

procedure TdxLookupComboBox.EditingDone;
var
  Key: PtrInt;
  i: Integer;
begin
  //ShowMessage(Text + '         ' + Field.AsString);
  if (Field = nil) or (Field.DataSet = nil) or
    (not (Field.DataSet.State in [dsEdit, dsInsert])) or ReadOnly then Exit;
  i := ItemIndex;
  if (i < 0) and (Text <> '') then
  	i := Items.IndexOf(Text);
  //if ItemIndex >= 0 then
  if i >= 0 then
  begin
    Key := PtrInt(Items.Objects[i]);
    {if Key <> KeyValue then KeyValue := Key;
    Text := Items[ItemIndex];
    Field.Text := Text; }
    if Key <> KeyValue then
    begin
      KeyValue := Key;
    	Text := Items[i];
    	Field.Text := Text;
    end;
  end
  else if (Text = '') or (Utf8CompareText(Text, Field.AsString) <> 0) then
  begin
    if KeyValue <> Null then KeyValue := Null;
    if Field.Value <> Null then Field.Value := Null;
    Text := '';
  end;
  inherited EditingDone;
end;

procedure TdxLookupComboBox.Clear;
begin
  Field.SetData(nil);
  KeyValue:=Null;
end;

constructor TdxLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoComplete:=True;
  ControlStyle := ControlStyle - [csSetCaption];

  FButton := TSpeedButton.Create(Self);
  FButton.Width := 23;//Self.Height;
  FButton.Height := 23;//Self.Height;
  FButton.FreeNotification(Self);
  FButton.OnClick:=@DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  FButton.LoadGlyphFromLazarusResource('form16');
  FButton.Flat := True;
  FButton.OnChangeBounds:=@ButtonChangeBounds;

  FOldItemIndex := -1;
  FFieldsTables := TStringList.Create;
end;

destructor TdxLookupComboBox.Destroy;
begin
  FFieldsTables.Free;
  FreeAndNil(FButton);
  inherited Destroy;
end;

{ TdxEdit }

procedure TdxEdit.MenuPopup(Sender: TObject);
var
  S: String;
  ae: Boolean;
begin
  ae := DataSource.AutoEdit;
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled:=(not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Text <> '') and (not ReadOnly) and ae;
  S := UTF8LowerCase(Text);
  PopupMenu.Items[5].Visible := IsUrl(S) or IsMail(S);
  PopupMenu.Items[6].Visible := PopupMenu.Items[5].Visible;
end;

procedure TdxEdit.SetDataSetEdit;
var
  n, m: Integer;
begin
  // При переводе в режим редактирования, курсор переводится на первый
  // символ. Это неудобно. Сохраняем позицию и восстанавливаем ее.
  n := SelStart;
  m := SelLength;
  Field.DataSet.Edit;
  if Field.DataSet.State = dsEdit then
  begin
	  SelStart := n;
  	SelLength := m;
  end
end;

procedure TdxEdit.MenuClick(Sender: TObject);
var
  S: String;
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4:
      begin
        if not (Field.DataSet.State in [dsInsert, dsEdit]) then
          Field.DataSet.Edit;
        Field.SetData(nil);
      end;
    6:
      begin
        S := Utf8LowerCase(Trim(Text));
        if IsUrl(S) or IsMail(S) then OpenUrl(S);
      end;
  end;
end;

procedure TdxEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ssCtrl in Shift then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TdxEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  S: String;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    S := Utf8LowerCase(Trim(Text));
    if IsUrl(S) or IsMail(S) then
      OpenUrl(S);
  end;
end;

procedure TdxEdit.Loaded;
begin
  inherited Loaded;
  FOldSize := FFieldSize;
end;

procedure TdxEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // Если форма не в режиме редактирования, то удаление символов в тексте с
  // маской не переведет форму в режим редактирования, т. к. предок "гасит"
  // клавишу (Key = 0).
  if (EditMask <> '') and (Key in [VK_DELETE, VK_BACK]) then
  	with Field.DataSet do
  	  if Active and CanModify and (not (State in [dsInsert, dsEdit])) and
      	Self.DataSource.AutoEdit then
      begin
    		SetDataSetEdit;
        if State <> dsEdit then Key := 0;
      end;
  inherited KeyDown(Key, Shift);
end;

procedure TdxEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  if Field = nil then
  begin
    Utf8Key := '';
    Exit;
  end;

  if EditMask <> '' then
	  with Field.DataSet do
  	  if Active and CanModify and (not (State in [dsInsert, dsEdit])) and
      	Self.DataSource.AutoEdit then
      begin
    		SetDataSetEdit;
        if State <> dsEdit then Utf8Key := '';
      end;
  inherited UTF8KeyPress(UTF8Key);
  if FOnMyUtf8KeyPress <> nil then
  begin
    S := UTF8Key;
    FOnMyUtf8KeyPress(Self, S);
    UTF8Key := S;
  end;
end;

constructor TdxEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  CustomEditMask := True;
  FFieldSize := 150; FOldSize := 150;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, 'cut16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, 'paste16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, ShortCut(VK_DELETE, [ssCtrl]), @MenuClick, 'delete16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil, '') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsGotoURL, 6, 0, @MenuClick, 'goto16') );
  PopupMenu.OnPopup:=@MenuPopup;
end;

procedure TdxEdit.ValidateEdit;
begin
  // Метод предка не позволяет добиться нужного поведения программы при
  // некорректном вводе. Нужное поведение - не позволять сохранять запись,
  // если есть ошибки ввода.
end;

function TdxEdit.ValidateText: Boolean;
begin
  Result := MaskedTextEmpty(Text, EditMask) or ValidText(Text, EditMask);
end;

function TdxEdit.MaskTextEmpty: Boolean;
begin
  Result := MaskedTextEmpty(Text, EditMask);
end;

procedure TdxEdit.EditingDone;
begin
  if Field=nil then Exit;

  // Маска может содержать литералы. Если текст содержит только литералы,
  // без введенных данных, то очищает поле совсем.
  if (EditMask <> '') and MaskedTextEmpty(Text, EditMask) then
    if Field.DataSet.State in [dsInsert, dsEdit] then
  		Field.SetData(nil);
  inherited EditingDone;
end;

{ TdxMemo }

procedure TdxMemo.DoButtonClick(Sender: TObject);
begin
  if FOnButtonClick <> nil then FOnButtonClick(Self);
end;

procedure TdxMemo.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
  end;
end;

procedure TdxMemo.MenuPopup(Sender: TObject);
var
  ae: Boolean;
begin
  ae := DataSource.AutoEdit;
  PopupMenu.Items[0].Enabled := (SelText <> '') and (not ReadOnly);
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := (not ReadOnly) and ae;
end;

procedure TdxMemo.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible and (FSourceFId > 0);
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

procedure TdxMemo.SetSourceFId(AValue: Integer);
begin
  if FSourceFId=AValue then Exit;
  FSourceFId:=AValue;
  FButton.Visible := AValue > 0;
end;

procedure TdxMemo.Loaded;
begin
  inherited Loaded;
  FOldSize := FFieldSize;
  DoPositionButton;
end;

procedure TdxMemo.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  DoPositionButton;
end;

procedure TdxMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TdxMemo.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if FButton <> nil then
    FButton.Height := Height;
end;

procedure TdxMemo.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FButton.Visible:=Value and (FSourceTId > 0);
end;

procedure TdxMemo.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FButton.Enabled:=Value;
end;

procedure TdxMemo.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  inherited UTF8KeyPress(UTF8Key);
  if FOnMyUtf8KeyPress <> nil then
  begin
    S := Utf8Key;
    FOnMyUtf8KeyPress(Self, S);
    Utf8Key := S;
  end;
end;

constructor TdxMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  ScrollBars:=ssBoth;
  FFieldSize := 400; FOldSize := 400;

  FButton := TSpeedButton.Create(Self);
  FButton.Width := 23;
  FButton.Height := Height;
  FButton.FreeNotification(Self);
  FButton.OnClick:=@DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable, csNoDesignVisible];
  FButton.LoadGlyphFromLazarusResource('add16');
  FButton.Flat := True;
  FButton.Visible := False;

  FDelimiter := ', ';
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, 'cut16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, 'copy16') );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, 'paste16') );
  PopupMenu.OnPopup:=@MenuPopup;
end;

destructor TdxMemo.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

{ TdxTabSheet }

procedure TdxTabSheet.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root is TdxForm  then
    inherited GetChildren(Proc, Root)
  else
    for I := 0 to ControlCount-1 do
      if not ((Controls[i] is TSpeedButton) or (Controls[i] is TGridButtons)) then
        Proc(Controls[i]);
end;

function TdxTabSheet.GetClientRect: TRect;
begin
  Result:=inherited GetClientRect;
  // В версии 1.7, которая используется в Windows есть ошибка с определением
  // клиентской области.
  {$ifdef windows}
  Result.Width := PageControl.Height;
  Result.Height := PageControl.Width;
  {$endif}
end;

constructor TdxTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := rsTabSheet;
end;

{ TdxPageControl }

procedure TdxPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root is TdxForm  then
    inherited GetChildren(Proc, Root)
  else
    for I := 0 to ControlCount-1 do
      if not ((Controls[i] is TSpeedButton) or (Controls[i] is TGridButtons)) then
        Proc(Controls[i]);
end;

function TdxPageControl.GetPageClass: TCustomPageClass;
begin
  Result:=TdxTabSheet;
end;

{ TdxGroupBox }

procedure TdxGroupBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root is TdxForm  then
    inherited GetChildren(Proc, Root)
  else
    for I := 0 to ControlCount-1 do
      if not ((Controls[i] is TSpeedButton) or (Controls[i] is TGridButtons)) then
        Proc(Controls[i]);
end;

constructor TdxGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := rsGroup;
end;

{ TdxCheckBox }

constructor TdxCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ValueChecked:='1';
  ValueUnchecked:='0';
  FCheckedText := rsYes;
  FUnCheckedText := rsNo;
  ControlStyle := ControlStyle - [csSetCaption];
  FDefaultValue := '0';
end;


{ TdxForm }

procedure TdxForm.SetTemplates(AValue: TStringList);
begin
  FTemplates.Assign(AValue);
end;

procedure TdxForm.SetTreeFont(AValue: TFont);
begin
  FTreeFont.Assign(AValue);
end;

{procedure TdxForm.BeginSearch;
begin
  FOldAfterScroll := FOnAfterScroll;
  FOldBeforeScroll := FOnBeforeScroll;
  FOnAfterScroll := nil;
  FOnBeforeScroll := nil;
  FState := fsFind;
end;

procedure TdxForm.EndSearch;
begin
  FOnAfterScroll := FOldAfterScroll;
  FOnBeforeScroll := FOldBeforeScroll;
  FState := fsBrowse;
end;

function TdxForm.CompareExpr: Boolean;
var
  V: Variant;
begin
  Result := False;
  if FExpr = nil then Exit;
  V := TExpression(FExpr).Calc;
  if VarIsBool(V) then Result := V;
end;       }

function TdxForm.GetEditWindow: TObject;
begin
  Result := PDataSetRec(FDSR)^.EditFm;
end;

function TdxForm.GetFilter: TFilterObject;
begin
  Result := PDataSetRec(FDSR)^.Filter;
end;

function TdxForm.GetAsI(Index: String): Integer;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxForm.GetAsDT(Index: String): TDateTime;
begin
  Result := Nz(Fields[Index], 0);
end;

function TdxForm.GetAsF(Index: String): Extended;
begin
	Result := Nz(Fields[Index], 0);
end;

function TdxForm.GetAsS(Index: String): String;
begin
	Result := Nz(Fields[Index], '');
end;

function TdxForm.GetFormByIndex(Index: Integer): TdxForm;
begin
  with TDataSetProcessor(FDSP) do
    Result := DataSets[Index + 1]^.Form;
end;

function TdxForm.GetFormCount: Integer;
begin
  Result := TDataSetProcessor(FDSP).DataSetCount - 1;
end;

function TdxForm.GetForms(Index: String): TdxForm;
var
  i: Integer;
  Fm: TdxForm;
begin
  Result := nil;
  with TDataSetProcessor(FDSP) do
    for i := 1 to DataSetCount - 1 do
    begin
      Fm := DataSets[i]^.Form;
      if UTF8CompareText(Fm.FormCaption, Index) = 0 then Exit(Fm);
    end;
  raise Exception.CreateFmt(rsFormNotFound, [Index]);
end;

function TdxForm.GetFormGrid: TdxGrid;
begin
  Result := PDataSetRec(FDSR)^.Grid;
end;

function TdxForm.GetModified: Boolean;
begin
  Result := FDataSet.Modified;
end;

function TdxForm.GetOldValues(Index: String): Variant;
var
  C: TComponent;
begin
  Result := Null;
  if Index = '' then raise Exception.Create(rsFieldNameEmpty);
  C := FindComponentByFieldName(Self, Index);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [Index]);
  Result := FDataSet.FieldByName(FieldStr(C)).OldValue;
end;

function TdxForm.GetParentForm: TdxForm;
begin
  Result := nil;
  if FPId > 0 then
	  Result := TDataSetProcessor(FDSP).Form;
end;

function TdxForm.GetQueries(Index: String): TObject;
var
  //Q: TdxQueryGrid;
  i: Integer;
  //RD: TReportData;
  Q: TQueryRec;
begin
  Result := nil;
  with TDataSetProcessor(FDSP) do
    for i := 0 to QueryCount - 1 do
    begin
      Q := Queries[i]^;
    	if UTF8CompareText(Index, Q.RD.Name) = 0 then Exit(Q.Grid);
    end;
  {for i := 0 to QueryCount - 1 do
  begin
    Q := TdxQueryGrid(QueryByIndex[i]);
    RD := ReportMan.FindReport(Q.Id);
    if Utf8CompareText(RD.Name, Index) = 0 then Exit(Q);
  end; }
  raise Exception.CreateFmt(rsQueryNotFound, [Index]);
end;

function TdxForm.GetQueryByIndex(Index: Integer): TObject;
begin
  with TDataSetProcessor(FDSP) do
    Result := Queries[Index]^.Grid;
end;

function TdxForm.GetQueryCount: Integer;
begin
  with TDataSetProcessor(FDSP) do
    Result := QueryCount;
end;

function TdxForm.GetState: TDataSetState;
begin
  Result := FDataSet.State;
end;

procedure TdxForm.ReadShopData(Reader: TReader);
var
  SD: TShopData;
  S: String;
  SL: TStringList;
begin
  SD := FShopData;
  S := Reader.ReadString;
  SL := TStringList.Create;
  SplitStr(S, '|', SL);
  SD.ObjId:=StrToInt(SL[0]);
  SD.QttyInput:=Str2Bool(SL[1]);
  SD.QttyFId:=StrToInt(SL[2]);
  SD.PriceInput:=Str2Bool(SL[3]);
  SD.PriceObjFId:=StrToInt(SL[4]);
  SD.PriceFId:=StrToInt(SL[5]);
  SD.AddToExisting:=Str2Bool(SL[6]);
  SL.Free;
end;

procedure TdxForm.WriteShopData(Writer: TWriter);
var
  SD: TShopData;
  S: String;
begin
  SD := FShopData;
  S := IntToStr(SD.ObjId) + '|' + Bool2Str(SD.QttyInput) + '|' +
    IntToStr(SD.QttyFId) + '|' + Bool2Str(SD.PriceInput) + '|' +
    IntToStr(SD.PriceObjFId) + '|' + IntToStr(SD.PriceFId) + '|' +
    Bool2Str(SD.AddToExisting);
  Writer.WriteString(S);
end;

procedure TdxForm.SetCalcFields(AValue: TStrings);
begin
  FCalcFields.Assign(AValue);
end;

function TdxForm.GetFields(Index: String): Variant;
begin
  Result := FormLookupFieldValue(Self, FDataSet, Index);
end;

procedure TdxForm.SetColoring(AValue: TStrings);
begin
  FColoring.Assign(AValue);
end;

procedure TdxForm.SetFields(Index: String; AValue: Variant);
var
  C: TComponent;
begin
  C := FindComponentByFieldName(Self, Index);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [Index]);
  FDataSet.FieldByName(FieldStr(C)).Value := AValue;
  if C is TdxLookupComboBox then
    if aValue <> Null then
      FDataSet.FieldByName(FieldStr(C) + 'l').Value := GetObjFieldValue(C, AValue, True);
end;

procedure TdxForm.SetFilters(AValue: TStrings);
begin
  FFilters.Assign(AValue);
end;

procedure TdxForm.SetGrid(AValue: TdxGrid);
begin
  FreeAndNil(FGrid);
  FGrid:=AValue;
end;

procedure TdxForm.SetHelpText(AValue: TStrings);
begin
  FHelpText.Assign(AValue);
end;

procedure TdxForm.Loaded;
begin
  inherited Loaded;
  FGrid := TdxGrid(FindComponent('Grid'));
end;

procedure TdxForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
  Proc(FGrid);
end;

procedure TdxForm.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Shopping', @ReadShopData, @WriteShopData, FShopData.ObjId > 0);
end;

procedure TdxForm.BeginDuplicate;
begin
  FOldBeforeInsert := FOnBeforeInsert;
  FOldBeforeScroll := FOnBeforeScroll;
  FOldBeforePost := FOnBeforePost;
  FOldAfterInsert := FOnAfterInsert;
  FOldAfterScroll := FOnAfterScroll;
  FOldAfterPost := FOnAfterPost;
  FOnBeforeInsert := nil;
  FOnBeforeScroll := nil;
  FOnBeforePost := nil;
  FOnAfterInsert := nil;
  FOnAfterScroll := nil;
  FOnAfterPost := nil;
  if FOnBeforeDuplicate <> nil then FOnBeforeDuplicate(Self);
end;

procedure TdxForm.EndDuplicate;
begin
  FOnBeforeInsert := FOldBeforeInsert;
  FOnBeforeScroll := FOldBeforeScroll;
  FOnBeforePost := FOldBeforePost;
  FOnAfterInsert := FOldAfterInsert;
  FOnAfterScroll := FOldAfterScroll;
  FOnAfterPost := FOldAfterPost;
  if FOnAfterDuplicate <> nil then FOnAfterDuplicate(Self);
end;

procedure TdxForm.BeginRecalc;
begin
  FOldBeforeEdit := FOnBeforeEdit;
  FOldBeforeScroll := FOnBeforeScroll;
  FOldBeforePost := FOnBeforePost;
  FOldAfterEdit := FOnAfterEdit;
  FOldAfterScroll := FOnAfterScroll;
  FOldAfterPost := FOnAfterPost;
  FOnBeforeEdit := nil;
  FOnBeforeScroll := nil;
  FOnBeforePost := nil;
  FOnAfterEdit := nil;
  FOnAfterScroll := nil;
  FOnAfterPost := nil;
  if FOnBeforeRecalculate <> nil then FOnBeforeRecalculate(Self);
end;

procedure TdxForm.EndRecalc;
begin
  FOnBeforeEdit := FOldBeforeEdit;
  FOnBeforeScroll := FOldBeforeScroll;
  FOnBeforePost := FOldBeforePost;
  FOnAfterEdit := FOldAfterEdit;
  FOnAfterScroll := FOldAfterScroll;
  FOnAfterPost := FOldAfterPost;
  if FOnAfterRecalculate <> nil then FOnAfterRecalculate(Self);
end;

constructor TdxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];
  ParentColor := False;
  ParentFont := False;
  FGrid := TdxGrid.Create(nil);
  FGrid.Name:='Grid';
  FGrid.Font.Name := 'Verdana';
  FGrid.Font.Size := 10;
  FGrid.ReadOnly:=True;
  FGrid.VisibleButtons:=[];
  //FGrid.Options := FGrid.Options - [dgTabs];
  FTemplates := TStringList.Create;
  FCalcFields := TStringListUtf8.Create;
  FFilters := TStringList.Create;
  FColoring := TStringList.Create;
  Font.Name:='Verdana';
  Font.Size := 10;
  FAutoOpen:=True;
  Width := 350; Height := 300;
  FViewType:=vtGridOnly;
  FShopData := TShopData.Create;
  FHelpText := TStringList.Create;
  FTreeFont := TFont.Create;
  FTreeFont.Assign(Font);
  FTreeBackColor := clWindow;
  FTreeLineColor := clWindowFrame;
  FTreeSelectColor := clHighlight;
  FTreeWidth := 250;
  FParams := TParamList.Create;
end;

destructor TdxForm.Destroy;
begin
  FParams.Free;
  FreeAndNil(FExpr);
  FTreeFont.Free;
  FHelpText.Free;
  FShopData.Free;
  FColoring.Free;
  FFilters.Free;
  FCalcFields.Free;
  FTemplates.Free;
  FGrid.Free;
  inherited Destroy;
end;

function TdxForm.HasShop: Boolean;
begin
  Result := FShopData.ObjId > 0;
end;

procedure TdxForm.Append;
begin
  with PDataSetRec(FDSR)^ do
  begin
    Grid.EditorMode:=False;
    DataSet.Append;
  end;
end;

procedure TdxForm.Edit;
begin
  with PDataSetRec(FDSR)^ do
  begin
    Grid.EditorMode:=False;
    DataSet.Edit;
  end;
  if FPId = 0 then
    TDataSetProcessor(FDSP).DoStateChange;
end;

function TdxForm.Delete: Boolean;
begin
  Result := True;
  if FPId = 0 then
    with TDataSetProcessor(FDSP) do
    begin
      if not CheckDeleteRecord(FId, FDataSet['id'], False) then Exit(False);
      InnerDelete;
    end
  else
    with PDataSetRec(FDSR)^ do
    begin
      Grid.EditorMode := False;
      DataSet.Delete;
    end;
end;

procedure TdxForm.Post;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Post
  else
    FDataSet.Post;
end;

procedure TdxForm.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TdxForm.Refresh;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Refresh;
end;

procedure TdxForm.MoveFirst;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).First
  else
    FDataSet.First;
end;

procedure TdxForm.MovePrior;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Prior
  else
    FDataSet.Prior;
end;

procedure TdxForm.MoveNext;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Next
  else
    FDataSet.Next;
end;

procedure TdxForm.MoveLast;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Last
  else
    FDataSet.Last;
end;

procedure TdxForm.MoveBy(Distance: Integer);
begin
  FDataSet.MoveBy(Distance);
end;

procedure TdxForm.MoveTo(aRecNo: Integer);
begin
  MoveBy(aRecNo - RecNo)
end;

function TdxForm.Bof: Boolean;
begin
  Result := FDataSet.BOF;
end;

function TdxForm.Eof: Boolean;
begin
  Result := FDataSet.EOF;
end;

function TdxForm.RecNo: Integer;
begin
  Result := FDataSet.RecNo;
end;

function TdxForm.RecId: Integer;
begin
  Result := FDataSet.FieldByName('id').AsInteger;
end;

function TdxForm.RecordCount: Integer;
begin
  Result := FDataSet.RecordCount;
end;

procedure TdxForm.Print(const TemplateName, OutFileName: String;
  var Errs: String; aOpenFile: Boolean);
var
  S: String;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  S := TemplateName;
  if not FilenameIsAbsolute(S) then
    S := GetTemplatesDir + TemplateName;
  TDataSetProcessor(FDSP).InnerPrint(S, OutFileName, Errs, aOpenFile);
end;

{function TdxForm.FindFirst(const aExpr: String): Boolean;
begin
  Result := False;
  FreeAndNil(FExpr);
  with TExpressionBuilder.Create do
  try
    SkipLabels:=True;
    Form := Self;
    DataSet := FDataSet;
    if FParentForm <> nil then
      ParentForm := FParentForm
    else
      ParentForm := Self;
    FExpr := Build(aExpr);
  finally
    Free;
  end;

  FFirstSearch := True;
  //Result := CompareExpr;
  //if not Result then Result := FindNext;
  Result := FindNext;
end;

function TdxForm.FindNext: Boolean;
var
  B: TBookmark;
begin
  Result := False;
  if FExpr = nil then Exit;

  BeginSearch;
  FDataSet.DisableControls;
  B := FDataSet.Getbookmark;

  try

  if FFirstSearch then
  begin
    FFirstSearch:=False;
    FDataSet.First;
  end;

  while not FDataSet.Eof do
  begin
    FDataSet.Next;
    Result := CompareExpr;
    if Result then Break;
  end;
  if not Result then
    FDataSet.GotoBookmark(B);

  finally
    FDataSet.FreeBookmark(B);
    FDataSet.EnableControls;
    EndSearch;
    if Result then FDataSet.AfterScroll(FDataSet);
  end;
end;

function TdxForm.FindPrior: Boolean;
var
  B: TBookmark;
begin
  Result := False;
  if FExpr = nil then Exit;

  BeginSearch;
  FDataSet.DisableControls;
  B := FDataSet.Getbookmark;

  try

  while not FDataSet.Bof do
  begin
    FDataSet.Prior;
    Result := CompareExpr;
    if Result then Break;
  end;
  if not Result then
    FDataSet.GotoBookmark(B);

  finally
    FDataSet.FreeBookmark(B);
    FDataSet.EnableControls;
    EndSearch;
    if Result then FDataSet.AfterScroll(FDataSet);
  end;
end;      }

function TdxForm.Locate(const FieldName: String; FieldValue: Variant;
  Options: TLocateOptions): Boolean;
var
  C: TComponent;
begin
  C := FindComponentByFieldName(Self, FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
  Result := FDataSet.Locate(FieldStr(C), FieldValue, Options);
end;

function TdxForm.GotoRecord(aRecId: Integer): Boolean;
begin
  Result := FDataSet.Locate('id', aRecId, []);
end;

procedure TdxForm.DisableControls;
begin
  FDataSet.DisableControls;
end;

procedure TdxForm.EnableControls;
begin
  FDataSet.EnableControls;
end;

function TdxForm.ControlsDisabled: Boolean;
begin
  Result := FDataSet.ControlsDisabled;
end;

function TdxForm.CanAppend: TAccessStatus;
var
  pDSR, pDSR0: PDataSetRec;
begin
  Result := asOk;
  pDSR := PDataSetRec(FDSR);
  if FPId = 0 then
  begin
    with TDataSetPRocessor(FDSP) do
      if not pDSR^.Adding then Result := asCantAppend
  end
  else
  begin
    pDSR0 := TDataSetProcessor(FDSP).DataSets[0];
    if (not pDSR0^.Editing) or (not pDSR^.Adding) then Result := asCantAppend;
  end;
end;

function TdxForm.CanEdit: TAccessStatus;
var
  pDSR, pDSR0: PDataSetRec;
begin
  Result := asOk;
  pDSR := PDataSetRec(FDSR);
  if FPId = 0 then
  begin
    with TDataSetPRocessor(FDSP) do
      if not pDSR^.Editing then Result := asCantEdit
      else if InnerCheckLockRecord(True) >= 0 then Result := asLocked
      else
        case InnerCheckModifyRecord of
          1: Result := asDeleted;
          2: Result := asModified;
        end;
  end
  else
  begin
    pDSR0 := TDataSetProcessor(FDSR).DataSets[0];
    if (not pDSR0^.Editing) or (not pDSR^.Editing) then Result := asCantEdit;
  end;
end;

function TdxForm.CanDelete: TAccessStatus;
var
  pDSR, pDSR0: PDataSetRec;
begin
  Result := asOk;
  pDSR := PDataSetRec(FDSR);
  if FPId = 0 then
  begin
    with TDataSetPRocessor(FDSP) do
      if not pDSR^.Deleting then Result := asCantDelete
      else if InnerCheckLockRecord(True) >= 0 then Result := asLocked
      else
        case InnerCheckModifyRecord of
          1: Result := asDeleted;
          2: Result := asModified;
        end;
  end
  else
  begin
    pDSR0 := TDataSetProcessor(FDSR).DataSets[0];
    if (not pDSR0^.Editing) or (not pDSR^.Deleting) then Result := asCantDelete;
  end;
end;

function TdxForm.Show: Boolean;
begin
  Result := TDataSetProcessor(FDSP).ShowEditForm(FDSRi) = mrOk;
end;

{procedure TdxForm.AddUserFilter(const FieldName: String; aNot, aNull: Boolean;
  Values: array of Variant);
var
  S, W: String;
  C: TComponent;
  i, MaxI: Integer;
  V: Variant;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  with PDataSetRec(FDSR)^ do
  begin
    S := Filter;
    if (S = '') or (S = 'FILTER:') then S := 'FILTER:'
    else S := S + ' ~~ ';
    C := FindComponentByFieldName(Self, FieldName);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    S := S + IntToStr(GetId(C)) + '|' + Bool2Str(aNot) + '|' + Bool2Str(aNull) + '|';
    i := 0; MaxI := High(Values);
    while i <= MaxI do
    begin
      if (C is TdxDateEdit) or (C is TdxTimeEdit) or (C is TdxCalcEdit) or
        (C is TdxCounter) then
      begin
        V := Values[i];
        W := VarToStr(V) + ' .. ';
        if i < MaxI then Inc(i);
        V := Values[i];
        W := W + VarToStr(V);
      end
      else
        W := VarToStr(Values[i]);
      S := S + W + ';';
      Inc(i);
    end;
    S := Copy(S, 1, Length(S) - 1);
    Filter := S;
  end;
end;

procedure TdxForm.ClearUserFilter;
begin
  if FPId = 0 then
    PDataSetRec(FDSR)^.Filter := 'FILTER:';
end; }

procedure TdxForm.Open;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Open;
end;

procedure TdxForm.Close;
begin
  if FPId = 0 then
    TDataSetProcessor(FDSP).Close;
end;

procedure TdxForm.OpenRecords(const aFilter: String; SelCond: Boolean);
var
  Flt, Flt2: String;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Flt := '';
  if SelCond then
  begin
    Flt := UserMan.GetSelCond(FId);
    if Flt <> '' then Flt := SqlSelCondFilter(Self, Flt);
    if Flt <> '' then Flt := '(' + Flt + ')';
  end;

  Flt2 := aFilter;
  if Flt2 <> '' then
    Flt2 := SqlSelCondFilter(Self, Flt2);
  if Flt2 <> '' then Flt2 := '(' + Flt2 + ')';
  if (Flt <> '') and (Flt2 <> '') then
    Flt := Flt + ' and ' + Flt2
  else
    Flt := Flt + Flt2;        // Одна из них непустая

  with TDataSetProcessor(FDSP) do
    _Open2(Flt);
end;

procedure TdxForm.OpenRecord(aRecId: Integer);
var
  pDS: PDataSetRec;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  pDS := PDataSetRec(FDSR);
  with TDataSetProcessor(FDSP) do
  begin
    pDS^.DataSet.SQL.Text := pDS^.SQL + ' where ' + TableStr(Self.Id) +
      '.id=' + IntToStr(aRecId);
    pDS^.DataSet.Open;
    if MasterSet.RecordCount = 0 then MasterSet.AfterScroll(MasterSet);
    RefreshLookups(0);
  end;
end;

function TdxForm.Opened: Boolean;
begin
  Result := FDataSet.Active;
end;

function TdxForm.Validate: Boolean;
begin
  Result := TDataSetProcessor(FDSP).Validate(FDSRi);
end;

constructor TWindow.CreateWindow;
begin
  CreateNew(nil);
end;

constructor TWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FParams := TParamList.Create;
end;

destructor TWindow.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

{ TParamList }

function TParamList.GetNames(Index: Integer): String;
begin
  Result := GetParams(Index).Name;
end;

function TParamList.GetObjectFromIndex(Index: Integer): TObject;
begin
  DoGetParam(Names[Index]);
  Result := GetParams(Index).Obj;
end;

function TParamList.GetObjects(Name: String): TObject;
var
  PD: TParamData;
begin
  DoGetParam(Name);
  Result := nil;
  PD := Find(Name);
  if PD <> nil then Result := PD.Obj;
end;

function TParamList.GetValueFromIndex(Index: Integer): Variant;
begin
  DoGetParam(Names[Index]);
  Result := GetParams(Index).Value;
end;

function TParamList.GetValues(Name: String): Variant;
var
  PD: TParamData;
begin
  DoGetParam(Name);
  Result := Null;
  PD := Find(Name);
  if PD <> nil then Result := PD.Value;
end;

procedure TParamList.SetObjectFromIndex(Index: Integer; AValue: TObject);
begin
  GetParams(Index).Obj := AValue;
  DoSetParam(Names[Index]);
end;

procedure TParamList.SetObjects(Name: String; AValue: TObject);
var
  PD: TParamData;
begin
  PD := Find(Name);
  if PD = nil then
    PD := AddParam(Name, Null, AValue)
  else
    PD.Obj := AValue;
  DoSetParam(Name);
end;

procedure TParamList.SetValueFromIndex(Index: Integer; AValue: Variant);
begin
  GetParams(Index).Value := AValue;
  DoSetParam(Names[Index]);
end;

procedure TParamList.SetValues(Name: String; AValue: Variant);
var
  PD: TParamData;
begin
  PD := Find(Name);
  if PD = nil then
    PD := AddParam(Name, AValue, nil)
  else
    PD.Value := AValue;
  DoSetParam(Name);
end;

function TParamList.GetParams(Index: Integer): TParamData;
begin
  Result := TParamData(Items[Index]);
end;

function TParamList.Find(const aName: String): TParamData;
var
  i: Integer;
  PD: TParamData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    PD := GetParams(i);
    if UTF8CompareText(PD.Name, aName) = 0 then Exit(PD);
  end;
end;

procedure TParamList.DoSetParam(const Name: String);
begin
  if OnSetParam <> nil then FOnSetParam(Self, Name);
end;

procedure TParamList.DoGetParam(const Name: String);
begin
  if OnGetParam <> nil then FOnGetParam(Self, Name);
end;

function TParamList.AddParam(const aName: String; aValue: Variant; aObj: TObject
  ): TParamData;
begin
  Result := TParamData.Create;
  Result.Name := aName;
  Result.Value := aValue;
  Result.Obj := aObj;
  Add(Result);
end;

procedure TParamList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetParams(i).Free;
  inherited Clear;
end;

function TParamList.ParamExists(const aName: String): Boolean;
begin
  Result := Find(aName) <> nil;
end;

initialization
  RegisterClass(TdxLabel);
  RegisterClass(TdxEdit);
  RegisterClass(TdxCalcEdit);
  RegisterClass(TdxDateEdit);
  RegisterClass(TdxMemo);
  RegisterClass(TdxCheckBox);
  RegisterClass(TdxComboBox);
  RegisterClass(TdxLookupComboBox);
  RegisterClass(TdxGrid);
  RegisterClass(TdxGroupBox);
  RegisterClass(TdxPageControl);
  RegisterClass(TdxTabSheet);
  RegisterClass(TdxShape);
  RegisterClass(TdxDBImage);
  RegisterClass(TdxImage);
  RegisterClass(TdxFile);
  RegisterClass(TdxForm);
  RegisterClass(TdxObjectField);
  RegisterClass(TdxTimeEdit);
  RegisterClass(TdxCounter);
  RegisterClass(TdxButton);

end.

