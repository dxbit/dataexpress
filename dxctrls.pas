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

unit DxCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, DBCtrls, StdCtrls, ExtCtrls, DBGrids,
  DbCtrlsEx, BGRABitmap, ComCtrls, Db, Grids, Graphics, strconsts, Menus,
  Buttons, Forms, timeedit, LazUtf8, LclType, lists, myctrls, LMessages,
  DxActions, myclasses,
  mytypes, sqldb, dximages, dxfiles, LclIntf, treeviewex
  {$ifdef linux}{,
  gtk2globals }
  {$else}
  {$endif};

const
  StorageTypeDB = 0;
  StorageTypeFolder = 1;
  StorageTypeLink = 2;

  DummyForm = -1;

type
  TdxForm = class;
  TParamList = class;

  TCreateFormEvent = procedure (Sender: TObject; Form: TdxForm) of object;
  TCreateListWindowEvent = procedure (Sender: TObject; aWindow: TForm) of object;
  TCreateReportWindowEvent = procedure (Sender: TObject; aWindow: TForm) of object;
  TValidateEvent = procedure (Sender: TObject; var Ok: Boolean) of object;
  //TPrintFieldEvent = procedure (Sender: TObject; const FieldName: String; var FieldValue: String; var Ok: Boolean) of object;
  TFieldChangeEvent = procedure (Sender, Control: TObject; const FieldName: String) of object;
  TParamNotifyEvent = procedure (Sender: TObject; const ParamName: String) of object;

  TPrintActionType = (paBeginPrint, paEndPrint, paPrintField, paBeginData, paNextData,
    paBeforeOpenFile, paAfterOpenFile, paPrintError);

  TPrintEvent = procedure (Sender: TObject; Action: TPrintActionType;
  	const SourceName, FieldName: String; var Value: String; var Accept: Boolean) of object;

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
    //FOldValue: Variant;
    FValue: Variant;
  protected
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(TheOwner: TComponent); override;
    property Value: Variant read FValue write FValue;
    //property OldValue: Variant read FOldValue write FOldValue;
    property FieldName: String read FFieldName write FFieldName;
  published
    property PopupMenu stored False;
    property Expression: String read FExpr write FExpr;
  end;

  { TdxEdit }

  TdxEdit = class(TDBEditEx)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FId: Integer;
    FOldSize: Integer;
    FFieldSize: Integer;
    //FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FRequired: Boolean;
    FStopTab: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDrawText: String; override;
    //procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; override;
    function ValidateText: Boolean;
    function MaskTextEmpty: Boolean;
    procedure EditingDone; override;
    property OldSize: Integer read FOldSize write FOldSize;
    //property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    //	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Required: Boolean read FRequired write FRequired;
    property Expression: String read FExpression write FExpression;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property PopupMenu stored False;
    property CustomEditMask stored False;
    property TabStop stored False;
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
    FNullToZero: Boolean;
    FPadZeros: Boolean;
    FPrecission: Integer;
    FRequired: Boolean;
    FGroupDigits: Boolean;
    FStopTab: Boolean;
    procedure MenuPopup(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    //function GetDefaultGlyphName: String; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
    function PrecStr: String;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property Precission: Integer read FPrecission write FPrecission;
    property Expression: String read FExpression write FExpression;
    property Required: Boolean read FRequired write FRequired;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property NullToZero: Boolean read FNullToZero write FNullToZero;
    property GroupDigits: Boolean read FGroupDigits write FGroupDigits;
    property PadZeros: Boolean read FPadZeros write FPadZeros;
    property StopTab: Boolean read FStopTab write FStopTab default True;

    property PopupMenu stored False;
    property AutoNum: Boolean read FAutoNum write FAutoNum stored False;
    property TabStop stored False;
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
    FStopTab: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    //function GetDefaultGlyphName: String; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property DateNow: Boolean read FDateNow write FDateNow;
    property PopupMenu stored False;
    property Expression: String read FExpression write FExpression;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
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
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTId: Integer;
    FStopTab: Boolean;
    FUpdateTree: Boolean;
    procedure DoPositionButton;
    function GetSourceFieldName: String;
    function GetSourceFormName: String;
    procedure SetFieldName(AValue: String);
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
    procedure SetReadOnly(AValue: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    function GetDrawText: String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnableButton(AValue: Boolean);
    procedure Change; override;
    property OldSize: Integer read FOldSize write FOldSize;
    property Button: TSpeedButton read FButton;
    property SourceFormName: String read GetSourceFormName;
    property SourceFieldName: String read GetSourceFieldName;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnCreateListWindow: TCreateListWindowEvent read FOnCreateListWindow
      write FOnCreateListWindow;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
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
    property UpdateTree: Boolean read FUpdateTree write FUpdateTree;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
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
    FStopTab: Boolean;
    FUnCheckedText: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
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
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
  end;

  { TdxComboBox }

  TdxComboBox = class(TDBComboBox)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    //FDSP: TObject;
    FEditable: Boolean;
    FExpression: String;
    FFieldName: String;
    FFieldSize: Integer;
    FFilter: String;
    FId: Integer;
    FOldSize: Integer;
    //FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FOnNeedData: TNotifyEvent;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTId: Integer;
    FStopTab: Boolean;
    function GetSourceFieldName: String;
    function GetSourceFormName: String;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure DoNeedData;
  protected
    procedure Loaded; override;
    procedure UpdateData(Sender: TObject); override;
    procedure Select; override;
    //procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure DropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    //procedure ClearData;
    property OldSize: Integer read FOldSize write FOldSize;
    property SourceFormName: String read GetSourceFormName;
    property SourceFieldName: String read GetSourceFieldName;
    //property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    //	write FOnMyUtf8KeyPress;
    property OnNeedData: TNotifyEvent read FOnNeedData write FOnNeedData;
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
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property OnMeasureItem;
  end;

  TInsertValueData = class
  public
    SrcField, DestField: Integer;
  end;

  { TInsertedValues }

  TInsertedValues = class(TList)
  private
    function GetValues(Index: Integer): TInsertValueData;
  public
    function AddValue: TInsertValueData;
    procedure DeleteValue(Index: Integer);
    procedure Clear; override;
    property Values[Index: Integer]: TInsertValueData read GetValues; default;
  end;

  { TLCbxListField }

  TLCbxListField = class(TCollectionItem)
  private
    FFieldId, FWidth: Integer;
    FSearchable: Boolean;
  published
    property FieldId: Integer read FFieldId write FFieldId;
	  property Width: Integer read FWidth write FWidth;
    property Searchable: Boolean read FSearchable write FSearchable;
  end;

  { TLCbxListFields }

  TLCbxListFields = class(TCollection)
  private
    function GetFields(Index: Integer): TLCbxListField;
  public
    constructor Create;
    function Add: TLCbxListField;
    property Fields[Index: Integer]: TLCbxListField read GetFields; default;
  end;

  TdxLookupComboBox = class;

  { TdxLCbxListForm }

  TdxLCbxListForm = class(TForm)
  private
    FControl: TdxLookupComboBox;
    FControlForm: TCustomForm;
    FGrid: TDropDownList;
   // FAccept, FCancel: Boolean;
    FRealDropDownCount: Integer;
    FFrags: TStringList;
    FOldChangeBounds: TNotifyEvent;
    procedure FormChangeBounds(Sender: TObject);
    function GetListHeight: Integer;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure SetControl(AValue: TdxLookupComboBox);
    procedure SetPosition;
    procedure AcceptSelected;
    procedure FindItem;
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetFocusControl(const Utf8Key: String);
  protected
    procedure Activate; override;
    procedure Deactivate; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    destructor Destroy; override;
    procedure ShowForm;
    function IsMouseEntered: Boolean;
    property Control: TdxLookupComboBox read FControl write SetControl;
    property Grid: TDropDownList read FGrid write FGrid;
  end;

  { TdxLookupComboBox }

  TNeedDataEvent = procedure (Sender: TObject; const Text: String) of object;

  TdxLookupComboBox = class(TDBEditEx)
  private
    FAutoComplete: Boolean;
    FCheckExpression: String;
    FClearTableBeforeFill: Boolean;
    FDefaultValue: String;
    FDestTable: Integer;
    FDropDownButton: TSpeedButton;
    FDropDownCount: Integer;
    FEditable: Boolean;
    FExpression: String;
    FFieldsTables: TStrings;
    FFillFilter: String;
    FHideButton: Boolean;
    FHideList: Boolean;
    FInsertedValues: TInsertedValues;
    FButton: TSpeedButton;
    FFieldName: String;
    FFilter: String;
    FId: Integer;
    FItemHeight: Integer;
    //FItemIndex: Integer;
    FKeyField: String;
    FListField: String;
    FListFieldIndex: Integer;
    FListFields: TLCbxListFields;
    FListSource: TDataSource;
    FListWidthExtra: Integer;
    FLookupCache: Boolean;
    FOnButtonClick: TNotifyEvent;
    //FOnCbxChange: TNotifyEvent;
    //FOnCbxDropDown: TNotifyEvent;
    FOnCreateForm: TCreateFormEvent;
    FOnCreateListWindow: TCreateListWindowEvent;
    FOnCtrlClick: TNotifyEvent;
    FOnNeedData: TNeedDataEvent;
    FOnKeyMatch: TNotifyEvent;
    FOnMenuClick: TNotifyEvent;
    //FOnMyUtf8KeyPress: TMyUtf8KeyPressEvent;
    FParams: String;
    FPromptFillTable: Boolean;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTable: Integer;
    FSourceTId: Integer;
    FForm: TdxLCbxListForm;
    FDropDownBnClick: Boolean;	// Когда пользователь нажимает кнопку открытия списка, когда список уже открыт, это позволяет закрыть список
    FChanging: Boolean;					// Чтобы различать изменения Change при нажатии клавиш
    FFiltering: Boolean;				// Когда пользователь что-то вводит в поле, флаг устанавливается и позволяет фильтровать данные в списке
    FKeyDown: Boolean;
    FKeepList: Boolean;
    FSkipKillFocus: Boolean;		// Для linux
    FGrid: TDropDownList;
    FStopTab: Boolean;
    FUpdateTree: Boolean;
    FPopup: TPopupMenu;
    FKeyTimer: TTimer;
    procedure DoButtonClick(Sender: TObject);
    procedure DoDropDownButtonClick(Sender: TObject);
    procedure DropDownButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetDroppedDown: Boolean;
    function GetGrid: TDropDownList;
    function GetKeyValue: Variant;
    function GetSourceFieldName: String;
    function GetSourceFormName: String;
    procedure KeyTimerTimer(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure ReadInsertValues(Reader: TReader);
    procedure SetEditable(AValue: Boolean);
    procedure SetFieldName(AValue: String);
    procedure SetFieldsTables(AValue: TStrings);
    procedure SetHideButton(AValue: Boolean);
    procedure SetHideList(AValue: Boolean);
    procedure SetKeyValue(AValue: Variant);
    procedure SetListFields(AValue: TLCbxListFields);
    procedure WriteInsertValues(Writer: TWriter);
    procedure CreateListForm;
    procedure ShowListForm;
    procedure CloseListForm;
    procedure ProcessKillFocus;
    function IsListVisible: Boolean;
    procedure SetControlState;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetReadOnly(Value: Boolean); override;
    //procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Change; override;
    procedure FontChanged(Sender: TObject); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure DoEnter; override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnableButtons(AValue: Boolean);
    procedure ClearData;
    procedure ClearInsertTableProps;
    procedure DoPositionButton;
    procedure SetButtonState;
    procedure FillGrid(DS: TDataSet; Empty: Boolean);
    function GetButtonWidths: Integer;
    procedure ApplyChanges;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property OnCtrlClick: TNotifyEvent read FOnCtrlClick write FOnCtrlClick;
    property Button: TSpeedButton read FButton;
    property DropDownButton: TSpeedButton read FDropDownButton;
    property InsertedValues: TInsertedValues read FInsertedValues;
    property SourceFormName: String read GetSourceFormName;
    property SourceFieldName: String read GetSourceFieldName;
    property DroppedDown: Boolean read GetDroppedDown;
    property DropDownList: TDropDownList read GetGrid;

    //property ItemIndex: Integer read FItemIndex write FItemIndex;
    //property Style: TComboBoxStyle read FStyle write FStyle;
    property OnNeedData: TNeedDataEvent read FOnNeedData write FOnNeedData;
    property OnKeyMatch: TNotifyEvent read FOnKeyMatch write FOnKeyMatch;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnMenuClick: TNotifyEvent read FOnMenuClick write FOnMenuClick;
    //property OnCbxDropDown: TNotifyEvent read FOnCbxDropDown write FOnCbxDropDown;
    //property OnCbxChange: TNotifyEvent read FOnCbxChange write FOnCbxChange;
    property OnCreateListWindow: TCreateListWindowEvent read FOnCreateListWindow
      write FOnCreateListWindow;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
    //property OnMyUtf8KeyPress: TMyUtf8KeyPressEvent read FOnMyUtf8KeyPress
    //	write FOnMyUtf8KeyPress;
  published
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property SourceTId: Integer read FSourceTId write FSourceTId;
    property SourceFId: Integer read FSourceFId write FSourceFId;
    property Filter: String read FFilter write FFilter;
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
    property Editable: Boolean read FEditable write SetEditable;
    property ListFields: TLCbxListFields read FListFields write SetListFields;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
    property ListWidthExtra: Integer read FListWidthExtra write FListWidthExtra;
    property HideList: Boolean read FHideList write SetHideList;
    property HideButton: Boolean read FHideButton write SetHideButton;
    property UpdateTree: Boolean read FUpdateTree write FUpdateTree;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;

    property AutoComplete: Boolean read FAutoComplete write FAutoComplete stored False;
    property ItemHeight: Integer read FItemHeight write FItemHeight stored False;

    property PopupMenu stored False;
    property ListSource: TDataSource read FListSource write FListSource stored False;
    property KeyField: String read FKeyField write FKeyField stored False;
    property ListField: String read FListField write FListField stored False;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex stored False;
    property LookupCache: Boolean read FLookupCache write FLookupCache stored False;
    property Params: String read FParams write FParams stored False;
  end;

  { TdxImage }

  {TdxImage = class(TImage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PopupMenu stored False;
  end;     }

  TdxGridCanSortEvent = procedure (Sender: TObject; var Cancel: Boolean) of object;
  TdxGridValidateEvent = procedure (Sender: TObject; var Ok: Boolean) of object;

  { TdxGrid }

  TdxGrid = class(TMyDBGrid)
  private
    FForm: TdxForm;
    FId: Integer;
    FMemo: TMemoCellEditor;
    FOnValidate: TdxGridValidateEvent;
    FShowRowDeleteButton: Boolean;
    FSortAZ: Boolean;
    FSortColumn: Integer;
    FMaskEdit: TMaskCellEditor;
    function DoValidate: Boolean;
    function GetTitleFontStyle: TFontStyles;
    procedure SetTitleFontStyle(AValue: TFontStyles);
    function IsTitleFontStyleStored: Boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
    procedure WMHScroll(var message: TLMHScroll); message LM_HSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure SelectEditor; override;
    procedure UpdateData; override;
    function EditorCanAcceptKey(const ch: TUTF8Char): boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ForceSelectEditor;
    property OnVaidate: TdxGridValidateEvent read FOnValidate
      write FOnValidate;
  public
    function GetFieldNameByColumn(aColumn: TColumn): String;
    function FindColumnByFieldName(const FieldName: String): TColumn;
    property Form: TdxForm read FForm write FForm;
    property MaskEdit: TMaskCellEditor read FMaskEdit;
    property Memo: TMemoCellEditor read FMemo;
  published
    property Id: Integer read FId write FId;
    property TitleFontStyle: TFontStyles read GetTitleFontStyle write SetTitleFontStyle stored IsTitleFontStyleStored;
    property ShowRowDeleteButton: Boolean read FShowRowDeleteButton write FShowRowDeleteButton default False;

    property SortColumn: Integer read FSortColumn write FSortColumn stored False;
    property SortAZ: Boolean read FSortAZ write FSortAZ stored False;
  end;

  { TdxGroupBox }

  TdxGroupBox = class(TGroupBox)
  private
    FStopTab: Boolean;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property PopupMenu stored False;
    property ClientWidth stored False;
    property ClientHeight stored False;
  end;

  { TdxPageControl }

  TdxPageControl = class(TPageControl)
  private
    FStopTab: Boolean;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetPageClass: TCustomPageClass; override;
    procedure BoundsChanged; override;
    procedure Change; override;
  public
    constructor Create(TheOwner: TComponent); override;
  	procedure SetActiveFirstVisiblePage;
    procedure UpdateAnchoredControls;
  published
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property PopupMenu stored False;
    property Options stored False;
  end;

  { TdxTabSheet }

  TdxTabSheet = class(TTabSheet)
  private
    FNeedUpdate: Boolean;
    FStopTab: Boolean;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetClientRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
    property PopupMenu stored False;
    property ClientWidth stored False;
    property ClientHeight stored False;
  end;

  { TdxShape }

  TdxShape = class(TShape)
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property PopupMenu stored False;
  end;

  TViewType = (vtGridTop, vtGridBottom, vtGridLeft, vtGridRight, vtGridOnly,
    vtWithoutGrid, vtSimpleForm, vtDefault);

  TAccessStatus = (asOk, asCantAppend, asCantEdit, asCantDelete, asModified,
    asDeleted, asLocked, asHasRef);

  TLockMode = (lmNoLock, lmPessimistic);

  TdxFormTree = class;

  { TdxForm }

  TdxForm = class(TCustomPanel)
    procedure ReadShopData(Reader: TReader);
    procedure WriteShopData(Writer: TWriter);
  private
    FAutoOpen: Boolean;
    FCalcFields: TStrings;
    FColoring: TStrings;
    FDataSet: TSQLQuery;
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
    FOnAfterDuplicate: TNotifyEvent;
    FOnBeforeDuplicate: TNotifyEvent;
    FOnFieldChange: TFieldChangeEvent;
  private
    FActionOnCreate: String;
    FActionResult: Variant;
    FConfirmAutoSaveRecord: Boolean;
    FConfirmCancelEditing: Boolean;
    FConfirmSaveRecord: Boolean;
    FCustomFilter: String;
    FCustomFilterForm: TdxForm;
    FDSRi: Integer;
    FRecordCaption: String;
    FRecordsCaption: String;
    FFormGroup: String;
    FIndex: Integer;
    FLevelCount: Integer;
    FLockMode: TLockMode;
    FOnDestroy: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnPrint: TPrintEvent;
    FParams: TParamList;
    FShowScrollBars: Boolean;
    FSoftCheck: Boolean;
    FTree: TdxFormTree;
    FScrollEventsCounter: Integer;
    FOldAfterScroll, FOldBeforeScroll: TDataSetNotifyEvent;
    FUseSelCond: Boolean;
    //FGridBmp: TBitmap;
    function GetAsDT(Index: String): TDateTime;
    function GetAsF(Index: String): Extended;
    function GetAsI(Index: String): Integer;
    function GetAsS(Index: String): String;
    function GetEditWindow: TObject;
    function GetField(Index: String): TField;
    function GetFiles(Index: String): TdxFile;
    function GetFilter: TFilterObject;
    function GetFormByIndex(Index: Integer): TdxForm;
    function GetFormCount: Integer;
    function GetForms(Index: String): TdxForm;
    function GetFormGrid: TdxGrid;
    function GetImages(Index: String): TdxDBImage;
    function GetModified: Boolean;
    function GetOldValues(Index: String): Variant;
    function GetParentForm: TdxForm;
    function GetQueries(Index: String): TObject;
    function GetQueryByIndex(Index: Integer): TObject;
    function GetQueryCount: Integer;
    function GetState: TDataSetState;
    procedure SetTree(AValue: TdxFormTree);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure RemoveGridTree;
    procedure UpdateTree;
    procedure RequeryIfNeed;
    procedure SetTabStops;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoPrintEvent(Step: TPrintActionType; const SourceName,
      FieldName: String; var Value: String; var Accept: Boolean);
    function HasShop: Boolean;
    function FindForm(const FormName: String): TdxForm;
    function FindQuery(const QueryName: String): TObject;
    function IsHide: Boolean;
    function HasFilterValues: Boolean;
    property ShopData: TShopData read FShopData;
    property Fields[Index: String]: Variant read GetFields write SetFields;
    property Field[Index: String]: TField read GetField;
    property AsI[Index: String]: Integer read GetAsI;
    property AsF[Index: String]: Extended read GetAsF;
    property AsDT[Index: String]: TDateTime read GetAsDT;
    property AsS[Index: String]: String read GetAsS;
    property OldValues[Index: String]: Variant read GetOldValues;
    property ParentForm: TdxForm read GetParentForm;
    property DataSet: TSQLQuery read FDataSet write FDataSet;
    property ActionResult: Variant read FActionResult write FActionResult;
    //property FieldChanged: String read FFieldChanged write FFieldChanged;
  public
    function Append: TAccessStatus;
    function Insert: TAccessStatus;
    function Edit: TAccessStatus;
    function Delete: TAccessStatus;
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
    function Print(const TemplateName, OutFileName: String; out Errs: String; aOpenFile: Boolean): String;
    function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    procedure DisableControls;
    procedure EnableControls;
    function ControlsDisabled: Boolean;
    function CanAppend: TAccessStatus;
    function CanEdit: TAccessStatus;
    function CanDelete: TAccessStatus;
    function Show: Boolean;
    procedure Open;
    procedure Close;
    procedure OpenRecords(const aFilter: String; Fm: TdxForm; SelCond: Boolean);
    procedure OpenRecord(aRecId: Integer);
    function Opened: Boolean;
    function Validate: Boolean;
    function FindComponentByFldName(const FieldName: String): TComponent;
    procedure EnableScrollEvents;
    procedure DisableScrollEvents;
    function ScrollEventsDisabled: Boolean;
    function WhoEdit(ARecId: Integer): String;
    function GetRecordCaption: String;
    function GetRecordsCaption: String;
    function IsBinded: Boolean;
    //procedure ApplyUpdates;
    //procedure CancelUpdates;
    property DSP: TObject read FDSP write FDSP;
    property DSR: Pointer read FDSR write FDSR;
    property DSRi: Integer read FDSRi write FDSRi;
    property Forms[Index: String]: TdxForm read GetForms;
    property FormByIndex[Index: Integer]: TdxForm read GetFormByIndex;
    property FormCount: Integer read GetFormCount;
    property Queries[Index: String]: TObject read GetQueries;
    property QueryByIndex[Index: Integer]: TObject read GetQueryByIndex;
    property QueryCount: Integer read GetQueryCount;
    property Images[Index: String]: TdxDBImage read GetImages;
    property Files[Index: String]: TdxFile read GetFiles;
    property FormGrid: TdxGrid read GetFormGrid;
    property EditWindow: TObject read GetEditWindow;
    property Params: TParamList read FParams;
    property State: TDataSetState read GetState;
    property Filter: TFilterObject read GetFilter;
    property Modified: Boolean read GetModified;
    property LockMode: TLockMode read FLockMode write FLockMode;
    property CustomFilter: String read FCustomFilter write FCustomFilter;
    property CustomFilterForm: TdxForm read FCustomFilterForm write FCustomFilterForm;
    property UseSelCond: Boolean read FUseSelCond write FUseSelCond;
  published
    property Layout: TAlign read FLayout write FLayout stored False;       // устаревшее
  	property GroupField: Integer read FGroupField write FGroupField stored False;
    property TreeBackColor: TColor read FTreeBackColor write FTreeBackColor stored False;
    property TreeLineColor: TColor read FTreeLineColor write FTreeLineColor stored False;
    property TreeSelectColor: TColor read FTreeSelectColor write FTreeSelectColor stored False;
    property TreeFont: TFont read FTreeFont write SetTreeFont stored False;
    property TreeWidth: Integer read FTreeWidth write FTreeWidth stored False;
  published
    property Id: Integer read FId write FId;
    property PId: Integer read FPId write FPId;
    property FormCaption: String read FFormCaption write FFormCaption;
    property FormGroup: String read FFormGroup write FFormGroup;
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
    property LevelCount: Integer read FLevelCount write FLevelCount;
    property Grid: TdxGrid read FGrid write SetGrid;
    property ShowScrollBars: Boolean read FShowScrollBars write FShowScrollBars;
    property ConfirmSaveRecord: Boolean read FConfirmSaveRecord write FConfirmSaveRecord;
    property ConfirmAutoSaveRecord: Boolean read FConfirmAutoSaveRecord write FConfirmAutoSaveRecord;
    property ConfirmCancelEditing: Boolean read FConfirmCancelEditing write FConfirmCancelEditing;
		property Tree: TdxFormTree read FTree write SetTree;
    property Index: Integer read FIndex write FIndex;
    property SoftCheck: Boolean read FSoftCheck write FSoftCheck;
    property RecordsCaption: String read FRecordsCaption write FRecordsCaption;
    property RecordCaption: String read FRecordCaption write FRecordCaption;

    property OnAfterCancel: TNotifyEvent read FOnAfterCancel write FOnAfterCancel;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterDelete: TNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property OnAfterEdit: TNotifyEvent read FOnAfterEdit write FOnAfterEdit;
    property OnAfterInsert: TNotifyEvent read FOnAfterInsert write FOnAfterInsert;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterPost: TNotifyEvent read FOnAfterPost write FOnAfterPost;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnAfterDuplicate: TNotifyEvent read FOnAfterDuplicate write FOnAfterDuplicate;

    property OnBeforeCancel: TNotifyEvent read FOnBeforeCancel write FOnBeforeCancel;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeDelete: TNotifyEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnBeforeEdit: TNotifyEvent read FOnBeforeEdit write FOnBeforeEdit;
    property OnBeforeInsert: TNotifyEvent read FOnBeforeInsert write FOnBeforeInsert;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforePost: TNotifyEvent read FOnBeforePost write FOnBeforePost;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnBeforeDuplicate: TNotifyEvent read FOnBeforeDuplicate write FOnBeforeDuplicate;

    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnFieldChange: TFieldChangeEvent read FOnFieldChange write FOnFieldChange;
		property OnPrint: TPrintEvent read FOnPrint write FOnPrint;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;

    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;

    property ActionOnCreate: String read FActionOnCreate write FActionOnCreate;
  end;

  { TdxObjectField }

  TdxObjectField = class(TDBEditEx)
  private
    FFieldId: Integer;
    FFieldName: String;
    FId: Integer;
    FObjId: Integer;
    FStopTab: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    function GetDrawText: String; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Init;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property ObjId: Integer read FObjId write FObjId;
    property FieldId: Integer read FFieldId write FFieldId;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
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
    FStopTab: Boolean;
    FTimeFormat: TdxTimeFormat;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    function TimeFormatStr: String;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property CurTime: Boolean read FCurTime write FCurTime;
    property TimeFormat: TdxTimeFormat read FTimeFormat write FTimeFormat;
    property Expression: String read FExpression write FExpression;
    property Editable: Boolean read FEditable write FEditable;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
  end;

  { TdxCounter }

  TdxCounter = class(TDBEditEx)
  private
    FCheckExpression: String;
    FFieldName: String;
    FId: Integer;
    FRequired: Boolean;
    FRestart: Boolean;
    FStartWith: Integer;
    FStopTab: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
    property StartWith: Integer read FStartWith write FStartWith;
    property Restart: Boolean read FRestart write FRestart;
  published
    property PopupMenu stored False;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write SetFieldName;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;
  end;

  { TdxScrollBox }

  TdxScrollBox = class(TScrollBox)
  public
    procedure Paint; override;
  end;

  { TdxButton }

  TdxButton = class(TBitBtn)
  private
    FActionId: String;
    FActionOnClick: String;
    FActionProps: String;
    FActionType: TdxActionType;
    FButtonName: String;
    FImageName: String;
    FResName: String;
    FStopTab: Boolean;
    procedure SetImageName(AValue: String);
    function IsStoredGlyph: Boolean;
  protected
    procedure TextChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure RenameImage(const S: String);
  published
    property ActionType: TdxActionType read FActionType write FActionType stored False;
    property ActionId: String read FActionId write FActionId stored False;
    property ActionProps: String read FActionProps write FActionProps stored False;
    property ResName: String read FResName write FResName stored False;
    property Glyph stored IsStoredGlyph;
    property ActionOnClick: String read FActionOnClick write FActionOnClick;
    property ImageName: String read FImageName write SetImageName;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property TabStop stored False;

    property PopupMenu stored False;
    property ButtonName: String read FButtonName write FButtonName stored False;
    property Images stored False;
    property ImageIndex stored False;
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
    FAutoPosition: Boolean;
    FParams: TParamList;
    FFirstShow: Boolean;
    procedure SetAutoPosition(AValue: Boolean);
  protected
    procedure DoShow; override;
    procedure Activate; override;
  public
    function ShowModal: Integer; override;
    constructor CreateWindow;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    destructor Destroy; override;
    property Params: TParamList read FParams;
    property AutoPosition: Boolean read FAutoPosition write SetAutoPosition;
  end;

  TdxFormTreeFieldSource = (tfsForm, tfsObject);

  { TdxFormTreeField }

  TdxFormTreeField = class(TCollectionItem)
  private
    FFieldId: Integer;
    FFieldSource: TdxFormTreeFieldSource;
    // API
    FFieldName: String;
    function GetForm: TdxForm;
    procedure SetFieldName(AValue: String);
    //
  public
    function FieldSourceToStr: String;
    property FieldName: String read FFieldName write SetFieldName;
  published
    property FieldId: Integer read FFieldId write FFieldId;
    property FieldSource: TdxFormTreeFieldSource read FFieldSource write FFieldSource;
  end;

  { TdxFormTreeFields }

  TdxFormTreeFields = class(TCollection)
  private
    FTree: TdxFormTree;
    function GetFields(Index: Integer): TdxFormTreeField;
    procedure SetFields(Index: Integer; AValue: TdxFormTreeField);
  public
    constructor Create(ATree: TdxFormTree);
    function Add: TdxFormTreeField;
    function FindField(FieldId: Integer; FieldSource: TdxFormTreeFieldSource): TdxFormTreeField;
    property Fields[Index: Integer]: TdxFormTreeField read GetFields write SetFields; default;
    property Tree: TdxFormTree read FTree;
  end;

  TdxTreeNodeData = class
  public
    Field: TdxFormTreeField;
    RecId: Integer;
  end;

  { TdxFormTree }

  TdxFormTree = class(TTreeViewEx)
  private
    FExpandLevels: Byte;
    FFields: TdxFormTreeFields;
    FForm: TdxForm;
    FOnUpdateTree: TNotifyEvent;
    FPopup: TPopupMenu;
    FAllNode, FOldNode: TTreeNode;
    FNodeDataList: TList;
    FNoHandleSelect: Boolean;
    procedure PopupHandler(Sender: TObject);
    procedure SearchFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetFields(AValue: TdxFormTreeFields);
    function GenerateSQLForm(RecId: Integer; AllFields: Boolean): String;
    function GenerateSQLObjects: String;
    procedure CheckFields;
    function GetNodePath(N: TTreeNode; const D: Char): String;
    function GetNodeWithRecId(RecId: Integer; ParentNode: TTreeNode): TTreeNode;
    function GetNodeWithText(const S: String; ParentNode: TTreeNode): TTreeNode;
    function GetObjectNodeWithRecId(RecId: Integer; Field: TdxFormTreeField): TTreeNode;
    procedure AddNodeData(Node: TTreeNode; Field: TdxFormTreeField; RecId: Integer);
    procedure UpdateTreeForm;
    procedure UpdateTreeObjects;
    procedure UserUpdateTree;
	protected
    procedure DoSelectionChanged; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure Loaded; override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTree;
    procedure SelectByRecord(RecId: Integer);
    procedure UpdateSelection;
    //procedure SelectFirstNode(SelectionDisabled: Boolean);
    //procedure ClearSelection(KeepPrimary: Boolean=false); override;
    procedure SoftClearSelection;
    function GetFieldNameByNode(N: TTreeNode): String;
    function GetFieldValueByNode(N: TTreeNode): Variant;
    property Form: TdxForm read FForm write FForm;
  published
    property Fields: TdxFormTreeFields read FFields write SetFields;
    property ExpandLevels: Byte read FExpandLevels write FExpandLevels;
    property OnUpdateTree: TNotifyEvent read FOnUpdateTree write FOnUpdateTree;
  end;

  { TdxRecordId }

  TdxRecordId = class(TDBEditEx)
  private
    FFieldName: String;
    FId: Integer;
    FStopTab: Boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure SetFieldName(AValue: String);
  protected
    function GetDrawText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
  published
    property PopupMenu stored False;
    property FieldName: String read FFieldName write SetFieldName;
    property StopTab: Boolean read FStopTab write FStopTab default True;
    property Id: Integer read FId write FId;
    property TabStop stored False;
  end;

function HasFId(C: TComponent): Boolean;
function HasProp(C: TComponent; const PropName: String): Boolean;
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
function FindComponentByFieldName(Fm: TdxForm; const FieldName: String; aExclude: TComponent = nil): TComponent;
//function GetParentColor(C: TComponent): Boolean;
//function SetParentColor(C: TComponent; Value: Boolean): Boolean;
//function GetParentFont(C: TComponent): Boolean;
function SetParentFont(C: TComponent; Value: Boolean): Boolean;
function GetEditButton(C: TComponent): TSpeedButton;
procedure SetEnableEditButton(C: TComponent; AValue: Boolean);
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
//function IsFieldExist(Fm: TdxForm): Boolean;
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
function GetFieldClass(Fm: TdxForm; FieldId: Integer): TClass;
function ObjectFieldIsObject(C: TComponent): Boolean;
function GetImageName(C: TComponent): String;
procedure SetImageName(C: TComponent; const S: String);
function GetStopTab(C: TComponent): Boolean;
procedure SetStopTab(C: TComponent; Value: Boolean);

implementation

uses
  TypInfo, formmanager, Dialogs, appsettings,
  FileUtil, BGRABitmapTypes, FPReadJpeg,
  FPImage, BGRAReadPNG, {dximages, dxfiles, }apputils, reportmanager,
  DXReports, DateUtils, pivotgrid, sqlgen, datasetprocessor,
  Variants, expressions, LazFileUtils, scriptfuncs, MaskEdit,
  dbengine, clipbrd, outputform, dxusers, imagemanager, appimagelists, crossapi;

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

function HasProp(C: TComponent; const PropName: String): Boolean;
var
  pInf: PPropInfo;
begin
  pInf := GetPropInfo(C, PropName);
  Result := pInf <> nil;
end;

function IsField(C: TComponent): Boolean;
begin
  Result := (GetId(C) > 0) and not (C is TDBGrid) and not (C is TdxObjectField)
    and not (C is TdxPivotGrid) and not (C is TdxRecordId);
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
  aExclude: TComponent = nil): TComponent;
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
    if MyUtf8CompareText(GetFieldName(C), FieldName) = 0 then
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
  if C is TCustomDBEditButton then
  	Result := TCustomDBEditButton(C).Button
  {if C is TdxDateEdit then
    Result := TSpeedButton(TdxDateEdit(C).Button)
  else if C is TdxCalcEdit then
    Result := TSpeedButton(TdxCalcEdit(C).Button)
  else if C is TdxFile then
    Result := TSpeedButton(TdxFile(C).Button)
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).Button }
  else if C is TdxLookupComboBox then
    Result := TdxLookupComboBox(C).Button
  else if C is TdxMemo then
    Result := TdxMemo(C).Button;
end;

procedure SetEnableEditButton(C: TComponent; AValue: Boolean);
begin
  if C is TCustomDBEditButton then
    TCustomDBEditButton(C).EnableButton(AValue)
  else if C is TdxLookupComboBox then
  	TdxLookupComboBox(C).EnableButtons(AValue)
  else if C is TdxMemo then
  	TdxMemo(C).EnableButton(AValue);
end;

function GetComponentType(C: TComponent): String;
const
  Cls: array [1..25] of String = ('TdxLabel', 'TdxEdit', 'TdxCalcEdit',
    'TdxDateEdit', 'TdxMemo', 'TdxCheckBox', 'TdxComboBox', 'TdxLookupComboBox',
    'TdxGrid', 'TdxGroupBox', 'TdxPageControl', 'TdxForm', 'TdxTabSheet', 'TdxShape',
    'TdxDBImage', 'TdxImage', 'TdxFile', 'TdxQueryGrid', 'TdxObjectField',
    'TdxTimeEdit', 'TdxCounter', 'TdxButton', 'TdxPivotGrid', 'TdxChart',
    'TdxRecordId');
var
  i: Integer;
  Tps: array [1..25] of String;
begin
  Result := '';
  Tps[1] := rsLabel; Tps[2] := rsText; Tps[3] := rsNumber;
  Tps[4] := rsDate; Tps[5] := rsMemo; Tps[6] := rsCheckBox;
  Tps[7] := rsList; Tps[8] := rsObject; Tps[9] := rsTable;
  Tps[10] := rsGroup; Tps[11] := rsPages; Tps[12] := rsForm;
  Tps[13] := rsPage; Tps[14] := rsShape; Tps[15] := rsImage;
  Tps[16] := rsDsgnBackImage; Tps[17] := rsDsgnFile; Tps[18] := rsQuery;
  Tps[19] := rsObjField; Tps[20] := rsTime; Tps[21] := rsCounter;
  Tps[22] := rsButton; Tps[23] := rsPivotTable; Tps[24] := rsChart;
  Tps[25] := rsRecordId;
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
    //if AppConfig.ExpertMode then
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
        if (All or (Expression <> '')) and (MyUtf8CompareText(FieldName, S) = 0) then
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

{function IsFieldExist(Fm: TdxForm): Boolean;
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
end;  }

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

function GetFieldClass(Fm: TdxForm; FieldId: Integer): TClass;
var
  C: TComponent;
begin
  Result := nil;
  C := FindById(Fm, FieldId);
  if C <> nil then Result := C.ClassType;
end;

function ObjectFieldIsObject(C: TComponent): Boolean;
begin
  Result := False;
  if C is TdxObjectField then
  begin
    C := GetObjectFieldField(TdxObjectField(C));
    if (C <> nil) and (C is TdxLookupComboBox) then Result := True;
  end;
end;

function GetImageName(C: TComponent): String;
begin
  Result := GetStr(C, 'imagename');
end;

procedure SetImageName(C: TComponent; const S: String);
begin
  SetStr(C, 'imagename', S);
end;

function GetStopTab(C: TComponent): Boolean;
begin
  Result := Boolean(GetInt(C, 'StopTab'));
end;

procedure SetStopTab(C: TComponent; Value: Boolean);
begin
  SetInt(C, 'StopTab', Integer(Value));
end;

function _GetSourceFieldName(Obj: TComponent): String;
var
  Fm: TdxForm;
  C: TComponent;
begin
  Result := '';
  Fm := FormMan.FindForm(GetSourceTId(Obj));
  if Fm <> nil then
  begin
    C := FindById(Fm, GetSourceFId(Obj));
    if C <> nil then Result := GetFieldName(C);
  end;
end;

function _GetSourceFormName(Obj: TComponent): String;
var
  Fm: TdxForm;
begin
  Result := '';
  Fm := FormMan.FindForm(GetSourceTId(Obj));
  if Fm <> nil then Result := Fm.FormCaption;
end;

{ TdxRecordId }

procedure TdxRecordId.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxRecordId.MenuPopup(Sender: TObject);
begin
  if CanFocus then SetFocus;
  PopupMenu.Items[0].Enabled := SelText <> '';
end;

procedure TdxRecordId.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CopyToClipboard;
  end;
end;

function TdxRecordId.GetDrawText: String;
begin
  Result := FFieldName;
end;

constructor TdxRecordId.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  FStopTab := True;
  Width := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 0, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

procedure TdxRecordId.Init;
begin
  StopTab := False;
end;

////////////////////////////////////////////////////////////////////////////////

{ TdxShape }

constructor TdxShape.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := ScaleToScreen(100);
  Height := ScaleToScreen(100);
end;

{ TdxImage }

{constructor TdxImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  Height := ScaleToScreen(100);
end;   }

{ TLCbxListFields }

function TLCbxListFields.GetFields(Index: Integer): TLCbxListField;
begin
  Result := TLCbxListField(Items[Index]);
end;

constructor TLCbxListFields.Create;
begin
  inherited Create(TLCbxListField);
end;

function TLCbxListFields.Add: TLCbxListField;
begin
	Result := TLCbxListField( inherited Add );
end;

{ TdxLCbxListForm }

function TdxLCbxListForm.GetListHeight: Integer;
var
  n: Integer;
begin
  //FGrid.DefaultRowHeight := FGrid.Canvas.TextHeight('Yy') + 2;

  n := FControl.DropDownCount + FGrid.FixedRows;
  if FGrid.RowCount < n then n := FGrid.RowCount;
  if n = FGrid.FixedRows then Inc(n);

  Result := n * FGrid.DefaultRowHeight + 2;
end;

procedure TdxLCbxListForm.FormChangeBounds(Sender: TObject);
begin
  if FOldChangeBounds <> nil then FOldChangeBounds(Sender);
  //FCancel := True;
  if Visible then
  begin
    SetFocusControl('');
    FControl.CloseListForm;
  end;
end;

procedure TdxLCbxListForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, p, x, w, LenS: Integer;
  S, LowerS, Txt, PartTxt, LowerTxt: String;
  C: TCanvas;
begin
  if not FGrid.HighlightSearchedText then Exit;

  if (FFrags.Count = 0) or
    (FGrid.Columns[aCol].ButtonStyle = cbsCheckboxColumn) or
    not FGrid.Columns[aCol].Searchable then Exit;
    //((aCol > 0) and (not FControl.ListFields[aCol-1].Searchable)) then Exit;

  C := FGrid.Canvas;
  if (gdSelected in aState) then
  	C.Brush.Color := FGrid.SelectedHighlightColor
  else
	  C.Brush.Color := FGrid.HighlightColor;

  Txt := FGrid.Cells[aCol, aRow];
  LowerTxt := Utf8LowerCase(Txt);

  for i := 0 to FFrags.Count - 1 do
  begin
    S := FFrags[i];
    LenS := Utf8Length(S);
    if LenS = 0 then Continue;

    LowerS := Utf8LowerCase(S);
    p := Utf8Pos(LowerS, LowerTxt);
    while p > 0 do
    begin
      S := Utf8Copy(Txt, p, LenS);
      PartTxt := Utf8Copy(Txt, 1, p-1);
      w := C.TextWidth(S);
      x := C.TextWidth(PartTxt) + constCellPadding;
      C.FillRect(x + aRect.Left, aRect.Top + constCellPadding,
      	x + w + aRect.Left, aRect.Bottom - constCellPadding);
      p := Utf8Pos(LowerS, LowerTxt, p + 1);
    end;
  end;

  FGrid.DrawText(aCol, aRow, aRect, aState, Txt);
end;

procedure TdxLCbxListForm.GridUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Utf8Key >= ' ' then
	  SetFocusControl(UTF8Key);
end;

procedure TdxLCbxListForm.SetControl(AValue: TdxLookupComboBox);
begin
  if FControl=AValue then Exit;
  FControl:=AValue;

  FGrid.OnMouseUp:=@GridMouseUp;
  FGrid.OnKeyDown:=@GridKeyDown;
  FGrid.OnDrawCell:=@GridDrawCell;
  FGrid.OnUTF8KeyPress:=@GridUtf8KeyPress;
end;

procedure TdxLCbxListForm.SetPosition;
var
  HUp, HDn, H: Integer;
  M: TMonitor;
  R, CR: TRect;
begin
  GetWindowRect(FControl.Handle, CR);
	Left := CR.Left;
  Top := CR.Bottom;
  Width := FControl.Width + FControl.GetButtonWidths + FControl.ListWidthExtra;
  Height := GetListHeight;
  M := Screen.MonitorFromRect(CR);
  if M = nil then Exit;
  R := M.WorkareaRect;

  // Корректировка положения списка по горизонтали
  if Left + Width > R.Right then Left := R.Right - Width;
  if Left < R.Left then Left := R.Left;
  if Width > R.Width then Width := R.Width;

	HUp := Top - FControl.Height - R.Top;
  HDn := R.Bottom - Top;

  // Корректировка положения списка по вертикали
  // Список не помещается вниз
  if Height > HDn then
  begin
  	// Но и вверх он не помещается
    if Height > HUp then
    begin
      // Тогда высота списка будет равна наибольшему значению: выше компонента больше места...
      if HUp > HDn then
      begin
        Top := R.Top;
        Height := HUp;
      end
      // ... ниже компонента больше места
      else
        Height := HDn;
    end
    // Вверх помещается
    else
      Top := Top - FControl.Height - Height;

    H := (Height div FGrid.DefaultRowHeight) * FGrid.DefaultRowHeight + 2;
    if Top < FControl.Top then Top := Top + (Height - H);
    Height := H;
  end;
end;

procedure TdxLCbxListForm.AcceptSelected;
var
  id: Integer;
  txt: String;
begin
  with FGrid do
  begin
	  //id := PtrInt(Objects[0, Row]);
    id := RecId[Row];
    txt := Cells[0, Row];
  end;
  Hide;
  if (id > 0) and (FControl.DataSource.DataSet.State in [dsInsert, dsEdit]) then
  begin
  	FControl.Field.Text := txt;
    FControl.SelStart := 0;
    FControl.SelLength := Utf8Length(txt);
    FControl.FFiltering := False;
    FControl.FChanging := False;
    if txt <> '' then
  	  FControl.KeyValue:=id
    // Все равно при потере фокуса пустое поле станет Null, поэтому делаем
    // это сразу при выборе. Иначе, функция "Заполнить таблицу" работает с
    // ошибкой, когда выбирается элемент с id, но пустым текстом.
    else
      FControl.KeyValue:=Null;
	end;
  SetFocusControl('');
  FControl.CloseListForm;
end;

procedure TdxLCbxListForm.FindItem;
var
  i, id, tr: Integer;
  //obj: TObject;
begin
  if FControl.KeyValue = Null then
  begin
    if FGrid.RowCount > FGrid.FixedRows then FGrid.Row := FGrid.FixedRows;
    Exit;
  end;

	id := FControl.KeyValue;
  //obj := TObject(PtrInt(id));

  for i := 0 to FGrid.RowCount - 1 do
  begin
		//if FGrid.Objects[0, i] = obj then
    if FGrid.RecId[i] = id then
    begin
      tr := Round(i - FRealDropDownCount / 2);
      if tr + FRealDropDownCount > FGrid.RowCount then
      	tr := FGrid.RowCount - FRealDropDownCount;
      FGrid.TopRow := tr + FGrid.FixedRows;
      FGrid.Row := i;
      Exit;
    end;
  end;
end;

procedure TdxLCbxListForm.Deactivate;
begin
  inherited Deactivate;

  FGrid.SelectedColor := FGrid.InactiveSelectedColor;// clSilver;

  if {FAccept or FCancel or} FControl.FKeepList then Exit;

  if FControl.DropDownButton.MouseEntered then
    FControl.FDropDownBnClick:=True
  else if not FControl.MouseEntered then
  begin
    FControl.CloseListForm;
  end;
end;

procedure TdxLCbxListForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  FControlForm.OnChangeBounds:=FOldChangeBounds;
end;

procedure TdxLCbxListForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //FControl.FGridKeyClose := Key in [VK_RETURN, VK_ESCAPE];

  if Key = VK_RETURN then AcceptSelected
  else if Key = VK_ESCAPE then
  begin
    //FCancel := True;
    SetFocusControl('');
    FControl.CloseListForm;
  end
  else if Key in [VK_LEFT, VK_RIGHT, VK_BACK, VK_DELETE, VK_TAB] then
  begin
    Key := 0;
    SetFocusControl('');
  end;
end;

procedure TdxLCbxListForm.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
begin
  MousePos := Mouse.CursorPos;
  MousePos := FGrid.ScreenToClient(MousePos);
  if FGrid.MouseToGridZone(MousePos.x, MousePos.y) = gzNormal then
	  AcceptSelected;
end;

procedure TdxLCbxListForm.SetFocusControl(const Utf8Key: String);
begin
  FControl.FKeepList := True;
  if FControl.CanFocus then FControl.SetFocus;
  if Utf8Key <> '' then
  begin
    FControl.SelText := Utf8Key;
    FControl.FChanging:=True;
  end;
  FControl.FKeepList := False;
end;

procedure TdxLCbxListForm.Activate;
begin
  inherited Activate;
  FGrid.SelectedColor := FGrid.SelColor;
end;

constructor TdxLCbxListForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  BorderStyle:=bsNone;
  //FormStyle := fsStayOnTop;
  ShowInTaskBar := stNever;
  FFrags := TStringList.Create;
end;

destructor TdxLCbxListForm.Destroy;
begin
  FFrags.Free;
  inherited Destroy;
end;

procedure TdxLCbxListForm.ShowForm;
begin
  FControlForm := GetParentForm(FControl);

  if FControlForm.OnChangeBounds <> @FormChangeBounds then
    FOldChangeBounds := FControlForm.OnChangeBounds;

  FControlForm.OnChangeBounds:=@FormChangeBounds;

  SetPosition;

  FRealDropDownCount := Round(Height / FGrid.DefaultRowHeight);

  FFrags.Clear;

  if FControl.FFiltering then
  begin
    if FGrid.RowCount > FGrid.FixedRows then
    begin
      FGrid.Row := FGrid.FixedRows;
      FGrid.TopRow := FGrid.FixedRows;
    end;
    SplitStr(FControl.Text, ' ', FFrags);
  end
  else
	  FindItem;

  if not Visible then
    Show;
end;

function TdxLCbxListForm.IsMouseEntered: Boolean;
var
  X, Y: LongInt;
begin
  X := Mouse.CursorPos.X;
  Y := Mouse.CursorPos.Y;
  Result := (X >= Left) and (X <= Left + Width) and (Y >= Top) and (Y <= Top + Height);
end;

{ TInsertedValues }

function TInsertedValues.GetValues(Index: Integer): TInsertValueData;
begin
  Result := TInsertValueData(Items[Index]);
end;

function TInsertedValues.AddValue: TInsertValueData;
begin
  Result := TInsertValueData.Create;
  Add(Result);
end;

procedure TInsertedValues.DeleteValue(Index: Integer);
begin
  Values[Index].Free;
  Delete(Index);
end;

procedure TInsertedValues.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Values[i].Free;
  inherited Clear;
end;

function CheckCutPasteKeys(C: TComponent; Field: TField; var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  if Field = nil then Exit;
  if (Field.DataSet.State in [dsInsert, dsEdit]) and
  	((Trim(GetExpression(C)) = '') or GetEditable(C)) and (not GetReadOnly(C)) then Result := True
  else if (Key in [VK_X, VK_V, VK_INSERT, VK_DELETE]) and ([ssCtrl, ssShift] * Shift <> []) then Key := 0;
end;

{ TdxFormTreeField }

function TdxFormTreeField.GetForm: TdxForm;
begin
  Result := TdxFormTreeFields(Collection).Tree.Form;
end;

procedure TdxFormTreeField.SetFieldName(AValue: String);
var
  C: TComponent;
begin
  if AValue = FFieldName then Exit;
  FFieldName := AValue;
  C := FindComponentByFieldName(TdxFormTreeFields(Collection).Tree.Form, AValue);
  if C <> nil then FFieldId := GetId(C)
  else FFieldId := 0;
end;

function TdxFormTreeField.FieldSourceToStr: String;
begin
  case FFieldSource of
    tfsForm: Result := rsForm;
    tfsObject: Result := rsObject;
    else Result := '';
  end;
end;

{ TdxFormTreeFields }

function TdxFormTreeFields.GetFields(Index: Integer): TdxFormTreeField;
begin
  Result := TdxFormTreeField(Items[Index]);
end;

procedure TdxFormTreeFields.SetFields(Index: Integer; AValue: TdxFormTreeField);
begin
  Items[Index] := AValue;
end;

constructor TdxFormTreeFields.Create(ATree: TdxFormTree);
begin
  inherited Create(TdxFormTreeField);
  FTree := ATree;
end;

function TdxFormTreeFields.Add: TdxFormTreeField;
begin
  Result := TdxFormTreeField(inherited Add);
end;

function TdxFormTreeFields.FindField(FieldId: Integer;
  FieldSource: TdxFormTreeFieldSource): TdxFormTreeField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  	if (Fields[i].FieldId = FieldId) and (Fields[i].FieldSource = FieldSource) then
    	Exit(Fields[i]);
end;

{ TdxFormTree }

procedure TdxFormTree.SetFields(AValue: TdxFormTreeFields);
begin
  FFields.Assign(AValue);
end;

procedure TdxFormTree.PopupHandler(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: UserUpdateTree;
  end;
end;

procedure TdxFormTree.SearchFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if not TTreeSearchForm(Sender).Accept then
  begin
    ClearSelection;
    if FOldNode <> nil then FOldNode.Selected := True;
  end;
  //UnlockSelectionChangeEvent;
  FNoHandleSelect:=False;
  DoSelectionChanged;
end;

function TdxFormTree.GenerateSQLForm(RecId: Integer; AllFields: Boolean
  ): String;
var
  C, CC: TComponent;
  FL: TList;
  i: Integer;
  OStr: String;
  TF: TdxFormTreeField;
begin
  OStr := '';
  FL := TList.Create;
  for i := 0 to FFields.Count - 1 do
  begin
    TF := FFields[i];

    if (TF.FieldSource <> tfsForm) and (not AllFields) then Continue;

    C := FindById(FForm, TF.FieldId);
    FL.Add(C);
    if C is TdxLookupComboBox then
      OStr := OStr + FieldStr(C) + 'l,' + FieldStr(C) + ','
    else if C is TdxObjectField then
    begin
    	OStr := OStr + FieldStr(C) + ',';
      CC := GetObjectFieldField(TdxObjectField(C));
      if CC is TdxLookupComboBox then OStr := OStr + FieldStr(C) + 'id,'
    end
    else
      OStr := OStr + FieldStr(C) + ',';
  end;
  OStr := Copy(OStr, 1, Length(OStr) - 1);
  try
    if FL.Count > 0 then
    begin
  	  Result := SqlSelectStatement(FForm, FL, FForm.UseSelCond, True, TDataSetProcessor(FForm.DSP).CallerObject, '', RecId, True);
      if (RecId < 0) and (OStr <> '') then Result := Result + ' order by ' + OStr;
    end;
  finally
    FL.Free;
  end;
end;

function TdxFormTree.GenerateSQLObjects: String;
var
  i: Integer;
  TF: TdxFormTreeField;
  C, SrcC: TComponent;
  S, FlNm, ParFNm: String;
  Fm: TdxForm;
begin
  Result := '';
  for i := 0 to FFields.Count - 1 do
  begin
    TF := FFields[i];
    if TF.FieldSource <> tfsObject then Continue;
    C := FindById(FForm, TF.FieldId);
    if C is TdxLookupComboBox then
    begin
      //S := SqlSelectGroupsForTree(TdxLookupComboBox(C));
      //if S = '' then
      Fm := FormMan.FindForm(GetSourceTId(C));
      if (Fm.ParentField > 0) and (GetFormParentFieldFieldId(Fm) = GetSourceFId(C)) then
        ParFNm := FieldStr(Fm.ParentField)
      else
        ParFNm := '0';
      SrcC := GetListSourceField(C);
      if SrcC is TdxRecordId then FlNm := 'id'
      else FlNm := FieldStr(GetSourceFId(C));
    	S := 'select id,' + IntToStr(i) + ',' +
     		FlNm + ',' + ParFNm + ' from ' + TableStr(GetSourceTId(C));
      Result := Result + S;
    end;
    Result := Result + ' union all ';
  end;
  if Result <> '' then
	  Result := Copy(Result, 1, Length(Result) - 11) + ' order by 2,3';
end;

procedure TdxFormTree.CheckFields;
var
  i: Integer;
  C: TComponent;
begin
  for i := FFields.Count - 1 downto 0 do
  begin
    C := FindById(FForm, FFields[i].FieldId);
    if C = nil then FFields.Delete(i)
    else if (C is TdxLookupComboBox) and ((GetSourceTId(C) = 0) or (GetSourceFId(C) = 0)) then FFields.Delete(i)
    else if (C is TdxObjectField) and (GetObjectFieldField(TdxObjectField(C)) = nil) then
    	FFields.Delete(i);
  end;
end;

function TdxFormTree.GetNodePath(N: TTreeNode; const D: Char): String;
begin
  Result := '';
  while N <> nil do
  begin
    Result := N.Text + D + Result;
    N := N.Parent;
  end;
end;

function TdxFormTree.GetNodeWithRecId(RecId: Integer; ParentNode: TTreeNode
  ): TTreeNode;
var
  i: Integer;
  N: TTreeNode;
  ND: TdxTreeNodeData;
begin
  Result := nil;
  for i := 0 to ParentNode.Count - 1 do
  begin
    N := ParentNode.Items[i];
    ND := TdxTreeNodeData(N.Data);
    if ND.RecId = RecId then Exit(N);
  end;
end;

function TdxFormTree.GetNodeWithText(const S: String; ParentNode: TTreeNode
  ): TTreeNode;
var
  i: Integer;
  N: TTreeNode;
begin
  Result := nil;
  for i := 0 to ParentNode.Count - 1 do
  begin
    N := ParentNode.Items[i];
    if N.Text = S then Exit(N);
  end;
end;

function TdxFormTree.GetObjectNodeWithRecId(RecId: Integer; Field: TdxFormTreeField
  ): TTreeNode;
var
  i: Integer;
  ND: TdxTreeNodeData;
  N: TTreeNode;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
  begin
    N := Items[i];
    ND := TdxTreeNodeData(N.Data);
    if (ND <> nil) and (ND.Field = Field) and (ND.RecId = RecId) then Exit(N);
  end;
end;

procedure TdxFormTree.AddNodeData(Node: TTreeNode; Field: TdxFormTreeField;
  RecId: Integer);
var
  ND: TdxTreeNodeData;
begin
  ND := TdxTreeNodeData.Create;
  ND.Field := Field;
  ND.RecId := RecId;
  Node.Data := ND;
  FNodeDataList.Add(ND);
end;

procedure TdxFormTree.UpdateTreeForm;
var
  DS: TSQLQuery;
  FL, KeyL, TFL: TList;
  C: TComponent;
  FNm, SQL, Value: String;
  i, Level: Integer;
  TF: TdxFormTreeField;
  N, PNode: TTreeNode;
  Nodes: array of TTreeNode;
  Values: array of String;
  IsFirst: Boolean;
begin
  SQL := GenerateSQLForm(-1, False);
  if SQL = '' then Exit;

  DS := DBase.OpenDataSet(SQL);
  FL := TList.Create;
  KeyL := TList.Create;
  TFL := TList.Create;

  try

  for i := 0 to FFields.Count - 1 do
  begin
    TF := FFields[i];
    if TF.FFieldSource <> tfsForm then Continue;
    C := FindById(FForm, TF.FieldId);
    FNm := FieldStr(C);
    if C is TdxLookupComboBox then
    begin
      KeyL.Add(DS.FieldByName(FNm));
      FL.Add(DS.FieldByName(FNm + 'l'));
    end
    else if ObjectFieldIsObject(C) then
    begin
      KeyL.Add(DS.FieldByName(FNm + 'id'));
      FL.Add(DS.FieldByName(FNm));
    end
    else
    begin
      KeyL.Add(nil);
      FL.Add(DS.FieldByName(FNm));
    end;
    TFL.Add(TF);
  end;
  if TFL.Count > 0 then
  begin
	  FAllNode := Items.AddChild(nil, rsTreeAll);

    SetLength(Nodes, TFL.Count);
    SetLength(Values, TFL.Count);
    for i := 0 to TFL.Count - 1 do
    begin
      Nodes[i] := nil;
      Values[i] := '';
    end;

    IsFirst := True;
    while not DS.EOF do
    begin
      // Определяем уровень
      if IsFirst then
      begin
        Level := 0;
        PNode := FAllNode;
        IsFirst := False;
      end
      else
      begin
        // На случай, если записи одинаковы
        Level := TFL.Count;
        //PNode := Nodes[TFL.Count-1].Parent;
        //
        for i := 0 to TFL.Count - 1 do
        begin
          if KeyL[i] = nil then
          	Value := TField(FL[i]).AsString
          else
            Value := IntToStr(TField(KeyL[i]).AsInteger);
          if Value <> Values[i] then
          begin
            Level := i;
            PNode := Nodes[i].Parent;
            Break;
          end;
        end;
      end;
      //
      for i := Level to TFL.Count - 1 do
      begin
        TF := TdxFormTreeField(TFL[i]);
        Value := TField(FL[i]).AsString;
        N := Items.AddChild(PNode, Value);
        if KeyL[i] = nil then
        begin
          AddNodeData(N, TF, 0);
          Values[i] := Value;
        end
        else
        begin
          AddNodeData(N, TF, TField(KeyL[i]).AsInteger);
          Values[i] := IntToStr(TField(KeyL[i]).AsInteger);
        end;
        Nodes[i] := N;
        PNode := N;
      end;
      DS.Next;
    end;

    SetLength(Nodes, 0);
    SetLength(Values, 0);
  end;

  finally
    DS.Free;
    KeyL.Free;
    FL.Free;
    TFL.Free;
  end;
end;

procedure TdxFormTree.UpdateTreeObjects;
var
  DS: TSQLQuery;
  TF: TdxFormTreeField;
  idx, oldidx: LongInt;
  C: TComponent;
  N, PN: TTreeNode;
  SQL: String;
  IsPath: Boolean;
  SL: TStringList;
  StartBmk: TBookMark;
  SrcFm: TdxForm;

  {function _AddNodePath(ParNode: TTreeNode; const Path: String): TTreeNode;
  var
    i: Integer;
    N: TTreeNode;
  begin
    SplitStr(Path, '\', SL);
    N := ParNode;
    for i := 0 to SL.Count - 1 do
    begin
      N := N.FindNode(SL[i]);
      if N = nil then
      	N := Items.AddChild(ParNode, SL[i]);
      ParNode := N;
    end;
    Result := N;
  end;   }

  // Находим полный путь от потомка до корневого предка.
  procedure _GetPath;
  var
    B: TBookmark;
    PId: Variant;
    PIdList: TIntegerList;
  begin
    SL.Clear;
    B := DS.GetBookmark;
    PIdList := TIntegerList.Create;
    while True do
    begin
      SL.InsertObject(0, DS.Fields[2].AsString, TObject(PtrInt(DS.Fields[0].AsInteger)));
      PId := DS.Fields[3].Value;
      if PId = Null then Break;
      // Обнаружение зацикливания
      if PIdList.FindValue(PId) >= 0 then Break;
      PIdList.AddValue(PId);
      //
      DS.GotoBookmark(StartBmk);
      while not DS.Eof do
      begin
        if DS.Fields[0].Value = PId then Break
        else if DS.Fields[1].AsInteger <> idx then Break;
        DS.Next;
      end;
    end;
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    PIdList.Free;
  end;

  // Добавляем с учетом сортировки
  function _AddNodePathItem(ParNode: TTreeNode; const S: String): TTreeNode;
  var
    i: Integer;
    N: TTreeNode;
  begin
    for i := 0 to ParNode.Count - 1 do
    begin
      N := ParNode.Items[i];
      if MyUtf8CompareText(N.Text, S) > 0 then
      begin
        Result := Items.Insert(N, S);
        Exit;
      end;
    end;
    Result := Items.AddChild(ParNode, S);
  end;

  // Добавляем иерархический элемент
  function _AddNodePath(ParNode: TTreeNode): TTreeNode;
  var
    i, j, RecId: Integer;
    N, PN: TTreeNode;
    ND: TdxTreeNodeData;
    Ok: Boolean;
  begin
    _GetPath;
    PN := ParNode;
    for i := 0 to SL.Count - 1 do
    begin
      Ok := False;
      RecId := PtrInt(SL.Objects[i]);
      for j := 0 to PN.Count - 1 do
      begin
        N := PN.Items[j];
        ND := TdxTreeNodeData(N.Data);
        if ND.RecId = RecId then
        begin
          Ok := True;
          PN := N;
          Break;
        end;
      end;
      if not Ok then
      begin
        j := i;
        Break;
      end;
    end;
    if not Ok then
    begin
      for i := j to SL.Count - 1 do
      begin
        N := _AddNodePathItem(PN, SL[i]);
        RecId := PtrInt(SL.Objects[i]);
        AddNodeData(N, TF, RecId);
        PN := N;
      end;
    end;
    Result := PN;
  end;

begin
  SQL := GenerateSQLObjects;
  if SQL = '' then Exit;

  DS := DBase.OpenDataSet(SQL);

	SL := TStringList.Create;
  PN := nil;
  StartBmk := nil;
  oldidx := -1;
  while not DS.EOF do
  begin
    idx := DS.Fields[1].AsInteger;
    TF := FFields[idx];
    if idx <> oldidx then
    begin
      oldidx := idx;
      if StartBmk <> nil then
      begin
        DS.FreeBookmark(StartBmk);
        StartBmk := nil;
      end;

      C := FindById(FForm, TF.FieldId);
      SrcFm := FormMan.FindForm(GetSourceTId(C));
      IsPath := (SrcFm.ParentField > 0) and (GetFormParentFieldFieldId(SrcFm) =
        GetSourceFId(C));
      //IsPath := FormMan.FindForm(GetSourceTId(C)).ParentField > 0;
      N := Items.AddChild(nil, GetFieldName(C));
      AddNodeData(N, TF, 0);
      PN := N;

      StartBmk := DS.GetBookmark;
  	end;
    if not IsPath then
    begin
	    N := Items.AddChild(PN, DS.Fields[2].AsString);
      AddNodeData(N, TF, DS.Fields[0].AsInteger);
    end
    else
      N := _AddNodePath(PN);
    DS.Next;
  end;
  if StartBmk <> nil then DS.FreeBookmark(StartBmk);

  DS.Free;
  SL.Free;
end;

procedure TdxFormTree.UserUpdateTree;
var
  Node: TTreeNode;
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  for i := 0 to SelectionCount - 1 do
    SL.Add(GetNodePath(Selections[i], '/'));
  UpdateTree;
  for i := 0 to SL.Count - 1 do
  begin
	  Node := Items.FindNodeWithTextPath(SL[i]);
	  if Node <> nil then Node.Selected := True;
  end;
  SL.Free;
end;

procedure TdxFormTree.DoSelectionChanged;
var
  i, FId: Integer;
  N: TTreeNode;
  FF: TFilterField;
  Nodes: TList;
  C: TComponent;
  ND: TdxTreeNodeData;
begin
  inherited DoSelectionChanged;

  if FNoHandleSelect then Exit;

  Nodes := TList.Create;
  // Очищаем поля фильтра
  for i := 0 to FFields.Count - 1 do
  begin
    FF := FForm.Filter.FindField(FFields[i].FieldId);
    if FF <> nil then
    begin
      FF.Values.Clear;
	    FF.IsNull := False;
      FF.IsNot := False;
    end;
  end;
  for i := 0 to SelectionCount - 1 do
  begin
    N := Selections[i];
    ND := TdxTreeNodeData(N.Data);
    if N = FAllNode then Continue
    else if ND.Field.FieldSource = tfsObject then
    begin
      if ND.RecId > 0 then Nodes.Add(N);
    end
    else
      repeat
        if Nodes.IndexOf(N) < 0 then
	        Nodes.Add(N);
        N := N.Parent;
      until N = FAllNode;
  end;
  // Заполняем фильтр
  for i := 0 to Nodes.Count - 1 do
  begin
    N := TTreeNode(Nodes[i]);
    ND := TdxTreeNodeData(N.Data);
    //L := N.Level-1;
    //FId := FFields[L].FieldId;
    FId := ND.Field.FieldId;
    FF := FForm.Filter.FindField(FId);
    if FF = nil then
    begin
    	FF := FForm.Filter.AddField;
      FF.FId:=FId;
    end;
    C := FindById(FForm, FF.FId);
    if N.Text = '' then
    	FF.IsNull := True
    else if (C is TdxCalcEdit) or (C is TdxDateEdit) or (C is TdxTimeEdit) or
    	(C is TdxCounter) or (C is TdxRecordId) then
    	FF.Values.Add(N.Text + ' .. ' + N.Text)
    else if (C is TdxLookupComboBox) or ObjectFieldIsObject(C) then
    	FF.Values.Add(IntToStr(ND.RecId))
    else
      FF.Values.Add(N.Text);
  end;
  FForm.Refresh;
  Nodes.Free;

  //if Selected <> nil then ShowMessage(IntToStr(TdxTreeNodeData(Selected.Data).Field.FieldId));
end;

procedure TdxFormTree.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  if Utf8Key < ' ' then Exit;
  //LockSelectionChangeEvent;
  FNoHandleSelect := True;
  with TTreeSearchForm.CreateNew(nil) do
  begin
    OnClose:=@SearchFormClose;
    FOldNode := Selected;
    ShowForm(Self, Utf8Key);
  end;
end;

procedure TdxFormTree.Loaded;
var
  i: Integer;
  C: TComponent;
begin
  inherited Loaded;
  for i := 0 to FFields.Count - 1 do
  begin
    C := FindById(FForm, FFields[i].FieldId);
    if C <> nil then FFields[i].FFieldName := GetFieldName(C);
  end;
end;

constructor TdxFormTree.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  ParentFont := False;
  ParentColor := False;
  ReadOnly := True;
  MultiSelect := True;
  Width := ScaleToScreen(200);
  Options:=Options-[tvoThemedDraw]+[tvoRowSelect];

  FPopup := TPopupMenu.Create(Self);
  FPopup.Images := Images16;
  FPopup.Items.Add( CreateMenuItem(FPopup, rsRefresh, 0, 0, @PopupHandler, IMG16_REFRESH) );
  PopupMenu := FPopup;

  FFields := TdxFormTreeFields.Create(Self);
  FNodeDataList := TList.Create;
end;

destructor TdxFormTree.Destroy;
begin
  ClearList(FNodeDataList);
  FNodeDataList.Free;
  FFields.Free;
  inherited Destroy;
end;

procedure TdxFormTree.UpdateTree;
var
  i: Integer;
  N: TTreeNode;
begin
  CheckFields;
  BeginUpdate;
  Items.Clear;
  try
    UpdateTreeForm;
    UpdateTreeObjects;
    for i := 0 to Items.Count - 1 do
    begin
      N := Items[i];
      if N.Level <= FExpandLevels then N.Expand(False);
    end;
  finally
    EndUpdate;
  end;
  if FOnUpdateTree <> nil then FOnUpdateTree(Self);
end;

procedure TdxFormTree.SelectByRecord(RecId: Integer);
var
  FNm: String;
  i: Integer;
  PN, N: TTreeNode;
  DS: TSQLQuery;
  C: TComponent;
  TF: TdxFormTreeField;
begin
  //LockSelectionChangeEvent;
  FNoHandleSelect := True;
  ClearSelection;
  PN := FAllNode;
  DS := DBase.OpenDataSet( GenerateSQLForm(RecId, True) );
  if DS.RecordCount > 0 then
  begin
    for i := 0 to FFields.Count - 1 do
    begin
      TF := FFields[i];
      if TF.FieldSource <> tfsForm then Continue;
      C := FindById(FForm, TF.FieldId);
      FNm := FieldStr(C);
      if C is TdxLookupComboBox then
	      N := GetNodeWithRecId(DS.FieldByName(FNm).AsInteger, PN)
      else if ObjectFieldIsObject(C) then
        N := GetNodeWithRecId(DS.FieldByName(FNm+'id').AsInteger, PN)
      else
      	N := GetNodeWithText(DS.FieldByName(FNm).AsString, PN);
      if N = nil then Break;
      PN := N;
    end;
    if PN <> nil then PN.Selected := True;
  end;

  for i := 0 to FFields.Count - 1 do
  begin
    TF := FFields[i];
    if TF.FieldSource <> tfsObject then Continue;
    FNm := FieldStr(TF.FieldId);
    N := GetObjectNodeWithRecId(DS.FieldByName(FNm).AsInteger, TF);
    if N <> nil then N.Selected := True;
  end;

  DS.Free;
  //UnlockSelectionChangeEvent;
  FNoHandleSelect := False;
  DoSelectionChanged;
end;

procedure TdxFormTree.UpdateSelection;
begin
  DoSelectionChanged;
end;

procedure TdxFormTree.SoftClearSelection;
begin
  FNoHandleSelect := True;
  ClearSelection;
  FNoHandleSelect := False;
end;

{procedure TdxFormTree.SelectFirstNode(SelectionDisabled: Boolean);
begin
  FDisableSelection:=SelectionDisabled;
  ClearSelection;
  Selected := FAllNode;
  FDisableSelection:=False;
end;}

{procedure TdxFormTree.ClearSelection(KeepPrimary: Boolean);
begin
  LockSelectionChangeEvent;
  inherited ClearSelection(KeepPrimary);
  UnlockSelectionChangeEvent;
end; }

function TdxFormTree.GetFieldNameByNode(N: TTreeNode): String;
var
  ND: TdxTreeNodeData;
begin
  ND := TdxTreeNodeData(N.Data);
  if ND <> nil then Result := ND.Field.FieldName
  else Result := '';
end;

function TdxFormTree.GetFieldValueByNode(N: TTreeNode): Variant;
var
  ND: TdxTreeNodeData;
  C: TComponent;
begin
  Result := Null;
  ND := TdxTreeNodeData(N.Data);
  if ND = nil then Exit;

  C := FindById(FForm, ND.Field.FieldId);
  if C = nil then Exit;

  if (C is TdxLookupComboBox) or ObjectFieldIsObject(C) then
  	Result := ND.RecId
  else
    Result := N.Text;
end;

{ TdxButton }

function TdxButton.IsStoredGlyph: Boolean;
begin
  Result := FImageName = '';
end;

procedure TdxButton.SetImageName(AValue: String);
var
  St: TStream;
  Pic: TPicture;
  pImg: PImageCacheData;
begin
  if FImageName=AValue then Exit;
  FImageName:=AValue;
  Glyph.Clear;
  Images := nil;
  ImageIndex := -1;
  if FImageName = '' then Exit;

  pImg := ImageCache.FindImage(AValue);
  if pImg <> nil then
  begin
    Images := pImg^.ImageList;
    ImageIndex := pImg^.ImageIndex;
    // !!!
    Exit;
  end;

  ImageMan.GetImageStreamPPI(FImageName, St);
  if St = nil then Exit;
  Pic := TPicture.Create;
  try try
    Pic.LoadFromStream(St);
    pImg := ImageCache.AddImage(AValue, Pic.Bitmap);
    if pImg <> nil then
    begin
      Images := pImg^.ImageList;
      ImageIndex := pImg^.ImageIndex;
    end
    else
      Glyph := Pic.Bitmap;
  except
    ;
  end;
  finally
    St.Free;
    Pic.Free;
  end;
end;

procedure TdxButton.TextChanged;
begin
  inherited TextChanged;
  if Caption = '' then Spacing:=0
  else Spacing := 4;
end;

procedure TdxButton.RenameImage(const S: String);
begin
  FImageName := S;
end;

constructor TdxButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := ScaleToScreen(100);
  Height := ScaleToScreen(30);
  FStopTab := True;
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

function TdxGrid.GetTitleFontStyle: TFontStyles;
begin
  Result := TitleFont.Style;
end;

procedure TdxGrid.SetTitleFontStyle(AValue: TFontStyles);
begin
  TitleFont.Style := AValue;
end;

function TdxGrid.IsTitleFontStyleStored: Boolean;
begin
  Result := (TitleFont.Style = []) and (Font.Style <> []);
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
  if DataSource = nil then Exit;

  if DataSource.DataSet.State in [dsInsert, dsEdit] then Exit;
  inherited WMVScroll(Message);
end;

procedure TdxGrid.WMHScroll(var message: TLMHScroll);
begin
  if EditorMode then EditorMode := False;
  inherited WMHScroll(message);
end;

procedure TdxGrid.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if DataSource = nil then Exit;

  if DataSource.DataSet.State in [dsInsert, dsEdit] then Exit;
  inherited WMMouseWheel(Message);
end;

procedure TdxGrid.SelectEditor;
begin
  inherited SelectEditor;
end;

procedure TdxGrid.UpdateData;
begin
  if (not ReadOnly) and (SelectedColumn <> nil) and
    (FindById(FForm, SelectedColumn.Tag) is TdxLookupComboBox) then
    // Пропускаем стандартную обработку для объектов, потому что при редактировании
    // в гриде бывает, что текст в поле исчезает, хотя id остается на месте.
  else
	  inherited UpdateData;
end;

function TdxGrid.EditorCanAcceptKey(const ch: TUTF8Char): boolean;
begin
  Result:=inherited EditorCanAcceptKey(ch);
	// Первый нажатый символ при неактивном редакторе объекта не вызывает KeyDown
  // компонента, следовательно, не срабатывает фильтрация и
  // потеря фокуса не восстанавливает значение.
  if Result and (Editor <> nil) and (Editor is TdxLookupComboBox) then
  	TdxLookupComboBox(Editor).FChanging:=True;
end;

procedure TdxGrid.Paint;
var
  TS: TTextStyle;
  Fm: TdxForm;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := taCenter;
    TS.Layout := tlCenter;
    TS.SingleLine := False;
    TS.WordBreak := True;
    Fm := FormMan.FindForm(FId);
    Canvas.Font := Fm.Grid.Font;
    Canvas.Font.Color := clWindowText;
    Canvas.TextRect(ClientRect, 0, 0, Fm.FormCaption, TS);
  end;
end;

constructor TdxGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Options := Options + [dgAlwaysShowEditor];
  VisibleButtons := [gbnAppend, gbnEdit, gbnDelete, gbnDuplicate, gbnShopping,
    gbnMoveUp, gbnMoveDown];
  FMaskEdit := TMaskCellEditor.Create(nil);
  FMaskEdit.BorderStyle := bsNone;
  FMaskEdit.Grid := Self;
  FMemo := TMemoCellEditor.Create(nil);
  FMemo.BorderStyle := bsNone;
end;

destructor TdxGrid.Destroy;
begin
  FMemo.Free;
  FMaskEdit.Free;
  inherited Destroy;
end;

procedure TdxGrid.ForceSelectEditor;
begin
  SelectEditor;
end;

function TdxGrid.GetFieldNameByColumn(aColumn: TColumn): String;
var
  C: TComponent;
begin
  Result := '';
  C := FindById(FForm, aColumn.Tag);
  if C <> nil then Result := GetFieldName(C);
end;

function TdxGrid.FindColumnByFieldName(const FieldName: String): TColumn;
var
  C: TComponent;
  i, FlId: Integer;
  Column: TMyDBGridColumn;
begin
  C := Form.FindComponentByFldName(FieldName);
  FlId := GetId(C);
  // Должна быть колонка найдена.
  for i := 0 to Columns.Count - 1 do
  begin
    Column := Columns[i];
    if Column.Tag = FlId then Exit(Column);
  end;
  // Это не должно произойти.
  raise Exception.CreateFmt('Column with field name [%s] not found.', [FieldName]);
end;

{ TdxCounter }

procedure TdxCounter.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);//Text := '';
  end;
end;

procedure TdxCounter.MenuPopup(Sender: TObject);
var
  ae: Boolean;
  N: Longint;
begin
  if CanFocus then SetFocus;
  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled := (Clipboard.AsText <> '') and
  	(TryStrToInt(Clipboard.AsText, N)) and (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Text <> '') and (not ReadOnly) and ae;

  PopupMenu.Items[0].Visible := not ReadOnly;
  PopupMenu.Items[2].Visible := not ReadOnly;
  PopupMenu.Items[3].Visible := not ReadOnly;
  PopupMenu.Items[4].Visible := not ReadOnly;
end;

procedure TdxCounter.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxCounter.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

function TdxCounter.GetDrawText: String;
begin
  Result:=FieldName;
end;

constructor TdxCounter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

procedure TdxCounter.Init;
begin
  ReadOnly := True;
  StopTab := False;
end;

{ TdxTimeEdit }

procedure TdxTimeEdit.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);//Text := '';
    6: Field.Value := TMenuItem(Sender).Caption;
  end;
end;

procedure TdxTimeEdit.MenuPopup(Sender: TObject);
var
  ae, rae: Boolean;
  DT: TDateTime;
begin
  if CanFocus then
  begin
    SetFocus;
    //Selstart := 0;
    //SelLength := Utf8Length(Text);
  end;
  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  rae := (not ReadOnly) and ae;
  PopupMenu.Items[0].Enabled := (SelText <> '') and rae;
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := (Clipboard.AsText <> '') and
  	(TryStrToTime(Clipboard.AsText, DT)) and rae;
  PopupMenu.Items[4].Enabled := (Text <> '') and rae;
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

procedure TdxTimeEdit.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

function TdxTimeEdit.GetDrawText: String;
begin
  Result:=FieldName;
end;

constructor TdxTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  TimeFormat := ttHHMM;
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '08:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '09:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '10:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '11:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '12:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '13:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '14:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '15:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '16:00', 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '17:00', 6, 0, @MenuClick) );
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
  if CanFocus then SetFocus;
  PopupMenu.Items[0].Enabled := SelText <> '';
end;

procedure TdxObjectField.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

function TdxObjectField.GetDrawText: String;
begin
  Result:=FieldName;
end;

constructor TdxObjectField.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 0, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

procedure TdxObjectField.Init;
begin
  StopTab := False;
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

constructor TdxLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

{ TdxDateEdit }

procedure TdxDateEdit.MenuPopup(Sender: TObject);
var
  ae: Boolean;
  D: TDateTime;
begin
  if CanFocus then SetFocus;
  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled := (SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := (Clipboard.AsText <> '') and
  	(TryStrToDate(Clipboard.AsText, D)) and (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled := (Text <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[6].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[7].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[8].Enabled := (not ReadOnly) and ae;
  PopupMenu.Items[9].Enabled := (not ReadOnly) and ae;
end;

procedure TdxDateEdit.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxDateEdit.MenuClick(Sender: TObject);
var
  D: TDateTime;
  n: PtrInt;
begin
  D := SysUtils.Date;
  n := TMenuItem(Sender).Tag;
  case n of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);//Text := '';
    6: Self.Date := D;
    7: Self.Date := IncDay(D, -DayOfTheWeek(D) + 1);
    8: Self.Date := IncDay(D, -DayOf(D) + 1);
    9: Self.Date := EncodeDate(YearOf(D), 1, 1);
  end;
  if n in [6..9] then SelStart := Utf8Length(Text);
end;

{function TdxDateEdit.GetDefaultGlyphName: String;
begin
  Result:='date16';
end;}

procedure TdxDateEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then
  begin
    Button.Images := Images48;
    Button.ImageIndex := IMG48_DATE;
  end
  else if AHeight > 36 then
  begin
    Button.Images := Images32;
    Button.ImageIndex := IMG32_DATE;
  end
  else if AHeight > 28 then
  begin
    Button.Images := Images24;
    Button.ImageIndex := IMG24_DATE;
  end
  else
  begin
    Button.Images := Images16;
    Button.ImageIndex := IMG16_DATE;
  end
  {if AHeight > 52 then S := 'date48'
  else if AHeight > 36 then S := 'date32'
  else if AHeight > 28 then S := 'date24'
  else S := 'date16';
  Button.LoadGlyphFromLazarusResource(S); }
end;

procedure TdxDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

function TdxDateEdit.GetDrawText: String;
begin
  Result:=FieldName;
end;

constructor TdxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsToday, 6, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginWeek, 7, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginMonth, 8, 0, @MenuClick) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsBeginYear, 9, 0, @MenuClick) );
  PopupMenu.OnPopup:=@MenuPopup;
  Button.PopupMenu := PopupMenu;
  Button.Images := Images16;
  Button.ImageIndex := IMG16_DATE;
end;

{ TdxCalcEdit }

procedure TdxCalcEdit.MenuPopup(Sender: TObject);
var
  ae: Boolean;
  E: Extended;
begin
  if CanFocus then SetFocus;
  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled := (Clipboard.AsText <> '') and
  	(TryStrToFloat(Clipboard.AsText, E)) and (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Text <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[5].Enabled := (not ReadOnly) and ae;
end;

procedure TdxCalcEdit.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
    4: Field.SetData(nil);//Text := '';
    5:

      begin
        Field.Value := 0;
        //Text := '0';
        SelStart := Utf8Length(Text);
      end;
  end;
end;

procedure TdxCalcEdit.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

{function TdxCalcEdit.GetDefaultGlyphName: String;
begin
  Result:='calc16';
end;   }

procedure TdxCalcEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Button = nil then Exit;
  Button.Width:=AHeight;
  Button.Height := AHeight;
  if AHeight > 52 then
  begin
    Button.Images := Images48;
    Button.ImageIndex := IMG48_CALC;
  end
  else if AHeight > 36 then
  begin
    Button.Images := Images32;
    Button.ImageIndex := IMG32_CALC;
  end
  else if AHeight > 28 then
  begin
    Button.Images := Images24;
    Button.ImageIndex := IMG24_CALC;
  end
  else
  begin
    Button.Images := Images16;
    Button.ImageIndex := IMG16_CALC;
  end
  {if AHeight > 52 then S := 'calc48'
  else if AHeight > 36 then S := 'calc32'
  else if AHeight > 28 then S := 'calc24'
  else S := 'calc16';
  Button.LoadGlyphFromLazarusResource(S);  }
end;

procedure TdxCalcEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  OldKey: TUTF8Char;
  p, N, CaretX: Integer;
  S: TCaption;
  Tmp: Word;
begin
  Tmp := 0;

  if Pos(Utf8Key, '0123456789') > 0 then
  begin
    CaretX := CaretPos.X;
    S := Text;
    if (S > '') and (S[1] in ['-', '+']) then
    begin
    	Delete(S, 1, 1);
      Dec(CaretX);
    end;
    if CaretX < 0 then CaretX := 0;
    p := Pos(DefaultFormatSettings.DecimalSeparator, S);
    if p > 0 then
	    N := Length(Copy(S, 1, p - 1))
    else
    begin
      N := Length(S);
      p := 16;
    end;
    if (N >= 15 - Precission) and (SelLength = 0) and (CaretX < p) then UTF8Key := '';
  end;

  OldKey := Utf8Key;
  inherited UTF8KeyPress(UTF8Key);
  if (Utf8Key = '') and ((OldKey = ' ') or (OldKey = '.')) and
  	(CheckCutPasteKeys(Self, Field, Tmp, [])) then
  	Utf8Key := DefaultFormatSettings.DecimalSeparator;
end;

procedure TdxCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

function TdxCalcEdit.GetDrawText: String;
begin
  Result:=FieldName;
end;

constructor TdxCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  Width := ScaleToScreen(100);
  FNullToZero := True;
  FGroupDigits := True;
  FPadZeros := True;
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsSetToZero, 5, 0, @MenuClick) );
  PopupMenu.OnPopup:=@MenuPopup;
  Button.PopupMenu := PopupMenu;
  Button.Images := Images16;
  Button.ImageIndex := IMG16_CALC;
end;

procedure TdxCalcEdit.Init;
begin
  FDefaultValue := '0';
end;

function TdxCalcEdit.PrecStr: String;
begin
  Result := MakeNumberFormat(Precission, GroupDigits, PadZeros);
end;

{ TdxScrollBox }

procedure TdxScrollBox.Paint;
begin
  inherited Paint;
  if Color = clDefault then Exit;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, HorzScrollBar.Range + ClientWidth,
    VertScrollBar.Range + ClientHeight));
end;

{ TUtf8StringList }

{function TUtf8StringList.DoCompareText(const s1, s2: string): PtrInt;
begin
  Result:=MyUtf8CompareText(s1, s2);
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

function TdxComboBox.GetSourceFieldName: String;
begin
  Result := _GetSourceFieldName(Self);
end;

function TdxComboBox.GetSourceFormName: String;
begin
  Result := _GetSourceFormName(Self);
end;

procedure TdxComboBox.MenuClick(Sender: TObject);
begin
	case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);//ClearData;
  end;
end;

procedure TdxComboBox.MenuPopup(Sender: TObject);
var
  ae: Boolean;
begin
  if CanFocus then SetFocus;
  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled:=(not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Clipboard.AsText <> '') and (Text <> '') and (not ReadOnly) and ae;
end;

procedure TdxComboBox.DoNeedData;
begin
  if (SourceTId > 0) and (SourceFId > 0) and (FOnNeedData <> nil) then
  	FOnNeedData(Self);
end;

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
  if (ItemIndex >= 0) and (Field.DataSet.State in [dsInsert, dsEdit]) then
    Field.Text := Text;
end;

{procedure TdxComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
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
end; }

procedure TdxComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbRight then
  	PopupMenu.Popup
  else
	  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TdxComboBox.Change;
begin
  inherited Change;
  if (Field <> nil) and (Field.DataSet.State in [dsInsert, dsEdit]) then
	  DoNeedData;
end;

procedure TdxComboBox.DropDown;
begin
  inherited DropDown;
  if Field <> nil then
	  DoNeedData;
end;

constructor TdxComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  AutoComplete:=True;
  ControlStyle := ControlStyle - [csSetCaption];
  FFieldSize := 50; FOldSize := 50;
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.OnPopup:=@MenuPopup;
end;

procedure TdxComboBox.CutToClipboard;
begin
  Clipboard.AsText := SelText;
  SelText := '';
  Change;
end;

procedure TdxComboBox.CopyToClipboard;
begin
  Clipboard.AsText := SelText;
end;

procedure TdxComboBox.PasteFromClipboard;
var
  S: String;
begin
  S := Clipboard.AsText;
	SelText := S;
  SelStart := SelStart + Utf8Length(S);
  // Чтобы после выхода из компонента, данные попали в набор.
  Change;
end;

{procedure TdxComboBox.ClearData;
begin
  Text := '';
  Change;
end; }

{ TdxLookupComboBox }

procedure TdxLookupComboBox.DoButtonClick(Sender: TObject);
begin
  {$ifdef linux}
  FSkipKillFocus := True;
  {$endif}
  if (FOnButtonClick <> nil) and (not FHideButton) and (FButton.Enabled) then
    FOnButtonClick(Self);
end;

procedure TdxLookupComboBox.DoPositionButton;
begin
  if (FButton = nil) or (FDropDownButton = nil) then exit;
  FDropDownButton.Parent := Parent;
  FDropDownButton.Visible := Visible;
  FDropDownButton.AnchorToCompanion(akLeft,0,Self);

  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,FDropDownButton);
end;

procedure TdxLookupComboBox.FillGrid(DS: TDataSet; Empty: Boolean);
var
  r, i: Integer;
  FmFields: TList;

  procedure AddColumn(const Caption: String; W: Integer; IsCheckBox, ASearchable: Boolean);
  begin
    with FGrid.Columns.Add do
    begin
			Title.Caption := Caption;
      if W > 0 then
      begin
	      SizePriority := 0;
        Width := W;
      end;
      if IsCheckBox then ButtonStyle:=cbsCheckboxColumn;
      Searchable := ASearchable;
    end;
  end;

  procedure InitGrid;
  var
    Fm: TdxForm;
    LF: TLCbxListField;
    C: TComponent;
    i: Integer;
  begin
		FGrid.Columns.Clear;
    Fm := FormMan.FindForm(FSourceTId);
    C := FindById(Fm, SourceFId);
    AddColumn(GetFieldName(C), 0, False, True);
    FmFields.Add(C);
    for i := 0 to FListFields.Count - 1 do
    begin
      LF := FListFields[i];
      C := FindById(Fm, LF.FieldId);
      AddColumn(GetFieldName(C), LF.Width, C is TdxCheckBox, LF.Searchable);
      FmFields.Add(C);
    end;
    if FListFields.Count = 0 then
      FGrid.Options := FGrid.Options - [loTitles]
      //FGrid.FixedRows := 0
    else
      FGrid.Options := FGrid.Options + [loTitles]
      //FGrid.FixedRows := 1;
  end;

  // Преобразуем дату вида d.M.yy в dd.MM.yyyy. Порядок дня, месяца и года
  // может быть любой.
  function ShortDateToFullDateFormat: String;
  var
    Fmt, S, W: String;
    SL: TStringList;
    i: Integer;
    Sep: Char;
  begin
    Result := '';
    Fmt := DefaultFormatSettings.ShortDateFormat;
    Sep := DefaultFormatSettings.DateSeparator;
    SL := TStringList.Create;

    SplitStr(Fmt, Sep, SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      if S = '' then Continue;
      case S[1] of
        'd': W := 'dd';
        'M': W := 'MM';
        'y': W := 'yyyy';
        else Continue;
      end;
      Result := Result + W + Sep;
    end;
    SL.Free;

    Result := Copy(Result, 1, Length(Result) - 1);
  end;

  function ConvertFieldValue(Idx: Integer; Field: TField): String;
  var
    C: TComponent;
    FS: TFormatSettings;
  begin
    if Field.IsNull then Exit('');

    C := TComponent(FmFields[Idx]);
    if C is TdxCalcEdit then
    begin
      Result := FormatFloat(TdxCalcEdit(C).PrecStr, Field.AsFloat);
    end
    else if C is TdxDateEdit then
    begin
      FS := DefaultFormatSettings;
      FS.ShortDateFormat := ShortDateToFullDateFormat;
      Result := DateToStr(Field.AsDateTime, FS);
    end
    else if C is TdxTimeEdit then
    begin
      FS := DefaultFormatSettings;
      FS.LongTimeFormat:='hh:nn:ss';
      Result := TimeToStr(Field.AsDateTime, FS);
    end
    else
  		Result := Field.AsString;
  end;

begin
  FmFields := TList.Create;

  InitGrid;

  if Empty then
  begin
    FGrid.RowCount := FGrid.FixedRows;
    FGrid.RowCount := FGrid.RowCount + 1;
    FmFields.Free;
    Exit;
  end;

  DS.First;
  FGrid.BeginUpdate;
  FGrid.RowCount := 10000;
  FGrid.Row := 0;

  if FListFields.Count = 0 then r := 0
  else r := 1;

  while not DS.EOF do
  begin
    FGrid.RecId[r] := DS.Fields[0].AsInteger;
    //FGrid.Objects[0, r] := TObject(PtrInt(DS.Fields[0].AsInteger));
    FGrid.Cells[0, r] := ConvertFieldValue(0, DS.Fields[1]);  // DS.Fields[1].AsString;

    for i := 0 to FListFields.Count - 1 do
    	FGrid.Cells[i+1, r] := ConvertFieldValue(i+1, DS.Fields[i+2]); //DS.Fields[i+2].AsString;

    DS.Next;
    Inc(r);
    if r = FGrid.RowCount then FGrid.RowCount := FGrid.RowCount + 10000;

  end;
  if r = FGrid.FixedRows then Inc(r);
  FGrid.RowCount := r;
  FGrid.EndUpdate;

  FmFields.Free;
end;

function TdxLookupComboBox.GetKeyValue: Variant;
begin
  Result := DataSource.DataSet.FieldByName(FKeyField).Value;
end;

procedure TdxLookupComboBox.DoDropDownButtonClick(Sender: TObject);
begin
  if FChanging then Exit;

  if CanFocus then
  	SetFocus;
  if (not FDropDownBnClick) and not IsListVisible{ (FForm = nil)} then
  begin
    // Устанавливаем флаг KeepList, чтобы предотвратить AV, которое возникает
    // если сделать двойной клик по кнопке открытия списка в то время, когда
    // раскрыт список. В этом случае почему-то не срабатывает условие
    // FDropDownBn.MouseEntered хотя указатель мыши находится над кнопкой,
    // и вызывается CloseListForm в процессе открытия формы.
    // (Баг от Kiss от 22.02.2018)
    FKeepList := True;
    {$ifdef linux}
    FSkipKillFocus := True;
    {$endif}
    ShowListForm;
    FKeepList := False;
  end
  else
  begin
    if IsListVisible then CloseListForm;
  	FDropDownBnClick := False;
  end;
end;

procedure TdxLookupComboBox.DropDownButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then FDropDownBnClick:=False;
end;

function TdxLookupComboBox.GetDroppedDown: Boolean;
begin
  Result := IsListVisible;//FForm <> nil;
end;

function TdxLookupComboBox.GetGrid: TDropDownList;
begin
  if HideList then Exit(nil);

  CreateListForm;
  Result := FGrid;
end;

function TdxLookupComboBox.GetSourceFieldName: String;
begin
  Result := _GetSourceFieldName(Self);
end;

function TdxLookupComboBox.GetSourceFormName: String;
begin
  Result := _GetSourceFormName(Self);
end;

procedure TdxLookupComboBox.KeyTimerTimer(Sender: TObject);
begin
  FKeyTimer.Enabled := False;
  if CanFocus then SetFocus;
  FKeepList := False;
end;

procedure TdxLookupComboBox.MenuClick(Sender: TObject);
var
  i: PtrInt;
begin
  // В семерке при добавлении/изменении список не скрывается
  if IsListVisible then CloseListForm;
  i := TComponent(Sender).Tag;
  case i of
  	0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
    4: ClearData;
  end;

  if i in [0, 2] then FFiltering := True;

  // Остальные пункты обрабатываются в DSProc
  if FOnMenuClick <> nil then FOnMenuClick(Sender);
end;

procedure TdxLookupComboBox.MenuPopup(Sender: TObject);
begin
  if CanFocus then SetFocus;
  SetControlState;
end;

procedure TdxLookupComboBox.ReadInsertValues(Reader: TReader);
var
  S: String;
  i, p, SrcId, DestId: Integer;
  Vl: TInsertValueData;
  SL: TStringList;
begin
  S := Reader.ReadString;
  SL := TStringList.Create;
  SplitStr(S, '|', SL);
	for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    TryStrToInt( Copy(S, 1, p - 1), SrcId );
    TryStrToInt( Copy(S, p + 1, 255), DestId );
    Vl := FInsertedValues.AddValue;
    Vl.SrcField := SrcId;
    Vl.DestField := DestId;
  end;
  SL.Free;
end;

procedure TdxLookupComboBox.SetEditable(AValue: Boolean);
begin
  if FEditable=AValue then Exit;
  FEditable:=AValue;
  SetButtonState;
end;

procedure TdxLookupComboBox.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxLookupComboBox.SetFieldsTables(AValue: TStrings);
begin
  FieldsTables.Assign(AValue);
end;

procedure TdxLookupComboBox.SetHideButton(AValue: Boolean);
begin
  if FHideButton=AValue then Exit;
  FHideButton:=AValue;
  SetButtonState;
end;

procedure TdxLookupComboBox.SetHideList(AValue: Boolean);
begin
  if FHideList = AValue then Exit;
  FHideList := AValue;
  SetButtonState;
end;

procedure TdxLookupComboBox.SetKeyValue(AValue: Variant);
begin
  if Field.DataSet.State in [dsInsert, dsEdit] then
  begin
    Field.DataSet.FieldByName(FKeyField).Value := AValue;
  end;
end;

procedure TdxLookupComboBox.SetListFields(AValue: TLCbxListFields);
begin
  FListFields.Assign(AValue);
end;

procedure TdxLookupComboBox.WriteInsertValues(Writer: TWriter);
var
  i: Integer;
  S: String;
  Vl: TInsertValueData;
begin
  S := '';
  for i := 0 to FInsertedValues.Count - 1 do
  begin
    Vl := FInsertedValues[i];
    S := S + IntToStr(Vl.SrcField) + ';' + IntToStr(Vl.DestField);
    if i < FInsertedValues.Count - 1 then S := S + '|';
  end;
  Writer.WriteString(S);
end;

procedure TdxLookupComboBox.CreateListForm;
begin
  if FForm = nil then
  begin
  	FForm := TdxLCbxListForm.CreateNew(Self);
    FForm.Grid := FGrid;
    FGrid.Parent := FForm;
    FForm.Control := Self;
  end;
end;

procedure TdxLookupComboBox.ShowListForm;
var
  S: String;
begin
  if FHideList then Exit;

  if FFiltering then S := Text
  else S := '';

	CreateListForm;
  {$ifdef windows}
  FForm.PopupParent := TCustomForm(Self.GetTopParent);
  {$endif}

  if FOnNeedData <> nil then FOnNeedData(Self, S);
  //if FGrid.RowCount = FGrid.FixedRows then Exit;

  FForm.Enabled := True;
  FForm.ShowForm;
end;

procedure TdxLookupComboBox.CloseListForm;
begin
  FForm.Close;
end;

function TdxLookupComboBox.GetButtonWidths: Integer;
begin
  Result := 0;
  if not FHideList then Result := Result + FDropDownButton.Width;
  if not FHideButton then Result := Result + FButton.Width;
end;

procedure TdxLookupComboBox.ApplyChanges;
begin
  ProcessKillFocus;
end;

procedure TdxLookupComboBox.SetButtonState;
begin
  FDropDownButton.Visible := Visible and (not FHideList);
  FDropDownButton.Enabled := (not ReadOnly) and Enabled;
  FButton.Visible := Visible and (not FHideButton);
  FButton.Enabled := (not ReadOnly) and Enabled;
end;

procedure TdxLookupComboBox.ProcessKillFocus;
begin
  if Field = nil then Exit;

  {$ifdef windows}
  if FKeepList then Exit;

  if (not FKeyDown) and (FDropDownButton.MouseInClient or
    ((IsListVisible) and FForm.IsMouseEntered)) then Exit
  else if (Text = '') and (KeyValue <> Null) then ClearData
  else
  begin
  	if (FFiltering or FChanging or (Trim(Text) = '')) and (FOnKeyMatch <> nil) then FOnKeyMatch(Self);
  end;

  FFiltering := False;
  FChanging := False;
	if IsListVisible then CloseListForm;

  // KeyUp может и не сработать
  FKeyDown := False;

  {$else}
  if FSkipKillFocus then
  begin
    FSkipKillFocus := False;
    Exit;
  end;
  if (Text = '') and (KeyValue <> Null) then ClearData
  else if (FFiltering or FChanging) and (FOnKeyMatch <> nil) then FOnKeyMatch(Self);
  {$endif}
end;

function TdxLookupComboBox.IsListVisible: Boolean;
begin
  Result := (FForm <> nil) and FForm.Visible;
end;

procedure TdxLookupComboBox.SetControlState;
var
  ae, hbns, src: Boolean;
  Pop: TPopupMenu;
begin
  if (PopupMenu <> FPopup) and
    ((DropDownButton.PopupMenu <> FPopup) or HideList) and
    ((Button.PopupMenu <> FPopup) or HideButton) then Exit;

  ae := DataSource.DataSet.State in [dsInsert, dsEdit];
  src := (SourceTId > 0) and (SourceFId > 0);
  hbns := FHideList and FHideButton;
  Pop := FPopup;
  Pop.Items[0].Enabled := src and (SelText<>'') and (not ReadOnly) and (not FHideList) and ae;
  Pop.Items[1].Enabled := src and (SelText <> '');
  Pop.Items[2].Enabled := src and (not ReadOnly) and (not FHideList) and ae;
  Pop.Items[4].Enabled := src and (Text <> '') and (not ReadOnly) and ae and (not hbns);
  Pop.Items[6].Enabled := src and ae and (not ReadOnly) and (not hbns) and Pop.Items[6].Visible;
  Pop.Items[7].Enabled := src and ae and (KeyValue <> Null) and Pop.Items[7].Visible;
  Pop.Items[9].Enabled := src and (KeyValue <> Null);
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
  begin
    if FOnCtrlClick <> nil then FOnCtrlClick(Self);
  end
  {$ifdef windows}
  {else if Button = mbRight then
  begin
    if PopupMenu <> nil then PopupMenu.Popup;
  end;  }
  {$endif}
end;

procedure TdxLookupComboBox.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  DoPositionButton;
  SetButtonState;
end;

procedure TdxLookupComboBox.Loaded;
begin
  inherited Loaded;
  DoPositionButton;
  SetButtonState;
end;

procedure TdxLookupComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  //if (AComponent = FButton) and (Operation = opRemove) then
  //  FButton := nil;
end;

procedure TdxLookupComboBox.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  SetButtonState;
end;

{procedure TdxLookupComboBox.UpdateData(Sender: TObject);
begin
  if (not ReadOnly) and (Field.DataSet.State in [dsEdit, dsInsert]) then
    inherited UpdateData(Sender)
  else Text := Field.Text;
end;  }

procedure TdxLookupComboBox.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  SetButtonState;
end;

procedure TdxLookupComboBox.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  SetButtonState;
end;

{procedure TdxLookupComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  inherited UTF8KeyPress(UTF8Key);

  //FChanging := True;

  if FOnMyUtf8KeyPress <> nil then
  begin
    S := Utf8Key;
    FOnMyUtf8KeyPress(Self, S);
    Utf8Key := S;
  end;
end;    }

procedure TdxLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  res: Boolean;
begin
  {$ifdef windows}
  res := (SourceTId > 0) and (SourceFId > 0) and CheckCutPasteKeys(Self, Field, Key, Shift);

  if res then
  begin
    if FChanging then Exit;

    if FHideList and (not (Key in [VK_LEFT, VK_RIGHT, VK_TAB, VK_INSERT, VK_RETURN, VK_ESCAPE])) then
    begin
      if (Key in [VK_C, VK_SPACE]) and (Shift = [ssCtrl]) then
      else Key := 0
    end
    else if Key = VK_ESCAPE then
	  begin
      if IsListVisible then
      begin
        Key := 0;
	 		  CloseListForm
      end
      else
      begin
        FKeyDown := True;
        ProcessKillFocus;
        SelStart := 0;
        SelLength := Utf8Length(Text);
      end;
	  end
    else if Key = VK_DOWN then
    begin
      Key := 0;
      FKeepList := True;
      if not IsListVisible then
	  	  ShowListForm
      else if FForm.CanFocus then
	      FForm.SetFocus;
      FKeepList := False;
    end
    else if (Key = VK_RETURN) and (Shift = [ssShift]) and (not FHideButton) then
    begin
      Key := 0;
      FButton.Click;
    end
    else
	    FKeyDown := True;
  end;
  if IsListVisible {FForm <> nil} then FForm.Enabled := not FKeyDown;
  {$else}
  if (Key = VK_RETURN) and (Shift = [ssShift]) and not FHideButton then
  begin
    Key := 0;
    FSkipKillFocus := True;
    FButton.Click;
  end
  else if Key = VK_DOWN then
  begin
    Key := 0;
		FSkipKillFocus := True;
    ShowListForm;
  end
  else FKeyDown := True;
  {$endif}
  inherited KeyDown(Key, Shift);
end;

procedure TdxLookupComboBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  if (SourceTId = 0) or (SourceFId = 0) or
  	(not CheckCutPasteKeys(Self, Field, Key, Shift)) then Exit;

  {$ifdef windows}
  if Key = VK_RETURN then
  begin
    if IsListVisible {FForm <> nil} then
    begin
      Key := 0;
      FForm.AcceptSelected
    end
    else
    begin
      if FFiltering then Key := 0;
      ProcessKillFocus;
      if Key = 0 then
      begin
        SelStart := 0;
        SelLength := Utf8Length(Text);
      end;
    end;
  end
  else if FChanging then
  begin

    FChanging := False;

    if Trim(Text) <> '' then
    begin
	    FFiltering := True;
  	  FKeepList := True;
      ShowListForm;
      if not AppConfig.IsWine then
      begin
    	  if CanFocus then SetFocus;
   		  FKeepList := False;
      end
      else FKeyTimer.Enabled := True;
    end
    else
    begin
      FFiltering := False;
      if IsListVisible then CloseListForm;
    end;

  end;

  FKeyDown := False;
  //if FForm <> nil then FForm.Enabled := (not FKeyDown) and (FGrid.RowCount > FGrid.FixedRows);
  if (IsListVisible{FForm <> nil}) and (FGrid.RowCount = FGrid.FixedRows) then CloseListForm;
  if {FForm <> nil} IsListVisible then FForm.Enabled := True;
  {$else}
  if Key = VK_RETURN then ProcessKillFocus
  else if FChanging then
  begin

    FChanging := False;

    if Trim(Text) <> '' then
	    FFiltering := True
    else
      FFiltering := False;

  end;

  FKeyDown := False;
  {$endif}
end;

procedure TdxLookupComboBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('InsertedValues', @ReadInsertValues, @WriteInsertValues, FInsertedValues.Count > 0);
end;

procedure TdxLookupComboBox.Change;
begin
  inherited Change;
  if FKeyDown then
    FChanging := True;
  SetControlState;
end;

procedure TdxLookupComboBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FGrid.Font := Font;
  FGrid.Font.Color := clWindowText;
end;

procedure TdxLookupComboBox.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);

  if (Button = nil) or (DropDownButton = nil) then Exit;
  Button.Width := Height;
  Button.Height := Height;
  if AHeight > 52 then
  begin
    Button.Images := Images48;
    Button.ImageIndex := IMG48_FORM;
  end
  else if AHeight > 36 then
  begin
    Button.Images := Images32;
    Button.ImageIndex := IMG32_FORM;
  end
  else if AHeight > 28 then
  begin
    Button.Images := Images24;
    Button.ImageIndex := IMG24_FORM;
  end
  else
  begin
    Button.Images := Images16;
    Button.ImageIndex := IMG16_FORM;
  end;
  {if Height > 52 then S := 'form48'
  else if Height > 36 then S := 'form32'
  else if Height > 28 then S := 'form24'
  else S := 'form16';
  Button.LoadGlyphFromLazarusResource(S);  }

	DropDownButton.Height := Height;
end;

procedure TdxLookupComboBox.DoEnter;
begin
  inherited DoEnter;
  if not (csDesigning in ComponentState) then
    SetControlState;
end;

procedure TdxLookupComboBox.WMSetFocus(var Message: TLMSetFocus);
begin
  // KeyUp может и не сработать
  FKeyDown := False;
  {$ifdef linux}
  if IsListVisible then CloseListForm;
  {$endif}
  inherited WMSetFocus(Message);
end;

procedure TdxLookupComboBox.WMKillFocus(var Message: TLMKillFocus);
begin
	ProcessKillFocus;
  inherited WMKillFocus(Message);
end;

function TdxLookupComboBox.GetDrawText: String;
begin
  Result:=FieldName;
end;

procedure TdxLookupComboBox.ClearData;
begin
  if Field.DataSet.State in [dsInsert, dsEdit] then
  begin
    Field.SetData(nil);
    KeyValue:=Null;
  end;
end;

procedure TdxLookupComboBox.ClearInsertTableProps;
begin
  SourceTable:=0;
  DestTable:=0;
  FillFilter:='';
  FieldsTables.Clear;
  ClearTableBeforeFill:=False;
  PromptFillTable:=False;
end;

constructor TdxLookupComboBox.Create(AOwner: TComponent);
var
  Pop: TPopupMenu;
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  FStopTab := True;

  FDropDownButton := TSpeedButton.Create(Self);
  FDropDownButton.Width := 18;//Self.Height;
  FDropDownButton.Height := 23;//Self.Height;
  //FDropDownButton.FreeNotification(Self);
  FDropDownButton.OnClick:=@DoDropDownButtonClick;
  FDropDownButton.Cursor := crArrow;
  FDropDownButton.ControlStyle := FDropDownButton.ControlStyle + [csNoDesignSelectable, csNoDesignVisible];
  FDropDownButton.Images := Images16;
  FDropDownButton.ImageIndex := IMG16_DROPDOWN;
  //FDropDownButton.LoadGlyphFromLazarusResource('cbx_dropdown16');
  FDropDownButton.Flat := True;
  FDropDownButton.OnMouseDown:=@DropDownButtonMouseDown;

  FButton := TSpeedButton.Create(Self);
  FButton.Width := 23;//Self.Height;
  FButton.Height := 23;//Self.Height;
  FButton.OnClick:=@DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable, csNoDesignVisible];
  //FButton.LoadGlyphFromLazarusResource('form16');
  FButton.Images := Images16;
  FButton.ImageIndex := IMG16_FORM;
  FButton.Flat := True;
  FButton.ShowHint:=True;
  FButton.Hint:=rsObjectButtonHint;

  FGrid := TDropDownList.Create(Self);
  with FGrid do
  begin
    Align := alClient;
    FixedRows := 0;
    FixedCols := 0;
    RowCount := 1;
    ColCount := 1;
    DefaultRowHeight := -1;
  end;

  FDropDownCount := 8;
  FFieldsTables := TStringList.Create;
  FInsertedValues := TInsertedValues.Create;
  FListFields := TLCbxListFields.Create;

  Pop := TPopupMenu.Create(Self);
  Pop.Images := Images16;
  Pop.Items.Add( CreateMenuItem(Pop, rsCut, 0, ShortCut(VK_X, [ssCtrl]),
    @MenuClick, IMG16_CUT) );
  Pop.Items.Add( CreateMenuItem(Pop, rsCopy, 1, ShortCut(VK_C, [ssCtrl]),
    @MenuClick, IMG16_COPY) );
  Pop.Items.Add( CreateMenuItem(Pop, rsPaste, 2, ShortCut(VK_V, [ssCtrl]),
    @MenuClick, IMG16_PASTE) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 3, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsClear, 4, 0,
    @MenuClick, IMG16_DELETE) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 5, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsAppend, 6, ShortCut(VK_INSERT, []), @MenuClick, IMG16_ADD) );
  Pop.Items.Add( CreateMenuItem(Pop, rsEdit, 7, ShortCut(VK_SPACE, [ssCtrl]), @MenuClick, IMG16_EDIT) );
  Pop.Items.Add( CreateMenuItem(Pop, '-', 8, 0, nil) );
  Pop.Items.Add( CreateMenuItem(Pop, rsGoto, 9, 0, @MenuClick, IMG16_GOTO) );
  Pop.OnPopup:=@MenuPopup;
  PopupMenu := Pop;
  Button.PopupMenu := Pop;
  DropDownButton.PopupMenu := Pop;
  FPopup := Pop;
  if AppConfig.IsWine then
  begin
    FKeyTimer := TTimer.Create(Self);
    FKeyTimer.Enabled := False;
    FKeyTimer.Interval := 100;
    FKeyTimer.OnTimer:=@KeyTimerTimer;
  end;
end;

destructor TdxLookupComboBox.Destroy;
begin
  FListFields.Free;
  FInsertedValues.Free;
  FFieldsTables.Free;
  //FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TdxLookupComboBox.EnableButtons(AValue: Boolean);
begin
  if Enabled and (not ReadOnly) then
  begin
    FButton.Enabled := AValue;
    FDropDownButton.Enabled := AValue;
  end;
end;

{ TdxEdit }

procedure TdxEdit.MenuPopup(Sender: TObject);
var
  S: String;
  ae: Boolean;
begin
  if CanFocus then SetFocus;
  ae := Field.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled:=(SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled:=SelText <> '';
  PopupMenu.Items[2].Enabled:=(Clipboard.AsText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled:=(Text <> '') and (not ReadOnly) and ae;
  S := UTF8LowerCase(Text);
  PopupMenu.Items[5].Visible := IsUrl(S) or IsMail(S);
  PopupMenu.Items[6].Visible := PopupMenu.Items[5].Visible;
end;

procedure TdxEdit.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
end;

procedure TdxEdit.MenuClick(Sender: TObject);
var
  S: String;
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);// Text := '';
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
    S := {Utf8LowerCase}(Trim(Text));
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
  // Del, Backspace почему-то срабатывают в поле с маской
  if not CheckCutPasteKeys(Self, Field, Key, Shift) and (Key in [VK_BACK, VK_DELETE]) then Key := 0;
  inherited KeyDown(Key, Shift);
end;

function TdxEdit.GetDrawText: String;
begin
  Result := FieldName;
end;

{procedure TdxEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: String;
begin
  inherited UTF8KeyPress(UTF8Key);
  if FOnMyUtf8KeyPress <> nil then
  begin
    S := UTF8Key;
    FOnMyUtf8KeyPress(Self, S);
    UTF8Key := S;
  end;
end;   }

constructor TdxEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  CustomEditMask := True;
  Width := ScaleToScreen(100);
  FFieldSize := 50; FOldSize := 50;
  FStopTab := True;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 5, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsGotoURL, 6, 0, @MenuClick, IMG16_GOTO) );
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
  {if Field=nil then Exit;

  // Маска может содержать литералы. Если текст содержит только литералы,
  // без введенных данных, то очищает поле совсем.
  if (EditMask <> '') and MaskedTextEmpty(Text, EditMask) then
    if (Field.DataSet.State in [dsInsert, dsEdit]) and (Field.Value <> Null) then
      Field.SetData(nil);  }
  inherited EditingDone;
end;

{ TdxMemo }

procedure TdxMemo.DoButtonClick(Sender: TObject);
begin
  if CanFocus then SetFocus;
  if FOnButtonClick <> nil then FOnButtonClick(Self);
end;

procedure TdxMemo.MenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: CutToClipBoard;
    1: CopyToClipboard;
    2: PasteFromClipBoard;
    4: Field.SetData(nil);//Text := '';
  end;
end;

procedure TdxMemo.MenuPopup(Sender: TObject);
var
  ae: Boolean;
begin
  if CanFocus then SetFocus;
  ae := Field.DataSet.State in [dsInsert, dsEdit];
  PopupMenu.Items[0].Enabled := (SelText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[1].Enabled := SelText <> '';
  PopupMenu.Items[2].Enabled := (Clipboard.AsText <> '') and (not ReadOnly) and ae;
  PopupMenu.Items[4].Enabled := (Text <> '') and (not ReadOnly) and ae;
end;

procedure TdxMemo.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible and (FSourceFId > 0);
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

function TdxMemo.GetSourceFieldName: String;
begin
  Result := _GetSourceFieldName(Self);
end;

function TdxMemo.GetSourceFormName: String;
begin
  Result := _GetSourceFormName(Self);
end;

procedure TdxMemo.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  Invalidate;
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
  FButton.Enabled:=Value and (not ReadOnly);
end;

procedure TdxMemo.SetReadOnly(AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  FButton.Enabled:=Enabled and (not ReadOnly);
end;

{procedure TdxMemo.UTF8KeyPress(var UTF8Key: TUTF8Char);
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
end;   }

procedure TdxMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckCutPasteKeys(Self, Field, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TdxMemo.WMPaint(var Msg: TLMPaint);
begin
  if csDesigning in ComponentState then
  begin
    Include(FControlState, csCustomPaint);
    inherited WMPaint(Msg);
    Exclude(FControlState, csCustomPaint);
  end
  else
    inherited WMPaint(Msg);
end;

procedure TdxMemo.PaintWindow(DC: HDC);
var
  S: String;
  R: TRect;
begin
  inherited PaintWindow(DC);
  if csDesigning in ComponentState then
  begin
    S := GetDrawText;
    if S <> '' then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
      SelectObject(DC, Font.Handle);
      SetBkMode(DC, TRANSPARENT);
      SetTextColor(DC, Font.Color);
      DrawText(DC, PChar(S), -1, R, DT_WORDBREAK);
    end;
  end;
end;

function TdxMemo.GetDrawText: String;
begin
  Result := FieldName;
end;

constructor TdxMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(200);
  Height := ScaleToScreen(100);
  ControlStyle := ControlStyle - [csSetCaption];
  ScrollBars:=ssBoth;
  FFieldSize := 300; FOldSize := 300;
  FStopTab := True;

  FButton := TSpeedButton.Create(Self);
  FButton.Width := 23;
  FButton.Height := Height;
  FButton.FreeNotification(Self);
  FButton.OnClick:=@DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable, csNoDesignVisible];
  FButton.Images := Images16;
  FButton.ImageIndex := IMG16_ADD;
  //FButton.LoadGlyphFromLazarusResource('add16');
  FButton.Flat := True;
  FButton.Visible := False;

  FDelimiter := ', ';
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Images16;
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCut, 0, ShortCut(VK_X, [ssCtrl]), @MenuClick, IMG16_CUT) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsCopy, 1, ShortCut(VK_C, [ssCtrl]), @MenuClick, IMG16_COPY) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsPaste, 2, ShortCut(VK_V, [ssCtrl]), @MenuClick, IMG16_PASTE) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, '-', 3, 0, nil) );
  PopupMenu.Items.Add( CreateMenuItem(PopupMenu, rsClear, 4, 0, @MenuClick, IMG16_DELETE) );
  PopupMenu.OnPopup:=@MenuPopup;
  FButton.PopupMenu := PopupMenu;
end;

destructor TdxMemo.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TdxMemo.EnableButton(AValue: Boolean);
begin
  if Enabled and (not ReadOnly) then
  	Button.Enabled:=AValue;
end;

procedure TdxMemo.Change;
begin
  inherited Change;
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
  // Без этих строчек при загрузке формы из базы компоненты с якорями могут
  // иметь неправильный размер.
  {$ifdef windows}
  Result.Height := PageControl.Height;
  Result.Width := PageControl.Width;
  {$endif}
end;

constructor TdxTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := rsTabSheet;
  FStopTab := True;
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

procedure TdxPageControl.BoundsChanged;
var
  i: Integer;
begin
  inherited BoundsChanged;
  if csDesigning in ComponentState then
    for i := 0 to PageCount - 1 do
      if i <> ActivePageIndex then
        TdxTabSheet(Pages[i]).FNeedUpdate:=True;
end;

procedure TdxPageControl.Change;
begin
  inherited Change;
  if csDesigning in ComponentState then
    if ActivePage <> nil then
      TdxTabSheet(ActivePage).FNeedUpdate:=False;
end;

constructor TdxPageControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := ScaleToScreen(300);
  Height := ScaleToScreen(150);
  Options := [nboDoChangeOnSetIndex];
  FStopTab := True;
end;

procedure TdxPageControl.SetActiveFirstVisiblePage;
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
  	if Pages[i].TabVisible then
    begin
      ActivePageIndex := i;
      Exit;
    end;
  ActivePage := nil;
end;

procedure TdxPageControl.UpdateAnchoredControls;

  function AnchorsExists(WC: TWinControl): Boolean;
  var
    i: Integer;
    C: TControl;
  begin
    Result := False;
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      if (C.Owner = Owner) and (C.Anchors * [akRight, akBottom] <> []) then Exit(True);
      if (C is TWinControl) and AnchorsExists(TWinControl(C)) then Exit(True)
    end;
  end;

var
  i, idx: Integer;
  T: TdxTabSheet;
begin
  idx := ActivePageIndex;
  for i := 0 to PageCount - 1 do
  begin
    if i = idx then Continue;
    T := TdxTabSheet(Pages[i]);
    if T.FNeedUpdate then
    begin
      T.FNeedUpdate:=False;
      if AnchorsExists(T) then
        ActivePageIndex := i;
    end;
  end;
  ActivePageIndex := idx;
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

function TdxGroupBox.GetClientRect: TRect;
begin
  Result:=inherited GetClientRect;
  // Без этих строчек при загрузке формы из базы компоненты с якорями могут
  // иметь неправильный размер.
  {$ifdef windows}
  Result.Height :=Height;
  Result.Width := Width;
  {$endif}
end;

constructor TdxGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := ScaleToScreen(300);
  Height := ScaleToScreen(150);
  ControlStyle := ControlStyle - [csSetCaption];
  ParentBackground := False;
  FStopTab := True;
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
  TabStop := True;
  FStopTab := True;
end;

procedure TdxCheckBox.Init;
begin
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

function TdxForm.GetRecordCaption: String;
begin
  Result := FRecordCaption;
  if Result = '' then Result := FRecordsCaption;
  if Result = '' then Result := FFormCaption;
end;

function TdxForm.GetField(Index: String): TField;
var
  C: TComponent;
begin
  C := FindComponentByFldName(Index);
  Result := GetComponentField(FDataSet, C);
end;

function TdxForm.GetFiles(Index: String): TdxFile;
var
  C: TComponent;
begin
  RequeryIfNeed;
  C := FindComponentByFieldName(Self, Index);
  if C is TdxFile then Result := TdxFile(C)
  else raise Exception.CreateFmt(rsFileFieldNotFound, [Index]);
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
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  with TDataSetProcessor(FDSP) do
    Result := DataSets[Index + 1]^.Form;
end;

function TdxForm.GetFormCount: Integer;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Result := TDataSetProcessor(FDSP).DataSetCount - 1
end;

function TdxForm.GetForms(Index: String): TdxForm;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Result := FindForm(Index);
  if Result = nil then
    raise Exception.CreateFmt(rsFormNotFound, [Index]);
end;

function TdxForm.GetFormGrid: TdxGrid;
begin
  Result := PDataSetRec(FDSR)^.Grid;
end;

function TdxForm.GetImages(Index: String): TdxDBImage;
var
  C: TComponent;
begin
  RequeryIfNeed;
  C := FindComponentByFieldName(Self, Index);
  if C is TdxDBImage then Result := TdxDBImage(C)
  else raise Exception.CreateFmt(rsImageFieldNotFound, [Index]);
end;

function TdxForm.GetModified: Boolean;
begin
  RequeryIfNeed;
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
  RequeryIfNeed;
  Result := FDataSet.FieldByName(FieldStr(C)).OldValue;
end;

function TdxForm.GetRecordsCaption: String;
begin
  Result := FRecordsCaption;
  if Result = '' then Result := FFormCaption;
end;

function TdxForm.IsBinded: Boolean;
begin
  Result := FDSP <> nil;
end;

function TdxForm.GetParentForm: TdxForm;
begin
  Result := nil;
  if FPId > 0 then
	  Result := TDataSetProcessor(FDSP).Form;
end;

function TdxForm.GetQueries(Index: String): TObject;
begin
  Result := FindQuery(Index);
  if Result = nil then
    raise Exception.CreateFmt(rsQueryNotFound, [Index]);
end;

function TdxForm.GetQueryByIndex(Index: Integer): TObject;
var
  i: Integer;
begin
  if (Index < 0) or (Index >= QueryCount) then
    raise EListError.CreateFmt('List index (%d) out of bounds', [Index]);

  with TDataSetProcessor(FDSP) do
    for i := 0 to QueryCount - 1 do
    begin
      if Queries[i]^.DSRi = Self.DSRi then Dec(Index);
      if Index < 0 then Exit(Queries[i]^.Grid);
    end;
//  Result := Queries[Index]^.Grid;
end;

function TdxForm.GetQueryCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  with TDataSetProcessor(FDSP) do
    for i := 0 to QueryCount - 1 do
      if Queries[i]^.DSRi = Self.DSRi then
        Inc(Result);
//  Result := QueryCount;
end;

function TdxForm.GetState: TDataSetState;
begin
  RequeryIfNeed;
  Result := FDataSet.State;
end;

procedure TdxForm.SetTree(AValue: TdxFormTree);
begin
  FreeAndNil(FTree);
  FTree:=AValue;
  FTree.Form := Self;
end;

procedure TdxForm.RequeryIfNeed;
begin
  if FDSRi > 0 then
    with PDataSetRec(FDSR)^ do
      if NeedRefresh then TDataSetProcessor(FDSP).RequeryDetail(FDSRi);
end;

procedure TdxForm.SetTabStops;
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    C := Components[i];
    if HasProp(C, 'StopTab') then
    begin
      if TWinControl(C).TabStop then
        TWinControl(C).TabStop := GetStopTab(C)
      else
        SetStopTab(C, TWinControl(C).TabStop);
    end;
  end;
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
  RequeryIfNeed;
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
  RequeryIfNeed;
  if C is TdxLookupComboBox then
  begin
    if aValue <> Null then
      FDataSet.FieldByName(FieldStr(C) + 'l').Value := GetObjFieldValue(C, AValue, True)
    else
      FDataSet.FieldByName(FieldStr(C) + 'l').Value := Null;
      //TdxLookupComboBox(C).ClearData;
  end;
  FDataSet.FieldByName(FieldStr(C)).Value := AValue;
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

procedure TdxForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
  Proc(FGrid);
  Proc(FTree);
end;

procedure TdxForm.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Shopping', @ReadShopData, @WriteShopData, FShopData.ObjId > 0);
end;

procedure TdxForm.RemoveGridTree;
begin
  RemoveComponent(FGrid);
  RemoveControl(FGrid);
  FGrid.Parent := nil;

  RemoveComponent(FTree);
  RemoveControl(FTree);
  FTree.Parent := nil;
end;

procedure TdxForm.UpdateTree;
begin
  if FTree.Visible then FTree.UpdateTree;
end;

constructor TdxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];
  ParentColor := False;
  ParentFont := False;
  ParentBackground := False;

  FGrid := TdxGrid.Create(nil);
  FGrid.Name:='Grid';
  FGrid.Font.Name := 'Verdana';
  FGrid.Font.Size := 10;
  FGrid.ReadOnly:=True;
  FGrid.VisibleButtons:=[];
  FGrid.AllowChangeSort := True;

  FTree := TdxFormTree.Create(nil);
  FTree.Form := Self;
  FTree.Name := 'Tree';
  FTree.Font.Name := 'Verdana';
  FTree.Font.Size := 10;
  FTree.ReadOnly:=True;

  //FGrid.Options := FGrid.Options - [dgTabs];
  FTemplates := TStringList.Create;
  FCalcFields := TStringListUtf8.Create;
  FFilters := TStringList.Create;
  FColoring := TStringList.Create;
  Font.Name:='Verdana';
  Font.Size := 10;
  FAutoOpen:=True;
  Width := ScaleToScreen(350);
  Height := ScaleToScreen(300);
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
  FActionResult := Null;
  FLevelCount := 5;
  FLockMode := lmPessimistic;
  FUseSelCond := True;
end;

destructor TdxForm.Destroy;
begin
  //FreeAndNil(FGridBmp);
  FParams.Free;
  FreeAndNil(FExpr);
  FTreeFont.Free;
  FHelpText.Free;
  FShopData.Free;
  FColoring.Free;
  FFilters.Free;
  FCalcFields.Free;
  FTemplates.Free;
  FreeAndNil(FGrid);
  FreeAndNil(FTree);
  inherited Destroy;
end;

procedure TdxForm.DoPrintEvent(Step: TPrintActionType; const SourceName,
  FieldName: String; var Value: String; var Accept: Boolean);
begin
  if FOnPrint <> nil then
  begin
  	FOnPrint(Self, Step, SourceName, FieldName,
    	Value, Accept);
  end;
end;

function TdxForm.HasShop: Boolean;
begin
  Result := FShopData.ObjId > 0;
end;

function TdxForm.FindForm(const FormName: String): TdxForm;
var
  i: Integer;
  Fm: TdxForm;
begin
  Result := nil;
  with TDataSetProcessor(FDSP) do
    for i := 1 to DataSetCount - 1 do
    begin
      Fm := DataSets[i]^.Form;
      if MyUtf8CompareText(Fm.FormCaption, FormName) = 0 then Exit(Fm);
    end;
end;

function TdxForm.FindQuery(const QueryName: String): TObject;
var
  i: Integer;
  Q: TQueryRec;
begin
  Result := nil;
  with TDataSetProcessor(FDSP) do
    for i := 0 to QueryCount - 1 do
    begin
      Q := Queries[i]^;
    	if (Q.DSRi = FDSRi) and (MyUtf8CompareText(QueryName, Q.RD.Name) = 0) then Exit(Q.Grid);
    end;
end;

function TdxForm.IsHide: Boolean;
begin
  Result := (FViewType = vtGridOnly) and not PDataSetRec(FDSR)^.EditFm.Visible;
end;

function TdxForm.HasFilterValues: Boolean;
var
  i: Integer;
begin
  Result := Filter.ValuesExists;
  if not Result then
    for i := 0 to FormCount - 1 do
      if FormByIndex[i].Filter.ValuesExists then Exit(True);
end;

function TdxForm.Append: TAccessStatus;
begin
  if (ParentForm <> nil) and not (ParentForm.State in [dsInsert, dsEdit]) then
  	Exit(asCantAppend);
  RequeryIfNeed;
  with PDataSetRec(FDSR)^ do
  begin
    Grid.EditorMode:=False;
    DataSet.Append;
    Result := asOk;
  end
end;

function TdxForm.Insert: TAccessStatus;
begin
  if (ParentForm <> nil) and not (ParentForm.State in [dsInsert, dsEdit]) then
  	Exit(asCantAppend);
  RequeryIfNeed;
  with PDataSetRec(FDSR)^ do
  begin
    Grid.EditorMode:=False;
    DataSet.Insert;
    Result := asOk;
  end
end;

function TdxForm.Edit: TAccessStatus;
begin
  RequeryIfNeed;
  with TDataSetProcessor(FDSP) do
  begin
  	Result := InnerEdit(FDSRi, False, True, True);
  end
end;

function TdxForm.Delete: TAccessStatus;
begin
  RequeryIfNeed;
  with TDataSetProcessor(FDSP) do
  begin
  	Result := InnerDelete(FDSRi, False, True, True);
  end
end;

procedure TdxForm.Post;
begin
  RequeryIfNeed;
  if FPId = 0 then
    TDataSetProcessor(FDSP).Post
  else
    FDataSet.Post;
end;

procedure TdxForm.Cancel;
begin
  RequeryIfNeed;
  FDataSet.Cancel;
end;

procedure TdxForm.Refresh;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  TDataSetProcessor(FDSP).Refresh;
end;

procedure TdxForm.MoveFirst;
begin
  RequeryIfNeed;
  if FPId = 0 then
    TDataSetProcessor(FDSP).First
  else
    FDataSet.First;
end;

procedure TdxForm.MovePrior;
begin
  RequeryIfNeed;
  if FPId = 0 then
    TDataSetProcessor(FDSP).Prior
  else
    FDataSet.Prior;
end;

procedure TdxForm.MoveNext;
begin
  RequeryIfNeed;
  if FPId = 0 then
    TDataSetProcessor(FDSP).Next
  else
    FDataSet.Next;
end;

procedure TdxForm.MoveLast;
begin
  RequeryIfNeed;
  if FPId = 0 then
    TDataSetProcessor(FDSP).Last
  else
    FDataSet.Last;
end;

procedure TdxForm.MoveBy(Distance: Integer);
begin
  RequeryIfNeed;
  FDataSet.MoveBy(Distance);
end;

procedure TdxForm.MoveTo(aRecNo: Integer);
begin
  RequeryIfNeed;
  MoveBy(aRecNo - RecNo)
end;

function TdxForm.Bof: Boolean;
begin
  RequeryIfNeed;
  Result := FDataSet.BOF;
end;

function TdxForm.Eof: Boolean;
begin
  RequeryIfNeed;
  Result := FDataSet.EOF;
end;

function TdxForm.RecNo: Integer;
begin
  RequeryIfNeed;
  Result := FDataSet.RecNo;
end;

function TdxForm.RecId: Integer;
begin
  RequeryIfNeed;
  Result := FDataSet.FieldByName('id').AsInteger;
end;

function TdxForm.RecordCount: Integer;
begin
  RequeryIfNeed;
  Result := FDataSet.RecordCount;
end;

function TdxForm.Print(const TemplateName, OutFileName: String; out
  Errs: String; aOpenFile: Boolean): String;
var
  S: String;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  S := TemplateName;
  if not FilenameIsAbsolute(S) then
    S := GetTemplatesDir + TemplateName;
  Result := OutFileName;
  TDataSetProcessor(FDSP).InnerPrint(S, Result, Errs, aOpenFile, True);
end;

function TdxForm.Locate(const FieldNames: String;
  FieldValues: array of Variant; Options: TLocateOptions): Boolean;
var
  VArr: Variant;
  i: Integer;
  S: String;
  C: TComponent;
  SL: TStringList;
begin
  RequeryIfNeed;
  S := '';
  SL := TStringList.Create;
  try
    SplitStr(FieldNames, ';', SL);
    for i := 0 to SL.Count - 1 do
    begin
      C := FindComponentByFldName(SL[i]);
      S := S + FieldStr(C);
      if i < SL.Count - 1 then S := S + ';';
    end;
    VArr := VarArrayOf(FieldValues);
    Result := FDataSet.Locate(S, VArr, Options);
  finally
    SL.Free;
    VarClear(VArr);
  end;
end;

function TdxForm.GotoRecord(aRecId: Integer): Boolean;
begin
  RequeryIfNeed;
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
  RequeryIfNeed;
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

// TODO: Генерировать исключение, если есть ошибка в условии редактирования.
function TdxForm.CanEdit: TAccessStatus;
begin
  RequeryIfNeed;
  Result := TDataSetProcessor(FDSP).InnerEdit(FDSRi, False, False, False);
end;

// TODO: Генерировать исключение, если есть ошибка в условии удаления.
function TdxForm.CanDelete: TAccessStatus;
begin
  RequeryIfNeed;
  Result := TDataSetProcessor(FDSP).InnerDelete(FDSRi, False, False, False);
end;

function TdxForm.Show: Boolean;
begin
  Result := TDataSetProcessor(FDSP).ShowEditForm(FDSRi) = mrOk;
end;

procedure TdxForm.Open;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  if Opened then Close;
  TDataSetProcessor(FDSP).Open;
end;

procedure TdxForm.Close;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  TDataSetProcessor(FDSP).Close;
end;

procedure TdxForm.OpenRecords(const aFilter: String; Fm: TdxForm; SelCond: Boolean);
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  if Opened then Close;
  Filter.Clear;
  FCustomFilter := aFilter;
  FCustomFilterForm := Fm;
  FUseSelCond := SelCond;
  TDataSetProcessor(FDSP).Open;
end;

procedure TdxForm.OpenRecord(aRecId: Integer);
var
  pDS: PDataSetRec;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  if Opened then Close;
  pDS := PDataSetRec(FDSR);
  with TDataSetProcessor(FDSP) do
  begin
    pDS^.DataSet.SQL.Text := SqlSelectStatement(Self, nil, False, False, nil, '', aRecId, False);
    pDS^.DataSet.Open;
    if MasterSet.RecordCount = 0 then MasterSet.AfterScroll(MasterSet);
    RefreshLookups(0);
  end;
end;

function TdxForm.Opened: Boolean;
begin
  RequeryIfNeed;
  Result := FDataSet.Active;
end;

function TdxForm.Validate: Boolean;
begin
  RequeryIfNeed;
  Result := TDataSetProcessor(FDSP).Validate(FDSRi);
end;

function TdxForm.FindComponentByFldName(const FieldName: String): TComponent;
begin
  Result := FindComponentByFieldName(Self, FieldName);
  if Result = nil then raise Exception.CreateFmt(
    rsComponentWithFieldNameNotFound, [FieldName]);
end;

procedure TdxForm.EnableScrollEvents;
begin
  if FScrollEventsCounter = 0 then Exit;
  Dec(FScrollEventsCounter);
  if FScrollEventsCounter = 0 then
  begin
    FDataset.AfterScroll := FOldAfterScroll;
    FDataSet.BeforeScroll := FOldBeforeScroll;
  end;
end;

procedure TdxForm.DisableScrollEvents;
begin
  if FScrollEventsCounter = 0 then
  begin
    FOldAfterScroll := FDataset.AfterScroll;
    FOldBeforeScroll := FDataSet.BeforeScroll;
    FDataSet.AfterScroll := nil;
    FDataSet.BeforeScroll := nil;
  end;
  Inc(FScrollEventsCounter);
end;

function TdxForm.ScrollEventsDisabled: Boolean;
begin
  Result := FScrollEventsCounter > 0;
end;

function TdxForm.WhoEdit(ARecId: Integer): String;
var
  S: String;
  U: TdxUser;
begin
  Result := '';
  S := 'select uid from dx_lock where fmid=' + IntToStr(FId) + ' and recid=' +
    IntToStr(ARecId);
  with DBase.OpenDataSet(S) do
  begin
    if RecordCount > 0 then
    begin
      U := UserMan.Users.FindUser(Fields[0].AsInteger);
      if U <> nil then Result := U.Name;
    end;
    Free;
  end;
end;

procedure TWindow.SetAutoPosition(AValue: Boolean);
begin
  if FAutoPosition=AValue then Exit;
  FAutoPosition:=AValue;
  if AValue then Position := poDesigned;
end;

procedure TWindow.DoShow;
begin
  if FAutoPosition and (Position = poDesigned) then
    PositionActiveFormCenter(Self);

  {$ifdef windows}
  if OutputFm <> nil then RecreateOutputForm;
  {$else}
  FFirstShow := True;
  // В модальном режиме, если окно появляется после двойного клика или
  // контекстное меню, то speed-кнопки и гриды не реагируют на клик в первый раз.
  // Куда-то теряется событие OnMouseDown.
  //FillChar(LastMouse, SizeOf(LastMouse), 0);
  {$endif}
  inherited DoShow;
end;

procedure TWindow.Activate;
begin
  {$ifdef linux}
  if FFirstShow then
  begin
    ReCreateOutputForm;
    FFirstShow := False;
  end;
  {$endif}
  inherited Activate;
end;

function TWindow.ShowModal: Integer;
begin
  //PopupParent := GetPrevActiveForm(Self);
  Result:=inherited ShowModal;
end;

constructor TWindow.CreateWindow;
begin
  CreateNew(nil);
end;

constructor TWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FParams := TParamList.Create;
  AutoPosition := True;
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
    if MyUtf8CompareText(PD.Name, aName) = 0 then Exit(PD);
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
  RegisterClass(TdxFormTree);
  RegisterClass(TdxRecordId);

end.

