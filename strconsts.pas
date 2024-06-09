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

unit StrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rsError = 'Error';
  //rsDefaultFormCaption = 'Form %s';
  rsSave = 'Save';
  rsSelection = 'Selection';
  rsLabel = 'Label';
  rsText = 'Text';
  rsNumber = 'Number';
  rsDate = 'Date';
  rsCheckBox = 'Checkbox';
  rsMemo = 'Memo';
  rsList = 'List';
  rsObject = 'Object';
  rsTable = 'Table';
  rsGroup = 'Group';
  rsPages = 'Pages';
  rsShape = 'Shape';
  //rsNewForm = 'New Form';
  rsDelete = 'Delete';
  rsFieldName = 'Field name';
  rsColor = 'Color';
  rsFont = 'Font';
  rsForm = 'Form';
  rsAppend = 'Append';
  rsEdit = 'Edit';
  rsOK = 'OK';
  rsCancel = 'Cancel';
  rsListSource = 'List Source';
  rsFilter = 'Filter';
  rsFilterBnHint = 'Filter <F7>';
  rsStatusInsert = 'Insert';
  rsStatusEdit = 'Edit';
  rsStatusBrowse = 'Browse';
  rsCantDeleteRecord = 'On that record referenced %s can not be deleted.';
  rsFile = 'File';
  rsNewDB = 'New';
  rsOpenDB = 'Open';
  rsDesigner = 'Designer';
  rsSettings = 'Settings';
  rsExit = 'Exit';
  rsDataMenu = 'Data';
  rsHelp = 'Help';
  rsAbout = 'About';
  rsCreateDialogTitle = 'Create new database';
  rsDialogFilter = 'DataExpress databases (*.fdb)|*.fdb;*.FDB|All files (*.*)|*.*';
  rsDialogFilterDXDB = 'New DataExpress databases (*.dxdb)|*.dxdb;*.DXDB|';
  rsDialogFilterAllBases = 'All DataExpress databases (*.dxdb, *.fdb)|*.dxdb;*.DXDB;*.fdb;*.FDB|';
  rsCanNotDeleteFile = 'Can not delete the file.';
  rsOpenDatabase = 'Open database';
  rsGridTop = 'Grid top';
  rsGridBottom = 'Grid bottom';
  rsGridLeft = 'Grid left';
  rsGridRight = 'Grid right';
  rsGridOnly = 'Grid only';
  rsWithoutGrid = 'Without grid';
  rsMoveFirst = 'Move first';
  rsMovePrevious = 'Move previous';
  rsMoveNext = 'Move next';
  rsMoveLast = 'Move last';
  rsRefresh = 'Refresh';
  rsPrint = 'Print';
  rsViewType = 'View';
  rsAppName = 'DataExpress';
  rsSearch = 'Search';
  rsSearchBn = 'Search <F3>';
  rsSearchText = 'Search text';
  rsField = 'Field';
  rsSearchAllFields = 'Search all fields';
  rsPrevious = 'Previous';
  rsNext = 'Next';
  rsClose = 'Close';
  rsExportData = 'Export data';
  rsImportExportDataFilter = 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
  rsFileNotExists = 'File %s not exists.';
  rsImportData = 'Import data';
  rsTemplates = 'Templates';
  rsCaption = 'Caption';
  rsEnterCaption = 'Enter caption';
  rsEnterFieldName = 'Enter field name';
  rsSelectColor = 'Select color';
  rsSelect = 'Select';
  rsExample = 'Example';
  rsFontName = 'Font name';
  rsFontBold = 'B';
  rsFontItalic = 'I';
  rsFontUnderline = 'U';
  rsSize = 'Size';
  rsFontStrikeOut = 'S';
  rsClear = 'Clear';
  rsPrecission = 'Precission';
  rsEnterValue = 'Enter value';
  rsNow = 'Current date';
  rsAlternateColor = 'Alternate color';
  rsRowHeight = 'Row height';
  rsAdd = 'Add';
  rsMoveLeft = 'Move left';
  rsMoveRight = 'Move right';
  rsTabOrder = 'Tab order';
  rsMoveUp = 'Move up';
  rsMoveDown = 'Move down';
  rsAutoTabOrder = 'Set tab order automatically';
  rsMoveComponents = 'Move';
  rsDeleteComponents = 'Delete';
  rsAlignment = 'Alignment';
  rsAlignmentLeft = 'Left align';
  rsAlignmentRight = 'Right align';
  rsAlignmentTop = 'Top';
  rsAlignmentBottom = 'Bottom';
  rsHorzCenter = 'Horz center';
  rsVertCenter = 'Vert center';
  rsMaxWidth = 'Max width';
  rsMinWidth = 'Min width';
  rsMaxHeight = 'Max height';
  rsMinHeight = 'Min height';
  rsBringToFront = 'Bring to front';
  rsSendToBack = 'Send to back';
  rsWarning = 'Warning';
  rsExitMsg = 'You are sure to exit';
  rsPage = 'Page';
  rsSelectedComponentCount = 'Components: %d';
  rsLanguage = 'Language';
  rsTemplatesFolder = 'Templates folder';
  rsOpenTemplate = 'Open template with';
  rsWordXML = 'Word XML';
  rsWordDocument = 'Word Document';
  rsOODocument = 'OpenOffice Document';
  rsOOSpreadSheet = 'OpenOffice Spreadsheet';
  rsWebPage = 'Web page';
  rsOutputFolder = 'Output folder';
  rsSelectApplication = 'Select application';
  rsApplicationsFilter = 'Applications (*.exe)|*.exe|Batch files (*.bat, *.cmd)|*.bat;*.cmd|All files (*.*)|*.*';
  rsApplicationsFilterWine = 'Scripts (*.sh)|*.sh|All files (*.*)|*.*';
  rsApplicationsFilterUnix = 'Applications|*';
  rsAppType = 'Type';
  rsAppDescription = 'Description';
  rsAppPath = 'Path';
  //rsMainAppDescription = 'Universal database and documents generator';
  //rsDeveloper = 'Developer: Pavel Duborkin';
  rsShapeType = 'Shape type';
  rsLineStyle = 'Line style';
  rsLineWidth = 'Line width';
  rsLineColor = 'Line color';
  rsFillColor = 'Fill color';
  rsStorageType = 'Storage type';
  rsImage = 'Image';
  rsSTDatabase = 'Database';
  rsSTFolder = 'Folder';
  rsSTLink = 'Link';
  rsThumb = 'Thumbnail';
  rsLoadImage = 'Load image';
  rsOpenPicturesFilter = 'All pictures (*.bmp, *.gif, *.ico, *.jpg, *.jpeg, *.jfif, *.png, *.tif, *.xpm)|*.bmp;*.gif;*.ico;*.jpg;*.jpeg;*.jfif;*.png;*.tif;*.xpm';
  //rsOpenPicturesFilter = 'All pictures (*.bmp, *.jpg, *.png, *.tif)|*.bmp;*.jpg;*.png;*.tif';
  rsSavePicturesFilter = 'All pictures (*.bmp, *.jpg, *.png, *.tif)|*.bmp;*.jpg;*.png;*.tif';
  rsCantCopyFile = 'Can not copy file %s.';
  rsSaveImage = 'Save image';
  rsOpen = 'Open';
  rsSelectFields = 'Select fields';
  rsOpenFile = 'Open file';
  rsExportSuccess = 'Data has been successfully exported to a file %s.';
  rsFileNameEmpty = 'Enter filename';
  rsFieldsNotSelected = 'Select any fields';
  rsEncoding = 'Encoding';
  rsCheckUnique = 'Check unique';
  rsImportSucess = '%d records were imported sucessfully.';
  rsDsgnBackImage = 'Background image';
  rsClearImage = 'Clear';
  rsSaveFile = 'Save file';
  rsLoadFile = 'Load file';
  rsDsgnFile = 'File';
  rsFileFilter = 'Files %s|*.%s|All files (*.*)|*.*';
  rsAllFilesFilter = 'All files (*.*)|*.*';
  rsCanceledByUser = 'Canceled by user.';
  rsProcessing = 'Processing...';
  rsFilename = 'Filename';
  rsFieldType = 'Field type';
  rsAddFields = 'Add fields';
  rsColumns = 'Columns';
  rsDataArea = 'Data';
  rsTitle = 'Title';
  rsLines = 'Lines';
  rsVert = 'Vertical';
  rsHorz = 'Horizontal';
  rsGridLines = 'Grid lines';
  rsInsert = 'Insert';
  rsConfirmDelete = 'Confirm delete.';
  rsTabSheet = 'Tab';
  rsPrintOut = 'Printout';
  rsYes = 'Yes';
  rsNo = 'No';
  rsSelectedColor = 'Selected color';
  rsFlat = 'Flat';
  rsOther = 'Other';
  rsFatalError = 'Unexpected exception';
  rsFatalErrorMsg = 'Oops! Sorry for the inconvenience. An unexpected exception.%s' +
    'Do you want to continue running the program?';
  rsFatalErrorMsgDesign = 'Oops! Sorry for the inconvenience. An unexpected exception.%s';
  rsDBNotFound = 'Database %s not found.';
  rsSite = 'Site: ';
  rsSupport = 'Support: ';
  rsExpression = 'Expression';
  rsWeekNames = 'Monday Tuesday Wednesday Thursday Friday Saturday Sunday';
  rsWeekNamesBrief = 'Mon Tue Wed Thu Fri Sat Sun';
  rsMonthNames = 'January February March April May June July August September October November December';
  rsMonthNamesBrief = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec';
  rsFunctions = 'Functions';
  rsFuncNotSel = 'Function not selected.';
  //rsFormFieldNotSel = 'Form or field not selected';
  rsFields = 'Fields';
  rsAddition = 'Addition';
  rsSubtraction = 'Subtraction';
  rsMultiplication = 'Multiplication';
  rsDivision = 'Division';
  rsEqual = 'Equal';
  rsNotEqual = 'Not equal';
  rsLess = 'Less';
  rsLessOrEqual = 'Less or equal';
  rsGreat = 'Great';
  rsGreatOrEqual = 'Great or equal';
  rsLogicalAND = 'Logical AND';
  rsLogicalOR = 'Logical OR';
  rsCalcFields = 'Calculated fields';
  rsName = 'Name';
  rsSourceNum = 'Source %d';
  rsIncoming = 'Incoming';
  rsOutcoming = 'Outcoming';
  rsValue = 'Value';
  rsRangeFrom = 'from ';
  rsRangeTo = ' to ';
  rsPeriodFrom = 'from ';
  rsPeriodTo = ' to ';
  rsParameter = 'Parameter';
  rsVisible = 'Visible';
  rsSourceType = 'Type';
  rsTotal = 'Total';
  rsReport = 'Report';
  rsTotalFunc = 'Total function';
  rsSum = 'Sum';
  rsAverage = 'Average';
  rsMaximum = 'Maximum';
  rsMinimum = 'Minimum';
  rsBalance = 'Balance';
  rsCount = 'Count';
  rsSelectSourceType = 'Select type';
  rsIncompatibleFields = 'Incompatible fields.';
  //rsFieldNameIsEmpty = 'Field name is empty';
  rsFieldNotSel = 'Field not selected.';
  rsFormNotSel = 'Form not selected.';
  rsSourceNotSel = 'Source not select.';
  rsReportExportFilename = 'Filename (optional)';
  rsReportExport = 'Report export';
  rsRenameReport = 'Rename report';
  rsReportName = 'Report name';
  rsNewReport = 'New report';
  rsReports = 'Reports';
  //rsReportSort = 'Sort';
  rsSortZA = 'Z-A';
  //rsCantDeleteFormReport = 'Can not delete form [%s], because it used in '
  //  +'report [%s]';
  //rsCantDeleteFormQuery = 'Can not delete form [%s], because it used in '
  //  +'query [%s] in form [%s]';
  //rsCantDeleteFieldReport = 'Can not delete field [%s], because it used in '
  //  +'report [%s].';
  //rsCantDeleteFieldQuery = 'Can not delete field [%s], because it used in '
  //  +'query [%s] in form [%s].';
  rsRename = 'Rename';
  rsAppendField = 'Append field';
  rsDeleteField = 'Delete field';
  rsAppendSource = 'Append source';
  rsDeleteSource = 'Delete source';
  rsCountFieldName = 'Total';
  rsNoVisibleFiels = 'No visible fields';
  //rsExecuteReport = 'Execute';
  rsDetailingPeriod = 'Detailing period';
  rsDDDay = 'Day';
  rsDDWeek = 'Week';
  rsDDMonth = 'Month';
  rsDDQuarter = 'Quarter';
  rsDDHalfYear = 'Half-Year';
  rsDDYear = 'Year';
  rsPeriod = 'Period';
  rsObjField = 'Object field';
  //rsSourceField = 'Source field';
  rsValues = 'Values';
  //rsFormFields = 'Form fields';
  rsProperties = 'Properties';
  rsFilePathTooLong = 'File path too long (> 255).';
  rsFileNameTooLong = 'File name too long (> 150).';
  rsContents = 'Contents';
  rsFormName = 'Form name';
  rsEnterFormName = 'Enter form name';
  rsVersion = 'Version';
  rsCreate = 'Create';
  rsGoTo = 'Go to';
  rsOrderVisibleForms = 'The order and visibility of forms in tabs';
  rsSelectAll = 'Select all';
  rsHeaders = 'Headers';
  rsColumn = 'Column';
  //rsVisibleColumns = 'Visible columns';
  rsHeader = 'Header';
  rsSolid = 'Solid';
  rsDash = 'Dash';
  rsDot = 'Dot';
  //rsReportGrid = 'Report grid';
  rsDefaultRpColCaption = 'Field';
  rsCopyStyle = 'Copy style';
  rsObjectFields = 'Object fields';
  rsFunction = 'Function';
  //rsTableNotSelect = 'Table not selected';
  //rsFuncNotSelect = 'Function not selected';
  //rsAreYouSure = 'Are you sure?';
  rsCopyReportStyle = 'Copy style';
  rsErrorCalcField = 'Error in calculated field [%s]: ';
  rsErrorsInExpr = 'Errors in expressions';
  rsFieldNotFound = 'Field not found: %s';
  rsUnknownTag = 'Unknown tag: %s';
  rsFormNotFound = 'Form not found: %s';
  rsEndWthBegin = 'End tag without begin tag.';
  rsErrorMsg = 'Error: ';
  rsErrorExpr = 'Expression: ';
  rsIncompatibleTypes = 'Incompatible types.';
  rsIllegalStrOp = 'Illegal string operation.';
  rsIllegalNumOp = 'Illegal numeric operation.';
  rsIllegalBoolOp = 'Illegal boolean operation.';
  rsDivZero = 'Division by zero.';
  rsInvalidNumber = 'Invalid number: %s';
  rsArgExpected = 'Argument expected.';
  rsEndLineExpected = 'Expected end of line.';
  rsSqBrExpected = 'Expected closing square bracket.';
  rsOperandExpected = 'Operand expected.';
  rsCommaExpected = 'Comma expected.';
  rsFieldNameEmpty = 'Field name is empty.';
  rsNotObjField = 'Field is not object';
  rsIllegalDateOp = 'Illegal date/time operation.';
  rsQuery = 'Query';
  rsSort = 'Sort';
  rsMoreProps = 'More properties';
  rsQueryName = 'Query name';
  rsEnterQueryName = 'Enter query name';
  rsCheckExpr = 'Check';
  rsNot = 'Not';
  rsNull = 'Null';
  rsFilterPresets = 'Filter presets';
  rsFilterName = 'Filter name';
  rsEnterFilterName = 'Enter filter name';
  rsLinkedObject = 'Linked object';
  rsAddField = 'Add field';
  rsAddValue = 'Add value';
  rsDeleteValue = 'Delete value';
  rsFiltersHint = 'The first filter used by default.';
  rsTime = 'Time';
  rsCurrentTime = 'Current time';
  rsTimeFormat = 'Time format';
  rsEditing = 'Editing';
  rsColoring = 'Coloring';
  rsRowN = 'Row %d, Col %s: ';
  rsImpIncorrectNumber = 'Incorrect number: %s';
  rsImpIncorrectDate = 'Incorrect date: %s';
  rsImpIncorrectCheck = 'Incorrect check: %s';
  rsImpIncorrectTime = 'Incorrect time: %s';
  rsSelectComponents = 'Select components';
  rsQueryGrid = 'Query grid';
  rsCounter = 'Counter';
  rsImpIncorrectCounter = 'Incorrect counter value: %s';
  rsChangeCounter = 'Change counter';
  rsBeginValue = 'Begin value';
  rsStartWith = 'Start with';
  rsFieldSize = 'Field size';
  rsInsertValues = 'Insert values';
  rsFormField = 'Form field';
  rsObjectFieldNotSel = 'Object field not selected';
  rsFormFieldNotSel = 'Form field not selected.';
  rsParentForm = '-- Parent form --';
  rsRequired = 'Required';
  rsHH = 'HH';
  rsHHMM = 'HH:MM';
  rsHHMMSS = 'HH:MM:SS';
  rsReadOnly = 'Read-only';
  rsLblText = 'Label text';
  rsEnterText = 'Enter text';
  rsRange = 'Range';
  rsEnterMinMax = 'Enter minimal and maximal value';
  rsAll = 'All';
  rsInvalidRange = 'Invalid range.';
  rsFieldIsRequired = 'Field [%s] is required.';
  rsRangeMsg = '[%s] should be in the range from %s to %s.';
  rsRangeMsg2 = 'Value should be in the range from %s to %s.';
  rsFieldSizeRangeMsg = 'Value should be in the range from 1 to 2000.';
  rsInvalidCharInFieldName = 'You can not use the following characters in the names of fields: '' " | ! < > [ ] { } / =';
  rsInvalidCharInFormName = 'You can not use the following characters in the names of forms, queries and reports: '' " | ! < > [ ] { } / \ ? : *';
  //rsInvalidChars2 = 'This property is not available because the label caption contains invalid characters.';
  rsEnterName = 'Enter name';
  rsExprCheck = 'Check expression <F5/F9>';
  rsExprCheck2 = 'Check expression';
  rsObjectInForm = 'Object in form';
  rsDXSite = 'DataExpress website';
  rsOnlyList = 'Only values from the list';
  rsShopping = 'Shopping';
  rsQuantity = 'Quantity';
  rsInput = 'Input';
  rsPrice = 'Price';
  rsAddToExisting = 'Add to existing';
  rsQttyFldNotSel = 'Quantity field not selected.';
  rsPriceFldNotSel = 'Price field not selected.';
  rsObjectNotSel = 'Object not selected.';
  rsLinkSetup = 'Link setup';
  rsQueryField = 'Query field';
  rsQueryFieldNotSel = 'Query field not selected.';
//  rsFirstRemoveQueries = 'First remove queries';
  rsErrorsOccured = 'Errors occured.';
  rsFPSrcFldNotFound = 'Source field [%s] not found.';
  rsFPSrcFldIsEmpty = 'Source field name is empty.';
  rsFPSrcFldExcept = 'Source field expected.';
  rsFPCmpOpExpect = 'Comparsion operation expected.';
  rsFPExprNotParsed = 'Expression not parsed.';
  //rsFPErrInExpr = 'Error in expression ( %s ): %s';
  rsFPBoolOpExpect = 'Boolean operation expected.';
  rsFPSrcFldNotFoundIn = 'Source field [%s] not found in [%s].';
  rsFPSrcFldObjFieldNotValid = 'It is not valid to use the object field (component) [%s] as a source field.';
  //rsSourceFields = 'Source fields';
  rsSuccess = 'Success';
  rsLoopDetected = 'Loop detected in the calculation chain: %s';
  rsLoopDetectedCalc = 'Loop detected in calculations.';
  rsFindLost = 'Find lost';
  rsSelectReport = 'Select report';
  rsRecalculate = 'Recalculate';
  rsNoFormsAvailable = 'No forms available.';
  rsTools = 'Tools';
  rsReplacedBy = 'Replaced by';
  rsInvalidCmpOp = 'Invalid comparison operation.';
  rsCharLBrExpext = 'Expected left brace {.';
  rsCharRBrExpect = 'Expected right brace }.';
  rsDelimiter = 'Delimiter';
  rsCancelChange = 'Cancel changes';
  rsSaveChanges = 'Save changes';
  rsSelectionSet = 'Selection settings';
  rsGroupByDate = 'Group by date';
  rsNotAllFieldsSel = 'Not all fields are selected.';
  rsFieldShouldBeNum = 'Field type must be "Number".';
  rsFieldShouldBeVisibleOrParam = 'Field should be visible or parameter.';
  rsDateFieldNotFound = 'Date field not found.';
  rsFilterByTotals = 'Filter by totals';
  rsShowGrandTotal = 'Show grand total';
  rsUsers = 'Users';
  rsDateFieldShoulBeVisible = 'Date field should be visible.';
  rsRoles = 'Roles';
  rsRoleUsed = 'This role used by user %s';
  rsUser = 'User';
  rsPassword = 'Password';
  rsRole = 'Role';
  rsDeveloper = 'Developer';
  rsRoleNotSel = 'Role not selected.';
  rsEnterPwd = 'Enter password';
  rsAccessToForms = 'Access to forms';
  rsAccessToReports = 'Access to reports';
  rsSelCond = 'Selection condition';
  rsEditCond = 'Edit condition';
  rsCmpType = 'Type';
  rsAccessToCmp = 'Access to components';
  rsComponent = 'Component';
  rsWelcome = 'Welcome to %s!';
  rsAddDevel = 'Add a user-developer';
  rsConnectToDB = 'Connect to database';
  rsDBPath = 'Database path';
  rsErrConnect = 'Can not connect to database: %s.%s';
  rsErrCreate = 'Can not create database: %s.%s';
  rsConnect = 'Connect';
  rsDelCond = 'Delete condition';
  //rsCantEditRec = 'You can not edit this record';
  rsCantDelRec = 'You can not delete this record. Access denied.';
  rsChangePwd = 'Change password';
  rsAccessToFields = 'Access to fields';
  rsThereProblem = 'There is a problem';
  rsProblemSolution = 'To program this feature to work, you need to enter and '
    +'exit the designer.';
  rsApplySelCondToObj = 'Apply condition of selection to objects';
  rsErrorInField = 'Error in field [%s]: %s';
  rsCut = 'Cut';
  rsCopy = 'Copy';
  rsPaste = 'Paste';
  //rsVarNotFound = 'Variable %s not found';
  rsExportProject = 'Export Project';
  rsImportExportPrjFilter = 'DataExpress Project (*.dxp)|*.dxp|All files (*.*)'
    +'|*.*';
  rsImportProject = 'Import Project';
  rsExportPrjOk = 'Export of project was successfull.';
  rsImportPrjOk = 'Import of project was successfull.';
  rsMergePrjOk = 'Merge projects was successfull.';
  rsDuplicate = 'Duplicate';
  rsDuplicateAll = 'Duplicate all';
  rsLook = 'Look';
  rsLineComment = 'Single line comment';
  rsMultiLineComment = 'Multiline comment';
  rsTemplateFileNotFound = 'Template file %s not found.';
  rsConfirmDelete2 = 'One or more tables contain data. Are you sure you want '
    +'to delete this record?';
  //rsCantSortCol = 'You can not sort on a calculated fields.';
  rsOutputFilter = 'Output filter';
  //rsErrorSrcFlt = 'Error in source filter %s';
  //rsErrorOutFlt = 'Error in output filter %s';
  rsTotals = 'Totals';
  rsListFilter = 'List filter';
  rsSourceFilter = 'Source filter';
  rsDeleteLimitedMsg = 'Access to the table [%s] is limited. Deleting is not '
    +'possible.';
  rsAccess = 'Access';
  rsOnlyView = 'Only viewing';
  rsOnlyEdit = 'Only editing';
  rsFullAccess = 'Full access';
  rsIncorrectAccess = 'Incorrect access';
  rsNoAccess = 'No access';
  rsNotAvailableSubForm = 'Not available for child form.';
  rsDblClickChange = 'Double-click to change';
  rsViewing = 'Viewing';
  rsLimitedAccess = 'Limited access';
  rsDefaultValue = 'Default value';
  rsFormQryNotFound = 'Form or query not found: %s';
  rsFormNotAvail = 'In this expression forms is not available.';
  rsFieldNotDate = 'Field [%s] not date.';
  rsHint = 'Hint';
  rsHelpText = 'Help text';
  rsBold = 'Bold <Ctrl-B>';
  rsItalic = 'Italic <Ctrl-I>';
  rsUnderline = 'Underline <Ctrl-U>';
  rsTextColor = 'Text color';
  rsLeftJustify = 'Left justify';
  rsCenterText = 'Center text';
  rsRightJustify = 'Right justify';
  rsOrderedList = 'Ordered list';
  rsUnorderedList = 'Unordered list';
  rsNewLine = 'New line <Ctrl-Space>';
  rsPreview = 'Preview <F9>';
  rsUndo = 'Undo';
  rsRedo = 'Redo';
  rsToday = 'Today';
  rsThisWeek = 'This week';
  rsThisMonth = 'This month';
  rsThisYear = 'This year';
  rsClearValue = 'Clear value';
  rsClearAllValues = 'Clear all values';
  rsBeginWeek = 'Beginning of week';
  rsBeginMonth = 'Beginning of month';
  rsBeginYear = 'Beginning of year';
  rsGotoURL = 'Go to URL';
  rsSetToZero = 'Set to zero';
  rsParentField = 'Parent field';
  rsTree = 'Tree';
  rsGroupField = 'Group field';
  rsBackColor = 'Back color';
  rsWidth = 'Width';
  rsClearSelection = 'Clear selection';
  rsExpandAll = 'Expand all';
  rsCollapseAll = 'Collapse all';
  rsDeleteRecords = 'Delete records';
  rsErrDelRecs = 'Some records have not been deleted.';
  rsDelRecsMsg = 'Warning!%sDo you want to delete all records selected '
    +'current filters. The data will be deleted forever.%sAre you sure?';
  rsDelRecsConfirm = 'I confirm deletion';
  rsSetValue = 'Set value';
  rsEnterExpr = 'Enter expression';
  rsUnexpectedToken = 'Unexpected token %s';
  rsExchgOldNewRow = 'You are trying to exchange the old and new rows. ' +
    'You must first save the record. Continue?';
  rsButtons = 'Buttons';
  rsConfirmExit = 'Confirm when application exit';
  rsDefaultIntf = ' (Default)';
  rsDefault = 'Default';
  rsInterface = 'Interface';
  rsMenu = 'Menu';
  rsTabs = 'Tabs';
  rsSection = 'Section';
  rsSubsection = 'Subsection';
  rsNewSection = 'New section';
  rsSelectForm = 'Select form';
  rsRenameSection = 'Rename section';
  rsInterfaces = 'Interfaces';
  rsShowButtons = 'Show buttons';
  rsVisibleButtons = 'Visible buttons';
  rsVisibleCaptions = 'Visible captions';
  rsBackground = 'Background';
  rsHideWhenLostFocus = 'Hide when lost focus';
  rsFlatButtons = 'Flat buttons';
  rsLeft = 'Left';
  rsRight = 'Right';
  rsAllowSingleUserMode = 'Allow single-user mode';
  rsAllowMultiAuth = 'Allow multiple authorization';
  rsUserAlreadyLogged = 'User "%s" is already logged.';
  rsDBInSingleMode = 'The database is into single-user mode. Log is not '
    +'possible.';
  rsSingleUserMode = 'Single-user mode';
  rsActiveUsersInDB = 'There are active users in the database. Log into single'
    +' mode is not possible.';
  rsSingleModeActiveUsers = 'You are logged in single user mode, but there are'
    +' active users in the database.';
  rsUserMonitor = 'User monitor';
  rsRecHasBeenDeleted = 'Record has been deleted.';
  rsRecHasBeenDeletedRefresh = 'Record has been deleted. Refresh data?';
  rsRecEditUser = 'This record is already being edited by the user "%s". The record will be opened in view mode.';
  rsRecEditAnotherUser = 'This record is already being edited by another user.';
  rsRecChangesDetect = 'Record changes detected. Refresh data?';
  //rsRecChangedYou = 'This record is changed by you. Refresh data?';
  rsYouCantDelCurUser = 'You can not delete the current user.';
  rsActiveUsers = 'Active users';
  rsEditableRecs = 'Editable records';
  rsCID = 'CID';
  rsUsername = 'Username';
  rsSingle = 'Single';
  rsLoginTime = 'Login time';
  rsElapsedTime = 'Elapsed time';
  rsStarted = 'Started';
  rsCheckValue = 'Check value';
  rsDistinctCount = 'Distinct count';
  rsSrcFldNotSel = 'Source field not selected.';
  rsDestFldNotSel = 'Destination field not selected.';
  rsSourceTableFilter = 'Source table filter';
  rsDestTblNotSel = 'Destination table not selected.';
  rsSrcTblNotSel = 'Source table not selected.';
  rsFillTable = 'Fill table';
  rsSourceTable = 'Source table';
  rsDestTable = 'Destination table';
  rsSource = 'Source';
  rsDestination = 'Destination';
  rsPromptFillTable = 'Do you want to fill the table?';
  rsErrorInFillFilter = 'Error in the filter source table. Object is'
    +' %s';
  rsPromptBeforeFill = 'Prompt before filling';
  rsClearTableBeforeFill = 'Clear table before filling';
  rsDefaultPwd = 'Default password';
  rsEditable = 'Editable';
  //rsFPUnsupportedParam = 'Unsupported parameter type in function %s, no %d';
  //rsFPUnsupportedReslt = 'Unsupported returned type in function %s';
  //rsFailedLoadPlugin = 'Failed to load plugin %s';
  //rsErrorsNotFound = 'Errors not found.';
  //rsShowErrors = 'Show errors';
  rsAuthor = 'Author';
  rsFuncNotFound2 = 'Function not found: %s';
  //rsInvalidFN = 'Invalid function name: %s';
  rsAllFunctions = 'All functions';
  rsFunctionsBn = 'Functions. Show help by function under cursor <F2>';
  rsLastErrorMsg = 'The last time there was a critical error. Continue '
    +'database connection?';
  rsSaveRecMsg = 'You must first save the record. Continue?';
  rsPrnError = 'An error occurred during printing.';
  rsEmpty = 'Empty';
  rsClearHistory = 'Clear history';
  rsClearHistoryMsg = 'You want to clear the history of recently opened '
    +'databases. Continue?';
  rsWordWrap = 'Word wrap';
  rsRecents = 'Recents';
  rsImportErrorRow = 'Row %d: %s';
  rsButton = 'Button';
  rsAction = 'Action';
  rsGlyph = 'Glyph';
  rsPNGFiles = 'PNG files (*.png)|*.png';
  rsGoToForm = 'Go to form';
  rsTemplateFile = 'Template file';
  rsOrExpression = 'or Expression';
  //rsErrorInExpr = 'Error in expression: %s';
  rsTemplateUnknown = 'Template unknown';
  rsMassCalc = 'Mass calculation';
  rsConfirmationMsg = 'Confirmation message';
  rsSuccessMsg = 'Success message';
  //rsButtonName = 'Button name';
  rsReportNotSel = 'Report not selected';
  rsOpenReport = 'Open report';
  rsExecCond = 'Execution condition';
  rsSaveRecord = 'Save record';
  rsCommonParams = 'Common parameters';
  rsSimpleForm = 'Simple form';
  //rsYesMsg = 'Yes';
  //rsNoMsg = 'No';
  //rsCancelMsg = 'Cancel';
  rsMergeAll = 'Merge All';
  rsMerge = 'Merge';
  rsPivotTable = 'Pivot table';
  rsNone = 'None';
  rsHeaderTotal = 'Header total';
  rsGrandTotal = 'Grand total';
  rsShowRowTotal = 'Show row total';
  rsShowColumnTotal = 'Show column total';
  rsColumnWidthTotal = 'Column width total';
  rsCorner = 'Corner';
  rsIndication = 'Indication';
  rsHeaderLines = 'Header lines';
  rsRows = 'Rows';
  rsFieldProps = 'Field properties: %s';
  rsSubtotalColumn = 'Subtotal (column)';
  rsTextAlignment = 'Text alignment';
  rsTextAlignInDataCells = 'Text alignment in data cells';
  rsCellSize = 'Cell size';
  rsHeight = 'Height';
  rsShowSubtotal = 'Show subtotal';
  rsCaptionSubtotal = 'Caption subtotal';
  rsSelected = 'Selected';
  rsDataType = 'Data type';
  rsDataDelimiter = 'Data delimiter';
  rsIndent = 'Indent';
  rsSelectQuery = 'Select query';
  rsCouldNotCreateFolder = 'Could not create folder: %s';
  rsFailedToLoadImage = 'Failed to load image.';
  rsLoading = 'Loading...';
  rsQueryNotFound = 'Query not found: %s';
  rsReportNotFound = 'Report not found: %s';
  rsObjectFieldNotFound = 'Object field not found: %s';
  rsErrorInExprFix = 'The expression contains an error:%sYou want to fix it?';
  //rsNoSaveChangesExit = 'You close the application. All the changes that were made after the command "Save" will not be saved. Continue?';
  rsCancelChangesMsg = 'Are you sure want to cancel the changes?';
  rsMergeProjects = 'Merge projects';
  rsForms = 'Forms';
  rsFormLinkTo = 'Form link to';
  rsFormsLinkToSelFm = 'Forms link to selected form';
  rsReportsLinkToFm = 'Reports that link to selected form';
  rsReportRefersTo = 'Report refers to';
  rsMergePrjRenameMsg = 'Some components have been renamed because the '
    +'project already have such names.';
  rsMergePrjBeforeRenameMsg = 'Some components will be renamed because the '
    +'project already have such names.';
  rsContinueMergeProjects = 'Continue to merge projects?';
  rsCallFunction = 'Call function';
  rsDefaultImage = 'Default image';
  rsIP = 'IP';
  rsID = 'ID';
  rsFieldCantParam = 'Field can not be a parameter.';
  //rsScript = 'Script';
  //rsScriptChangeMsg = 'The script has been modified. Save changes?';
  rsCompilationSuccess = 'Compilation is successful.';
  rsOutput = 'Output';
  rsDuplicateComponentName = 'A component name already exists.';
  rsInvalidCmpName = 'Invalid component name.';
  rsComponentName = 'Component name';
  rsComponentFieldNameExists = 'A component with the same field name already exists.';
  rsCalcFieldNameExists = 'A calculation field with the same field name already exists.';
  rsCalcFieldSqlNameExists = 'A calculation field with the same SQL field name already exists.';
  rsCalcLabelCaptionExists = 'A calculation label with same caption already exists.';
  rsCalcLabelCaptionExists2 = 'This property is not available, because a calculation label same caption already exists.';
  rsFormNameExists = 'A form with the same name already exists.';
  rsReportNameExists = 'A report or query with the same name already exists.';
  rsComponentNameEmpty = 'Component name is empty.';
  rsDuplicateFieldName = 'Duplicate field name.';
  rsDuplicateSqlFieldName = 'Duplicate SQL field name.';
  rsQueryFieldNameExists = 'A field with the same name already exists.';
  rsNewModule = 'New module';
  rsModuleName = 'Module name';
  //rsInvalidModuleName = 'Invalid module name';
  rsRenameModule = 'Rename module';
  rsUnknownModule = 'Unknown module: %s';
  rsUserModules = 'User modules';
  rsFormsModules = 'Forms modules';
  rsCircularRefModules = 'Detected circular reference between modules %s and %'
    +'s.';
  rsHideAncestors = 'Hide ancestors';
  rsSearchTextNotFound = 'Search text "%s" was not found.';
  rsFind = 'Find';
  rsReplace = 'Replace';
  rsReplaceAll = 'Replace All';
  rsReplaceAllMsg = 'Do you really want to replace all?';
  rsModified = 'Modified';
  rsInserting = 'INS';
  rsReplacing = 'REPL';
  rsModuleAlreadyExists = 'The module named "%s" already exists.';
  rsReplaceOldModuleNewModule = 'Do you want to replace an old module with a new module?';
  rsCompileErrorMsg = 'During script compilation errors have been detected. '
    + 'If you see this window after updating the application, then most likely '
    + 'there are changes in the application that make your script incompatible. '
    + 'To solve the problem, contact the developer of the module in which the '
    + 'error occurred.';
  rsScriptEditor = 'Script Editor';
  rsScriptEditorBn = 'Script Editor <F4>';
  rsAddFormModule = 'Add form module';
  rsAddUserModule = 'Add user module';
  //rsFindToInternet = 'Find to Internet';
  rsSearchSettings = 'Search settings';
  rsURLPatternToSearch = 'URL pattern to search for';
  rsFindPrevious = 'Find previous';
  rsCompile = 'Compile';
  rsFindReplace = 'Find / Replace';
  rsSearchedText = 'Searched text';
  rsSearchOptions = 'Search options';
  rsWholeWords = 'Whole words';
  rsCaseSensitive = 'Case sensitive';
  rsStart = 'Start';
  rsFromCursor = 'From cursor';
  rsFromBegin = 'From the beginning';
  rsSearchArea = 'Search area';
  rsEntireScope = 'Entire scope';
  rsInSelected = 'In selected';
  rsFindNode = 'Find node';
  rsCompilerMsgs = 'Compiler messages';
  rsInvalidMethotCall = 'Invalid method call child form: %s';
  rsExpertMode = 'Expert mode';
  rsSQLEditor = 'SQL Editor';
  rsSQLTextCopyToClip = 'SQL text is successfully copied to the clipboard.';
  rsExecuteSQL = 'Execute SQL <F9>';
  rsCopyToPasteScript = 'Copy to paste in the script';
  rsPasteFromScript = 'Paste from script';
  rsProgInDX = 'Programming in DX';
  rsTest = 'Test';
  rsTestForm = 'Test form';
  rsCursorPos = 'Cursor position';
  //rsHelpFunction = 'Help function';
  rsTableOrQuery = 'Table or Query';
  rsInsertTotalFunc = 'Insert total function';
  //rsBoolean = 'Boolean';
  rsSelectFormField = 'Select form field';
  rsSelectSourceField = 'Select source field';
  rsSelectQueryField = 'Select query field';
  rsSelectedField = 'Selected field';
  rsInsertSourceField = 'Insert source field';
  rsInsertQueryField = 'Insert query field';
  rsInsertFormField = 'Insert form field';
  rsContainingText = 'Containing text';
  rsNotContainingText = 'Not containing text';
  rsShowScrollbars = 'Show scroll bars';
  rsAnchors = 'Anchors';
  rsLeftSide = 'Left side';
  rsTopSide = 'Top side';
  rsRightSide = 'Right side';
  rsBottomSide = 'Bottom side';
  //rsFilterOn = 'Filter on';
  //rsExtensions = 'Extensions';
  rsExtensions = 'Extensions';
  rsExportModule = 'Export module';
  rsImportModule = 'Import module';
  rsImportModules = 'Import modules';
  rsFormModulesFlt = 'Form modules (*.fpas)|*.fpas|';
  rsUserModulesFlt = 'User modules (*.pas)|*.pas|';
  rsExtModulesFlt = 'Extensions modules (*.epas)|*.epas|';
  rsWebExtModulesFlt = 'Web extensions modules (*.wepas)|*.wepas|';
  rsWebFormModulesFlt = 'Web form modules (*.wfpas)|*.wfpas|';
  rsAllFilesFlt = 'All files|*.*';
  rsAllExtModulesFlt = 'All extensions|*.epas;*.wepas|';
  rsAllModulesFlt = 'All modules|*.pas;*.epas;*.wepas|';
  //rsAddModule = 'Add module';
  rsDeleteExtModuleConfirm = 'Some expressions and actions may stop working. Do you '
    +'really want to delete this module?';
  rsDeleteExtModuleNote = 'Note: the changes will take effect the next time you '
    +'connect to the database.';
  rsUnsuccessfullCompile = 'Unsuccessfull compile';
  //rsFieldsWithExprNotFound = 'Fields with expressions %snot found.';
  rsCalcLabels = 'Calculation labels';
  rsIncompatibleTypesSrcFieldAndExpr = 'The result of the expression is not compatible with the source field. Got %s, expected %s.';
  rsEditMask = 'Edit mask';
  rsCharForBlanks = 'Character for blanks';
  rsSaveLiteralChars = 'Save literal characters';
  rsTestInput = 'Test input';
  rsSampleMasks = 'Sample masks';
  rsPhoneNumber = 'Phone number';
  rsPassportSeries = 'Passport series';
  rsPassportID = 'Passport ID';
  rsSNILS = 'SNILS';
  rsIPAddress = 'IP address';
  rsMACAddress = 'MAC address';
  rsMaskErrorMsg = 'The entered value does not match the mask';
  rsNothingFound = 'Nothing found';
  rsEnterSearchText = 'Enter search text';
  rsExportCantCreateFolder = 'Can not create folder: %s. The export operation will be aborted.';
  rsExportCantCreateFile = 'Can not create file: %s. The export operation will'
    +' be aborted.';
  rsCantCopyFileToStorageFolder = 'Can not copy file from %s to %s.';
  rsConfirmSaveMsg = 'Continue saving changes?';
  rsConfirmAutoSaveMsg = 'Save changes?';
  rsConfirmCancelEditMsg = 'Cancel changes?';
  rsConfirmSaveRec = 'Confirm save record';
  rsConfirmAutosaveRec = 'Confirm autosave record';
  rsConfirmCancelEditing = 'Confirm cancel editing';
  rsLabelEmpty = 'The label can not be empty or filled with spaces.';
  rsRowCountInList = 'Number of visible lines in the list';
  rsSummary = 'Summary';
  rsComponents = 'Components';
  rsFindComponent = 'Find component';
  rsFindFieldOrExpr = 'Find field or expression';
  rsCodeFolding = 'Code folding';
  rsFoldAllCode = 'Fold all code';
  rsUnfoldAllCode = 'Unfold all code';
  rsMaximizeEditor = 'Maximize editor <F12>';
  rsThisCommandNotApplySimpleForm = 'This command does not apply to a simple '
    +'form.';
  rsExtModuleFailedMsg = 'Call function %s failed in the extension module %s. Try to find and '
    +'remove the module that causes the error.';
  rsExecActionFailedMsg = 'Execute action %s failed in the extension module %s. Try to find and '
    +'remove the module that causes the error.';
  rsEnterColumnTitle = 'Enter column title';
  rsSizePrec = 'Size/Prec';
  rsSelectFieldType = 'Select field type';
  rsEnterFieldSize = 'Enter field size';
  rsFieldSizeMustNotExceed = 'Field size must not exceed 1000 characters.';
  rsFieldSizeMustRange = 'Field size must be in range from 1 to 2000 characters.';
  rsEnterPrecisionNum = 'Enter precision of number';
  rsPrecisionMustNotExceed = 'Precision must not exceed 10 digits.';
  //rsFieldTypeMustNumeric = 'Field type must be numeric for selected function.';
  rsArticles = 'Articles';
  rsForum = 'Forum';
  rsVideoLessons = 'Video lessons';
  rsARoleWithThisNameExists = 'A role with this name exists.';
  rsAUserWithThisNameExists = 'A user with this name exists.';
  rsAnIntfWithSameNameExists = 'An interface with this name exists.';
  rsDownload = 'Download';
  rsUnableFindFileManager = 'Unable find a supported file manager from list: '
    +'Nautilus, Thunar, Nemo, Dolphin, PCManFM.';
  rsUnknownIdentifier = 'Unknown Identifier';
  rsRunning = 'Running';
  rsPaused = 'Paused';
  rsDebugFormCaption = 'Debug: %s [%s]';
  rsStepIntoBn = 'Step Into <F7>';
  rsStepOverBn = 'Step Over <F8>';
  rsStepInto = 'Step Into';
  rsStepOver = 'Step Over';
  rsBreakpoints = 'Breakpoints';
  rsRowNum = 'Row Num';
  rsDeletingBreakpoint = 'Deleting breakpoint';
  rsDeletingAllBreakpoints = 'Deleting all breakpoints';
  rsDeleteAll = 'Delete all';
  rsContinueRunBn = 'Continue running <F9>';
  rsCanNotGetValue = '[Can not get value]';
  rsGetValue = 'Get value under cursor';
  //rsRecDeletedYouRefresh = 'This record was deleted by you. Refresh data?';
  //rsAccessDenied = 'Access denied';
  //rsLimit64KMsg = 'The total size of the form fields exceeds 64 KB. Reduce the'
  //  +' size of the fields or delete unnecessary.';
  rsTextPropNotAvail = 'This property is not available for a calculated label.';
  rsClearFields = 'Clear fields';
  rsAddLangFilter = 'DataExpress language files (*.lng)|*.lng';
  rsAboutLang = 'About language';
  rsAboutLangMsg = 'Author: %s/Version: %s/Description: %s';
  rsManualRefresh = 'Manual refresh';
  rsSaveChangesBeforeTestMsg = 'The database structure was changed. Save '
    +'changes and continue testing?';
  rsSaveChangesBeforeImportMsg = 'The database structure was changed. Save '
    +'changes before import project.';
  rsInvalidUsernamePwd = 'Invalid username or password.';
  rsEnterUsername = 'Enter username';
  rsSingleModeNotAllowed = 'For this user single-user mode not allowed.';
  rsDoNotShowInUserList = 'Do not show in user list';
  rsNoDataSourceSpecif = 'No data source is specified.';
  rsObjectMustSelForDataSource = 'An object must be selected for this data '
    +'source.';
  rsDuplicateField = 'Duplicate field';
  rsDataSource = 'Data source';
  rsTreeAll = '[All]';
  //rsCantEditRecMsg = 'You can not modify the record. The record will be opened in view mode.';
  rsViewOnlyCaption = '%s (View only)';
  rsNewRecordCaption = '%s (New record)';
  rsEditingCaption = '%s (Editing)';
  rsExpandLevels = 'Expand levels';
  rsObjectButtonHint = 'Select <Shift-Enter>';
  rsSaveChangesHint = 'Save changes <Ctrl-Enter>';
  rsCancelChangesHint = 'Cancel changes <Shift-Esc>';
  rsCloseEditWindowHint = 'Close window <Shift-Esc>';
  rsAcceptSelectionHint = 'Accept selection <Enter>';
  rsCancelSelectionHint = 'Cancel selection <Esc>';
  rsCloseListWindowHint = 'Close window <Esc>';
  rsShowHelpHint = 'Show help <F1>';
  rsFieldDateDetailCantParam = 'A field can not be a parameter, because it is used in grouping by date.';
  rsOnlyTextFieldCanBeParamWithMergeFunc = 'Only the text field can be a parameter with function [%s].';
  rsAbort = 'Abort';
  rsAbortOperation = 'Abort operation?';
  rsDonate = 'Donate';
  //rsDXVersion = 'Version: %s';
  rsCopyrightText = 'Copyright © 2015-2024 Pavel Duborkin';
  rsFloatFieldOutRange = 'The value [%s] of field [%s] out of range.';
  rsFloatOutRange = 'The value [%s] out of range.';
  rsTextTooLong = 'Text too long (%d > %d).';
  rsImportAllFieldsLostMsg = 'We could not find any matching fields in the database. '
    +'Make sure that the field names in the first line are correct and that '
    +'the file''s encoding is correct.';
  rsImportFieldsLostMsg = 'The following fields are not found in the database: '
    +'%s. Make sure the field names are correct.';
  rsImportUnsupportedFieldsMsg = 'The following fields found '
    +'have an unsupported field type: %s.';
  rsImportAnyway = 'Do you want to import anyway?';
  rsNumberOutOfRange = 'Number out of range: %s';
  rsContinue = 'Continue';
  rsImportCheckErrorMsg = 'During the verification of the input data, the '
    +'following errors were found:';
  rsContinueImportMsg = 'To continue importing anyway, click [Continue]. The rows containing errors will not be imported.';
  rsNoDataToImport = 'No data to import. The file is empty.';
  rsCheckingData = 'Checking data...';
  rsAfterImportErrsMsg = 'When importing, errors occurred. The following rows were not imported:';
  rsLoadingData = 'Loading data...';
  rsSelectFile = 'Select file';
  rsParamRequired = 'Parameter [%s] is required.';
  rsCellRequired = 'Cell [%s] is required.';
  rsBuiltInActions = 'Built-in actions';
  rsSelectAction = 'Select action';
  rsTreeFilter = '(filter)';
  rsActionNotFoundMsg = 'Could not find an action with the ID %s. Make sure '
    +'that the corresponding extension is added.';
  rsPasteSpecial = 'Paste special';
  rsPasteGUID = 'Paste GUID';
  rsPasteImageAsBase64 = 'Paste image as base64';
  rsLoadImageBase64ErrorMsg = 'Could not load the image from the extension '
    +'module. Make sure the image is valid format. Error message: %s';
  //rsParamNoTVariantArray2d = 'The parameter of procedure [%s] under number %d '
  //  +'should be of type TVariantArray2d.';
  rsExtensionsModules = 'Extensions modules';
  rsPasteImgError = 'An error occurred while pasting image: %s';
  //rsFailedToShowActionCmp = 'Failed to init action components: %s';
  //rsActionSourceCmpNotFound = 'Message from dependent component [%s]: source '
  //  +'component [%s] not found or not a list';
  rsActionExecError = 'An error occurred while executing action [%s].%s';
  rsSaveRecordBeforePrint = 'Save record before print';
  rsInsertAction = 'Insert action...';
  rsLogicalExpr = 'Logical expression';
  rsIfCondition = 'If %s';
  rsElseIfCondition = 'Else If %s';
  rsElseCondition = 'Else';
  rsComment = 'Comment';
  //rsActionsEditorTitle = 'Actions editor: %s';
  rsActionNotSelected = 'Action not selected';
  rsIf = 'If';
  rsElseIf = 'Else If';
  rsShowMessage = 'Show message';
  rsMessage = 'Message';
  rsMsgType = 'Message type';
  rsInformation = 'Information';
  rsConfirmation = 'Confirmation';
  rsRetry = 'Retry';
  rsIgnore = 'Ignore';
  rsNoToAll = 'No to all';
  rsYesToAll = 'Yes to all';
  {rsActionsWhen = 'Actions when...';
  rsCreateForm = 'Create form';
  rsDestroyForm = 'Destroy form';
  rsFieldChange = 'Field change';
  rsValidateRecord = 'Validate record';
  rsAfterOpenData = 'After open data';
  rsAfterGoToRecord = 'After go to record';
  rsAfterInsertRecord = 'After insert record';
  rsAfterEditRecord = 'After edit record';
  rsAfterSaveRecord = 'After save record';
  rsAfterCancelChanges = 'After cancel changes';
  rsAfterDeleteRecord = 'After delete record';
  rsAfterCloseData = 'After close data';
  rsBeforeOpenData = 'Before open data';
  rsBeforeGoToRecord = 'Before go to record';
  rsBeforeInsertRecord = 'Before insert record';
  rsBeforeEditRecord = 'Before edit record';
  rsBeforeSaveRecord = 'Before save record';
  rsBeforeCancelChanges = 'Before cancel changes';
  rsBeforeDeleteRecord = 'Before delete record';
  rsBeforeCloseData = 'Before close data';
  rsShowEditWindow = 'Show edit window';
  rsCloseEditWindow = 'Close edit window';}
  rsSaveYourChanges = 'Save your changes before exiting?';
  rsComponentWithFieldNameNotFound = 'Component with the specified field name '
      +'[%s] not found.';
  rsTableChangesLimitMsg = 'The form [%s] has been changed 255 times. The limit on '
    +'the number of database table changes has been reached. Use the GBAK '
    +'utility to back up and restore the database.';
  rsAddExtModule = 'Add extensions module';
  rsHierarchy = 'Hierarchy';
  rsLevelCount = 'Level count';
  rsHierarchyCantReferItself = 'The hierarchy element can not refer to itself.';
  rsHierarchyCantReferChild = 'A hierarchy element can not reference its child'
    +' element.';
  rsOpenDemoDB = 'Open demo database';
  rsCanNotChangeListSourceMsg = 'Can not change source because object fields '
    +'are used in query/report [%s].';
  rsTestFormErrorFounds = 'Errors were found during compilation. Open the '
    +'script editor to fix the errors.';
  rsInsertMore = 'Insert...';
  rsDBFunction = 'DB-function';
  rsDBAVGFunc = 'DBAVG - calculate average value';
  rsDBCOUNTFunc = 'DBCOUNT - count';
  rsDBCOUNTDFunc = 'DBCOUNTD - count of distinct';
  rsDBGETFunc = 'DBGET - get field value';
  rsDBGETIDFunc = 'DBGETID - get id of record';
  rsDBGETBYIDFunc = 'DBGETBYID - get field value by id of record';
  rsDBMAXFunc = 'DBMAX - maximum value';
  rsDBMERGEFunc = 'DBMERGE - merge distinct values';
  rsDBMERGEALLFunc = 'DBMERGEALL - merge all values';
  rsDBMINFunc = 'DBMIN - minimum value';
  rsDBSUMFunc = 'DBSUM - calculate sum';
  rsValueFromForm = 'Value from form';
  rsFunctionGET = 'Function GET';
  rsIDCurrentRec = 'ID current record';
  rsTakeSelectionSingleQuotes = 'Take the selected text in single quotes';
  rsTakeSelectionDoubleQuotes = 'Take the selected text in double quotes';
  rsTakeSelectionSquareBr = 'Take the selected text in square brackets';
  rsTakeSelectionCurlyBr = 'Take the selected text in curly brackets';
  rsCondition = 'Condition';
  rsAddGroup = 'Add group';
  rsAddForm = 'Add form';
  rsDeleteForm = 'Delete form';
  rsDeleteGroup = 'Delete group';
  rsEnterGroupName = 'Enter group name';
  rsGroupName = 'Group name';
  rsGroupNameIsRequired = 'Group name is required.';
  rsGroupNameInvalidChars = 'You can not use characters \ and | in the group name.';
  rsThisGroupAlreadyExists = 'This group already exists.';
  rsRenameGroup = 'Rename group';
  rsRemoveFromGroup = 'Remove from group';
  rsFindForm = 'Find form';
  rsEmptyGroupsNotSaved = 'After exiting the designer empty groups are not '
    +'saved: %sContinue?';
  rsNullToZero = 'Null to 0';
  rsAdditionalFieldsInList = 'Additional fields in the list';
  rsExtraListWidth = 'Extra list width';
  rsColumnWidth = 'Column width';
  rsHideList = 'Hide list';
  rsHideButton = 'Hide button';
  rsLinkToForm = 'Link to form';
  rsIncludeInSearch = 'Include in search';
  //rsEReadErrorMsg = 'Perhaps you are trying to open a database created in a newer version ' +
  //	'of the application. Make sure you are using the latest version of the application.';
  rsCheckEmptyObjMsg = 'Records with an empty object are detected. When the "'
    +'Required" property is set, these entries may not be visible.';
  rsCanRenameLabelMsg = 'You can rename the label to open the expression '
    +'editor.';
  rsPivotGridNotFound = 'Pivot grid not found: %s';
  rsGrid = 'Grid';
  rsRowSelect = 'Row select';
  rsEllipsisInCell = 'Ellipsis in cell';
  rsShowHints = 'Show hints';
  rsMoveWhenScroll = 'Move when scroll';
  rsResetSettings = 'Reset settings';
  rsResetSettingsMsg = 'Reset all grid settings?';
  rsCellBkgnd = 'Cell background';
  rsAltCellBkgnd = 'Alt. cell background';
  rsSelectedText = 'Selected text';
  rsInactiveSelected = 'Inactive selected';
  rsInactiveSelectedText = 'Inactive selected text';
  //rsTestText = 'The quick brown fox jumps over the lazy dog.';
  rsOrderVisibleColumns = 'Order and visibility of columns';
  rsAllowChangeSort = 'Allow change sort';
  rsHighlightCurLine = 'Highlight current line';
  rsAllowColumnsMove = 'Allow columns move';
  rsTabStop = 'Tab stop';
  rsDisableTabStopMsg = 'Disable Tab stop in non-editable fields';
  rsDisableTabStopMsg2 = 'Disable Tab stop in non-editable fields?';
  rsFailedToCreateDB = 'Failed to create database.%s';
  rsErrorCheckDB = 'Error occured when checking database.%s';
  rsErrorLoadMeta = 'Error occured when loading metadata.%s ';
  rsErrorReadingFormMetadata = 'Error reading form metadata: %sPerhaps you are'
    +' trying to open a database created in a newer version of the application'
    +'. Make sure you are using the latest version of the application.';
  rsErrorRunDB = 'An error occurred while the database was running.%sPress '
    +'Shift key while opening the database to enter the Designer.';
  rsDeleteConnectInfoFromList = 'Delete connection information from list?';
  rsAppendCurrentDB = 'Append current database';
  rsConfirmDeleteGroup = 'Do you want to delete group?';
  rsAppendConnect = 'Append connection';
  rsCantMoveGroupExists = 'Can not move a group. This group already exists.';
  rsConnectSettings = 'Connection settings';
  rsConnectName = 'Connection name';
  rsDBLocation = 'Database location';
  //rsRemoteDB = 'Remote database';
  rsPwdSYSDBA = 'Password SYSDBA';
  rsEnterConnectName = 'Enter connection name.';
  rsConnectNameInvalidChars = 'You can not use character | in connection name';
  rsConnectNameExists = 'This connection name already exists.';
  rsConnectAlreadyExists = 'One or more connections with the same database '
    +'location and user name were found: %s Do you want to add a new '
    +'connection anyway?';
  rsConnectSaveAlreadyExists = 'One or more connections with the same database '
    +'location and user name were found: %s Do you want to save this '
    +'connection anyway?';
  rsConnectionAlreadyAdded = 'Connection [%s] has been already added.';
  rsCantDelCurConnect = 'You can not delete the current connection.';
  rsCopyConnect = 'Copy connection';
  rsEnterDBLocation = 'Enter database location.';
  rsOnlyDevelCanEnterDesigner = 'Only the developer can enter the designer.';
  rsExampleConnectName = 'Example: My Database';
  rsExampleDBLocation = 'Example: computer_name:d:\mybases\mydb.fdb or 10.2'
    +'0.11.15:/home/user/base.fdb or e:\localdb\db.fdb';
  rsHintTemplateDefaultFolder = 'Default folder (<application folder>\'
    +'templates)';
  rsHintOutputDefaultFolder = 'Default folder (user temporary folder)';
  rsConnectNotFound = 'Connection [%s] not found.';
  rsConnectNameCopy = '%s - copy';
  rsAppendConnectToExistDB = 'Append a connection to an existing database';
  rsChangeConnectOrGroup = 'Change connection settings or group name';
  rsDeleteConnectOrEmptyGroup = 'Delete connection or empty group';
  rsCurrentConnectSettingsChanged = 'The settings for the current connection '
    +'have been changed. Reconnect to make the changes take effect.';
  rsConnectionsTreeFilter = 'Filter by connection name or database location';
  rsOpenLastDB = 'Open the last database';
  rsOpenConnectList = 'Open connection list';
  rsApplyFilter = 'Apply filter <Ctrl-Enter>';
  rsReference = 'Reference';
  rsUserManual = 'User manual';
  rsPascalScriptReference = 'Pascal Script reference';
  rsOpenFileLocation = 'Open file location';
  rsCantDelFormMsg = 'Can not delete form [%s], because it used in %s.';
  rsCantDelFormMsgRpt = 'reports:%s';
  rsCantDelFormMsgQry = 'queries:%s';
  rsCantDelFieldMsg = 'Can not delete field [%s], because it used in %s.';
  rsScript = 'Script';
  rsCreateFormModuleMsg = 'The form module [%s] has not yet been created. '
    +'Create a module?';
  rsFmtDateMonth = 'January February March April May June July August September October November December';
  rsURLSuccessCopied = 'URL was successfully copied to clipboard:%s';
  rsCopyURLToClipboard = 'Copy URL to clipboard';
  rsCheckAll = 'Check all';
  rsUncheckAll = 'Uncheck all';
  rsStartupAction = 'Startup action';
  rsLoopLinkDetect = 'Found a circular dependency between queries: %s';
  rsImageFieldNotFound = 'Image field [%s] not found.';
  rsFileFieldNotFound = 'File field [%s] not found.';
  rsIIFNullDetect = ' A null is detected. Verify that all operands of the logical expression have a specific values (not empty). To ensure a certain value, use function NZ or an additional null check. See also function NULL.';
  rsIIFParamNotLogic = 'The type of the result of evaluating a logical '
    +'expression is not a logical type (true or false).';
  rsErrorLoadLib = 'Error while loading library %s. Error message: %s';
  //rsScriptErrorMsg = 'Exception class: %sException message: %sModule name: %'
  //  +'sModule kind: %sProcedure name: %s';
  rsScriptErrorMsg = '%sException class: %sModule name: %'
    +'sModule kind: %sProcedure name: %s';
  rsExtension = 'Extension';
  rsScriptError = 'Script error';
  rsScriptErrorOccurred = 'An error occurred while executing the script.%s';
  rsIncompatibleValueForField = 'The result of evaluating the expression is '
    +'incompatible with the field. For example, you can not write text '
    +'into a numeric field.';
  rsCircularLinkBetweenFieldQuery = 'A circular link between the field %s '
    +'and the query [%s] was found.';
  rsLoopDetectSourceFilter = '%s (source filter %d)';
  rsLoopDetectSQL = '%s (SQL expression)';
  rsLoopDetectOutputFilter = '%s (output filter)';
  rsLoopDetectCalcField = '%s (calc field %s)';
  rsNumberOfDigitsAfterPoint = 'Number of digits after the decimal point';
  rsGroupDigits = 'Group digits';
  rsConnectToDB2 = 'Connect to database...';
  rsPreparingToWork = 'Preparing to work...';
  rsCantRenameReportMsg = 'You can not rename the report because the program '
    +'can not rename it in related actions. This operation only works in designer mode.';
  rsCantRenameRpFieldMsg = 'The field was renamed, but the program can not '
    +'rename it in linked actions, because this operation only works in '
    +'designer mode. Incorrect field names can lead to errors in the work '
    +'actions. Unfortunately, you can not save selection settings with '
    +'new field names in user mode.';
  rsCmpUsedInActions = '%s [%s] is used in actions:%s%s';
  rsShowOldFieldNameBeforeRename = 'The message shows the old name of the '
    +'field before renaming, which is still stored in the related actions.';
  rsCantHideRpFieldMsg = 'You can not hide this field because it is used in '
    +'actions.';
  rsChangeObjSourceActionsMsg = 'You have changed the source data object. If '
    +'the above actions are used object field, the action may not work '
    +'properly.';
  rsCantDownloadHelpIndex = 'Can not download help index. Check the connection'
    +' to the Internet. Error message: %s';
  rsHelpTopicNotAvailable = 'Sorry, the help topic is not yet available. Try '
    +'again later.';
  rsNameNotFoundInHelpIndex = '[%s] not found in help index.';
  rsInvalidCharInModuleName = 'You can not use the following characters in the names of modules: \ / : * ? " < > |';
  rsConnectionError = 'Connection error';
  {rsDBConnectLost = 'Database connection lost. Further normal '
    +'operation of the application is not possible. Make sure that the database '
    +'server is available and run the application again. Do you still want to '
    +'continue the application?';
  rsDBConnectLostDesign = 'Database connection lost. Make '
    +'sure that the database server is available and run the application again.'; }
  rsDatabaseConnectLost = 'Unable to establish a connection to the database. Make sure that the '
    +'database server is available and run the application again.';
  //rsUsedDefaultFieldNameInHeader = 'You have entered one space. This means '
  //  +'that the original field name in the header will be used.';
  rsReportWindow = 'Report window';
  rsParamExists = 'Report parameter with same name already exists.';
  rsPrintFieldExists = 'Field with same name already exists in calculations '
    +'when printing.';
  rsTotalsExists = 'Totals with same caption already exists.';
  rsFieldNameExists = 'Field with same name already exists.';
  rsSqlFieldNameExists = 'Field with same SQL name already exists.';
  rsReportComponentFieldNameExists = 'Component of report window with same field name already exists.';
  rsCalcWhenPrint = 'Calculations when printing';
  rsWhenPrinting = 'When printing';
  rsRpParamsChanged = 'Report parameters have been changed. Need to refresh '
    +'the report. Continue?';
  rsEndingsReserved = 'Endings _not, _null, _text, _begin, _end in field names are reserved '
    +'by the report printing subsystem.';
  rsDBNotSupport = 'Database is not supported by this version of DataExpress. Save the database in the DataExpress version dated 02.11.2018 and try again.';
  rsStartupActions = 'Startup actions';
  rsButtonActions = 'Button actions';
  rsFormActions = 'Form actions';
  rsLoadDataErrorMsg = 'Could not load script code.%sError message: %sModule name: %sModule kind: %s';
  rsInvalidUsingGroupTag = 'Using more than one tag "group" in the same '
    +'paragraph or the same row of the table is invalid.';
  rsInvalidUseTagForm = 'Tags {form|%s} and {form|%s} are in the same paragraph. '
    +'Using more than one tag "form" in the same paragraph is invalid. ';
  rsInvalidUseTagGrid = 'Tags {grid|%s} and {grid|%s} are in the same table row. '
    +'Using more than one tag "grid" in the same table row is invalid.';
  rsPrintAborted = 'Print aborted.';
  rsTagWithoutClosingBracket = 'Tag {%s without a closing bracket }.';
  rsFieldWithoutClosingBracket = 'Field [%s without a closing bracket ].';
  rsPrintErrors = 'Print errors';
  rsCannotUseGroupDateAndFunc =
    'Cannot use both function and grouping by date.';
  rsDeleteRecordMsg = 'Are you sure you want to delete the record?';
  {rsDBInRemoteMode = 'Make sure all users connect to the database remotely. In'
    +' the Connection Settings (File -> Connect) should be checked "remote '
    +'database".';   }
  rsTreeInListWindow = 'If there is a tree in the list window';
  rsRefreshTreeFirstShow = 'Refresh the tree only at the first show window';
  rsRefreshTreeEveryShow = 'Refresh the tree every time the window is shown';
  rsNewThemeLangApply = 'The selected language or theme will be applied after the application is restarted.';
  rsNewLangApply = 'The selected language will be applied after the application is restarted.';
  rsUseTheme = 'Use theme';
  rsErrorSelCond = 'An error was found in the selection condition: %sIn this '
    +'regard, the selection condition will be ignored.';
  //rsOptimizeSelection = 'Optimize selection records';
  rsErrorCreatingTab = 'An error occurred while creating tab [%s].%s';
  rsImgProcessPrint = 'Processing when printing';
  rsSaveSize = 'Save size';
  rsReduceTo = 'Reduce to';
  rsErrorReadingCfg = 'Unable to read settings from dataexpress.cfg file. Maybe the file is being used by another process. Error message:%s';
  rsErrorWritingCfg = 'Unable to write settings to dataexpress.cfg file. Maybe the file is being used by another process. Error message:%s';
  rsNoThumbnail = 'No thumbnail';
  rsThumbnailSize = 'Thumbnail size';
  rsShowThumbnail = 'Show thumbnail only';
  rsReadMetadataError = 'The metadata of the forms was read with errors or ' +
    'unrecognized. Some features of the database may not work or work ' +
    'incorrectly. Maybe the database was created in a newer version of the ' +
    'application. Please make sure you are using the latest version of DateExpress.%s';
  rsSaveToFile = 'Save to file';
  rsNoAction = 'No action';
  rsFileAction = 'File action';
  rsSaveToOutputFolder = 'Save to output folder';
  rsOpenWithApp = 'Open with application';
  rsSentToPrinter = 'Sent to printer';
  rsShowButton = 'Show button';
  rsStorageFolderNotSpec = 'Storage folder not specified.';
  rsBoolean = 'Boolean';
  rsNumberOrText = 'Number or Text';
  rsUnknown = 'Unknown';
  rsExpectedOperationOrBracket = 'Expected operation or detected excess closing bracket.';
  //rsIncompatibleTypesGotExpect = 'Incompatible types. Got %s expected %s.';
  rsExpectedClosingBracketOrOp = 'Expected closing bracket or operation.';
  rsInvalidNumberArgsFunc = 'Invalid number arguments of function %s. Got %d expected %d.';
  rsIncompatibleArgOfFunc = 'Incompatible argument #%d of function %s. Got %s '
    +'expected %s.';
  rsObjectNotHaveLinkForm = 'The object [%s] does not have an linked form.';
  rsFieldNotObject = 'Field [%s] not is objects.';
  rsFillTableFilter = 'Fill table filter';
  rsCalcField = 'Calculation field: %s';
  rsInvalidOpForType = 'Operation %s is not valid for %s type.';
  rsInvalidOpForTypeExpected = 'Operation %s is not valid for %s type. Expected %s.';
  //rsNotValidCompareDifTypes = 'It is not valid to compare operands of '
  //  +'different types: %s and %s. Operands must be of the same type.';
  rsOpNotValidForDifOperands = 'Operation %s is not valid for operands of '
    +'different types: %s and %s. Operands must be of the same type.';
  rsCopyFormStyle = 'Copy form style';
  rsSaveProject = 'Save project';
  //rsSaveProjectError = 'An error occurred while saving the project:%s';
  rsLimit64KMsg = 'The total size of the fields on the form [%s] exceeds 65535'
    +' bytes and is %d bytes. The total size of the fields should be reduced '
    +'by %d bytes (%n chars). To do this, reduce the size of text fields or delete '
    +'unnecessary fields.';
  rsLimit64KOldMsg = 'The total size of the fields on the form [%s] exceeds 65535'
    +' bytes and is %d bytes. The total size of the fields should be reduced '
    +'by %d bytes (%n chars). To do this, delete the newly added or unnecessary '
    +'fields. Reducing the size of text fields in this case does not help.';
  rsLimit64KObjMsg = 'The total size of the fields, with the objects and object '
    +'fields, on the form [%s] exceeds 65535 '
    +'bytes and is %d bytes. The total size of the fields should be reduced '
    +'by %d bytes (%n chars). To do this, delete some fields of objects, reduce '
    +'the size of fields associated with objects or fields of objects, reduce '
    +'the size of form fields.';
  rsSaveProjectError = 'Failed to save project.';
  rsNotLoseChanges = 'On a note. In order not to lose the '
    +'done changes, make the project export (menu Tools - Project Export). '
    +'After successfully connecting to the database, import the project '
    +'(Tools - Import Project) to restore the changes made.';
  {rsSaveFormsError = 'Unable to save forms metadata.%sIn order not to lose the '
    +'done changes, make the project export (menu Tools - Project Export). '
    +'After successfully connecting to the database, import the project '
    +'(Tools - Import Project) to restore the changes made.';    }
  rsRecSizeLimit = 'Total record size exceeded 65535 bytes. Reduce the size of'
    +' text fields, including in linked forms, or remove unnecessary fields.';
  rsExportProjectError = 'An error occured when exporting project.';
  rsImportProjectError = 'An error occured when importing project.';
  rsExportDataError = 'An error occured when exporting data.';
  rsDataImportError = 'An error occured when importing data.';
  rsMergeProjectsError = 'An error occured when merging projects.';
  rsErrorMessage = 'Error message: %s';
  rsExceptionClass = 'Exception class: %s';
  rsDeleteRecsError = 'An error occured when deleting records.';
  rsRecalculateError = 'An error occured when recalculating.';
  rsSaveModulesError = 'An error occured when saving modules.';
  rsSaveReportsError = 'An error occured when saving reports.';
  rsSaveUsersError = 'An error occured when saving users.';
  rsSaveDBSettingsError = 'An error occured when saving database settings.';
  {rsResizeFieldsError = 'Could not resize the following fields:%s%sThe resize '
    +'operation has been canceled for these fields. The form metadata will be '
    +'saved again.'; }
  rsCheckResizeFieldsError = 'Failed to save project. Unable to resize the following fields:';
  rsSaveFormsOk = 'Forms metadata saved successfully.';
  rsGetChangesCountError = 'Unable to determine the number of changes to the '
    +'database tables.';
  rsInitFieldsError = 'Cannot initialize fields.';
  //rsResizeFieldsCancel = 'Resize these fields has been canceled:%s';
  rsResizeFieldsError = 'Unable to resize fields.%sResize '
    +'following fields has been canceled:%s';
  rsSaveFormsAgain = 'Now the form metadata will be saved again.';
  rsSaveFormsError = 'Failed to save forms.';
  rsTemplateWizard = 'Template wizard';
  rsCurrentForm = 'Current form';
  rsDataSources = 'Data sources';
  rsFindField = 'Find field';
  rsSetCurrentForm = 'Set current form';
  rsOpenTemplatesFolder = 'Open templates folder';
  rsTemplatesFilesFilter = 'Template files *.docx, *.docm, *.odt, *.ods, *.xml'
    +', *.html|*.docx;*.docm;*.odt;*.ods;*.xml;*.html';
  rsOpenTemplateFile = 'Open template';
  rsCouldNotFindFormRp = 'Could not find form or report.';
  rsCopyTagGrid = 'Copy tag grid';
  rsCopyTagForm = 'Copy tag form';
  rsCopyTagGroup = 'Copy tag group';
  rsCopyTagEnd = 'Copy tag end';
  rsCurrentReport = 'Current report';
  rsAgreement = 'Agreement';
  rsLicenseAgreement = 'License agreement';
  rsChart = 'Chart';
  rsChartNotFound = 'Chart not found: %s';
  rsVerticalBars = 'Vertical bars';
  rsHorizontalBars = 'Horizontal bars';
  rsPie = 'Pie';
  rsGraph = 'Graph';
  rsArea = 'Area';
  rsOrientation = 'Orientation';
  rsStyle = 'Style';
  rsMarks = 'Marks';
  rsSeriesNum = 'Series %d';
  rsSeries = 'Series';
  rsGroupingType = 'Grouping type';
  rsFilling = 'Filling';
  rsPoint = 'Point';
  rsFrame = 'Frame';
  rsLine = 'Line';
  rsDepth = 'Depth';
  rsEffects = 'Effects';
  rsDensityPerc = 'Density, %';
  rsPoints = 'Points';
  rsAreaLines = 'Area lines';
  rsEdges = 'Edges';
  rsShadow = 'Shadow';
  rsTransparency = 'Transparency';
  rsOffsetX = 'Offset X';
  rsOffsetY = 'Offset Y';
  rsMarksOfData = 'Marks of data';
  rsContent = 'Content';
  rsPosition = 'Position';
  rsByCenter = 'By center';
  rsRotateLabels = 'Rotate labels';
  rsMargins = 'Margins';
  rsMarginLeft = 'Margin left';
  rsMarginTop = 'Margin top';
  rsMarginRight = 'Margin right';
  rsMarginBottom = 'Margin bottom';
  rsDistance = 'Distance';
  rsCalloutAngle = 'Callout angle';
  rsLinkLines = 'Link lines';
  rsGeneralView = 'General view';
  rsBuildArea = 'Build area';
  rsMarginsExternal = 'Margins external';
  rsCategoryAxis = 'Category axis';
  rsDistanceToAxis = 'Distance to axis';
  rsAxisLine = 'Axis line';
  rsTicks = 'Ticks';
  rsLength = 'Length';
  rsInnerLength = 'Inner length';
  rsValueAxis = 'Value axis';
  rsIntervals = 'Intervals';
  rsMaxLength = 'Max length';
  rsMinLength = 'Min length';
  rsMargin = 'Margin';
  rsFoot = 'Foot';
  rsLegend = 'Legend';
  rsDisplay = 'Display';
  rsHorizontalGrid = 'Horizontal grid';
  rsVerticalGrid = 'Vertical grid';
  rsSymbolFrame = 'Symbol frame';
  rsSymbolWidth = 'Symbol width';
  rsMarginsLeftRight = 'Margins left/right';
  rsMarginsTopBottom = 'Margins top/bottom';
  rsSpacing = 'Spacing';
  rsRectangle = 'Rectangle';
  rsRoundRect = 'Round rectangle';
  rsRoundSide = 'Round side';
  rsEllipse = 'Ellipse';
  rsSideBySide = 'Side by side';
  rsStack = 'Stack';
  rsAround = 'Around';
  rsInside = 'Inside';
  rsOutSide = 'Outside';
  rsTopCenter = 'Top center';
  rsBottomCenter = 'Bottom center';
  rsTopRight = 'Top right';
  rsCenterRight = 'Center right';
  rsBottomRight = 'Bottom right';
  rsTopLeft = 'Top left';
  rsCenterLeft = 'Center left';
  rsBottomLeft = 'Bottom left';
  rsMark = 'Mark';
  rsMarkAndValue = 'Mark and value';
  rsCircle = 'Circle';
  rsDiamond = 'Diamond';
  rsTriangle = 'Triangle';
  rsStar = 'Star';
  rsFullStar = 'Full star';
  rsHexagon = 'Hexagon';
  rsNothing = 'Nothing';
  rsGlow = 'Glow';
  rsChocolate = 'Chocolate';
  rsTextEditor = 'Text editor';
  rsLinesNum = '%d lines';
  rsElementsNum = '%d elements';
  rsHorizontalLines = 'Horizontal lines';
  rsVerticalLines = 'Vertical lines';
  rsDiagonalToLeft = 'Diagonal to left lines';
  rsDiagonalToRight = 'Diagonal to right lines';
  rsCross = 'Cross';
  //rsClearFill = 'Clear fill';
  //rsSolidFill = 'Solid fill';
  rsDashDot = 'Dash dot';
  rsDashDotDot = 'Dash dot dot';
  rsChartType = 'Chart type';
  rsPrintChart = 'Print chart';
  rsOriginalSize = 'Original size';
  rsSpecifiedSize = 'Specified size';
  rsCategories = 'Categories';
  rsOverlapPolicy = 'Overlap policy';
  rsHideNighbour = 'Hide nighbour';
  rsTextLengthExceedMemo = 'Text length exceeded. Adding text to a memo is not possible.';
  rsModifiedState = '(modified)';
  rsObjFieldNotUseDBUnique = 'You cannot use the field of object [%s].';
  rsTooManyIndex = 'Too many indexes, can not add index. In some form, there '
    +'are too many date fields or objects. To resolve this issue'
    +' you need to review the structure of the form, remove unnecessary '
    +'objects and date fields.';
  rsMultilineLabelsNotAllow = 'Multiline labels not allow.';
  rsSelectLang = 'Select language';
  rsFileNotValidDatabase = 'The file is not a valid Firebird database.';
  rsFormComponentNameExists = 'A form with the same component name already '
    +'exists.';
  rsAddToFilter = 'Add to filter';
  rsClearFilter = 'Clear all filters';
  rsEditRoles = 'Compare and edit roles';
  rsDeleting = 'Deleting';
  rsApplySelection = 'Apply selection';
  rsCompareAndEditRoles = 'Compare and edit selected roles';
  rsErrorCloseApp = 'An error occurred while closing the application.';
  rsClickToLoadImage = 'Click to load image';
  rsConfirmDeleteImage = 'Are you sure you want to delete the image?';
  rsEnterImageName = 'Enter image name';
  rsInvalidCharInImageName = 'You can not use the following characters in the names of images: \ / : * ? " < > |';
  rsImageAlreadyExists = 'The image with same name already exists.';
  rsAddImage = 'Add image';
  rsRenameImage = 'Rename image';
  rsFailedToSaveImage = 'Failed to save image.';
  rsGallery = 'Gallery';
  rsSelectFromGallery = 'Select from gallery';
  //rsImageUsedInMsg = 'Image used in:%sContinue to delete?';
  rsImageUsedInCmp = 'Image used in components:';
  rsImageUsedInAct = 'Image used in actions:';
  rsContinueToDelete = 'Continue to delete?';
  rsFailedToWriteData = 'Failed to write data to the database. You can try '
    +'again.';
  rsPickFromListWindow = 'Pick from list window';
  rsEnterDelimiter = 'Enter delimiter.';
  rsRestoreDefColor = 'Restore default color';
  rsModule = 'Module';
  rsOpenHomePage = 'Open homepage';
  rsHomepage = 'Homepage';
  rsDisabled = 'Disabled';
  rsReplaceActionMsg = 'Do you want to replace the action?';
  rsCopyCells = 'Copy cells';
  rsPasteCells = 'Paste cells';
  rsCalcAggFuncError = 'The current record is being edited. Calling the '
    +'function will implicitly save the record without checking the data entry.';
  rsSimpleFormCantDataSource = 'Simple form [%s] cannot be a data source, because it does not store data.';
  rsErrorDetails = 'Error details:';
  rsTakeValue = 'Take value';
  rsUnlimited = 'Unlimited';
  rsCantReplaceMemo = 'You cannot replace an unlimited memo with another component.';
  rsCantMakeUnlimitedMemo = 'You cannot make a memo unlimited, since it is '
    +'used in the following components:%sThese components can not be used '
    +'unlimited memo.';
  rsFontSize = 'Font size';
  rsReset = 'Reset';
  rsHideColumn = 'Hide column';
  rsLayout = 'Layout';
  rsLayoutTop = 'Top';
  rsCenter = 'Center';
  rsLayoutBottom = 'Bottom';
  rsAuto = 'Auto';
  rsPangram1 = 'Jackdaws love my big sphinx of quartz.';
  rsPangram2 = 'The quick brown fox jumps over the lazy dog 1234567890 times.';
  rsPangram3 = 'Pack my box with five dozen liquor jugs.';
  rsPangram4 = 'The jay, pig, fox, zebra and my wolves quack!';
  rsAdjustByCompWidth = 'Adjust by component width';
  rsIndicator = 'Indicator';
  rsClearTable = 'Clear table';
  rsClearTableMsg = 'Are you sure you want to clear the entire table?';
  rsDeleteFieldMsg = 'Are you sure you want to delete the field?';
  rsDeleteBreakpointMsg = 'Are you sure you want to remove the breakpoint?';
  rsDeleteAllBreakpointsMsg =
    'Are you sure you want to remove all breakpoints?';
  rsClearAllValuesMsg = 'Are you sure you want to clear all values?';
  rsCancelAddFieldsMsg = 'Are you sure you want to cancel adding fields?';
  rsClearSettingsMsg = 'Are you sure you want to reset all settings?';
  rsOptions = 'Options';
  rsResetFontMsg = 'Are you sure you want to reset the font settings?';
  rsResetFontToDefault = 'Reset font to default';
  rsDefaultTitle = 'Default title';
  rsHeaderColor = 'Header color';
  rsHeaderFont = 'Header font';
  rsColumnColor = 'Column color';
  rsColumnFont = 'Column font';
  rsFuncNotFoundInModule = 'Function [%s] not found in module [%s].';
  rsGoToHelp = 'Go to Help';
  rsInsertEventHandler = 'Insert event handler';
  rsHideContextHelp = 'Hide context help';
  rsGoToNextItem = 'Go to next item';
  rsReturnToPrevItem = 'Return to previous item <Backspace>';
  rsNoContextHelp = 'No context help for this element.';
  rsAPIReference = 'API reference';
  rsFindInSyntaxPanel = 'Find in syntax-panel';
  rsHideBaseClasses = 'Hide base classes';
  rsAppDescript = 'Databases constructor';
  rsPadZeros = 'Pad fractional part with zeros';
  rsPickUp = 'Pick up';
  rsPickUpBnHint = 'Quickly adding fields to selection';
  rsDBSumFieldNotNumeric = 'Field [%s] is incompatible with the function. The function works only with fields of the "Number" type.';
  rsQueries = 'Queries';
  rsParseExtModulesErrors = 'During the analysis modules extensions detected '
    +'errors:%sSome actions and functions may not be found. To find the '
    +'errors, open Script Editor and run the compilation.';
  rsImages = 'Images';
  rsValidateActionError = 'An error occurred while validating the action:%s';
  rsComponentNotFound = 'Component [%s] not found.';
  rsActionCheck = 'Action check';
  rsActionFuncRequires = 'The function requires the following component: %s.';
  rsExprContainsError = 'An error was detected in the expression. Unable to determine'
    +' the type of expression.';
  rsFieldNotFoundError = 'Field not found. Unable to determine field type.';
  rsComponentNotFoundError = 'Component not found. Unable to determine component type.';
  rsUnableToFindGridColumn = 'Unable to find grid column. Make sure you '
    +'entered the correct component name.';
  rsIfGridAtIndex = '[ifgrid] at %d, [if] at %d.';
  rsIfAtIndex = '[if] at %d.';
  rsErrOccurredInTag = 'The error occurred in the following tag: ';
  rsNewDBDetectMsg = 'The database was created in a newer version of the '
    +'program. Some features may not be available or work incorrectly. It is '
    +'recommended to update the program to the latest version.';
  rsChangeProjectProhibited = 'Project changes are prohibited!';
  rsSaveProjectProhibited = 'Saving the project is prohibited!';
  rsChangeProhibitedDetails = 'The database project was modified by another '
    +'instance of the program. Changing the database design together will lead '
    +'to errors and data loss! You should reconnect to the database to get the '
    +'latest project changes.';
  rsIknow = 'I know what I do';
  rsErrorWhileDisconnect = 'An error occurred while disconnecting from the '
    +'database.';
  rsErrorWhileDragDropFiles = 'An error occurred while dragging and dropping '
    +'files.';
  rsDropFilesNotSupport = 'The following files are not supported and have not '
    +'been processed:%sOnly extension modules *.epas are supported.';
  rsDragDropExtensionsMsg = 'The following extensions will be imported into '
    +'the database:%sContinue?';
  rsDragDropExtensionsAfterMsg = 'The following extensions have been imported '
    +'into the database:';
  rsOldModule = 'Old module';
  rsModulesContainSameFeatures = 'Modules "%s" and "%s" contain the same '
    +'features.';
  rsExprNotLogical = 'The expression in the attribute "Expr" is not a logical '
    +'expression.';
  rsSoftCheckValues = 'Soft check of values';
  rsSoftCheckEnableMsg = 'You have enabled soft data entry validation. If you '
    +'have configured the user role so that some fields are hidden or cannot '
    +'be edited, then these fields do not require entering or validating '
    +'values.';
  rsErrorCloseDBMsg = 'An error occurred while closing the database.';
  rsShowOnlyFirstRecords = 'Show only the first';
  rsRecords = 'records';
  rsMultipleRecCaption = 'Multiple records caption';
  rsOneRecCaption = 'One record caption';
  rsButtonAction = 'Button action';
  rsFormAction = 'Form action';
  rsPasteExtTemplate = 'Paste extension template';
  rsForAllAction = 'For all action';
  rsExtensionKind = 'Extension kind';
  rsExtensionName = 'Extension name';
  rsFunctionName = 'Function name';
  rsTypeName = 'Type name';
  rsVariableName = 'Variable name';
  rsArrayName = 'Array name';
  rsIdentValidNameMsg = 'Function, type, and variable names can only contain '
    +'the characters A-Z, a-z, 0-9, _ and must not start with a digit.';
  rsIdentNotSameNames = 'The names of functions, types and variables must not '
    +'be the same.';
  rsEnterExtName = 'Enter extension name.';
  rsEnterFuncName = 'Enter function name.';
  rsEnterTypeName = 'Enter type name.';
  rsEnterVarName = 'Enter variable name.';
  rsEnterArrayName = 'Enter array name.';
  rsFormNotFoundStr = 'Form not found.';
  rsElemNotFoundStr = 'Element not found.';
  rsMyActionOrMyFunc = 'My action or MyFunction';
  rsMyGroupMySubGroup = 'My group/My subgroup';
  rsFindActions = 'Find actions';
  rsAllModulesList = '[All modules]';
  rsAllActionsList = '[All actions]';
  rsAllTypesList = '[All types]';
  rsExtensionsModule = 'Extensions module';
  rsActionType = 'Action type';
  rsSearchResult = 'Search result';
  rsAllExpressions = 'All expressions';
  rsProperty = 'Property';
  rsFindExpressions = 'Find expressions';
  rsAllValues = 'All values';
  rsSearchInIF = 'Search in IF';
  rsFindInModules = 'Find in modules';
  rsAllFieldsList = '[All fields]';
  rsImageProps = 'Image properties';
  rsShrinkStretch = 'Shrink/stretch';
  rsAlignCenter = 'Align Center';
  rsAspectRatio = 'Aspect Ratio';
  rsDoNotStretch = 'Shrink only (no stretch)';
  rsContainedInList = 'Contained in the list';
  rsNotContainedInList = 'Not contained in the list';
  rsModuleNameReserved = 'Module name "%s" is reserved.';
  rsFindModule = 'Find module';
  rsModuleWebMainExists = 'Module "WebMain" already exists.';
  rsFormWebModules = 'Form web modules';
  rsFormWebModule = 'Form web module';
  rsAddWebMainModule = 'Add WebMain module';
  rsAddFormWebModule = 'Add form web module';
  rsWebExtensionsModule = 'Extensions web module';
  rsWebExtensionsModules = 'Extensions web modules';
  rsAddExtWebModule = 'Add extensions web module';
  rsFormModule = 'Form module';
  rsUserModule = 'User module';
  rsErrorWhenImportModule = 'An error occured when importing module.';
  rsShowRowDeleteButton = 'Show row delete button (web)';
  rsCacheMetadata = 'Cache metadata';
  rsCheckUpdates = 'Check updates';
  rsUpdatesDBPath = 'Updates database path';
  rsUploadFilesToUpdatesDBMsg = 'The files you select will overwrite the '
    +'contents of the update database. Continue?';
  rsCanNotCleanDir = 'Can not clean temporary directory %s';
  rsCanNotCreateTempDir = 'Can not create temporary directory %s';
  //rsCopyFileErrMsg = 'Can not copy file %s into %s';
  rsUploadFilesError = 'Upload files error';
  //rsCreateArchiveErr = 'Can not create archive %s';
  rsAppFolder = 'Application folder';
  rsUpdatesDB = 'Updates database';
  rsUpdatesDBIsEmpty = 'Database is empty';
  rsUpdatesDBUnavailable = 'Database unavailable';
  rsUpdatesDBPathNotSet = 'The path to the updates database was not set.';
  //rsUpdatesManagement = 'Updates database management';
  rsUploadSelectedFilesIntoDB = 'Upload selected files into database';
  rsUpdates = 'Updates';
  rsDownloadUpdatesMsg = 'Download updates...';
  rsErrConnectUpdatesDB = 'Can not connect to updates database: %s.';
  rsErrCreateUpdatesDB = 'Can not create updates database: %s.';
  rsDXUpdateAvailableMsg = 'DataExpress update is available. Do you want to '
    +'start the update?';
  rsUpdateError = 'Update error';
  rsUpdateSettings = 'Update settings';
  rsCheckBoxDBFiles = 'Check database files';
  rsMsgWhenUpdateFound = 'Message when an update is found';
  rsUpdateDBContent = 'Updates database content';
  rsUpdateDBFileInfo = 'File size: %s | File changed: %s';
  rsShowGrid = 'Show grid';
  rsGridSizeX = 'Grid size X';
  rsGridSizeY = 'Grid size Y';
  rsGridColor = 'Grid color';
  rsRecordID = 'Record ID';
  rsFbLibNotUnloadMsg = 'Library %s was not unloaded. To work correctly, '
    +'restart the application.';
  rsSupportDXDB = 'Support new databases (*.DXDB)';
  rsDestinationForms = 'Destination forms';
  rsDestinationReports = 'Destination reports';
  rsDestinationQueries = 'Destination queries';
  rsErrorLogging = 'Error logging';
  rsSwitchSQLModeMsg = 'Are you sure you want to switch to SQL mode? '
    +'Please note that after saving the SQL expression, your previous settings will be cleared.';
  rsSwitchBuilderModeMsg = 'Are you sure you want to switch to builder mode? '
    +'Please note that after saving the settings, the SQL expression will be cleared.';
  rsSQLName = 'SQL Name';
  rsFormat = 'Format';
  rsUndefinedAliasOrFm = 'Undefined alias or form name: %s';
  rsEmptySquareBracketsDetected = 'Empty square brackets detected';
  rsExprEmpty = 'Expression is empty';
  rsSQLExpression = 'SQL expression';
  rsSwitchToBuilderMode = 'Switch to builder mode';
  rsSwitchToSQLMode = 'Switch to SQL mode';
  rsLicensedApache = 'Licensed under the Apache License, Version 2.0';
  rsListSourceClickMoreBn = 'Click the "More" button to see the list source.';
  rsKeyFieldNotSelected = 'Key field not selected.';
  rsKeyField = 'Key field';
  rsFieldsInTheList = 'Fields in the list';
  rsListFieldsNotSpecify = 'Specify at least one field in the list.';
  rsListSourceNotReady = 'The list source is not configured correctly.';
  rsSetWidthToAutoWidth = '*Set width to 0 for auto width';
  //rsContainingTextFrags = 'Containing one or more text fragments';

implementation

end.

