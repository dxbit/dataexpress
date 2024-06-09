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

unit RunDecls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime, DB, DBCtrls, dxctrls, DBCtrlsEx, Buttons,
  Grids, DBGrids, ComCtrls, ImgList, Controls, timeedit, dximages, dxfiles,
  dxreports, Menus, Graphics, StdCtrls, LazUtf8, LazFileUtils, listform,
  ButtonPanel, formview, editform, Forms, GraphType, Dialogs, PrintersDlgs,
  KGrids, myclasses, fphttpserver, httpdefs, webserver, httpclient, fphttpclient,
  laz2_dom, fpjson, csvfiles, strconsts;

procedure RIRegister_Json(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Xml(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Chart(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Template(Cl: TPSRuntimeClassImporter);
procedure RIRegister_HttpClient(Cl: TPSRuntimeClassImporter);
procedure RIRegister_HttpServer(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Splitter(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ButtonPanel(Cl: TPSRuntimeClassImporter);
procedure RIRegister_StatusBar(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Toolbar(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Window(Cl: TPSRuntimeClassImporter);
procedure RIRegister_MainFm(Cl: TPSRuntimeClassImporter);
procedure RIRegister_FormView(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Clipboard(Cl: TPSRuntimeClassImporter);
procedure RIRegister_IniFiles(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ReportWindow(Cl: TPSRuntimeClassImporter);
procedure RIRegister_KGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxSQLQuery(Cl: TPSRuntimeClassImporter);
//procedure RIRegister_dxQuery(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxQueryGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxFile(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxDBImage(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxButton(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCounter(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxTimeEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBTimeEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxObjectField(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxShape(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxPageControl(Cl: TPSRuntimeClassImporter);
procedure RIRegister_PageControl(Cl: TPSRuntimeClassImporter);
procedure RIRegister_ImageList(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxGroupBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBGrid(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxImage(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxLookupComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_CustomDBComboBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCheckBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBCheckBox(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxMemo(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBMemo(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxDateEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBDateEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCalcEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBCalcEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_CustomDBEditButton(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_DBEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_MaskEdit(Cl: TPSRuntimeClassImporter);
//procedure RIRegister_CustomMaskEdit(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxLabel(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxForm(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxFormTree(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Dialogs(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxCtrls(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxTypes(Cl: TPSRuntimeClassImporter);
procedure RIRegister_TreeView(Cl: TPSRuntimeClassImporter);
procedure RIRegister_TrayIcon(Cl: TPSRuntimeClassImporter);
procedure RIRegister_CsvData(Cl: TPSRuntimeClassImporter);
procedure RIRegister_dxRecordId(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Functions(Exec: TPSExec);

implementation

uses
  ScriptFuncs, maskedit, mainform, ExtCtrls, Math, exprfuncs, DateUtils,
  apputils, pivotgrid, reportwindow, IniFiles, BGRABitmap, Clipbrd, MyCtrls,
  DxSQLQuery, HMAC, Variants, dxcharts, imagemanager, mytypes, lists;

function TJSONDataJSONType(Self: TJSONData): TJSONtype; begin Result := Self.JSONType; end;
procedure TJSONDataCount_R(Self: TJSONData; var T: Integer); begin T := Self.Count; end;
procedure TJSONDataItems_R(Self: TJSONData; var T: TJSONData; I: Integer); begin T := Self.Items[I]; end;
procedure TJSONDataItems_W(Self: TJSONData; T: TJSONData; I: Integer); begin Self.Items[I] := T; end;
procedure TJSONDataValue_R(Self: TJSONData; var T: Variant); begin T := Self.Value; end;
procedure TJSONDataValue_W(Self: TJSONData; T: Variant); begin Self.Value := T; end;
procedure TJSONDataAsString_R(Self: TJSONData; var T: String); begin T := Self.AsString; end;
procedure TJSONDataAsString_W(Self: TJSONData; T: String); begin Self.AsString := T; end;
procedure TJSONDataAsUnicodeString_R(Self: TJSONData; var T: UnicodeString); begin T := Self.AsUnicodeString; end;
procedure TJSONDataAsUnicodeString_W(Self: TJSONData; T: UnicodeString); begin Self.AsUnicodeString := T; end;
procedure TJSONDataAsFloat_R(Self: TJSONData; var T: Double); begin T := Self.AsFloat; end;
procedure TJSONDataAsFloat_W(Self: TJSONData; T: Double); begin Self.AsFloat := T; end;
procedure TJSONDataAsInteger_R(Self: TJSONData; var T: Integer); begin T := Self.AsInteger; end;
procedure TJSONDataAsInteger_W(Self: TJSONData; T: Integer); begin Self.AsInteger := T; end;
procedure TJSONDataAsInt64_R(Self: TJSONData; var T: Int64); begin T := Self.AsInt64; end;
procedure TJSONDataAsInt64_W(Self: TJSONData; T: Int64); begin Self.AsInt64 := T; end;
procedure TJSONDataAsQWord_R(Self: TJSONData; var T: QWord); begin T := Self.AsQWord; end;
procedure TJSONDataAsQWord_W(Self: TJSONData; T: QWord); begin Self.AsQWord := T; end;
procedure TJSONDataAsBoolean_R(Self: TJSONData; var T: Boolean); begin T := Self.AsBoolean; end;
procedure TJSONDataAsBoolean_W(Self: TJSONData; T: Boolean); begin Self.AsBoolean := T; end;
procedure TJSONDataIsNull_R(Self: TJSONData; var T: Boolean); begin T := Self.IsNull; end;
procedure TJSONDataAsJSON_R(Self: TJSONData; var T: String); begin T := Self.AsJSON; end;
procedure TJSONDataClear(Self: TJSONData); begin Self.Clear; end;
function TJSONDataClone(Self: TJSONData): TJSONData; begin Result := Self.Clone; end;
function TJSONDataFormatJSON(Self: TJSONData; Options: Integer; IdentSize: Integer): String;
begin
  // Дополнительная опция foFormatFloat, которой нет в fpjson, но есть у меня в скриптах.
  if (Options and 32) = 32 then JSONFormatFloatEnabled := True;
  try
    Result := Self.FormatJSON(TFormatOptions(Options), IdentSize);
  finally
    JSONFormatFloatEnabled := False;
  end;
end;

Function VariantToJSON(V: Variant) : TJSONData;
begin
  Result:=Nil;
  case VarType(V) of
    varnull, varempty: Result := CreateJSON;
    varbyte, varsmallint, varinteger, varword: Result:=CreateJSON(Integer(V));
    varboolean: Result:=CreateJSON(Boolean(V));
    varsingle, vardouble, varcurrency: Result:=CreateJSON(Double(V));
    varint64: Result:=CreateJSON(Int64(V));
    varlongword, varqword: Result:=CreateJSON(QWord(V));
  else
    Result:=CreateJSON(VarToStr(V));
  end;
end;

function TJSONArrayCreate(Self: TClass; CreateNewInstance: Boolean; const Elements: array of const): TJSONArray;
begin Result := TJSONArray.Create(Elements); end;
function TJSONArrayAdd(Self: TJSONArray; Value: Variant): Integer;
begin Result := Self.Add(VariantToJSON(Value)); end;
function TJSONArrayAddArray(Self: TJSONArray; Arr: TJSONArray): Integer;
begin Result := Self.Add(Arr); end;
function TJSONArrayAddObject(Self: TJSONArray; Obj: TJSONObject): Integer;
begin Result := Self.Add(Obj); end;
procedure TJSONArrayInsert(Self: TJSONArray; Index: Integer; Value: Variant);
begin Self.Insert(Index, VariantToJSON(Value)); end;
procedure TJSONArrayInsertArray(Self: TJSONArray; Index: Integer; Arr: TJSONArray);
begin Self.Insert(Index, Arr); end;
procedure TJSONArrayInsertObject(Self: TJSONArray; Index: Integer; Obj: TJSONObject);
begin Self.Insert(Index, Obj); end;

function TJSONObjectCreate(Self: TClass; CreateNewInstance: Boolean; const Elements: array of const): TJSONObject;
begin Result := TJSONObject.Create(Elements); end;
function TJSONObjectAdd(Self: TJSONObject; const AName: String; Value: Variant): Integer;
begin Result := Self.Add(AName, VariantToJSON(Value)); end;
function TJSONObjectAddArray(Self: TJSONObject; const AName: String; Arr: TJSONArray): Integer;
begin Result := Self.Add(AName, Arr); end;
function TJSONObjectAddObject(Self: TJSONObject; const AName: String; Obj: TJSONObject): Integer;
begin Result := Self.Add(AName, Obj); end;
procedure TJSONObjectNames_R(Self: TJSONObject; var T: String; I: Integer); begin T := Self.Names[I]; end;
procedure TJSONObjectElements_R(Self: TJSONObject; var T: TJSONData; I: String); begin T := Self.Elements[I]; end;
procedure TJSONObjectElements_W(Self: TJSONObject; T: TJSONData; I: String); begin Self.Elements[I] := T; end;

procedure RIRegister_Json(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TJSONData) do
  begin
    //RegisterVirtualConstructor(@TJSONData.Create, 'Create');
    RegisterMethod(@TJSONDataClear, 'Clear');
    RegisterMethod(@TJSONData.FindPath, 'FindPath');
    RegisterMethod(@TJSONData.GetPath, 'GetPath');
    RegisterMethod(@TJSONDataClone, 'Clone');
    RegisterMethod(@TJSONDataJSONType, 'JSONType');
    RegisterMethod(@TJSONDataFormatJSON, 'FormatJSON');
    RegisterPropertyHelper(@TJSONDataCount_R, nil, 'Count');
    RegisterPropertyHelper(@TJSONDataItems_R, @TJSONDataItems_W, 'Items');
    RegisterPropertyHelper(@TJSONDataValue_R, @TJSONDataValue_W, 'Value');
    RegisterPropertyHelper(@TJSONDataAsString_R, @TJSONDataAsString_W, 'AsString');
    RegisterPropertyHelper(@TJSONDataAsUnicodeString_R, @TJSONDataAsUnicodeString_W, 'AsUnicodeString');
    RegisterPropertyHelper(@TJSONDataAsFloat_R, @TJSONDataAsFloat_W, 'AsFloat');
    RegisterPropertyHelper(@TJSONDataAsInteger_R, @TJSONDataAsInteger_W, 'AsInteger');
    RegisterPropertyHelper(@TJSONDataAsInt64_R, @TJSONDataAsInt64_W, 'AsInt64');
    //RegisterPropertyHelper(@TJSONDataAsQWord_R, @TJSONDataAsQWord_W, 'AsQWord');
    RegisterPropertyHelper(@TJSONDataAsBoolean_R, @TJSONDataAsBoolean_W, 'AsBoolean');
    RegisterPropertyHelper(@TJSONDataIsNull_R, nil, 'IsNull');
    RegisterPropertyHelper(@TJSONDataAsJSON_R, nil, 'AsJSON');
  end;
  with Cl.Add(TJSONArray) do
  begin
    RegisterConstructor(@TJSONArray.Create, 'Create');
    RegisterConstructor(@TJSONArrayCreate, 'CreateArray');
    //RegisterPropertyHelper(@TJSONArrayItems_R, @TJSONDataItems_W, 'Items');
    RegisterMethod(@TJSONArrayAdd, 'Add');
    RegisterMethod(@TJSONArrayAddArray, 'AddArray');
    RegisterMethod(@TJSONArrayAddObject, 'AddObject');
    RegisterMethod(@TJSONArray.Delete, 'Delete');
    RegisterMethod(@TJSONArray.Exchange, 'Exchange');
    RegisterMethod(@TJSONArrayInsert, 'Insert');
    RegisterMethod(@TJSONArrayInsertArray, 'InsertArray');
    RegisterMethod(@TJSONArrayInsertObject, 'InsertObject');
    RegisterMethod(@TJSONArray.Remove, 'Remove');
    RegisterMethod(@TJSONArray.IndexOf, 'IndexOf');
  end;

  with Cl.Add(TJSONObject) do
  begin
    RegisterConstructor(@TJSONObject.Create, 'Create');
    RegisterConstructor(@TJSONObjectCreate, 'CreateObject');
    RegisterMethod(@TJSONObjectAdd, 'Add');
    RegisterMethod(@TJSONObjectAddArray, 'AddArray');
    RegisterMethod(@TJSONObjectAddObject, 'AddObject');
    RegisterMethod(@TJSONObject.Delete, 'Delete');
    RegisterMethod(@TJSONObject.Remove, 'Remove');
    RegisterMethod(@TJSONObject.IndexOf, 'IndexOf');
    RegisterMethod(@TJSONObject.IndexOfName, 'IndexOfName');
    RegisterPropertyHelper(@TJSONObjectNames_R, nil, 'Names');
    RegisterPropertyHelper(@TJSONObjectElements_R, @TJSONObjectElements_W, 'Elements');
  end;
end;

procedure TDomNodeNodeName_R(Self: TDOMNode; var T: String); begin T := Self.NodeName; end;
procedure TDomNodeNodeValue_R(Self: TDOMNode; var T: String); begin T := Self.NodeValue; end;
procedure TDomNodeNodeValue_W(Self: TDOMNode; const T: String); begin Self.NodeValue := T; end;
procedure TDomNodeNodeType_R(Self: TDOMNode; var T: Integer); begin T := Self.NodeType; end;
procedure TDomNodeParentNode_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.ParentNode; end;
procedure TDomNodeFirstChild_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.FirstChild; end;
procedure TDomNodeLastChild_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.LastChild; end;
procedure TDomNodeChildNodes_R(Self: TDOMNode; var T: TDOMNodeList); begin T := Self.ChildNodes; end;
procedure TDomNodePreviousSibling_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.PreviousSibling; end;
procedure TDomNodeNextSibling_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.NextSibling; end;
procedure TDomNodeOwnerDocument_R(Self: TDOMNode; var T: TXmlDocument); begin T := TXmlDocument(Self.OwnerDocument); end;
procedure TDomNodeAttrCount_R(Self: TDOMNode; var T: LongWord); begin T := Self.Attributes.Length; end;
procedure TDomNodeAttrs_R(Self: TDOMNode; var T: String; I: String); begin T := (Self as TDOMElement).AttribStrings[I]; end;
procedure TDomNodeAttrs_W(Self: TDOMNode; const T: String; I: String); begin (Self as TDOMElement).AttribStrings[I] := T end;
procedure TDomNodeAttr_R(Self: TDOMNode; var T: TDOMNode; I: LongWord); begin T := Self.Attributes[I]; end;
function TDomNodeCloneNode(Self: TDOMNode; Deep: Boolean): TDOMNode; begin Result := Self.CloneNode(Deep); end;
function TDomNodeRemoveAttr(Self: TDOMNode; const AName: String): TDOMNode; begin Result := Self.Attributes.RemoveNamedItem(AName); end;
function TDomNodeAttrExists(Self: TDOMNode; const AName: String): Boolean; begin Result := Self.Attributes.GetNamedItem(AName) <> nil; end;

procedure TXmlDocumentRoot_R(Self: TXmlDocument; var T: TDomNode); begin T := Self.DocumentElement; end;
function TXmlDocumentCreateNode(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateElement(T); end;
function TXmlDocumentCreateText(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateTextNode(T); end;
function TXmlDocumentCreateCDATA(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateCDATASection(T); end;

procedure TDomNodeListItem_R(Self: TDomNodeList; var T: TDomNode; I: LongWord); begin T := Self[I]; end;
procedure TDomNodeListCount_R(Self: TDomNodeList; var T: LongWord); begin T := Self.Count; end;

procedure RIRegister_Xml(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDOMNode) do
  begin
    RegisterPropertyHelper(@TDomNodeNodeName_R, nil, 'NodeName');
    RegisterPropertyHelper(@TDomNodeNodeValue_R, @TDomNodeNodeValue_W, 'NodeValue');
    RegisterPropertyHelper(@TDomNodeNodeType_R, nil, 'NodeType');
    RegisterPropertyHelper(@TDomNodeParentNode_R, nil, 'ParentNode');
    RegisterPropertyHelper(@TDomNodeFirstChild_R, nil, 'FirstChild');
    RegisterPropertyHelper(@TDomNodeLastChild_R, nil, 'LastChild');
    RegisterPropertyHelper(@TDomNodeChildNodes_R, nil, 'ChildNodes');
    RegisterPropertyHelper(@TDomNodePreviousSibling_R, nil, 'PreviousSibling');
    RegisterPropertyHelper(@TDomNodeNextSibling_R, nil, 'NextSibling');
    RegisterPropertyHelper(@TDomNodeOwnerDocument_R, nil, 'OwnerDocument');
    RegisterPropertyHelper(@TDomNodeAttrCount_R, nil, 'AttrCount');
    RegisterPropertyHelper(@TDomNodeAttrs_R, @TDomNodeAttrs_W, 'Attrs');
    RegisterPropertyHelper(@TDomNodeAttr_R, nil, 'Attr');

    RegisterVirtualMethod(@TDOMNode.InsertBefore, 'InsertBefore');
    RegisterVirtualMethod(@TDOMNode.ReplaceChild, 'ReplaceChild');
    //RegisterVirtualMethod(@TDOMNode.DetachChild, 'DetachChild');
    RegisterMethod(@TDOMNode.RemoveChild, 'RemoveChild');
    RegisterMethod(@TDOMNode.AppendChild, 'AppendChild');
    RegisterVirtualMethod(@TDOMNode.HasChildNodes, 'HasChildNodes');
    RegisterMethod(@TDOMNodeCloneNode, 'CloneNode');
    RegisterMethod(@TDOMNode.GetLevel, 'GetLevel');
    RegisterVirtualMethod(@TDOMNode.FindNode, 'FindNode');
    RegisterMethod(@TDOMNodeRemoveAttr, 'RemoveAttr');
    RegisterMethod(@TDOMNodeAttrExists, 'AttrExists');
  end;

  with Cl.Add(TXmlDocument) do
  begin
    RegisterPropertyHelper(@TXmlDocumentRoot_R, nil, 'Root');
    RegisterConstructor(@TXmlDocument.Create, 'Create');
    RegisterMethod(@TXmlDocumentCreateNode, 'CreateNode');
    RegisterMethod(@TXmlDocumentCreateText, 'CreateText');
    RegisterMethod(@TXmlDocumentCreateCDATA, 'CreateCDATA');
  end;

  with Cl.Add(TDomNodeList) do
  begin
    RegisterPropertyHelper(@TDomNodeListItem_R, nil, 'Item');
    RegisterPropertyHelper(@TDomNodeListCount_R, nil, 'Count');
  end;
end;

procedure RIRegister_Chart(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxChart) do
end;

procedure TemplateStartDelimiter_R(Self: TTemplate; var T: String); begin T := Self.StartDelimiter; end;
procedure TemplateStartDelimiter_W(Self: TTemplate; const T: String); begin Self.StartDelimiter := T; end;
procedure TemplateEndDelimiter_R(Self: TTemplate; var T: String); begin T := Self.EndDelimiter; end;
procedure TemplateEndDelimiter_W(Self: TTemplate; const T: String); begin Self.EndDelimiter := T; end;
procedure TemplateParamStartDelimiter_R(Self: TTemplate; var T: String); begin T := Self.ParamStartDelimiter; end;
procedure TemplateParamStartDelimiter_W(Self: TTemplate; const T: String); begin Self.ParamStartDelimiter := T; end;
procedure TemplateParamEndDelimiter_R(Self: TTemplate; var T: String); begin T := Self.ParamEndDelimiter; end;
procedure TemplateParamEndDelimiter_W(Self: TTemplate; const T: String); begin Self.ParamEndDelimiter := T; end;
procedure TemplateParamValueSeparator_R(Self: TTemplate; var T: String); begin T := Self.ParamValueSeparator; end;
procedure TemplateParamValueSeparator_W(Self: TTemplate; const T: String); begin Self.ParamValueSeparator := T; end;
procedure TemplateTags_R(Self: TTemplate; var T: Variant; const I: String); begin T := Self.Tags[I]; end;
procedure TemplateTags_W(Self: TTemplate; const T: Variant; const I: String); begin Self.Tags[I] := T; end;
procedure TemplateTagByIndex_R(Self: TTemplate; var T: Variant; I: Integer); begin T := Self.TagByIndex[I]; end;
procedure TemplateTagByIndex_W(Self: TTemplate; const T: Variant; I: Integer); begin Self.TagByIndex[I] := T; end;
procedure TemplateTagCount_R(Self: TTemplate; var T: Integer); begin T := Self.TagCount; end;
{procedure TemplateOnReplaceTag_R(Self: TTemplate; var T: TReplaceTagEvent); begin T := Self.OnReplaceTag; end;
procedure TemplateOnReplaceTag_W(Self: TTemplate; T: TReplaceTagEvent); begin Self.OnReplaceTag := T; end;}

procedure RIRegister_Template(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TTemplate) do
  begin
    RegisterConstructor(@TTemplate.Create, 'Create');
    RegisterMethod(@TTemplate.HasContent, 'HasContent');
    RegisterMethod(@TTemplate.GetContent, 'GetContent');
    RegisterMethod(@TTemplate.ClearTags, 'ClearTags');
    RegisterPropertyHelper(@TemplateStartDelimiter_R, @TemplateStartDelimiter_W, 'StartDelimiter');
    RegisterPropertyHelper(@TemplateEndDelimiter_R, @TemplateEndDelimiter_W, 'EndDelimiter');
    RegisterPropertyHelper(@TemplateParamStartDelimiter_R, @TemplateParamStartDelimiter_W, 'ParamStartDelimiter');
    RegisterPropertyHelper(@TemplateParamEndDelimiter_R, @TemplateParamEndDelimiter_W, 'ParamEndDelimiter');
    RegisterPropertyHelper(@TemplateParamValueSeparator_R, @TemplateParamValueSeparator_W, 'ParamValueSeparator');
    RegisterPropertyHelper(@TemplateTags_R, @TemplateTags_W, 'Tags');
    RegisterPropertyHelper(@TemplateTagByIndex_R, @TemplateTagByIndex_W, 'TagByIndex');
    RegisterPropertyHelper(@TemplateTagCount_R, nil, 'TagCount');
    //RegisterPropertyHelper(@TemplateOnReplaceTag_R, @TemplateOnReplaceTag_W, 'OnReplaceTag');
  end;
end;

procedure HttpClientTerminated_R(Self: THttpClient; var T: Boolean); begin T := Self.Terminated; end;
procedure HttpClientConnectionCount_R(Self: THttpClient; var T: Integer); begin T := Self.ConnectionCount; end;

procedure ProxyDataHost_R(Self: TProxyData; var T: String); begin T := Self.Host; end;
procedure ProxyDataHost_W(Self: TProxyData; const T: String); begin Self.Host := T; end;
procedure ProxyDataPort_R(Self: TProxyData; var T: Word); begin T := Self.Port; end;
procedure ProxyDataPort_W(Self: TProxyData; T: Word); begin Self.Port := T; end;
procedure ProxyDataUserName_R(Self: TProxyData; var T: String); begin T := Self.UserName; end;
procedure ProxyDataUserName_W(Self: TProxyData; const T: String); begin Self.UserName := T; end;
procedure ProxyDataPassword_R(Self: TProxyData; var T: String); begin T := Self.Password; end;
procedure ProxyDataPassword_W(Self: TProxyData; const T: String); begin Self.Password := T; end;

procedure RIRegister_HttpClient(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TProxyData) do
  begin
    RegisterPropertyHelper(@ProxyDataHost_R, @ProxyDataHost_W, 'Host');
    RegisterPropertyHelper(@ProxyDataPort_R, @ProxyDataPort_W, 'Port');
    RegisterPropertyHelper(@ProxyDataUserName_R, @ProxyDataUserName_W, 'UserName');
    RegisterPropertyHelper(@ProxyDataPassword_R, @ProxyDataPassword_W, 'Password');
  end;

  with Cl.Add(THttpClient) do
  begin
    RegisterConstructor(@THttpClient.Create, 'Create');
    RegisterMethod(@THttpClient.Terminate, 'Terminate');
    RegisterMethod(@THttpClient.MyIndexOfHeader, 'IndexOfHeader');
    RegisterMethod(@THttpClient.MyAddHeader, 'AddHeader');
    RegisterMethod(@THttpClient.MyGetHeader, 'GetHeader');
    //RegisterVirtualMethod(@THttpClient.HTTPMethod, 'HTTPMethod');
    RegisterMethod(@THttpClient.Send, 'Send');
    RegisterMethod(@THttpClient.MyFormPost, 'FormPost');
    RegisterMethod(@THttpClient.MyStreamFormPost, 'StreamFormPost');
    RegisterPropertyHelper(@HttpClientTerminated_R, nil, 'Terminated');
    RegisterPropertyHelper(@HttpClientConnectionCount_R, nil, 'ConnectionCount');
  end;
end;

procedure HttpHeaderAccept_R(Self: THttpHeader; var T: String); begin T := Self.Accept; end;
procedure HttpHeaderAccept_W(Self: THttpHeader; const T: String); begin Self.Accept := T; end;
procedure HttpHeaderAcceptCharset_R(Self: THttpHeader; var T: String); begin T := Self.AcceptCharset; end;
procedure HttpHeaderAcceptCharset_W(Self: THttpHeader; const T: String); begin Self.AcceptCharset := T; end;
procedure HttpHeaderAcceptEncoding_R(Self: THttpHeader; var T: String); begin T := Self.AcceptEncoding; end;
procedure HttpHeaderAcceptEncoding_W(Self: THttpHeader; const T: String); begin Self.AcceptEncoding := T; end;
procedure HttpHeaderAcceptLanguage_R(Self: THttpHeader; var T: String); begin T := Self.AcceptLanguage; end;
procedure HttpHeaderAcceptLanguage_W(Self: THttpHeader; const T: String); begin Self.AcceptLanguage := T; end;
procedure HttpHeaderAuthorization_R(Self: THttpHeader; var T: String); begin T := Self.Authorization; end;
procedure HttpHeaderAuthorization_W(Self: THttpHeader; const T: String); begin Self.Authorization := T; end;
procedure HttpHeaderConnection_R(Self: THttpHeader; var T: String); begin T := Self.Connection; end;
procedure HttpHeaderConnection_W(Self: THttpHeader; const T: String); begin Self.Connection := T; end;
procedure HttpHeaderContentEncoding_R(Self: THttpHeader; var T: String); begin T := Self.ContentEncoding; end;
procedure HttpHeaderContentEncoding_W(Self: THttpHeader; const T: String); begin Self.ContentEncoding := T; end;
procedure HttpHeaderContentLanguage_R(Self: THttpHeader; var T: String); begin T := Self.ContentLanguage; end;
procedure HttpHeaderContentLanguage_W(Self: THttpHeader; const T: String); begin Self.ContentLanguage := T; end;
procedure HttpHeaderContentLength_R(Self: THttpHeader; var T: Integer); begin T := Self.ContentLength; end;
procedure HttpHeaderContentLength_W(Self: THttpHeader; const T: Integer); begin Self.ContentLength := T; end;
procedure HttpHeaderContentType_R(Self: THttpHeader; var T: String); begin T := Self.ContentType; end;
procedure HttpHeaderContentType_W(Self: THttpHeader; const T: String); begin Self.ContentType := T; end;
procedure HttpHeaderDate_R(Self: THttpHeader; var T: String); begin T := Self.Date; end;
procedure HttpHeaderDate_W(Self: THttpHeader; const T: String); begin Self.Date := T; end;
procedure HttpHeaderExpires_R(Self: THttpHeader; var T: String); begin T := Self.Expires; end;
procedure HttpHeaderExpires_W(Self: THttpHeader; const T: String); begin Self.Expires := T; end;
procedure HttpHeaderFrom_R(Self: THttpHeader; var T: String); begin T := Self.From; end;
procedure HttpHeaderFrom_W(Self: THttpHeader; const T: String); begin Self.From := T; end;
procedure HttpHeaderHost_R(Self: THttpHeader; var T: String); begin T := Self.Host; end;
procedure HttpHeaderHost_W(Self: THttpHeader; const T: String); begin Self.Host := T; end;
procedure HttpHeaderIfModifiedSince_R(Self: THttpHeader; var T: String); begin T := Self.IfModifiedSince; end;
procedure HttpHeaderIfModifiedSince_W(Self: THttpHeader; const T: String); begin Self.IfModifiedSince := T; end;
procedure HttpHeaderLastModified_R(Self: THttpHeader; var T: String); begin T := Self.LastModified; end;
procedure HttpHeaderLastModified_W(Self: THttpHeader; const T: String); begin Self.LastModified := T; end;
procedure HttpHeaderLocation_R(Self: THttpHeader; var T: String); begin T := Self.Location; end;
procedure HttpHeaderLocation_W(Self: THttpHeader; const T: String); begin Self.Location := T; end;
procedure HttpHeaderPragma_R(Self: THttpHeader; var T: String); begin T := Self.Pragma; end;
procedure HttpHeaderPragma_W(Self: THttpHeader; const T: String); begin Self.Pragma := T; end;
procedure HttpHeaderReferer_R(Self: THttpHeader; var T: String); begin T := Self.Referer; end;
procedure HttpHeaderReferer_W(Self: THttpHeader; const T: String); begin Self.Referer := T; end;
procedure HttpHeaderRetryAfter_R(Self: THttpHeader; var T: String); begin T := Self.RetryAfter; end;
procedure HttpHeaderRetryAfter_W(Self: THttpHeader; const T: String); begin Self.RetryAfter := T; end;
procedure HttpHeaderServer_R(Self: THttpHeader; var T: String); begin T := Self.Server; end;
procedure HttpHeaderServer_W(Self: THttpHeader; const T: String); begin Self.Server := T; end;
procedure HttpHeaderUserAgent_R(Self: THttpHeader; var T: String); begin T := Self.UserAgent; end;
procedure HttpHeaderUserAgent_W(Self: THttpHeader; const T: String); begin Self.UserAgent := T; end;
procedure HttpHeaderWarning_R(Self: THttpHeader; var T: String); begin T := Self.Warning; end;
procedure HttpHeaderWarning_W(Self: THttpHeader; const T: String); begin Self.Warning := T; end;
procedure HttpHeaderWWWAuthenticate_R(Self: THttpHeader; var T: String); begin T := Self.WWWAuthenticate; end;
procedure HttpHeaderWWWAuthenticate_W(Self: THttpHeader; const T: String); begin Self.WWWAuthenticate := T; end;
procedure HttpHeaderVia_R(Self: THttpHeader; var T: String); begin T := Self.Via; end;
procedure HttpHeaderVia_W(Self: THttpHeader; const T: String); begin Self.Via := T; end;
procedure HttpHeaderCookie_R(Self: THttpHeader; var T: String); begin T := Self.Cookie; end;
procedure HttpHeaderCookie_W(Self: THttpHeader; const T: String); begin Self.Cookie := T; end;
procedure HttpHeaderSetCookie_R(Self: THttpHeader; var T: String); begin T := Self.SetCookie; end;
procedure HttpHeaderSetCookie_W(Self: THttpHeader; const T: String); begin Self.SetCookie := T; end;
procedure HttpHeaderHTTPXRequestedWith_R(Self: THttpHeader; var T: String); begin T := Self.HTTPXRequestedWith; end;
procedure HttpHeaderHTTPXRequestedWith_W(Self: THttpHeader; const T: String); begin Self.HTTPXRequestedWith := T; end;
procedure HttpHeaderHttpVersion_R(Self: THttpHeader; var T: String); begin T := Self.HttpVersion; end;
procedure HttpHeaderHttpVersion_W(Self: THttpHeader; const T: String); begin Self.HttpVersion := T; end;
procedure HttpHeaderProtocolVersion_R(Self: THttpHeader; var T: String); begin T := Self.ProtocolVersion; end;
procedure HttpHeaderProtocolVersion_W(Self: THttpHeader; const T: String); begin Self.ProtocolVersion := T; end;
procedure HttpHeaderPathInfo_R(Self: THttpHeader; var T: String); begin T := Self.PathInfo; end;
procedure HttpHeaderPathInfo_W(Self: THttpHeader; const T: String); begin Self.PathInfo := T; end;
procedure HttpHeaderPathTranslated_R(Self: THttpHeader; var T: String); begin T := Self.PathTranslated; end;
procedure HttpHeaderPathTranslated_W(Self: THttpHeader; const T: String); begin Self.PathTranslated := T; end;
procedure HttpHeaderRemoteAddr_R(Self: THttpHeader; var T: String); begin T := Self.RemoteAddr; end;
procedure HttpHeaderRemoteAddr_W(Self: THttpHeader; const T: String); begin Self.RemoteAddr := T; end;
procedure HttpHeaderRemoteHost_R(Self: THttpHeader; var T: String); begin T := Self.RemoteHost; end;
procedure HttpHeaderRemoteHost_W(Self: THttpHeader; const T: String); begin Self.RemoteHost := T; end;
procedure HttpHeaderScriptName_R(Self: THttpHeader; var T: String); begin T := Self.ScriptName; end;
procedure HttpHeaderScriptName_W(Self: THttpHeader; const T: String); begin Self.ScriptName := T; end;
procedure HttpHeaderServerPort_R(Self: THttpHeader; var T: Word); begin T := Self.ServerPort; end;
procedure HttpHeaderServerPort_W(Self: THttpHeader; const T: Word); begin Self.ServerPort := T; end;
procedure HttpHeaderMethod_R(Self: THttpHeader; var T: String); begin T := Self.Method; end;
procedure HttpHeaderMethod_W(Self: THttpHeader; const T: String); begin Self.Method := T; end;
procedure HttpHeaderURL_R(Self: THttpHeader; var T: String); begin T := Self.URL; end;
procedure HttpHeaderURL_W(Self: THttpHeader; const T: String); begin Self.URL := T; end;
procedure HttpHeaderQuery_R(Self: THttpHeader; var T: String); begin T := Self.Query; end;
procedure HttpHeaderQuery_W(Self: THttpHeader; const T: String); begin Self.Query := T; end;
procedure HttpHeaderContent_R(Self: THttpHeader; var T: String); begin T := Self.Content; end;
procedure HttpHeaderContent_W(Self: THttpHeader; const T: String); begin Self.Content := T; end;
procedure HttpHeaderCookieFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.CookieFields; end;
procedure HttpHeaderCookieFields_W(Self: THttpHeader; const T: TStrings); begin Self.CookieFields := T; end;
procedure HttpHeaderContentFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.ContentFields; end;
procedure HttpHeaderQueryFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.QueryFields; end;
procedure HttpHeaderCustomHeaders_R(Self: THttpHeader; var T: TStringList); begin T := Self.CustomHeaders; end;

procedure UploadedFileFieldName_R(Self: TUploadedFile; var T: String); begin T := Self.FieldName; end;
procedure UploadedFileFieldName_W(Self: TUploadedFile; const T: String); begin Self.FieldName := T; end;
procedure UploadedFileFileName_R(Self: TUploadedFile; var T: String); begin T := Self.FileName; end;
procedure UploadedFileFileName_W(Self: TUploadedFile; const T: String); begin Self.FileName := T; end;
procedure UploadedFileStream_R(Self: TUploadedFile; var T: TStream); begin T := Self.Stream; end;
procedure UploadedFileSize_R(Self: TUploadedFile; var T: Int64); begin T := Self.Size; end;
procedure UploadedFileSize_W(Self: TUploadedFile; const T: Int64); begin Self.Size := T; end;
procedure UploadedFileContentType_R(Self: TUploadedFile; var T: String); begin T := Self.ContentType; end;
procedure UploadedFileContentType_W(Self: TUploadedFile; const T: String); begin Self.ContentType := T; end;
procedure UploadedFileDisposition_R(Self: TUploadedFile; var T: String); begin T := Self.Disposition; end;
procedure UploadedFileDisposition_W(Self: TUploadedFile; const T: String); begin Self.Disposition := T; end;
procedure UploadedFileLocalFileName_R(Self: TUploadedFile; var T: String); begin T := Self.LocalFileName; end;
procedure UploadedFileLocalFileName_W(Self: TUploadedFile; const T: String); begin Self.LocalFileName := T; end;
procedure UploadedFileDescription_R(Self: TUploadedFile; var T: String); begin T := Self.Description; end;
procedure UploadedFileDescription_W(Self: TUploadedFile; const T: String); begin Self.Description := T; end;

procedure UploadedFilesFiles_R(Self: TUploadedFiles; var T: TUploadedFile; I: Integer); begin T := Self.Files[I]; end;

procedure FPHTTPConnectionRequestFiles_R(Self: TFPHTTPConnectionRequest; var T: TUploadedFiles); begin T := Self.Files; end;

procedure CookieName_R(Self: TCookie; var T: String); begin T := Self.Name; end;
procedure CookieName_W(Self: TCookie; const T: String); begin Self.Name := T; end;
procedure CookieValue_R(Self: TCookie; var T: String); begin T := Self.Value; end;
procedure CookieValue_W(Self: TCookie; const T: String); begin Self.Value := T; end;
procedure CookieDomain_R(Self: TCookie; var T: String); begin T := Self.Domain; end;
procedure CookieDomain_W(Self: TCookie; const T: String); begin Self.Domain := T; end;
procedure CookiePath_R(Self: TCookie; var T: String); begin T := Self.Path; end;
procedure CookiePath_W(Self: TCookie; const T: String); begin Self.Path := T; end;
procedure CookieExpires_R(Self: TCookie; var T: TDateTime); begin T := Self.Expires; end;
procedure CookieExpires_W(Self: TCookie; const T: TDateTime); begin Self.Expires := T; end;
procedure CookieSecure_R(Self: TCookie; var T: Boolean); begin T := Self.Secure; end;
procedure CookieSecure_W(Self: TCookie; const T: Boolean); begin Self.Secure := T; end;
procedure CookieHttpOnly_R(Self: TCookie; var T: Boolean); begin T := Self.HttpOnly; end;
procedure CookieHttpOnly_W(Self: TCookie; const T: Boolean); begin Self.HttpOnly := T; end;
procedure CookieAsString_R(Self: TCookie; var T: String); begin T := Self.AsString; end;

procedure CookiesItems_R(Self: TCookies; var T: TCookie; I: Integer); begin T := Self.Items[I]; end;

procedure FPHTTPConnectionResponseCode_R(Self: TFPHTTPConnectionResponse; var T: Integer); begin T := Self.Code; end;
procedure FPHTTPConnectionResponseCode_W(Self: TFPHTTPConnectionResponse; const T: Integer); begin Self.Code := T; end;
procedure FPHTTPConnectionResponseCodeText_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.CodeText; end;
procedure FPHTTPConnectionResponseCodeText_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.CodeText := T; end;
procedure FPHTTPConnectionResponseAge_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Age; end;
procedure FPHTTPConnectionResponseAge_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Age := T; end;
procedure FPHTTPConnectionResponseAllow_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Allow; end;
procedure FPHTTPConnectionResponseAllow_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Allow := T; end;
procedure FPHTTPConnectionResponseCacheControl_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.CacheControl; end;
procedure FPHTTPConnectionResponseCacheControl_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.CacheControl := T; end;
procedure FPHTTPConnectionResponseContentLocation_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentLocation; end;
procedure FPHTTPConnectionResponseContentLocation_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentLocation := T; end;
procedure FPHTTPConnectionResponseContentMD5_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentMD5; end;
procedure FPHTTPConnectionResponseContentMD5_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentMD5 := T; end;
procedure FPHTTPConnectionResponseContentRange_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentRange; end;
procedure FPHTTPConnectionResponseContentRange_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentRange := T; end;
procedure FPHTTPConnectionResponseETag_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ETag; end;
procedure FPHTTPConnectionResponseETag_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ETag := T; end;
procedure FPHTTPConnectionResponseProxyAuthenticate_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ProxyAuthenticate; end;
procedure FPHTTPConnectionResponseProxyAuthenticate_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ProxyAuthenticate := T; end;
procedure FPHTTPConnectionResponseFirstHeaderLine_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.FirstHeaderLine; end;
procedure FPHTTPConnectionResponseFirstHeaderLine_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.FirstHeaderLine := T; end;
procedure FPHTTPConnectionResponseContentStream_R(Self: TFPHTTPConnectionResponse; var T: TStream); begin T := Self.ContentStream; end;
procedure FPHTTPConnectionResponseContentStream_W(Self: TFPHTTPConnectionResponse; const T: TStream); begin Self.ContentStream := T; end;
procedure FPHTTPConnectionResponseContent_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Content; end;
procedure FPHTTPConnectionResponseContent_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Content := T; end;
procedure FPHTTPConnectionResponseContents_R(Self: TFPHTTPConnectionResponse; var T: TStrings); begin T := Self.Contents; end;
procedure FPHTTPConnectionResponseContents_W(Self: TFPHTTPConnectionResponse; const T: TStrings); begin Self.Contents := T; end;
procedure FPHTTPConnectionResponseHeadersSent_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.HeadersSent; end;
procedure FPHTTPConnectionResponseContentSent_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.ContentSent; end;
procedure FPHTTPConnectionResponseCookies_R(Self: TFPHTTPConnectionResponse; var T: TCookies); begin T := Self.Cookies; end;
procedure FPHTTPConnectionResponseFreeContentStream_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.FreeContentStream; end;
procedure FPHTTPConnectionResponseFreeContentStream_W(Self: TFPHTTPConnectionResponse; const T: Boolean); begin Self.FreeContentStream := T; end;

{procedure WebServerActive_R(Self: THttpServer; var T: Boolean); begin T := Self.Active; end;
procedure WebServerPort_R(Self: THttpServer; var T: Word); begin T := Self.Port; end;
procedure WebServerPort_W(Self: THttpServer; const T: Word); begin Self.Port := T; end;
procedure WebServerOnRequest_R(Self: THttpServer; var T: THTTPServerRequestHandler); begin T := Self.OnRequest; end;
procedure WebServerOnRequest_W(Self: THttpServer; const T: THTTPServerRequestHandler); begin Self.OnRequest := T; end;
procedure WebServerOnError_R(Self: THttpServer; var T: THTTPServerErrorHandler); begin T := Self.OnError; end;
procedure WebServerOnError_W(Self: THttpServer; const T: THTTPServerErrorHandler); begin Self.OnError := T; end;}

procedure RIRegister_HttpServer(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(THttpHeader) do
  begin
    RegisterPropertyHelper(@HttpHeaderAccept_R, @HttpHeaderAccept_W, 'Accept');
    RegisterPropertyHelper(@HttpHeaderAcceptCharset_R, @HttpHeaderAcceptCharset_W, 'AcceptCharset');
    RegisterPropertyHelper(@HttpHeaderAcceptEncoding_R, @HttpHeaderAcceptEncoding_W, 'AcceptEncoding');
    RegisterPropertyHelper(@HttpHeaderAcceptLanguage_R, @HttpHeaderAcceptLanguage_W, 'AcceptLanguage');
    RegisterPropertyHelper(@HttpHeaderAuthorization_R, @HttpHeaderAuthorization_W, 'Authorization');
    RegisterPropertyHelper(@HttpHeaderConnection_R, @HttpHeaderConnection_W, 'Connection');
    RegisterPropertyHelper(@HttpHeaderContentEncoding_R, @HttpHeaderContentEncoding_W, 'ContentEncoding');
    RegisterPropertyHelper(@HttpHeaderContentLanguage_R, @HttpHeaderContentLanguage_W, 'ContentLanguage');
    RegisterPropertyHelper(@HttpHeaderContentLength_R, @HttpHeaderContentLength_W, 'ContentLength');
    RegisterPropertyHelper(@HttpHeaderContentType_R, @HttpHeaderContentType_W, 'ContentType');
    RegisterPropertyHelper(@HttpHeaderDate_R, @HttpHeaderDate_W, 'Date');
    RegisterPropertyHelper(@HttpHeaderExpires_R, @HttpHeaderExpires_W, 'Expires');
    RegisterPropertyHelper(@HttpHeaderFrom_R, @HttpHeaderFrom_W, 'From');
    RegisterPropertyHelper(@HttpHeaderHost_R, @HttpHeaderHost_W, 'Host');
    RegisterPropertyHelper(@HttpHeaderIfModifiedSince_R, @HttpHeaderIfModifiedSince_W, 'IfModifiedSince');
    RegisterPropertyHelper(@HttpHeaderLastModified_R, @HttpHeaderLastModified_W, 'LastModified');
    RegisterPropertyHelper(@HttpHeaderLocation_R, @HttpHeaderLocation_W, 'Location');
    RegisterPropertyHelper(@HttpHeaderPragma_R, @HttpHeaderPragma_W, 'Pragma');
    RegisterPropertyHelper(@HttpHeaderReferer_R, @HttpHeaderReferer_W, 'Referer');
    RegisterPropertyHelper(@HttpHeaderRetryAfter_R, @HttpHeaderRetryAfter_W, 'RetryAfter');
    RegisterPropertyHelper(@HttpHeaderServer_R, @HttpHeaderServer_W, 'Server');
    RegisterPropertyHelper(@HttpHeaderUserAgent_R, @HttpHeaderUserAgent_W, 'UserAgent');
    RegisterPropertyHelper(@HttpHeaderWarning_R, @HttpHeaderWarning_W, 'Warning');
    RegisterPropertyHelper(@HttpHeaderWWWAuthenticate_R, @HttpHeaderWWWAuthenticate_W, 'WWWAuthenticate');
    RegisterPropertyHelper(@HttpHeaderVia_R, @HttpHeaderVia_W, 'Via');
    // Headers, not in HTTP spec.
    RegisterPropertyHelper(@HttpHeaderCookie_R, @HttpHeaderCookie_W, 'Cookie');
    RegisterPropertyHelper(@HttpHeaderSetCookie_R, @HttpHeaderSetCookie_W, 'SetCookie');
    RegisterPropertyHelper(@HttpHeaderHTTPXRequestedWith_R, @HttpHeaderHTTPXRequestedWith_W, 'HTTPXRequestedWith');
    RegisterPropertyHelper(@HttpHeaderHttpVersion_R, @HttpHeaderHttpVersion_W, 'HttpVersion');
    RegisterPropertyHelper(@HttpHeaderProtocolVersion_R, @HttpHeaderProtocolVersion_W, 'ProtocolVersion');
    // Specials, mostly from CGI protocol/Apache.
    RegisterPropertyHelper(@HttpHeaderPathInfo_R, @HttpHeaderPathInfo_W, 'PathInfo');
    RegisterPropertyHelper(@HttpHeaderPathTranslated_R, @HttpHeaderPathTranslated_W, 'PathTranslated');
    RegisterPropertyHelper(@HttpHeaderRemoteAddr_R, @HttpHeaderRemoteAddr_W, 'RemoteAddr');
    RegisterPropertyHelper(@HttpHeaderRemoteHost_R, @HttpHeaderRemoteHost_W, 'RemoteHost');
    RegisterPropertyHelper(@HttpHeaderScriptName_R, @HttpHeaderScriptName_W, 'ScriptName');
    RegisterPropertyHelper(@HttpHeaderServerPort_R, @HttpHeaderServerPort_W, 'ServerPort');
    RegisterPropertyHelper(@HttpHeaderMethod_R, @HttpHeaderMethod_W, 'Method');
    RegisterPropertyHelper(@HttpHeaderURL_R, @HttpHeaderURL_W, 'URL');
    RegisterPropertyHelper(@HttpHeaderQuery_R, @HttpHeaderQuery_W, 'Query');
    RegisterPropertyHelper(@HttpHeaderContent_R, @HttpHeaderContent_W, 'Content');
    // Lists
    RegisterPropertyHelper(@HttpHeaderCookieFields_R, @HttpHeaderCookieFields_W, 'CookieFields');
    RegisterPropertyHelper(@HttpHeaderContentFields_R, nil, 'ContentFields');
    RegisterPropertyHelper(@HttpHeaderQueryFields_R, nil, 'QueryFields');
    RegisterPropertyHelper(@HttpHeaderCustomHeaders_R, nil, 'CustomHeaders');
  end;

  with Cl.Add(TUploadedFile) do
  begin
    RegisterPropertyHelper(@UploadedFileFieldName_R, @UploadedFileFieldName_W, 'FieldName');
    RegisterPropertyHelper(@UploadedFileFileName_R, @UploadedFileFileName_W, 'FileName');
    RegisterPropertyHelper(@UploadedFileStream_R, nil, 'Stream');
    RegisterPropertyHelper(@UploadedFileSize_R, @UploadedFileSize_W, 'Size');
    RegisterPropertyHelper(@UploadedFileContentType_R, @UploadedFileContentType_W, 'ContentType');
    RegisterPropertyHelper(@UploadedFileDisposition_R, @UploadedFileDisposition_W, 'Disposition');
    RegisterPropertyHelper(@UploadedFileLocalFileName_R, @UploadedFileLocalFileName_W, 'LocalFileName');
    RegisterPropertyHelper(@UploadedFileDescription_R, @UploadedFileDescription_W, 'Description');
  end;

  with Cl.Add(TUploadedFiles) do
  begin
    RegisterPropertyHelper(@UploadedFilesFiles_R, nil, 'Files');
  end;

  with Cl.Add(TFPHTTPConnectionRequest) do
  begin
    RegisterPropertyHelper(@FPHTTPConnectionRequestFiles_R, nil, 'Files');
  end;

  with Cl.Add(TCookie) do
  begin
    RegisterMethod(@TCookie.Expire, 'Expire');
    RegisterPropertyHelper(@CookieName_R, @CookieName_W, 'Name');
    RegisterPropertyHelper(@CookieValue_R, @CookieValue_W, 'Value');
    RegisterPropertyHelper(@CookieDomain_R, @CookieDomain_W, 'Domain');
    RegisterPropertyHelper(@CookiePath_R, @CookiePath_W, 'Path');
    RegisterPropertyHelper(@CookieExpires_R, @CookieExpires_W, 'Expires');
    RegisterPropertyHelper(@CookieSecure_R, @CookieSecure_W, 'Secure');
    RegisterPropertyHelper(@CookieHttpOnly_R, @CookieHttpOnly_W, 'HttpOnly');
    RegisterPropertyHelper(@CookieAsString_R, nil, 'AsString');
  end;

  with Cl.Add(TCookies) do
  begin
    RegisterMethod(@TCookies.Add, 'Add');
    RegisterMethod(@TCookies.CookieByName, 'CookieByName');
    RegisterMethod(@TCookies.FindCookie, 'FindCookie');
    RegisterMethod(@TCookies.IndexOfCookie, 'IndexOfCookie');
    RegisterPropertyHelper(@CookiesItems_R, nil, 'Items');
  end;

  with Cl.Add(TFPHTTPConnectionResponse) do
  begin
    RegisterMethod(@TFPHTTPConnectionResponse.SendContent, 'SendContent');
    RegisterMethod(@TFPHTTPConnectionResponse.SendHeaders, 'SendHeaders');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCode_R, @FPHTTPConnectionResponseCode_W, 'Code');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCodeText_R, @FPHTTPConnectionResponseCodeText_W, 'CodeText');
    RegisterPropertyHelper(@FPHTTPConnectionResponseAge_R, @FPHTTPConnectionResponseAge_W, 'Age');
    RegisterPropertyHelper(@FPHTTPConnectionResponseAllow_R, @FPHTTPConnectionResponseAllow_W, 'Allow');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCacheControl_R, @FPHTTPConnectionResponseCacheControl_W, 'CacheControl');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentLocation_R, @FPHTTPConnectionResponseContentLocation_W, 'ContentLocation');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentMD5_R, @FPHTTPConnectionResponseContentMD5_W, 'ContentMD5');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentRange_R, @FPHTTPConnectionResponseContentRange_W, 'ContentRange');
    RegisterPropertyHelper(@FPHTTPConnectionResponseETag_R, @FPHTTPConnectionResponseETag_W, 'ETag');
    RegisterPropertyHelper(@FPHTTPConnectionResponseProxyAuthenticate_R, @FPHTTPConnectionResponseProxyAuthenticate_W, 'ProxyAuthenticate');
    //RegisterPropertyHelper(@FPHTTPConnectionResponseRetryAfter_R, @FPHTTPConnectionResponseRetryAfter_W, 'RetryAfter');
    RegisterPropertyHelper(@FPHTTPConnectionResponseFirstHeaderLine_R, @FPHTTPConnectionResponseFirstHeaderLine_W, 'FirstHeaderLine');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentStream_R, @FPHTTPConnectionResponseContentStream_W, 'ContentStream');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContent_R, @FPHTTPConnectionResponseContent_W, 'Content');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContents_R, @FPHTTPConnectionResponseContents_W, 'Contents');
    RegisterPropertyHelper(@FPHTTPConnectionResponseHeadersSent_R, nil, 'HeadersSent');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentSent_R, nil, 'ContentSent');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCookies_R, nil, 'Cookies');
    RegisterPropertyHelper(@FPHTTPConnectionResponseFreeContentStream_R, @FPHTTPConnectionResponseFreeContentStream_W, 'FreeContentStream');
  end;

  with Cl.Add(THttpServer) do
  begin
    RegisterConstructor(@THttpServer.Create, 'Create');
    RegisterMethod(@THttpServer.Start, 'Start');
    {RegisterPropertyHelper(@WebServerActive_R, nil, 'Active');
    RegisterPropertyHelper(@WebServerPort_R, @WebServerPort_W, 'Port');
    RegisterEventPropertyHelper(@WebServerOnRequest_R, @WebServerOnRequest_W, 'OnRequest');
    RegisterEventPropertyHelper(@WebServerOnError_R, @WebServerOnError_W, 'OnError');}
  end;
end;

procedure TIniFileFileName_R(Self: TIniFile; var T: String); begin T := Self.FileName; end;
procedure TIniFileEscapeLineFeeds_R(Self: TIniFile; var T: Boolean); begin T := Self.EscapeLineFeeds; end;
procedure TIniFileCaseSensitive_R(Self: TIniFile; var T: Boolean); begin T := Self.CaseSensitive; end;
procedure TIniFileCaseSensitive_W(Self: TIniFile; T: Boolean); begin Self.CaseSensitive := T; end;
procedure TIniFileStripQuotes_R(Self: TIniFile; var T: Boolean); begin T := Self.StripQuotes; end;
procedure TIniFileStripQuotes_W(Self: TIniFile; T: Boolean); begin Self.StripQuotes := T; end;
procedure TIniFileCacheUpdates_R(Self: TIniFile; var T: Boolean); begin T := Self.CacheUpdates; end;
procedure TIniFileCacheUpdates_W(Self: TIniFile; T: Boolean); begin Self.CacheUpdates := T; end;

function TIniFileCreate(Self: TClass; CreateNewInstance: Boolean; FileName: string; AOptions: TIniFileOptions): TObject;
begin
  Result := TIniFile.Create(FileName, AOptions);
end;

function TClipboardHasText(Self: TClipboard): Boolean; begin Result := Self.HasFormat(CF_Text); end;
function TClipboardHasBitmap(Self: TClipboard): Boolean; begin Result := Self.HasFormat(CF_Bitmap); end;
procedure TClipboardAsText_R(Self: TClipboard; var T: String); begin T := Self.AsText; end;
procedure TClipboardAsText_W(Self: TClipboard; T: String); begin Self.AsText := T; end;

procedure RIRegister_Splitter(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSplitter) do
  begin

  end;
end;

procedure RIRegister_ButtonPanel(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPanelBitBtn) do
  begin
  end;

  with Cl.Add(TButtonPanel) do
  begin

  end;
end;

procedure TStatusBarCanvas_R(Self: TStatusBar; var T: TCanvas); begin T := Self.Canvas; end;

procedure TStatusPanelsItems_R(Self: TStatusPanels; var T: TStatusPanel; I: Integer); begin T := Self.Items[I]; end;
procedure TStatusPanelsItems_W(Self: TStatusPanels; T: TStatusPanel; I: Integer); begin Self.Items[I] := T; end;
procedure TStatusPanelsStatusBar_R(Self: TStatusPanels; var T: TStatusBar); begin T := Self.StatusBar; end;

procedure RIRegister_StatusBar(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TStatusPanel) do
  begin
    RegisterMethod(@TStatusPanel.StatusBar, 'StatusBar');
  end;

  with Cl.Add(TStatusPanels) do
  begin
    //RegisterConstructor(@TStatusBar.Create, 'Create');
    RegisterMethod(@TStatusPanels.Add, 'Add');
    RegisterPropertyHelper(@TStatusPanelsItems_R, @TStatusPanelsItems_W, 'Items');
    RegisterPropertyHelper(@TStatusPanelsStatusBar_R, nil, 'StatusBar');
  end;

  with Cl.Add(TStatusBar) do
  begin
    RegisterMethod(@TStatusBar.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TStatusBar.EndUpdate, 'EndUpdate');
    RegisterMethod(@TStatusBar.GetPanelIndexAt, 'GetPanelIndexAt');
    RegisterMethod(@TStatusBar.SizeGripEnabled, 'SizeGripEnabled');
    RegisterMethod(@TStatusBar.UpdatingStatusBar, 'UpdatingStatusBar');
    RegisterPropertyHelper(@TStatusBarCanvas_R, nil, 'Canvas');
  end;
end;

procedure TToolBarButtonCount_R(Self: TToolBar; var T: Integer); begin T := Self.ButtonCount; end;
procedure TToolBarButtons_R(Self: TToolBar; var T: TToolButton; I: Integer); begin T := Self.Buttons[I]; end;
procedure TToolBarRowCount_R(Self: TToolBar; var T: Integer); begin T := Self.RowCount; end;

procedure RIRegister_Toolbar(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TToolButton) do
  begin
    RegisterMethod(@TToolButton.Click, 'Click');
  end;

  with Cl.Add(TToolbar) do
  begin
    RegisterPropertyHelper(@TToolBarButtonCount_R, nil, 'ButtonCount');
    RegisterPropertyHelper(@TToolBarButtons_R, nil, 'Buttons');
    RegisterPropertyHelper(@TToolBarRowCount_R, nil, 'RowCount');
  end;
end;

procedure TWindowParams_R(Self: TWindow; var T: TParamList); begin T := Self.Params; end;
procedure TWindowAutoPosition_R(Self: TWindow; var T: Boolean); begin T := Self.AutoPosition; end;
procedure TWindowAutoPosition_W(Self: TWindow; T: Boolean); begin Self.AutoPosition := T; end;

procedure TListWindowButtons_R(Self: TListWindow; var T: TButtonPanel); begin T := Self.Buttons; end;
procedure TListWindowFormView_R(Self: TListWindow; var T: TFormView); begin T := TFormView(Self.FormView); end;
procedure TListWindowToolbar_R(Self: TListWindow; var T: TToolbar); begin T := Self.Toolbar; end;
function TListWindowCreateWindow(Self: TClass; CreateNewInstance: Boolean; const aFormName: String; aViewType: TViewType): TListWindow;
begin Result := TListWindow.CreateListWindow(AFormName, aViewType); end;

procedure TEditWindowForm_R(Self: TEditWindow; var T: TdxForm); begin T := Self.Form; end;
procedure TEditWindowScrollBox_R(Self: TEditWindow; var T: TScrollBox); begin T := Self.ScrollBox; end;
procedure TEditWindowButtons_R(Self: TEditWindow; var T: TButtonPanel); begin T := Self.Buttons; end;

procedure RIRegister_Window(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TWindow) do
  begin
    RegisterConstructor(@TWindow.CreateWindow, 'CreateWindow');
    RegisterConstructor(@TWindow.CreateWindow, 'Create');
    RegisterPropertyHelper(@TWindowParams_R, nil, 'Params');
    RegisterPropertyHelper(@TWindowAutoPosition_R, @TWindowAutoPosition_W, 'AutoPosition');
  end;

  with Cl.Add(TListWindow) do
  begin
    RegisterConstructor(@TListWindowCreateWindow, 'CreateWindow');
    RegisterConstructor(@TListWindowCreateWindow, 'Create');
    RegisterPropertyHelper(@TListWindowButtons_R, nil, 'Buttons');
    RegisterPropertyHelper(@TListWindowFormView_R, nil, 'FormView');
    RegisterPropertyHelper(@TListWindowToolbar_R, nil, 'Toolbar');
  end;

  with Cl.Add(TEditWindow) do
  begin
    RegisterMethod(@TEditWindow.Show, 'Show');
    RegisterPropertyHelper(@TEditWindowForm_R, nil, 'Form');
    RegisterPropertyHelper(@TEditWindowScrollBox_R, nil, 'ScrollBox');
    RegisterPropertyHelper(@TEditWindowButtons_R, nil, 'Buttons');
  end;
end;

procedure TMainFmFormViews_R(Self: TMainFm; var T: TFormView; I: Integer); begin T := Self.FormViews[I]; end;
procedure TMainFmPages_R(Self: TMainFm; var T: TPageControl); begin T := Self.Pages; end;
procedure TMainFmToolbar_R(Self: TMainFm; var T: TToolbar); begin T := Self.Toolbar; end;
procedure TMainFmStatusBar_R(Self: TMainFm; var T: TStatusBar); begin T := Self.StatusBar; end;
procedure TMainFmParams_R(Self: TMainFm; var T: TParamList); begin T := Self.Params; end;
procedure TMainFmOnCreateForm_R(Self: TMainFm; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TMainFmOnCreateForm_W(Self: TMainFm; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;
procedure TMainFmOnDestroyForm_R(Self: TMainFm; var T: TCreateFormEvent); begin T := Self.OnDestroyForm; end;
procedure TMainFmOnDestroyForm_W(Self: TMainFm; T: TCreateFormEvent); begin Self.OnDestroyForm := T; end;
procedure TMainFmOnCreateListWindow_R(Self: TMainFm; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TMainFmOnCreateListWindow_W(Self: TMainFm; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
procedure TMainFmOnCreateReportWindow_R(Self: TMainFm; var T: TCreateReportWindowEvent); begin T := Self.OnCreateReportWindow; end;
procedure TMainFmOnCreateReportWindow_W(Self: TMainFm; T: TCreateReportWindowEvent); begin Self.OnCreateReportWindow := T; end;
procedure TMainFmOnDatabaseClose_R(Self: TMainFm; var T: TNotifyEvent); begin T := Self.OnDatabaseClose; end;
procedure TMainFmOnDatabaseClose_W(Self: TMainFm; T: TNotifyEvent); begin Self.OnDatabaseClose := T; end;
procedure TMainFmOnDatabaseCloseQuery_R(Self: TMainFm; var T: TCloseQueryEvent); begin T := Self.OnDatabaseCloseQuery; end;
procedure TMainFmOnDatabaseCloseQuery_W(Self: TMainFm; T: TCloseQueryEvent); begin Self.OnDatabaseCloseQuery := T; end;
procedure TMainFmOnFatalError_R(Self: TMainFm; var T: TFatalErrorEvent); begin T := Self.OnFatalError; end;
procedure TMainFmOnFatalError_W(Self: TMainFm; T: TFatalErrorEvent); begin Self.OnFatalError := T; end;

procedure RIRegister_MainFm(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMainFm) do
  begin
    RegisterMethod(@TMainFm.CreatePage, 'CreatePage');
    RegisterMethod(@TMainFm.DestroyPage, 'DestroyPage');
    RegisterPropertyHelper(@TMainFmFormViews_R, nil, 'FormViews');
    RegisterPropertyHelper(@TMainFmPages_R, nil, 'Pages');
    RegisterPropertyHelper(@TMainFmToolbar_R, nil, 'Toolbar');
    RegisterPropertyHelper(@TMainFmStatusBar_R, nil, 'StatusBar');
    RegisterPropertyHelper(@TMainFmParams_R, nil, 'Params');
    RegisterEventPropertyHelper(@TMainFmOnCreateForm_R, @TMainFmOnCreateForm_W, 'OnCreateForm');
    RegisterEventPropertyHelper(@TMainFmOnDestroyForm_R, @TMainFmOnDestroyForm_W, 'OnDestroyForm');
    RegisterEventPropertyHelper(@TMainFmOnCreateListWindow_R, @TMainFmOnCreateListWindow_W, 'OnCreateListWindow');
    RegisterEventPropertyHelper(@TMainFmOnCreateReportWindow_R, @TMainFmOnCreateReportWindow_W, 'OnCreateReportWindow');
    RegisterEventPropertyHelper(@TMainFmOnDatabaseClose_R, @TMainFmOnDatabaseClose_W, 'OnDatabaseClose');
    RegisterEventPropertyHelper(@TMainFmOnDatabaseCloseQuery_R, @TMainFmOnDatabaseCloseQuery_W, 'OnDatabaseCloseQuery');
    RegisterEventPropertyHelper(@TMainFmOnFatalError_R, @TMainFmOnFatalError_W, 'OnFatalError');
  end;
end;

procedure TFormViewGrid_R(Self: TFormView; var T: TdxGrid); begin T := Self.Grid; end;
procedure TFormViewForm_R(Self: TFormView; var T: TdxForm); begin T := Self.Form; end;
procedure TFormViewTree_R(Self: TFormView; var T: TdxFormTree); begin T := Self.Tree; end;
procedure TFormViewScrollBox_R(Self: TFormView; var T: TScrollBox); begin T := Self.ScrollBox; end;
procedure TFormViewTreeSplitter_R(Self: TFormView; var T: TSplitter); begin T := Self.TreeSplitter; end;
procedure TFormViewFormSplitter_R(Self: TFormView; var T: TSplitter); begin T := Self.FormSplitter; end;

procedure RIRegister_FormView(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFormView) do
  begin
    RegisterConstructor(@TFormView.CreateView, 'CreateView');
    RegisterConstructor(@TFormView.CreateView, 'Create');
    RegisterPropertyHelper(@TFormViewGrid_R, nil, 'Grid');
    RegisterPropertyHelper(@TFormViewForm_R, nil, 'Form');
    RegisterPropertyHelper(@TFormViewTree_R, nil, 'Tree');
    RegisterPropertyHelper(@TFormViewScrollBox_R, nil, 'ScrollBox');
    RegisterPropertyHelper(@TFormViewTreeSplitter_R, nil, 'TreeSplitter');
    RegisterPropertyHelper(@TFormViewFormSplitter_R, nil, 'FormSplitter');
  end;
end;

procedure RIRegister_Clipboard(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TClipboard) do
  begin
    RegisterConstructor(@TClipboard.Create, 'Create');
    //RegisterMethod(@TClipboard.Close, 'Close');
    RegisterMethod(@TClipboard.Clear, 'Clear');
    RegisterMethod(@TClipboardHasText, 'HasText');
    RegisterMethod(@TClipboardHasBitmap, 'HasBitmap');
    //RegisterMethod(@TClipboard.Open, 'Open');
    RegisterPropertyHelper(@TClipboardAsText_R, @TClipboardAsText_W, 'AsText');
  end;
end;

procedure RIRegister_IniFiles(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIniFile) do
  begin
    RegisterConstructor(@TIniFileCreate, 'Create');
    RegisterMethod(@TIniFile.SectionExists, 'SectionExists');
    RegisterMethod(@TIniFile.ReadString, 'ReadString');
    RegisterMethod(@TIniFile.WriteString, 'WriteString');
    RegisterMethod(@TIniFile.ReadInteger, 'ReadInteger');
    RegisterMethod(@TIniFile.WriteInteger, 'WriteInteger');
    RegisterMethod(@TIniFile.ReadInt64, 'ReadInt64');
    RegisterMethod(@TIniFile.WriteInt64, 'WriteInt64');
    RegisterMethod(@TIniFile.ReadBool, 'ReadBool');
    RegisterMethod(@TIniFile.WriteBool, 'WriteBool');
    RegisterMethod(@TIniFile.ReadDate, 'ReadDate');
    RegisterMethod(@TIniFile.ReadDateTime, 'ReadDateTime');
    RegisterMethod(@TIniFile.ReadFloat, 'ReadFloat');
    RegisterMethod(@TIniFile.ReadTime, 'ReadTime');
    RegisterMethod(@TIniFile.ReadBinaryStream, 'ReadBinaryStream');
    RegisterMethod(@TIniFile.WriteDate, 'WriteDate');
    RegisterMethod(@TIniFile.WriteDateTime, 'WriteDateTime');
    RegisterMethod(@TIniFile.WriteFloat, 'WriteFloat');
    RegisterMethod(@TIniFile.WriteTime, 'WriteTime');
    RegisterMethod(@TIniFile.WriteBinaryStream, 'WriteBinaryStream');
    RegisterMethod(@TIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TIniFile.DeleteKey, 'DeleteKey');
    RegisterMethod(@TIniFile.UpdateFile, 'UpdateFile');
    RegisterMethod(@TIniFile.ValueExists, 'ValueExists');
    RegisterPropertyHelper(@TIniFileFileName_R, nil, 'FileName');
    RegisterPropertyHelper(@TIniFileEscapeLineFeeds_R, nil, 'EscapeLineFeeds');
    RegisterPropertyHelper(@TIniFileCaseSensitive_R, @TIniFileCaseSensitive_W, 'CaseSensitive');
    RegisterPropertyHelper(@TIniFileStripQuotes_R, @TIniFileStripQuotes_W, 'StripQuotes');

    RegisterMethod(@TIniFile.ReadSectionRaw, 'ReadSectionRaw');
    RegisterPropertyHelper(@TIniFileCacheUpdates_R, @TIniFileCacheUpdates_W, 'CacheUpdates');
  end;
end;

procedure TReportWindowToolbar_R(Self: TReportWindow; var T: TToolBar); begin T := Self.ToolBar; end;
procedure TReportWindowQGrid_R(Self: TReportWindow; var T: TdxQueryGrid); begin T := Self.QGrid; end;
procedure TReportWindowStatusBar_R(Self: TReportWindow; var T: TStatusBar); begin T := Self.StatusBar; end;
procedure TReportWindowFilterSplitter_R(Self: TReportWindow; var T: TSplitter); begin T := Self.FilterSplitter; end;
procedure TReportWindowFilter_R(Self: TReportWindow; var T: TCustomControl); begin T := Self.Filter; end;

procedure RIRegister_ReportWindow(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TReportWindow) do
  begin
    RegisterConstructor(@TReportWindow.CreateWindow, 'CreateWindow');
    RegisterConstructor(@TReportWindow.CreateWindow, 'Create');
    RegisterMethod(@TReportWindow.ShowReport, 'ShowReport');
    RegisterPropertyHelper(@TReportWindowToolbar_R, nil, 'ToolBar');
    RegisterPropertyHelper(@TReportWindowQGrid_R, nil, 'QGrid');
    RegisterPropertyHelper(@TReportWindowStatusBar_R, nil, 'StatusBar');
    RegisterPropertyHelper(@TReportWindowFilterSplitter_R, nil, 'FilterSplitter');
    RegisterPropertyHelper(@TReportWindowFilter_R, nil, 'Filter');
  end;
end;

procedure TKGridCells_R(Self: TKGrid; var T: String; C, R: Integer); begin T := Self.Cells[C, R]; end;
procedure TKGridCells_W(Self: TKGrid; T: String; C, R: Integer); begin Self.Cells[C, R] := T; end;
procedure TKGridCellSpan_R(Self: TKGrid; var T: TKGridCellSpan; C, R: Integer); begin T := Self.CellSpan[C, R]; end;
procedure TKGridCellSpan_W(Self: TKGrid; T: TKGridCellSpan; C, R: Integer); begin Self.CellSpan[C, R] := T; end;
procedure TKGridCol_R(Self: TKGrid; var T: Integer); begin T := Self.Col; end;
procedure TKGridCol_W(Self: TKGrid; T: Integer); begin Self.Col := T; end;
procedure TKGridColWidths_R(Self: TKGrid; var T: Integer; I: Integer); begin T := Self.ColWidths[I]; end;
procedure TKGridColWidths_W(Self: TKGrid; T, I: Integer); begin Self.ColWidths[I] := T; end;
procedure TKGridLeftCol_R(Self: TKGrid; var T: Integer); begin T := Self.LeftCol; end;
procedure TKGridLeftCol_W(Self: TKGrid; T: Integer); begin Self.LeftCol := T; end;
procedure TKGridObjects_R(Self: TKGrid; var T: TObject; C, R: Integer); begin T := Self.Objects[C, R]; end;
procedure TKGridObjects_W(Self: TKGrid; T: TObject; C, R: Integer); begin Self.Objects[C, R] := T; end;
procedure TKGridRow_R(Self: TKGrid; var T: Integer); begin T := Self.Row; end;
procedure TKGridRow_W(Self: TKGrid; T: Integer); begin Self.Row := T; end;
procedure TKGridRowHeights_R(Self: TKGrid; var T: Integer; I: Integer); begin T := Self.RowHeights[I]; end;
procedure TKGridRowHeights_W(Self: TKGrid; T, I: Integer); begin Self.RowHeights[I] := T; end;
procedure TKGridSelection_R(Self: TKGrid; var T: TKGridRect); begin T := Self.Selection; end;
procedure TKGridSelection_W(Self: TKGrid; T: TKGridRect); begin Self.Selection := T; end;
procedure TKGridSelectionCount_R(Self: TKGrid; var T: Integer); begin T := Self.SelectionCount; end;
procedure TKGridSelectionRect_R(Self: TKGrid; var T: TRect); begin T := Self.SelectionRect; end;
procedure TKGridSelections_R(Self: TKGrid; var T: TKGridRect; I: Integer); begin T := Self.Selections[I]; end;
procedure TKGridSelections_W(Self: TKGrid; T: TKGridRect; I: Integer); begin Self.Selections[I] := T; end;
procedure TKGridTopRow_R(Self: TKGrid; var T: Integer); begin T := Self.TopRow; end;
procedure TKGridTopRow_W(Self: TKGrid; T: Integer); begin Self.TopRow := T; end;
procedure TKGridVisibleColCount_R(Self: TKGrid; var T: Integer); begin T := Self.VisibleColCount; end;
procedure TKGridVisibleRowCount_R(Self: TKGrid; var T: Integer); begin T := Self.VisibleRowCount; end;
procedure TKGridOnSelectionChanged_R(Self: TKGrid; var T: TNotifyEvent); begin T := Self.OnSelectionChanged; end;
procedure TKGridOnSelectionChanged_W(Self: TKGrid; T: TNotifyEvent); begin Self.OnSelectionChanged := T; end;

procedure RIRegister_KGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TKGridColors) do
  begin

  end;

  with Cl.Add(TKGrid) do
  begin
    RegisterVirtualMethod(@TKGrid.CellSelected, 'CellSelected');
    RegisterVirtualMethod(@TKGrid.DeleteRow, 'DeleteRow');
    {RegisterMethod(@TKGrid.DeleteCol, 'DeleteCol');
    RegisterMethod(@TKGrid.InsertCol, 'InsertCol');
    RegisterMethod(@TKGrid.InsertRow, 'InsertRow'); }
    RegisterMethod(@TKGrid.MouseToCell, 'MouseToCell');
    //RegisterVirtualMethod(@TKGrid.LockUpdate, 'LockUpdate');
    //RegisterVirtualMethod(@TKGrid.UnLockUpdate, 'UnLockUpdate');
    RegisterPropertyHelper(@TKGridCells_R, @TKGridCells_W, 'Cells');
    RegisterPropertyHelper(@TKGridCellSpan_R, @TKGridCellSpan_W, 'CellSpan');
    RegisterPropertyHelper(@TKGridCol_R, @TKGridCol_W, 'Col');
    RegisterPropertyHelper(@TKGridColWidths_R, @TKGridColWidths_W, 'ColWidths');
    RegisterPropertyHelper(@TKGridLeftCol_R, @TKGridLeftCol_W, 'LeftCol');
    RegisterPropertyHelper(@TKGridObjects_R, @TKGridObjects_W, 'Objects');
    RegisterPropertyHelper(@TKGridRow_R, @TKGridRow_W, 'Row');
    RegisterPropertyHelper(@TKGridRowHeights_R, @TKGridRowHeights_W, 'RowHeights');
    RegisterPropertyHelper(@TKGridSelection_R, @TKGridSelection_W, 'Selection');
    RegisterPropertyHelper(@TKGridSelectionCount_R, nil, 'SelectionCount');
    RegisterPropertyHelper(@TKGridSelectionRect_R, nil, 'SelectionRect');
    RegisterPropertyHelper(@TKGridSelections_R, @TKGridSelections_W, 'Selections');
    RegisterPropertyHelper(@TKGridTopRow_R, @TKGridTopRow_W, 'TopRow');
    RegisterPropertyHelper(@TKGridVisibleColCount_R, nil, 'VisibleColCount');
    RegisterPropertyHelper(@TKGridVisibleRowCount_R, nil, 'VisibleRowCount');
    RegisterEventPropertyHelper(@TKGridOnSelectionChanged_R, @TKGridOnSelectionChanged_W, 'OnSelectionChanged');
  end;

  with Cl.Add(TdxPivotGrid) do
  begin

  end;
end;

procedure TdxSQLQueryFields_R(Self: TdxSQLQuery; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxSQLQueryFields_W(Self: TdxSQLQuery; T: Variant; I: String); begin Self.Fields[I] := T; end;
procedure TdxSQLQueryField_R(Self: TdxSQLQuery; var T: TField; I: Integer); begin T := Self.Field[I]; end;
procedure TdxSQLQueryAsI_R(Self: TdxSQLQuery; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxSQLQueryAsI_W(Self: TdxSQLQuery; T: Integer; I: String); begin Self.AsI[I] := T; end;
procedure TdxSQLQueryAsF_R(Self: TdxSQLQuery; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxSQLQueryAsF_W(Self: TdxSQLQuery; T: Extended; I: String); begin Self.AsF[I] := T; end;
procedure TdxSQLQueryAsDT_R(Self: TdxSQLQuery; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxSQLQueryAsDT_W(Self: TdxSQLQuery; T: TDateTime; I: String); begin Self.AsDT[I] := T; end;
procedure TdxSQLQueryAsS_R(Self: TdxSQLQuery; var T: String; I: String); begin T := Self.AsS[I]; end;
procedure TdxSQLQueryAsS_W(Self: TdxSQLQuery; T: String; I: String); begin Self.AsS[I] := T; end;
procedure TdxSQLQueryState_R(Self: TdxSQLQuery; var T: TDataSetState); begin T := Self.State; end;
procedure TdxSQLQueryUseGenerator_R(Self: TdxSQLQuery; var T: TUseGeneratorOption); begin T := Self.UseGenerator; end;
procedure TdxSQLQueryUseGenerator_W(Self: TdxSQLQuery; T: TUseGeneratorOption); begin Self.UseGenerator := T; end;
procedure TdxSQLQueryUseExecuteBlock_R(Self: TdxSQLQuery; var T: Boolean); begin T := Self.UseExecuteBlock; end;
procedure TdxSQLQueryUseExecuteBlock_W(Self: TdxSQLQuery; T: Boolean); begin Self.UseExecuteBlock := T; end;

procedure RIRegister_dxSQLQuery(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxSQLQuery) do
  begin
    RegisterConstructor(@TdxSQLQuery.Create, 'Create');
    RegisterMethod(@TdxSQLQuery.Open, 'Open');
    RegisterMethod(@TdxSQLQuery.Close, 'Close');
    RegisterMethod(@TdxSQLQuery.Opened, 'Opened');
    RegisterMethod(@TdxSQLQuery.Append, 'Append');
    RegisterMethod(@TdxSQLQuery.Edit, 'Edit');
    RegisterMethod(@TdxSQLQuery.Delete, 'Delete');
    RegisterMethod(@TdxSQLQuery.Cancel, 'Cancel');
    RegisterMethod(@TdxSQLQuery.Post, 'Post');
    RegisterMethod(@TdxSQLQuery.ApplyUpdates, 'ApplyUpdates');
    RegisterMethod(@TdxSQLQuery.CancelUpdates, 'CancelUpdates');
    RegisterMethod(@TdxSQLQuery.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxSQLQuery.MovePrior, 'MovePrior');
    RegisterMethod(@TdxSQLQuery.MoveNext, 'MoveNext');
    RegisterMethod(@TdxSQLQuery.MoveLast, 'MoveLast');
    RegisterMethod(@TdxSQLQuery.MoveBy, 'MoveBy');
    RegisterMethod(@TdxSQLQuery.MoveTo, 'MoveTo');
    RegisterMethod(@TdxSQLQuery.BOF, 'BOF');
    RegisterMethod(@TdxSQLQuery.EOF, 'EOF');
    RegisterMethod(@TdxSQLQuery.RecNo, 'RecNo');
    RegisterMethod(@TdxSQLQuery.RecordCount, 'RecordCount');
    RegisterMethod(@TdxSQLQuery.FieldCount, 'FieldCount');
    //RegisterMethod(@TdxSQLQuery.GenId, 'GenId');
    RegisterMethod(@TdxSQLQuery.Locate, 'Locate');
    RegisterMethod(@TdxSQLQuery.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TdxSQLQuery.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TdxSQLQueryFields_R, @TdxSQLQueryFields_W, 'Fields');
    RegisterPropertyHelper(@TdxSQLQueryField_R, nil, 'Field');
    RegisterPropertyHelper(@TdxSQLQueryAsI_R, @TdxSQLQueryAsI_W, 'AsI');
    RegisterPropertyHelper(@TdxSQLQueryAsF_R, @TdxSQLQueryAsF_W, 'AsF');
    RegisterPropertyHelper(@TdxSQLQueryAsDT_R, @TdxSQLQueryAsDT_W, 'AsDT');
    RegisterPropertyHelper(@TdxSQLQueryAsS_R, @TdxSQLQueryAsS_W, 'AsS');
    RegisterPropertyHelper(@TdxSQLQueryState_R, nil, 'State');
    RegisterPropertyHelper(@TdxSQLQueryUseGenerator_R, @TdxSQLQueryUseGenerator_W, 'UseGenerator');
    RegisterPropertyHelper(@TdxSQLQueryUseExecuteBlock_R,	@TdxSQLQueryUseExecuteBlock_W, 'UseExecuteBlock');
  end;
end;

{procedure TdxQFieldFieldName_R(Self: TdxQField; var T: String); begin T := Self.FieldName; end;
procedure TdxQFieldName_R(Self: TdxQField; var T: String); begin T := Self.Name; end;
procedure TdxQFieldFunc_R(Self: TdxQField; var T: TRpTotalFunc); begin T := Self.Func; end;
procedure TdxQFieldFieldType_R(Self: TdxQField; var T: TRpFieldType); begin T := Self.FieldType; end;

procedure TdxQueryFields_R(Self: TdxQuery; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxQueryAsI_R(Self: TdxQuery; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxQueryAsF_R(Self: TdxQuery; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxQueryAsDT_R(Self: TdxQuery; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxQueryAsS_R(Self: TdxQuery; var T: String; I: String); begin T := Self.AsS[I]; end;

procedure RIRegister_dxQuery(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxQField) do
  begin
    RegisterPropertyHelper(@TdxQFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxQFieldName_R, nil, 'Name');
    RegisterPropertyHelper(@TdxQFieldFunc_R, nil, 'Func');
    RegisterPropertyHelper(@TdxQFieldFieldType_R, nil, 'FieldType');
  end;
  with Cl.Add(TdxQSource) do
  begin
    RegisterConstructor(@TdxQSource.Create, 'Create');
    RegisterMethod(@TdxQSource.AddField, 'AddField');
  end;
  with Cl.Add(TdxQuery) do
  begin
    RegisterConstructor(@TdxQuery.Create, 'Create');
    RegisterMethod(@TdxQuery.AddSource, 'AddSource');
    RegisterMethod(@TdxQuery.AddSorting, 'AddSorting');
    RegisterMethod(@TdxQuery.GroupByDate, 'GroupByDate');
    RegisterMethod(@TdxQuery.Open, 'Open');
    RegisterMethod(@TdxQuery.Close, 'Close');
    RegisterMethod(@TdxQuery.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxQuery.MovePrior, 'MovePrior');
    RegisterMethod(@TdxQuery.MoveNext, 'MoveNext');
    RegisterMethod(@TdxQuery.MoveLast, 'MoveLast');
    RegisterMethod(@TdxQuery.MoveBy, 'MoveBy');
    RegisterMethod(@TdxQuery.MoveTo, 'MoveTo');
    RegisterMethod(@TdxQuery.BOF, 'BOF');
    RegisterMethod(@TdxQuery.EOF, 'EOF');
    RegisterMethod(@TdxQuery.RecNo, 'RecNo');
    RegisterMethod(@TdxQuery.RecId, 'RecId');
    RegisterMethod(@TdxQuery.RecordCount, 'RecordCount');
    RegisterPropertyHelper(@TdxQueryFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxQueryAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxQueryAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxQueryAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxQueryAsS_R, nil, 'AsS');
  end;
end;  }

procedure TdxQueryGridQueryName_R(Self: TdxQueryGrid; var T: String); begin T := Self.QueryName; end;
procedure TdxQueryGridFields_R(Self: TdxQueryGrid; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxQueryGridAsI_R(Self: TdxQueryGrid; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxQueryGridAsF_R(Self: TdxQueryGrid; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxQueryGridAsDT_R(Self: TdxQueryGrid; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxQueryGridAsS_R(Self: TdxQueryGrid; var T: String; I: String); begin T := Self.AsS[I]; end;

procedure TdxQueryGridOnCreateForm_R(Self: TdxQueryGrid; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxQueryGridOnCreateForm_W(Self: TdxQueryGrid; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;

procedure TdxQueryGridEditable_R(Self: TdxQueryGrid; var T: Boolean); begin T := Self.Editable; end;

procedure RIRegister_dxQueryGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxQueryGrid) do
  begin
    RegisterMethod(@TdxQueryGrid.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxQueryGrid.MovePrior, 'MovePrior');
    RegisterMethod(@TdxQueryGrid.MoveNext, 'MoveNext');
    RegisterMethod(@TdxQueryGrid.MoveLast, 'MoveLast');
    RegisterMethod(@TdxQueryGrid.MoveBy, 'MoveBy');
    RegisterMethod(@TdxQueryGrid.MoveTo, 'MoveTo');
    RegisterMethod(@TdxQueryGrid.EOF, 'EOF');
    RegisterMethod(@TdxQueryGrid.BOF, 'BOF');
    RegisterMethod(@TdxQueryGrid.RecNo, 'RecNo');
    RegisterMethod(@TdxQueryGrid.RecId, 'RecId');
    RegisterMethod(@TdxQueryGrid.EnableControls, 'EnableControls');
    RegisterMethod(@TdxQueryGrid.DisableControls, 'DisableControls');
    RegisterMethod(@TdxQueryGrid.ControlsDisabled, 'ControlsDisabled');
    RegisterMethod(@TdxQueryGrid.RecordCount, 'RecordCount');
    RegisterMethod(@TdxQueryGrid.Locate, 'Locate');
    RegisterMethod(@TdxQueryGrid.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxQueryGrid.Refresh, 'Refresh');
    RegisterMethod(@TdxQueryGrid.Close, 'Close');

    RegisterMethod(@TdxQueryGrid.DisableScrollEvents, 'DisableScrollEvents');
    RegisterMethod(@TdxQueryGrid.EnableScrollEvents, 'EnableScrollEvents');
    RegisterMethod(@TdxQueryGrid.ScrollEventsDisabled, 'ScrollEventsDisabled');

    RegisterPropertyHelper(@TdxQueryGridQueryName_R, nil, 'QueryName');
    RegisterPropertyHelper(@TdxQueryGridFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxQueryGridAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxQueryGridAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxQueryGridAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxQueryGridAsS_R, nil, 'AsS');
    RegisterEventPropertyHelper(@TdxQueryGridOnCreateForm_R, @TdxQueryGridOnCreateForm_W,
      'OnCreateForm');
    RegisterPropertyHelper(@TdxQueryGridEditable_R, nil, 'Editable');
  end;
end;

procedure TdxFileSourceFileName_R(Self: TdxFile; var T: String); begin T := Self.SourceFileName; end;
procedure TdxFileStoredFileName_R(Self: TdxFile; var T: String); begin T := Self.StoredFileName; end;
procedure TdxFileDescription_R(Self: TdxFile; var T: String); begin T := Self.Description; end;
procedure TdxFileDescription_W(Self: TdxFile; T: String); begin Self.Description := T; end;

procedure RIRegister_dxFile(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxFile) do
  begin
    RegisterMethod(@TdxFile.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxFile.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxFile.SaveToStream, 'SaveToStream');
    RegisterMethod(@TdxFile.Clear, 'Clear');
    RegisterMethod(@TdxFile.WasChanged, 'WasChanged');
    RegisterPropertyHelper(@TdxFileSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxFileStoredFileName_R, nil, 'StoredFileName');
    RegisterPropertyHelper(@TdxFileDescription_R, @TdxFileDescription_W, 'Description');
  end;
end;

procedure TdxDBImageReadOnly_R(Self: TdxDBImage; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TdxDBImageReadOnly_W(Self: TdxDBImage; T: Boolean); begin Self.ReadOnly := T; end;
procedure TdxDBImageSourceFileName_R(Self: TdxDBImage; var T: String); begin T := Self.SourceFileName; end;
procedure TdxDBImageStoredFileName_R(Self: TdxDBImage; var T: String); begin T := Self.StoredFileName; end;

procedure RIRegister_dxDBImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDBImage) do
  begin
    RegisterMethod(@TdxDBImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxDBImage.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxDBImage.SaveToStream, 'SaveToStream');
    RegisterMethod(@TdxDBImage.WasChanged, 'WasChanged');
    RegisterPropertyHelper(@TdxDBImageReadOnly_R, @TdxDBImageReadOnly_W, 'ReadOnly');
    RegisterPropertyHelper(@TdxDBImageSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxDBImageStoredFileName_R, nil, 'StoredFileName');
  end;
end;

//procedure TdxButtonOnButtonClick_R(Self: TdxButton; var T: TNotifyEvent); begin T := Self.OnButtonClick; end;
//procedure TdxButtonOnButtonClick_W(Self: TdxButton; T: TNotifyEvent); begin Self.OnButtonClick := T; end;

procedure RIRegister_dxButton(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxButton) do
  begin
    //RegisterEventPropertyHelper(@TdxButtonOnButtonClick_R, @TdxButtonOnButtonClick_W, 'OnClick');
  end;
end;

procedure RIRegister_dxCounter(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCounter) do
  begin

  end;
end;

procedure RIRegister_dxTimeEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTimeEdit) do
  begin

  end;
end;

procedure RIRegister_DBTimeEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBTimeEdit) do
  begin

  end;
end;

procedure RIRegister_dxObjectField(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxObjectField) do
  begin

  end;
end;

procedure RIRegister_dxShape(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxShape) do
  begin

  end;
end;

procedure RIRegister_dxPageControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTabSheet) do
  begin

  end;

  with Cl.Add(TdxPageControl) do
  begin

  end;
end;

procedure TPageControlPages_R(Self: TPageControl; var T: TTabSheet; I: Integer); begin T := Self.Pages[I]; end;
procedure TPageControlPageCount_R(Self: TPageControl; var T: Integer); begin T := Self.PageCount; end;
procedure TPageControlActivePageIndex_R(Self: TPageControl; var T: Integer); begin T := Self.ActivePageIndex; end;
procedure TPageControlActivePageIndex_W(Self: TPageControl; T: Integer); begin Self.ActivePageIndex := T; end;

procedure TTabSheetTabIndex_R(Self: TTabSheet; var T: Integer); begin T := Self.TabIndex; end;
procedure TTabSheetPageControl_R(Self: TTabSheet; var T: TPageControl); begin T := Self.PageControl; end;
procedure TTabSheetPageControl_W(Self: TTabSheet; T: TPageControl); begin Self.PageControl := T; end;
function TPageControlIndexOfPageAt(Self: TPageControl; X, Y: Integer): Integer;
begin Result := Self.IndexOfPageAt(X, Y); end;

procedure RIRegister_PageControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomTabControl) do
  begin

  end;

  with Cl.Add(TPageControl) do
  begin
    RegisterMethod(@TPageControlIndexOfPageAt, 'IndexOfPageAt');
    RegisterPropertyHelper(@TPageControlPages_R, nil, 'Pages');
    RegisterPropertyHelper(@TPageControlPageCount_R, nil, 'PageCount');
    RegisterPropertyHelper(@TPageControlActivePageIndex_R, @TPageControlActivePageIndex_W, 'ActivePageIndex');
  end;

  with Cl.Add(TCustomPage) do
  begin

  end;

  with Cl.Add(TTabSheet) do
  begin
    RegisterPropertyHelper(@TTabSheetTabIndex_R, nil, 'TabIndex');
    RegisterPropertyHelper(@TTabSheetPageControl_R, @TTabSheetPageControl_W, 'PageControl');
  end;
end;

function TCustomImageListAddFromFile(Self: TCustomImageList; const FileName: String): Integer;
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(FileName, True);
  try
    Result := Self.Add(Bmp.Bitmap, nil);
  finally
    Bmp.Free;
  end;
end;

function TCustomImageListAddFromStream(Self: TCustomImageList; Stream: TStream): Integer;
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(Stream);
  try
    Result := Self.Add(Bmp.Bitmap, nil);
  finally
    Bmp.Free;
  end;
end;

function TCustomImageListAddFromStringBase64(Self: TCustomImageList; const S: String): Integer;
var
  St: TStringStream;
begin
  St := TStringStream.Create(DecodeBase64(S, False));
  try
    Result := TCustomImageListAddFromStream(Self, St);
  finally
    St.Free;
  end;
end;

function TCustomImageListAddImage(Self: TCustomImageList; const AImageName: String): Integer;
var
  St: TStream;
begin
  Result := -1;
  ImageMan.GetImageStreamPPI(AImageName, St);
  if St <> nil then
    try
      Result := TCustomImageListAddFromStream(Self, St);
    finally
      St.Free;
    end;
end;

procedure TCustomImageListCount_R(Self: TCustomImageList; var T: Integer); begin T := Self.Count; end;

procedure RIRegister_ImageList(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomImageList) do
  begin
    RegisterMethod(@TCustomImageList.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TCustomImageList.EndUpdate, 'EndUpdate');
    RegisterMethod(@TCustomImageList.Add, 'Add');
    RegisterMethod(@TCustomImageListAddFromFile, 'AddFromFile');
    RegisterMethod(@TCustomImageListAddFromStream, 'AddFromStream');
    RegisterMethod(@TCustomImageListAddFromStringBase64, 'AddFromStringBase64');
    RegisterMethod(@TCustomImageListAddImage, 'AddImage');
    RegisterMethod(@TCustomImageList.Clear, 'Clear');
    RegisterMethod(@TCustomImageList.Delete, 'Delete');
    RegisterMethod(@TCustomImageList.GetBitmap, 'GetBitmap');
    RegisterPropertyHelper(@TCustomImageListCount_R, nil, 'Count');
  end;

  with Cl.Add(TImageList) do
  begin
  end;
end;

procedure RIRegister_dxGroupBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxGroupBox) do
  begin

  end;
end;

procedure TdxGridForm_R(Self: TdxGrid; var T: TdxForm); begin T := Self.Form; end;

procedure RIRegister_dxGrid(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TdxForm);

  with Cl.Add(TdxGrid) do
  begin
    RegisterMethod(@TdxGrid.GetFieldNameByColumn, 'GetFieldName');
    RegisterMethod(@TdxGrid.FindColumnByFieldName, 'FindColumnByFieldName');
    RegisterPropertyHelper(@TdxGridForm_R, nil, 'Form');
  end;
end;

//procedure TGridColumnWidthChanged_R(Self: TGridColumn; var T: Boolean); begin T := Self.WidthChanged; end;
procedure TGridColumnGrid_R(Self: TGridColumn; var T: TDBGrid); begin T := TDBGrid(Self.Grid); end;

procedure TGridColumnTitleColumn_R(Self: TGridColumnTitle; var T: TColumn); begin T := TColumn(Self.Column); end;

procedure TColumnField_R(Self: TColumn; var T: TField); begin T := Self.Field; end;
procedure TColumnDesignIndex_R(Self: TColumn; var T: Integer); begin T := Self.DesignIndex; end;

procedure TDBGridColumnsItems_R(Self: TDBGridColumns; var T: TColumn; t1: Integer); begin T := Self.Items[t1]; end;
procedure TDBGridColumnsItems_W(Self: TDBGridColumns; T: TColumn; t1: Integer); begin Self.Items[t1] := T; end;


procedure TDBGridBorderColor_R(Self: TDBGrid; var T: TColor); begin T := Self.BorderColor; end;
procedure TDBGridBorderColor_W(Self: TDBGrid; T: TColor); begin Self.BorderColor := T; end;
//procedure TDBGridDefaultTextStyle_R(Self: TDBGrid; var T: TTextStyle); begin T := Self.DefaultTextStyle; end;
//procedure TDBGridDefaultTextStyle_W(Self: TDBGrid; T: TTextStyle); begin Self.DefaultTextStyle := T; end;
//procedure TDBGridEditorBorderStyle_R(Self: TDBGrid; var T: TBorderStyle); begin T := Self.EditorBorderStyle; end;
//procedure TDBGridEditorBorderStyle_W(Self: TDBGrid; T: TBorderStyle); begin Self.EditorBorderStyle := T; end;
//procedure TDBGridEditorMode_R(Self: TDBGrid; var T: Boolean); begin T := Self.EditorMode; end;
//procedure TDBGridEditorMode_W(Self: TDBGrid; T: Boolean); begin Self.EditorMode := T; end;
procedure TDBGridExtendedColSizing_R(Self: TDBGrid; var T: Boolean); begin T := Self.ExtendedColSizing; end;
procedure TDBGridExtendedColSizing_W(Self: TDBGrid; T: Boolean); begin Self.ExtendedColSizing := T; end;
//procedure TDBGridFastEditing_R(Self: TDBGrid; var T: Boolean); begin T := Self.FastEditing; end;
//procedure TDBGridFastEditing_W(Self: TDBGrid; T: Boolean); begin Self.FastEditing := T; end;
procedure TDBGridFocusColor_R(Self: TDBGrid; var T: TColor); begin T := Self.FocusColor; end;
procedure TDBGridFocusColor_W(Self: TDBGrid; T: TColor); begin Self.FocusColor := T; end;
procedure TDBGridFocusRectVisible_R(Self: TDBGrid; var T: Boolean); begin T := Self.FocusRectVisible; end;
procedure TDBGridFocusRectVisible_W(Self: TDBGrid; T: Boolean); begin Self.FocusRectVisible := T; end;
procedure TDBGridGridLineColor_R(Self: TDBGrid; var T: TColor); begin T := Self.GridLineColor; end;
procedure TDBGridGridLineColor_W(Self: TDBGrid; T: TColor); begin Self.GridLineColor := T; end;
procedure TDBGridGridLineStyle_R(Self: TDBGrid; var T: TPenStyle); begin T := Self.GridLineStyle; end;
procedure TDBGridGridLineStyle_W(Self: TDBGrid; T: TPenStyle); begin Self.GridLineStyle := T; end;
//procedure TDBGridInplaceEditor_R(Self: TDBGrid; var T: TWinControl); begin T := Self.InplaceEditor; end;
procedure TDBGridSelectedColor_R(Self: TDBGrid; var T: TColor); begin T := Self.SelectedColor; end;
procedure TDBGridSelectedColor_W(Self: TDBGrid; T: TColor); begin Self.SelectedColor := T; end;
procedure TDBGridSelectedColumn_R(Self: TDBGrid; var T: TColumn); begin T := Self.SelectedColumn; end;
procedure TDBGridSelectedField_R(Self: TDBGrid; var T: TField); begin T := Self.SelectedField; end;
procedure TDBGridSelectedIndex_R(Self: TDBGrid; var T: Integer); begin T := Self.SelectedIndex; end;
procedure TDBGridReadOnly_R(Self: TDBGrid; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TDBGridReadOnly_W(Self: TDBGrid; T: Boolean);
begin
  Self.ReadOnly := T;
  if T then Self.Options := Self.Options - [dgEditing]
  else Self.Options := Self.Options + [dgEditing];
end;
procedure TDBGridOptions_R(Self: TDBGrid; var T: TDbGridOptions); begin T := Self.Options; end;
procedure TDBGridOptions_W(Self: TDBGrid; T: TDbGridOptions);
begin
  Self.Options := T;
  Self.ReadOnly := not (dgEditing in T);
end;

procedure TMyDBGridSelectedRowCount_R(Self: TMyDBGrid; var T: Integer); begin T := Self.SelectedRowCount; end;
procedure TMyDBGridSortCols_R(Self: TMyDBGrid; var T: TSortColumns); begin T := Self.SortCols; end;

procedure TSortColListCols_R(Self: TSortColumns; var T: TSortColumn; I: Integer); begin T := Self.Cols[I]; end;
procedure TSortColListCount_R(Self: TSortColumns; var T: Integer); begin T := Self.Count; end;
procedure TSortColListAddCol(Self: TSortColumns; const FieldName: String; Desc: Boolean);
var
  C: TComponent;
  G: TdxGrid;
  QG: TdxQueryGrid;
  Col: TColumn;
begin
  if Self.Grid is TdxGrid then
  begin
    G := TdxGrid(Self.Grid);
    C := FindComponentByFieldName(G.Form, FieldName);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Col := FindColumnByTag(G, GetId(C));
    G.SortCols.AddCol(Col, Desc);
  end
  else
  begin
    QG := TdxQueryGrid(Self.Grid);
    Col := QG.FindColumnByTitle(FieldName);
    if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    QG.SortCols.AddCol(Col, Desc);
  end;
end;
function TSortColListFindCol(Self: TSortColumns; const FieldName: String): TSortColumn;
var
  C: TComponent;
  G: TdxGrid;
  Col: TColumn;
  QG: TdxQueryGrid;
begin
  if Self.Grid is TdxGrid then
  begin
    G := TdxGrid(Self.Grid);
    C := FindComponentByFieldName(G.Form, FieldName);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Col := FindColumnByTag(G, GetId(C));
    Result := G.SortCols.FindCol(Col);
  end
  else
  begin
    QG := TdxQueryGrid(Self.Grid);
    Col := QG.FindColumnByTitle(FieldName);
    if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Result := QG.SortCols.FindCol(Col);
  end;
end;
procedure TSortColListRemoveCol(Self: TSortColumns; const FieldName: String);
var
  C: TComponent;
  G: TdxGrid;
  Col: TColumn;
  CD: TSortColumn;
  QG: TdxQueryGrid;
begin
  if Self.Grid is TdxGrid then
  begin
    G := TdxGrid(Self.Grid);
    C := FindComponentByFieldName(G.Form, FieldName);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    Col := FindColumnByTag(G, GetId(C));
    CD := G.SortCols.FindCol(Col);
    if CD <> nil then G.SortCols.RemoveCol(CD);
  end
  else
  begin
    QG := TdxQueryGrid(Self.Grid);
    Col := QG.FindColumnByTitle(FieldName);
    if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    CD := QG.SortCols.FindCol(Col);
    if CD <> nil then QG.SortCols.RemoveCol(CD);
  end;
end;
procedure TSortColDataFieldName_R(Self: TSortColumn; var T: String);
var
  C: TComponent;
begin
  T := '';
  if Self.Col.Grid is TdxGrid then
  begin
    C := FindById(TdxGrid(Self.Col.Grid).Form, Self.Col.Tag);
    if C <> nil then T := GetFieldName(C);
  end
  else
    T := Self.Col.Title.Caption;
end;
procedure TSortColDataFieldName_W(Self: TSortColumn; const T: String);
var
  G: TdxGrid;
  C: TComponent;
  Col: TColumn;
  QG: TdxQueryGrid;
begin
  if Self.Col.Grid is TdxGrid then
  begin
    G := TdxGrid(Self.Col.Grid);
    C := FindComponentByFieldName(G.Form, T);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [T]);
    Col := FindColumnByTag(G, GetId(C));
    TestNil(Col, 'TSortColDataFieldName_W: Col = nil');
  end
  else
  begin
    QG := TdxQueryGrid(Self.Col.Grid);
    Col := QG.FindColumnByTitle(T);
    if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [T]);
  end;
  Self.Col := Col;
end;
procedure TSortColDataDesc_R(Self: TSortColumn; var T: Boolean); begin T := Self.Desc; end;
procedure TSortColDataDesc_W(Self: TSortColumn; T: Boolean); begin Self.Desc := T; end;

procedure RIRegister_DBGrid(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMGRID);

  with Cl.Add(TGRIDCOLUMN) do
  begin
    //RegisterMethod(@TGridColumn.IsDefault, 'ISDEFAULT');
    //RegisterMethod(@TGridColumn.FillDefaultFont, 'FILLDEFAULTFONT');
    RegisterPropertyHelper(@TGridColumnGrid_R, nil, 'GRID');
    //RegisterPropertyHelper(@TGridColumnWidthChanged_R, nil, 'WIDTHCHANGED');
  end;

  with Cl.Add(TGRIDCOLUMNTITLE) do
  begin
    //RegisterMethod(@TGridColumnTitle.FillTitleDefaultFont, 'FILLTITLEDEFAULTFONT');
    //RegisterMethod(@TGridColumnTitle.IsDefault, 'ISDEFAULT');
    RegisterPropertyHelper(@TGridColumnTitleColumn_R, nil, 'COLUMN');
  end;

  with Cl.Add(TCOLUMN) do
  begin
    RegisterPropertyHelper(@TColumnField_R, nil, 'FIELD');
    RegisterPropertyHelper(@TColumnDesignIndex_R, nil, 'DESIGNINDEX');
  end;

  with Cl.Add(TDBGridColumns) do
  begin
    RegisterPropertyHelper(@TDBGridColumnsItems_R, @TDBGridColumnsItems_W, 'ITEMS');
  end;

  with Cl.Add(TCustomDBGrid) do
  begin
    RegisterMethod(@TCustomDBGrid.DefaultDrawColumnCell, 'DefaultDrawColumnCell');
  end;

  with Cl.Add(TDBGrid) do
  begin
    RegisterPropertyHelper(@TDBGridBorderColor_R, @TDBGridBorderColor_W, 'BorderColor');
    //RegisterPropertyHelper(@TDBGridDefaultTextStyle_R, @TDBGridDefaultTextStyle_W, 'DEFAULTTEXTSTYLE');
    //RegisterPropertyHelper(@TDBGridEditorBorderStyle_R, @TDBGridEditorBorderStyle_W, 'EditorBorderStyle');
    //RegisterPropertyHelper(@TDBGridEditorMode_R, @TDBGridEditorMode_W, 'EditorMode');
    RegisterPropertyHelper(@TDBGridExtendedColSizing_R, @TDBGridExtendedColSizing_W, 'ExtendedColSizing');
    //RegisterPropertyHelper(@TDBGridFastEditing_R, @TDBGridFastEditing_W, 'FastEditing');
    RegisterPropertyHelper(@TDBGridFocusColor_R, @TDBGridFocusColor_W, 'FocusColor');
    RegisterPropertyHelper(@TDBGridFocusRectVisible_R, @TDBGridFocusRectVisible_W, 'FocusRectVisible');
    RegisterPropertyHelper(@TDBGridGridLineColor_R, @TDBGridGridLineColor_W, 'GridLineColor');
    RegisterPropertyHelper(@TDBGridGridLineStyle_R, @TDBGridGridLineStyle_W, 'GridLineStyle');
    //RegisterPropertyHelper(@TDBGridInplaceEditor_R, nil, 'InplaceEditor');
    RegisterPropertyHelper(@TDBGridSelectedColor_R, @TDBGridSelectedColor_W, 'SelectedColor');
    RegisterPropertyHelper(@TDBGridSelectedColumn_R, nil, 'SelectedColumn');
    RegisterPropertyHelper(@TDBGridSelectedField_R, nil, 'SelectedField');
    RegisterPropertyHelper(@TDBGridSelectedIndex_R, nil, 'SelectedIndex');
    RegisterPropertyHelper(@TDBGridReadOnly_R, @TDBGridReadOnly_W, 'ReadOnly');
    RegisterPropertyHelper(@TDBGridOptions_R, @TDBGridOptions_W, 'Options');
  end;

  with Cl.Add(TSortColumn) do
  begin
    RegisterPropertyHelper(@TSortColDataFieldName_R, @TSortColDataFieldName_W, 'FieldName');
    RegisterPropertyHelper(@TSortColDataDesc_R, @TSortColDataDesc_W, 'Desc');
  end;

  with Cl.Add(TSortColumns) do
  begin
    RegisterMethod(@TSortColListAddCol, 'Add');
    RegisterMethod(@TSortColListFindCol, 'Find');
    RegisterMethod(@TSortColListRemoveCol, 'Remove');
    RegisterMethod(@TSortColumns.Clear, 'Clear');
    RegisterPropertyHelper(@TSortColListCols_R, nil, 'Columns');
    RegisterPropertyHelper(@TSortColListCount_R, nil, 'Count');
  end;

  with Cl.Add(TMyDBGrid) do
  begin
    RegisterMethod(@TMyDBGrid.MoveToSelectedRow, 'MoveToSelectedRow');
    RegisterMethod(@TMyDBGrid.ClearRowsSelection, 'ClearRowsSelection');
    RegisterMethod(@TMyDBGrid.CurrentRowSelected, 'CurrentRowSelected');
    RegisterMethod(@TMyDBGrid.FindColumnByTitle, 'FindColumnByTitle');
    RegisterPropertyHelper(@TMyDBGridSelectedRowCount_R, nil, 'SelectedRowCount');
    RegisterPropertyHelper(@TMyDBGridSortCols_R, nil, 'SortColumns');
  end;
end;

procedure TdxImageBitmap_R(Self: TdxImage; var T: TBitmap); begin T := Self.Bitmap; end;
procedure TdxImageBitmap_W(Self: TdxImage; T: TBitmap); begin Self.Bitmap := T; end;

procedure RIRegister_dxImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxImage) do
  begin
    RegisterMethod(@TdxImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxImage.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TdxImage.LoadFromStringBase64, 'LoadFromStringBase64');
    RegisterMethod(@TdxImage.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxImage.SaveToStream, 'SaveToStream');
    RegisterVirtualMethod(@TdxImage.Clear, 'Clear');
    RegisterPropertyHelper(@TdxImageBitmap_R, @TdxImageBitmap_W, 'Bitmap');
  end;
end;

procedure TdxLookupComboBoxButton_R(Self: TdxLookupComboBox; var T: TSpeedButton); begin T := Self.Button; end;
procedure TdxLookupComboBoxKeyValue_R(Self: TdxLookupComboBox; var T: Variant); begin T := Self.KeyValue; end;
procedure TdxLookupComboBoxKeyValue_W(Self: TdxLookupComboBox; T: Variant); begin Self.KeyValue := T; end;
procedure TdxLookupComboBoxOnCreateListWindow_R(Self: TdxLookupComboBox; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TdxLookupComboBoxOnCreateListWindow_W(Self: TdxLookupComboBox; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
procedure TdxLookupComboBoxOnCreateForm_R(Self: TdxLookupComboBox; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxLookupComboBoxOnCreateForm_W(Self: TdxLookupComboBox; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;
procedure TdxLookupComboBoxSourceFormName_R(Self: TdxLookupComboBox; var T: String); begin T := Self.SourceFormName; end;
procedure TdxLookupComboBoxSourceFieldName_R(Self: TdxLookupComboBox; var T: String); begin T := Self.SourceFieldName; end;
procedure TdxLookupComboBoxOnNeedData_R(Self: TdxLookupComboBox; var T: TNeedDataEvent); begin T := Self.OnNeedData; end;
procedure TdxLookupComboBoxOnNeedData_W(Self: TdxLookupComboBox; T: TNeedDataEvent); begin Self.OnNeedData := T; end;
procedure TdxLookupComboBoxDropDownList_R(Self: TdxLookupComboBox; var T: TDropDownList); begin T := Self.DropDownList; end;

procedure TDropDownColumnsItems_R(Self: TDropDownListColumns; var T: TDropDownListColumn; I: Integer); begin T := Self.Items[I]; end;
procedure TDropDownColumnsItems_W(Self: TDropDownListColumns; T: TDropDownListColumn; I: Integer); begin Self.Items[I] := T; end;

procedure TDropDownListColumns_R(Self: TDropDownList; var T: TDropDownListColumns); begin T := Self.Columns; end;
procedure TDropDownListRecId_R(Self: TDropDownList; var T: Integer; I: Integer); begin T := Self.RecId[I]; end;
procedure TDropDownListRecId_W(Self: TDropDownList; T: Integer; I: Integer); begin Self.RecId[I] := T; end;
procedure TDropDownListOptions_R(Self: TDropDownList; var T: TDropDownListOptions); begin T := Self.Options; end;
procedure TDropDownListOptions_W(Self: TDropDownList; T: TDropDownListOptions); begin Self.Options := T; end;
procedure TDropDownListRowCount_R(Self: TDropDownList; var T: Integer); begin T := Self.RowCount; end;
procedure TDropDownListRowCount_W(Self: TDropDownList; T: Integer); begin Self.RowCount := T; end;
procedure TDropDownListCells_R(Self: TDropDownList; var T: String; C, R: Integer); begin T := Self.Cells[C, R]; end;
procedure TDropDownListCells_W(Self: TDropDownList; T: String; C, R: Integer); begin Self.Cells[C, R] := T; end;
procedure TDropDownListFixedGridLineColor_R(Self: TDropDownList; var T: TColor); begin T := Self.FixedGridLineColor; end;
procedure TDropDownListFixedGridLineColor_W(Self: TDropDownList; T: TColor); begin Self.FixedGridLineColor := T; end;
procedure TDropDownListSelectedColor_R(Self: TDropDownList; var T: TColor); begin T := Self.SelColor; end;
procedure TDropDownListSelectedColor_W(Self: TDropDownList; T: TColor); begin Self.SelColor := T; end;

procedure RIRegister_dxLookupComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDropDownListColumn) do
  begin

  end;
  with Cl.Add(TDropDownListColumns) do
  begin
    RegisterMethod(@TDropDownListColumns.Add, 'Add');
    RegisterPropertyHelper(@TDropDownColumnsItems_R, @TDropDownColumnsItems_W, 'Items');
  end;
  with Cl.Add(TDropDownList) do
  begin
    RegisterPropertyHelper(@TDropDownListColumns_R, nil, 'Columns');
    RegisterPropertyHelper(@TDropDownListRecId_R, @TDropDownListRecId_W, 'RecId');
    RegisterPropertyHelper(@TDropDownListOptions_R, @TDropDownListOptions_W, 'Options');
    RegisterPropertyHelper(@TDropDownListRowCount_R, @TDropDownListRowCount_W, 'RowCount');
    RegisterPropertyHelper(@TDropDownListCells_R, @TDropDownListCells_W, 'Cells');
    RegisterPropertyHelper(@TDropDownListSelectedColor_R, @TDropDownListSelectedColor_W, 'SelectedColor');
    RegisterPropertyHelper(@TDropDownListFixedGridLineColor_R, @TDropDownListFixedGridLineColor_W, 'FixedGridLineColor');
  end;
  with Cl.Add(TdxLookupComboBox) do
  begin
    RegisterPropertyHelper(@TdxLookupComboBoxButton_R, nil, 'Button');
    RegisterPropertyHelper(@TdxLookupComboBoxKeyValue_R, @TdxLookupComboBoxKeyValue_W, 'KeyValue');
    RegisterPropertyHelper(@TdxLookupComboBoxSourceFormName_R, nil, 'SourceFormName');
    RegisterPropertyHelper(@TdxLookupComboBoxSourceFieldName_R, nil, 'SourceFieldName');
    RegisterPropertyHelper(@TdxLookupComboBoxDropDownList_R, nil, 'DropDownList');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnCreateListWindow_R, @TdxLookupComboBoxOnCreateListWindow_W,
      'OnCreateListWindow');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnCreateForm_R, @TdxLookupComboBoxOnCreateForm_W,
      'OnCreateForm');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnNeedData_R, @TdxLookupComboBoxOnNeedData_W,
      'OnNeedData');
  end;
end;

//procedure TdxComboBoxOnUtf8KeyPress_R(Self: TdxComboBox; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
//procedure TdxComboBoxOnUtf8KeyPress_W(Self: TdxComboBox; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;
procedure TdxComboBoxSourceFormName_R(Self: TdxComboBox; var T: String); begin T := Self.SourceFormName; end;
procedure TdxComboBoxSourceFieldName_R(Self: TdxComboBox; var T: String); begin T := Self.SourceFieldName; end;

procedure RIRegister_dxComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxComboBox) do
  begin
    RegisterPropertyHelper(@TdxComboBoxSourceFormName_R, nil, 'SourceFormName');
    RegisterPropertyHelper(@TdxComboBoxSourceFieldName_R, nil, 'SourceFieldName');
    //RegisterPropertyHelper(@TdxComboBoxOnUtf8KeyPress_R, @TdxComboBoxOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure RIRegister_DBComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBComboBox) do
  begin

  end;
end;

procedure TCustomDBComboBoxFieldR(Self: TCustomDBComboBox; var T: TField); begin T := Self.Field; end;

procedure RIRegister_CustomDBComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomDBComboBox) do
  begin
    RegisterPropertyHelper(@TCustomDBComboBoxFieldR, nil, 'Field');
  end;
end;

procedure RIRegister_dxCheckBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCheckBox) do
  begin
  end;
end;

procedure TDBCheckBoxFieldR(Self: TDBCheckBox; var T: TField); begin T := Self.Field; end;
procedure TDBCheckBoxCheckedR(Self: TDBCheckBox; var T: Boolean); begin T := Self.Checked; end;
procedure TDBCheckBoxCheckedW(Self: TDBCheckBox; T: Boolean); begin Self.Checked := T; end;

procedure RIRegister_DBCheckBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBCheckBox) do
  begin
    RegisterPropertyHelper(@TDBCheckBoxFieldR, nil, 'Field');
    RegisterPropertyHelper(@TDBCheckBoxCheckedR, @TDBCheckBoxCheckedW, 'Checked');
  end;
end;

procedure TdxMemoButtonR(Self: TdxMemo; var T: TSpeedButton); begin T := Self.Button; end;
procedure TdxMemoOnCreateListWindow_R(Self: TdxMemo; var T: TCreateListWindowEvent); begin T := Self.OnCreateListWindow; end;
procedure TdxMemoOnCreateListWindow_W(Self: TdxMemo; T: TCreateListWindowEvent); begin Self.OnCreateListWindow := T; end;
//procedure TdxMemoOnUtf8KeyPress_R(Self: TdxMemo; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
//procedure TdxMemoOnUtf8KeyPress_W(Self: TdxMemo; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;
procedure TdxMemoSourceFormName_R(Self: TdxMemo; var T: String); begin T := Self.SourceFormName; end;
procedure TdxMemoSourceFieldName_R(Self: TdxMemo; var T: String); begin T := Self.SourceFieldName; end;

procedure RIRegister_dxMemo(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxMemo) do
  begin
    RegisterPropertyHelper(@TdxMemoButtonR, nil, 'Button');
    RegisterPropertyHelper(@TdxMemoSourceFormName_R, nil, 'SourceFormName');
    RegisterPropertyHelper(@TdxMemoSourceFieldName_R, nil, 'SourceFieldName');
    RegisterEventPropertyHelper(@TdxMemoOnCreateListWindow_R, @TdxMemoOnCreateListWindow_W,
      'OnCreateListWindow');
    //RegisterPropertyHelper(@TdxMemoOnUtf8KeyPress_R, @TdxMemoOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure TDBMemoFieldR(Self: TDBMemo; var T: TField); begin T := Self.Field; end;

procedure RIRegister_DBMemo(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBMemo) do
  begin
    RegisterPropertyHelper(@TDBMemoFieldR, nil, 'Field');
  end;
end;

procedure RIRegister_dxDateEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDateEdit) do
  begin

  end;
end;

procedure RIRegister_DBDateEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBDateEditEx) do
  begin

  end;
end;

procedure TdxCalcEditPrecision_R(Self: TdxCalcEdit; var T: Integer); begin T := Self.Precission; end;
procedure TdxCalcEditPrecision_W(Self: TdxCalcEdit; T: Integer); begin Self.Precission := T; end;

procedure RIRegister_dxCalcEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCalcEdit) do
  begin
    RegisterPropertyHelper(@TdxCalcEditPrecision_R, @TdxCalcEditPrecision_W, 'Precision');
  end;
end;

procedure RIRegister_DBCalcEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBCalcEdit) do
  begin

  end;
end;

procedure TCustomDBEditButtonButtonR(Self: TCustomDBEditButton; var T: TSpeedButton); begin T := Self.Button; end;
procedure TCustomDBEditButtonOnlyWhenFocusedR(Self: TCustomDBEditButton; var T: Boolean); begin T := Self.ButtonOnlyWhenFocused; end;
procedure TCustomDBEditButtonOnlyWhenFocusedW(Self: TCustomDBEditButton; T: Boolean); begin Self.ButtonOnlyWhenFocused := T; end;
procedure TCustomDBEditHideButtonR(Self: TCustomDBEditButton; var T: Boolean); begin T := Self.HideButton; end;
procedure TCustomDBEditHideButtonW(Self: TCustomDBEditButton; T: Boolean); begin Self.HideButton := T; end;

procedure RIRegister_CustomDBEditButton(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomDBEditButton) do
  begin
    RegisterPropertyHelper(@TCustomDBEditButtonButtonR, nil, 'Button');
    RegisterPropertyHelper(@TCustomDBEditButtonOnlyWhenFocusedR,
      @TCustomDBEditButtonOnlyWhenFocusedW, 'ButtonOnlyWhenFocused');
    RegisterPropertyHelper(@TCustomDBEditHideButtonR,
      @TCustomDBEditHideButtonW, 'HideButton');
  end;
end;

//procedure TdxEditOnUtf8KeyPress_R(Self: TdxEdit; var T: TMyUtf8KeyPressEvent); begin T := Self.OnMyUTF8KeyPress; end;
//procedure TdxEditOnUtf8KeyPress_W(Self: TdxEdit; T: TMyUtf8KeyPressEvent); begin Self.OnMyUTF8KeyPress := T; end;

procedure RIRegister_dxEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxEdit) do
  begin
    RegisterMethod(@TdxEdit.ValidateText, 'ValidateText');
    RegisterMethod(@TdxEdit.MaskTextEmpty, 'MaskTextEmpty');
    //RegisterPropertyHelper(@TdxEditOnUtf8KeyPress_R, @TdxEditOnUtf8KeyPress_W, 'OnUtf8KeyPress');
  end;
end;

procedure TDBEditFieldR(Self: TDBEdit; var T: TField); begin T := Self.Field; end;

procedure RIRegister_DBEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDBEdit) do
  begin
    RegisterPropertyHelper(@TDBEditFieldR, nil, 'Field');
  end;
end;

procedure TMaskEditIsMaskedR(Self: TMaskEdit; var T: Boolean); begin T := Self.IsMasked; end;
procedure TMaskEditEditTextR(Self: TMaskEdit; var T: String); begin T := Self.EditText; end;
procedure TMaskEditEditTextW(Self: TMaskEdit; const T: String); begin Self.EditText := T; end;

procedure RIRegister_MaskEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMaskEditEx) do
  begin
    RegisterMethod(@TdxEdit.ValidateText, 'ValidateText');
    RegisterMethod(@TdxEdit.MaskTextEmpty, 'MaskTextEmpty');
    RegisterPropertyHelper(@TMaskEditIsMaskedR, nil, 'IsMasked');
    RegisterPropertyHelper(@TMaskEditEditTextR, @TMaskEditEditTextW, 'EditText');
  end;
end;

//procedure TCustomMaskEditModifiedR(Self: TCustomMaskEdit; var T: Boolean); begin T := Self.Modified; end;
//procedure TCustomMaskEditModifiedW(Self: TCustomMaskEdit; T: Boolean); begin Self.Modified:= T; end;

{procedure RIRegister_CustomMaskEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCustomMaskEdit) do
  begin
    //RegisterMethod(@TCustomMaskEdit.Clear, 'Clear');
    RegisterVirtualMethod(@TCustomMaskEdit.ValidateEdit, 'ValidateEdit');
    //RegisterPropertyHelper(@TCustomMaskEditModifiedR, @TCustomMaskEditModifiedW, 'Modified');
  end;
end; }

procedure TdxLabelFieldNameR(Self: TdxLabel; var T: String); begin T := Self.FieldName; end;
procedure TdxLabelFieldNameW(Self: TdxLabel; const T: String); begin Self.FieldName:= T; end;

procedure RIRegister_dxLabel(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxLabel) do
  begin
    RegisterPropertyHelper(@TdxLabelFieldNameR, @TdxLabelFieldNameW, 'FieldName');
  end;
end;

procedure TFilterFieldFieldName_R(Self: TFilterField; var T: String); begin T := Self.FieldName; end;
procedure TFilterFieldIsNot_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNot; end;
procedure TFilterFieldIsNot_W(Self: TFilterField; T: Boolean); begin Self.IsNot := T; end;
procedure TFilterFieldIsNull_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNull; end;
procedure TFilterFieldIsNull_W(Self: TFilterField; T: Boolean); begin Self.IsNull := T; end;
procedure TFilterFieldValues_R(Self: TFilterField; var T: TStringList); begin T := Self.Values; end;
procedure TFilterFieldValue_R(Self: TFilterField; var T: String; I: Integer); begin T := Self.Value[I]; end;
procedure TFilterFieldEndValue_R(Self: TFilterField; var T: String; I: Integer); begin T := Self.EndValue[I]; end;

procedure TFilterObjectFields_R(Self: TFilterObject; var T: TFilterField; I: Integer); begin T := Self.Fields[I]; end;
procedure TFilterObjectCount_R(Self: TFilterObject; var T: Integer); begin T := Self.Count; end;

procedure TdxFormFields_R(Self: TdxForm; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxFormFields_W(Self: TdxForm; const T: Variant; I: String); begin Self.Fields[I]:= T; end;
procedure TdxFormField_R(Self: TdxForm; var T: TField; I: String); begin T := Self.Field[I]; end;
procedure TdxFormAsI_R(Self: TdxForm; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxFormAsF_R(Self: TdxForm; var T: Extended; I: String); begin T := Self.AsF[I]; end;
procedure TdxFormAsDT_R(Self: TdxForm; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxFormAsS_R(Self: TdxForm; var T: String; I: String); begin T := Self.AsS[I]; end;
procedure TdxFormOldValues_R(Self: TdxForm; var T: Variant; I: String); begin T := Self.OldValues[I]; end;

procedure TdxFormForms_R(Self: TdxForm; var T: TdxForm; I: String); begin T := Self.Forms[I]; end;
procedure TdxFormFormByIndex_R(Self: TdxForm; var T: TdxForm; I: Integer); begin T := Self.FormByIndex[I]; end;
procedure TdxFormFormCount_R(Self: TdxForm; var T: Integer); begin T := Self.FormCount; end;

procedure TdxFormQueries_R(Self: TdxForm; var T: TdxQueryGrid; I: String); begin T := TdxQueryGrid(Self.Queries[I]); end;
procedure TdxFormQueryByIndex_R(Self: TdxForm; var T: TdxQueryGrid; I: Integer); begin T := TdxQueryGrid(Self.QueryByIndex[I]); end;
procedure TdxFormQueryCount_R(Self: TdxForm; var T: Integer); begin T := Self.QueryCount; end;

procedure TdxFormImages_R(Self: TdxForm; var T: TdxDBImage; I: String); begin T := Self.Images[I]; end;
procedure TdxFormFiles_R(Self: TdxForm; var T: TdxFile; I: String); begin T := Self.Files[I]; end;

procedure TdxFormFormGrid_R(Self: TdxForm; var T: TdxGrid); begin T := Self.FormGrid; end;
procedure TdxFormEditWindow_R(Self: TdxForm; var T: TEditWindow); begin T := TEditWindow(Self.EditWindow); end;
procedure TdxFormParams_R(Self: TdxForm; var T: TParamList); begin T := Self.Params; end;
procedure TdxFormState_R(Self: TdxForm; var T: TDataSetState); begin T := Self.State; end;
procedure TdxFormFilter_R(Self: TdxForm; var T: TFilterObject); begin T := Self.Filter; end;
procedure TdxFormParentForm_R(Self: TdxForm; var T: TdxForm); begin T := Self.ParentForm; end;
procedure TdxFormModified_R(Self: TdxForm; var T: Boolean); begin T := Self.Modified; end;
procedure TdxFormLockMode_R(Self: TdxForm; var T: TLockMode); begin T := Self.LockMode; end;
procedure TdxFormLockMode_W(Self: TdxForm; T: TLockMode); begin Self.LockMode := T; end;
procedure TdxFormCustomFilter_R(Self: TdxForm; var T: String); begin T := Self.CustomFilter; end;
procedure TdxFormCustomFilter_W(Self: TdxForm; T: String); begin Self.CustomFilter := T; end;
procedure TdxFormCustomFilterForm_R(Self: TdxForm; var T: TdxForm); begin T := Self.CustomFilterForm; end;
procedure TdxFormCustomFilterForm_W(Self: TdxForm; T: TdxForm); begin Self.CustomFilterForm := T; end;
procedure TdxFormUseSelCond_R(Self: TdxForm; var T: Boolean); begin T := Self.UseSelCond; end;
procedure TdxFormUseSelCond_W(Self: TdxForm; T: Boolean); begin Self.UseSelCond := T; end;


function MyCreateForm(Self: TClass; CreateNewInstance: Boolean; const FormName: String): TdxForm;
begin Result := CreateForm(FormName); end;
procedure MyDestroyForm(Fm: TdxForm); begin DestroyForm(Fm) end;

procedure RIRegister_dxForm(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFilterField) do
  begin
    RegisterPropertyHelper(@TFilterFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TFilterFieldIsNot_R, @TFilterFieldIsNot_W, 'IsNot');
    RegisterPropertyHelper(@TFilterFieldIsNull_R, @TFilterFieldIsNull_W, 'IsNull');
    RegisterPropertyHelper(@TFilterFieldValues_R, nil, 'Values');
    RegisterPropertyHelper(@TFilterFieldValue_R, nil, 'Value');
    RegisterPropertyHelper(@TFilterFieldEndValue_R, nil, 'EndValue');
  end;

  with Cl.Add(TFilterObject) do
  begin
    RegisterMethod(@TFilterObject.AddFieldByName, 'AddField');
    RegisterMethod(@TFilterObject.FindFieldByName, 'FindField');
    RegisterMethod(@TFilterObject.DeleteField, 'DeleteField');
    RegisterMethod(@TFilterObject.Clear, 'Clear');
    RegisterPropertyHelper(@TFilterObjectFields_R, nil, 'Fields');
		RegisterPropertyHelper(@TFilterObjectCount_R, nil, 'Count');
  end;

  with Cl.FindClass('TdxForm') do
  begin
    RegisterConstructor(@MyCreateForm, 'Create');
    RegisterMethod(@MyDestroyForm, 'Free');
    RegisterMethod(@TdxForm.Append, 'Append');
    RegisterMethod(@TdxForm.Insert, 'Insert');
    RegisterMethod(@TdxForm.Edit, 'Edit');
    RegisterMethod(@TdxForm.Delete, 'Delete');
    RegisterMethod(@TdxForm.Post, 'Post');
    RegisterMethod(@TdxForm.Cancel, 'Cancel');
    RegisterMethod(@TdxForm.Refresh, 'Refresh');
    RegisterMethod(@TdxForm.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxForm.MovePrior, 'MovePrior');
    RegisterMethod(@TdxForm.MoveNext, 'MoveNext');
    RegisterMethod(@TdxForm.MoveLast, 'MoveLast');
    RegisterMethod(@TdxForm.MoveBy, 'MoveBy');
    RegisterMethod(@TdxForm.MoveTo, 'MoveTo');
    RegisterMethod(@TdxForm.Bof, 'BOF');
    RegisterMethod(@TdxForm.Eof, 'EOF');
    RegisterMethod(@TdxForm.RecNo, 'RecNo');
    RegisterMethod(@TdxForm.RecId, 'RecId');
    RegisterMethod(@TdxForm.RecordCount, 'RecordCount');
    RegisterMethod(@TdxForm.Print, 'Print');
    {RegisterMethod(@TdxForm.FindFirst, 'FindFirst');
    RegisterMethod(@TdxForm.FindNext, 'FindNext');
    RegisterMethod(@TdxForm.FindPrior, 'FindPrior');  }
    RegisterMethod(@TdxForm.Locate, 'Locate');
    RegisterMethod(@TdxForm.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxForm.DisableControls, 'DisableControls');
    RegisterMethod(@TdxForm.EnableControls, 'EnableControls');
    RegisterMethod(@TdxForm.ControlsDisabled, 'ControlsDisabled');
    RegisterMethod(@TdxForm.CanAppend, 'CanAppend');
    RegisterMethod(@TdxForm.CanEdit, 'CanEdit');
    RegisterMethod(@TdxForm.CanDelete, 'CanDelete');
    //RegisterMethod(@TdxForm.AddUserFilter, 'AddUserFilter');
    //RegisterMethod(@TdxForm.ClearUserFilter, 'ClearUserFilter');
    RegisterMethod(@TdxForm.Open, 'Open');
    RegisterMethod(@TdxForm.OpenRecords, 'OpenRecords');
    RegisterMethod(@TdxForm.OpenRecord, 'OpenRecord');
    RegisterMethod(@TdxForm.Opened, 'Opened');
    RegisterMethod(@TdxForm.Close, 'Close');
    RegisterMethod(@TdxForm.Validate, 'Validate');
    RegisterMethod(@TdxForm.FindComponentByFldName, 'FindComponentByFieldName');

    RegisterMethod(@TdxForm.DisableScrollEvents, 'DisableScrollEvents');
    RegisterMethod(@TdxForm.EnableScrollEvents, 'EnableScrollEvents');
    RegisterMethod(@TdxForm.ScrollEventsDisabled, 'ScrollEventsDisabled');
    RegisterMethod(@TdxForm.WhoEdit, 'WhoEdit');
    RegisterMethod(@TdxForm.GetRecordsCaption, 'GetRecordsCaption');
    RegisterMethod(@TdxForm.GetRecordCaption, 'GetRecordCaption');

    RegisterPropertyHelper(@TdxFormFields_R, @TdxFormFields_W, 'Fields');
    RegisterPropertyHelper(@TdxFormField_R, nil, 'Field');
    RegisterPropertyHelper(@TdxFormAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxFormAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxFormAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxFormAsS_R, nil, 'AsS');
    RegisterPropertyHelper(@TdxFormOldValues_R, nil, 'OldValues');
    RegisterPropertyHelper(@TdxFormForms_R, nil, 'Forms');
    RegisterPropertyHelper(@TdxFormFormByIndex_R, nil, 'FormByIndex');
    RegisterPropertyHelper(@TdxFormFormCount_R, nil, 'FormCount');
    RegisterPropertyHelper(@TdxFormQueries_R, nil, 'Queries');
    RegisterPropertyHelper(@TdxFormQueryByIndex_R, nil, 'QueryByIndex');
    RegisterPropertyHelper(@TdxFormQueryCount_R, nil, 'QueryCount');
    RegisterPropertyHelper(@TdxFormImages_R, nil, 'Images');
    RegisterPropertyHelper(@TdxFormFiles_R, nil, 'Files');
    RegisterPropertyHelper(@TdxFormFormGrid_R, nil, 'Grid');
    RegisterPropertyHelper(@TdxFormEditWindow_R, nil, 'EditWindow');
    RegisterPropertyHelper(@TdxFormParams_R, nil, 'Params');
    RegisterPropertyHelper(@TdxFormState_R, nil, 'State');
    RegisterPropertyHelper(@TdxFormFilter_R, nil, 'Filter');
    RegisterPropertyHelper(@TdxFormParentForm_R, nil, 'ParentForm');
    RegisterPropertyHelper(@TdxFormModified_R, nil, 'Modified');
    RegisterPropertyHelper(@TdxFormLockMode_R, @TdxFormLockMode_W, 'LockMode');
    RegisterPropertyHelper(@TdxFormCustomFilter_R, @TdxFormCustomFilter_W, 'CustomFilter');
    RegisterPropertyHelper(@TdxFormCustomFilterForm_R, @TdxFormCustomFilterForm_W, 'CustomFilterForm');
    RegisterPropertyHelper(@TdxFormUseSelCond_R, @TdxFormUseSelCond_W, 'UseSelectCondition');
  end;
end;

procedure dxFormTreeFieldFieldName_R(Self: TdxFormTreeField; var T: String); begin T := Self.FieldName; end;
procedure dxFormTreeFieldFieldName_W(Self: TdxFormTreeField; const T: String); begin Self.FieldName := T; end;

procedure dxFormTreeFieldsFields_R(Self: TdxFormTreeFields; var T: TdxFormTreeField; I: Integer); begin T := Self.Fields[I]; end;

procedure dxFormTreeFields_R(Self: TdxFormTree; var T: TdxFormTreeFields); begin T := Self.Fields; end;
procedure dxFormTreeExpandLevels_R(Self: TdxFormTree; var T: Integer); begin T := Self.ExpandLevels; end;
procedure dxFormTreeExpandLevels_W(Self: TdxFormTree; T: Integer); begin Self.ExpandLevels := T; end;

procedure RIRegister_dxFormTree(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxFormTreeField) do
  begin
    RegisterPropertyHelper(@dxFormTreeFieldFieldName_R, @dxFormTreeFieldFieldName_W, 'FieldName');
  end;
  with Cl.Add(TdxFormTreeFields) do
  begin
    RegisterMethod(@TdxFormTreeFields.Add, 'Add');
    RegisterPropertyHelper(@dxFormTreeFieldsFields_R, nil, 'Fields');
  end;
  with Cl.Add(TdxFormTree) do
  begin
    RegisterMethod(@TdxFormTree.UpdateTree, 'UpdateTree');
    RegisterMethod(@TdxFormTree.SelectByRecord, 'SelectByRecord');
    RegisterMethod(@TdxFormTree.GetFieldNameByNode, 'GetFieldNameByNode');
    RegisterMethod(@TdxFormTree.GetFieldValueByNode, 'GetFieldValueByNode');
    RegisterPropertyHelper(@dxFormTreeFields_R, nil, 'Fields');
    RegisterPropertyHelper(@dxFormTreeExpandLevels_R, @dxFormTreeExpandLevels_W, 'ExpandLevels');
  end;
end;

procedure TFIELDREADONLY_W(Self: TFIELD; const T: BOOLEAN); begin Self.READONLY := T; end;
procedure TFIELDREADONLY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.READONLY; end;
procedure TFIELDALIGNMENT_W(Self: TFIELD; const T: TALIGNMENT); begin Self.ALIGNMENT := T; end;
procedure TFIELDALIGNMENT_R(Self: TFIELD; var T: TALIGNMENT); begin T := Self.ALIGNMENT; end;
procedure TFIELDVALUE_W(Self: TFIELD; const T: VARIANT); begin Self.VALUE := T; end;
procedure TFIELDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.VALUE; end;
//procedure TFIELDTEXT_W(Self: TFIELD; const T: String); begin Self.TEXT := T; end;
//procedure TFIELDTEXT_R(Self: TFIELD; var T: String); begin T := Self.TEXT; end;
procedure TFIELDOLDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.OLDVALUE; end;
procedure TFIELDISNULL_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ISNULL; end;
procedure TFIELDDATATYPE_R(Self: TFIELD; var T: TFIELDTYPE); begin T := Self.DATATYPE; end;
procedure TFIELDCANMODIFY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.CANMODIFY; end;
procedure TFIELDASVARIANT_W(Self: TFIELD; const T: VARIANT); begin Self.ASVARIANT := T; end;
procedure TFIELDASVARIANT_R(Self: TFIELD; var T: VARIANT); begin T := Self.ASVARIANT; end;
procedure TFIELDASSTRING_W(Self: TFIELD; const T: String); begin Self.ASSTRING := T; end;
procedure TFIELDASSTRING_R(Self: TFIELD; var T: String); begin T := Self.ASSTRING; end;
procedure TFIELDASINTEGER_W(Self: TFIELD; const T: LONGINT); begin Self.ASINTEGER := T; end;
procedure TFIELDASINTEGER_R(Self: TFIELD; var T: LONGINT); begin T := Self.ASINTEGER; end;
procedure TFIELDASFLOAT_W(Self: TFIELD; const T: DOUBLE); begin Self.ASFLOAT := T; end;
procedure TFIELDASFLOAT_R(Self: TFIELD; var T: DOUBLE); begin T := Self.ASFLOAT; end;
procedure TFIELDASDATETIME_W(Self: TFIELD; const T: TDATETIME); begin Self.ASDATETIME := T; end;
procedure TFIELDASDATETIME_R(Self: TFIELD; var T: TDATETIME); begin T := Self.ASDATETIME; end;
procedure TFIELDASCURRENCY_W(Self: TFIELD; const T: CURRENCY); begin Self.ASCURRENCY := T; end;
procedure TFIELDASCURRENCY_R(Self: TFIELD; var T: CURRENCY); begin T := Self.ASCURRENCY; end;
procedure TFIELDASBOOLEAN_W(Self: TFIELD; const T: BOOLEAN); begin Self.ASBOOLEAN := T; end;
procedure TFIELDASBOOLEAN_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ASBOOLEAN; end;
{procedure TFIELDDISPLAYFORMAT_R(Self: TFIELD; var T: String);
begin
  if Self is TNumericField then T := TNumericField(Self).DisplayFormat
  else if Self is TDateTimeField then T := TDateTimeField(Self).DisplayFormat
  else T := '';
end;
procedure TFIELDDISPLAYFORMAT_W(Self: TFIELD; T: String);
begin
  if Self is TNumericField then TNumericField(Self).DisplayFormat := T
  else if Self is TDateTimeField then TDateTimeField(Self).DisplayFormat := T;
end;
procedure TFIELDEDITFORMAT_R(Self: TFIELD; var T: String);
begin
  if Self is TNumericField then T := TNumericField(Self).EditFormat
  else T := '';
end;
procedure TFIELDEDITFORMAT_W(Self: TFIELD; T: String);
begin
  if Self is TNumericField then TNumericField(Self).EditFormat := T
end;  }
//procedure TFIELDINSERTSTATE_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.DataSet.State = dsInsert; end;
//procedure TFIELDEDITSTATE_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.DataSet.State = dsEdit; end;
procedure TFIELDSTATE_R(Self: TFIELD; var T: TDataSetState); begin T := Self.DataSet.State; end;
procedure TFIELDFIELDNAME_R(Self: TFIELD; var T: String); begin T := Self.FieldName; end;

procedure RIRegister_TFIELD(Cl: TPSRuntimeClassImporter);
Begin
  with Cl.Add(TFIELD) do
  begin
    RegisterVirtualMethod(@TFIELD.CLEAR, 'Clear');
    RegisterMethod(@TFIELD.FOCUSCONTROL, 'FocusControl');
    //RegisterVirtualMethod(@TFIELD.ISVALIDCHAR, 'IsValidChar');
    RegisterPropertyHelper(@TFIELDASBOOLEAN_R,@TFIELDASBOOLEAN_W,'AsBoolean');
    RegisterPropertyHelper(@TFIELDASCURRENCY_R,@TFIELDASCURRENCY_W,'AsCurrency');
    RegisterPropertyHelper(@TFIELDASDATETIME_R,@TFIELDASDATETIME_W,'AsDateTime');
    RegisterPropertyHelper(@TFIELDASFLOAT_R,@TFIELDASFLOAT_W,'AsFloat');
    RegisterPropertyHelper(@TFIELDASINTEGER_R,@TFIELDASINTEGER_W,'AsInteger');
    RegisterPropertyHelper(@TFIELDASSTRING_R,@TFIELDASSTRING_W,'AsString');
    RegisterPropertyHelper(@TFIELDASVARIANT_R,@TFIELDASVARIANT_W,'AsVariant');
    RegisterPropertyHelper(@TFIELDCANMODIFY_R,nil,'CanModify');
    RegisterPropertyHelper(@TFIELDDATATYPE_R,nil,'DataType');
    RegisterPropertyHelper(@TFIELDISNULL_R,nil,'IsNull');
    RegisterPropertyHelper(@TFIELDOLDVALUE_R,nil,'OldValue');
    //RegisterPropertyHelper(@TFIELDTEXT_R,@TFIELDTEXT_W,'Text');
    RegisterPropertyHelper(@TFIELDVALUE_R,@TFIELDVALUE_W,'Value');
    RegisterPropertyHelper(@TFIELDALIGNMENT_R,@TFIELDALIGNMENT_W,'Alignment');
    RegisterPropertyHelper(@TFIELDREADONLY_R,@TFIELDREADONLY_W,'ReadOnly');
    //RegisterPropertyHelper(@TFIELDDISPLAYFORMAT_R, @TFIELDDISPLAYFORMAT_W, 'DisplayFormat');
    //RegisterPropertyHelper(@TFIELDEDITFORMAT_R, @TFIELDEDITFORMAT_W, 'EditFormat');
    RegisterPropertyHelper(@TFIELDSTATE_R, nil, 'State');
    RegisterPropertyHelper(@TFIELDFIELDNAME_R, nil, 'FieldName');
    {RegisterPropertyHelper(@TFIELDINSERTSTATE_R, nil, 'InsertState');
    RegisterPropertyHelper(@TFIELDEDITSTATE_R, nil, 'EditState'); }
  end;
end;

procedure TFileDialogFiles_R(Self: TFileDialog; var T: TStrings); begin T := Self.Files; end;

procedure RIRegister_Dialogs(Cl: TPSRuntimeClassImporter);
begin
  with CL.Add(TCommonDialog) do
  begin
    RegisterVirtualMethod(@TCommonDialog.Execute, 'Execute');
  end;

  with Cl.Add(TFileDialog) do
  begin
    RegisterPropertyHelper(@TFileDialogFiles_R, nil, 'Files');
  end;

  with Cl.Add(TOpenDialog) do
  begin

  end;

  with Cl.Add(TSaveDialog) do
  begin

  end;

  with Cl.Add(TSelectDirectoryDialog) do
  begin

  end;

  with Cl.Add(TPrintDialog) do
  begin

  end;
end;

procedure RIRegister_dxCtrls(Cl: TPSRuntimeClassImporter);
begin
  RIRegister_ImageList(Cl);
  RIRegister_MaskEdit(Cl);
  RIRegister_Splitter(Cl);
  RIRegister_ButtonPanel(Cl);
  RIRegister_StatusBar(Cl);
  RIRegister_Toolbar(Cl);
  RIRegister_TreeView(Cl);
  RIRegister_PageControl(Cl);
  RIRegister_KGrid(Cl);

  RIRegister_Dialogs(Cl);
  RIRegister_IniFiles(Cl);
  RIRegister_Clipboard(Cl);

  RIRegister_TField(Cl);
  RIRegister_DBEdit(Cl);
  RIRegister_CustomDBEditButton(Cl);
  RIRegister_DBCalcEdit(Cl);
  RIRegister_DBDateEdit(Cl);
  RIRegister_DBTimeEdit(Cl);
  RIRegister_DBMemo(Cl);
  RIRegister_DBCheckBox(Cl);
  RIRegister_CustomDBComboBox(Cl);
  RIRegister_DBComboBox(Cl);
  RIRegister_DBGrid(Cl);

  RIRegister_dxTypes(Cl);
  RIRegister_dxLabel(Cl);
  RIRegister_dxEdit(Cl);
  RIRegister_dxCalcEdit(Cl);
  RIRegister_dxDateEdit(Cl);
  RIRegister_dxMemo(Cl);
  RIRegister_dxCheckBox(Cl);
  RIRegister_dxComboBox(Cl);
  RIRegister_dxLookupComboBox(Cl);
  RIRegister_dxImage(Cl);
  RIRegister_dxGrid(Cl);
  RIRegister_dxGroupBox(Cl);
  RIRegister_dxPageControl(Cl);
  RIRegister_dxShape(Cl);
  RIRegister_dxObjectField(Cl);
  RIRegister_dxTimeEdit(Cl);
  RIRegister_dxCounter(Cl);
  RIRegister_dxButton(Cl);
  RIRegister_dxDBImage(Cl);
  RIRegister_dxFile(Cl);
  RIRegister_dxForm(Cl);
  RIRegister_dxFormTree(Cl);
  RIRegister_dxQueryGrid(Cl);
  RIRegister_dxSQLQuery(Cl);

  RIRegister_FormView(Cl);
  RIRegister_Window(Cl);
  RIRegister_MainFm(Cl);
  RIRegister_ReportWindow(Cl);

  RIRegister_HttpServer(Cl);
  RIRegister_HttpClient(Cl);
  RIRegister_Template(Cl);
  RIRegister_TrayIcon(Cl);

  RIRegister_Chart(Cl);
  RIRegister_Xml(Cl);
  RIRegister_Json(Cl);
  RIRegister_CsvData(Cl);
  RIRegister_dxRecordId(Cl);
end;

procedure TTreeNodeAbsoluteIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.AbsoluteIndex; end;
procedure TTreeNodeCount_R(Self: TTreeNode; var T: Integer); begin T := Self.Count; end;
procedure TTreeNodeCut_R(Self: TTreeNode; var T: Boolean); begin T := Self.Cut; end;
procedure TTreeNodeCut_W(Self: TTreeNode; T: Boolean); begin Self.Cut := T; end;
procedure TTreeNodeData_R(Self: TTreeNode; var T: TObject); begin T := TObject(Self.Data); end;
procedure TTreeNodeData_W(Self: TTreeNode; T: TObject); begin Self.Data := T; end;
procedure TTreeNodeDeleting_R(Self: TTreeNode; var T: Boolean); begin T := Self.Deleting; end;
procedure TTreeNodeExpanded_R(Self: TTreeNode; var T: Boolean); begin T := Self.Expanded; end;
procedure TTreeNodeExpanded_W(Self: TTreeNode; T: Boolean); begin Self.Expanded := T; end;
procedure TTreeNodeFocused_R(Self: TTreeNode; var T: Boolean); begin T := Self.Focused; end;
procedure TTreeNodeFocused_W(Self: TTreeNode; T: Boolean); begin Self.Focused := T; end;
procedure TTreeNodeHandle_R(Self: TTreeNode; var T: THandle); begin T := Self.Handle; end;
procedure TTreeNodeHasChildren_R(Self: TTreeNode; var T: Boolean); begin T := Self.HasChildren; end;
procedure TTreeNodeHasChildren_W(Self: TTreeNode; T: Boolean); begin Self.HasChildren := T; end;
procedure TTreeNodeHeight_R(Self: TTreeNode; var T: Integer); begin T := Self.Height; end;
procedure TTreeNodeHeight_W(Self: TTreeNode; T: Integer); begin Self.Height := T; end;
procedure TTreeNodeImageIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.ImageIndex; end;
procedure TTreeNodeImageIndex_W(Self: TTreeNode; T: Integer); begin Self.ImageIndex := T; end;
procedure TTreeNodeIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.Index; end;
procedure TTreeNodeIndex_W(Self: TTreeNode; T: Integer); begin Self.Index := T; end;
procedure TTreeNodeIsFullHeightVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.IsFullHeightVisible; end;
procedure TTreeNodeIsVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.IsVisible; end;
procedure TTreeNodeItems_R(Self: TTreeNode; var T: TTreeNode; I: Integer); begin T := Self.Items[I]; end;
procedure TTreeNodeItems_W(Self: TTreeNode; T: TTreeNode; I: Integer); begin Self.Items[I] := T; end;
procedure TTreeNodeLevel_R(Self: TTreeNode; var T: Integer); begin T := Self.Level; end;
procedure TTreeNodeMultiSelected_R(Self: TTreeNode; var T: Boolean); begin T := Self.MultiSelected; end;
procedure TTreeNodeMultiSelected_W(Self: TTreeNode; T: Boolean); begin Self.MultiSelected := T; end;
procedure TTreeNodeNodeEffect_R(Self: TTreeNode; var T: TGraphicsDrawEffect); begin T := Self.NodeEffect; end;
procedure TTreeNodeNodeEffect_W(Self: TTreeNode; T: TGraphicsDrawEffect); begin Self.NodeEffect := T; end;
procedure TTreeNodeOverlayIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.OverlayIndex; end;
procedure TTreeNodeOverlayIndex_W(Self: TTreeNode; T: Integer); begin Self.OverlayIndex := T; end;
procedure TTreeNodeOwner_R(Self: TTreeNode; var T: TTreeNodes); begin T := Self.Owner; end;
procedure TTreeNodeParent_R(Self: TTreeNode; var T: TTreeNode); begin T := Self.Parent; end;
procedure TTreeNodeSelected_R(Self: TTreeNode; var T: Boolean); begin T := Self.Selected; end;
procedure TTreeNodeSelected_W(Self: TTreeNode; T: Boolean); begin Self.Selected := T; end;
procedure TTreeNodeSelectedIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.SelectedIndex; end;
procedure TTreeNodeSelectedIndex_W(Self: TTreeNode; T: Integer); begin Self.SelectedIndex := T; end;
procedure TTreeNodeStateIndex_R(Self: TTreeNode; var T: Integer); begin T := Self.StateIndex; end;
procedure TTreeNodeStateIndex_W(Self: TTreeNode; T: Integer); begin Self.StateIndex := T; end;
procedure TTreeNodeStates_R(Self: TTreeNode; var T: TNodeStates); begin T := Self.States; end;
procedure TTreeNodeSubTreeCount_R(Self: TTreeNode; var T: Integer); begin T := Self.SubTreeCount; end;
procedure TTreeNodeText_R(Self: TTreeNode; var T: String); begin T := Self.Text; end;
procedure TTreeNodeText_W(Self: TTreeNode; T: String); begin Self.Text := T; end;
procedure TTreeNodeTop_R(Self: TTreeNode; var T: Integer); begin T := Self.Top; end;
procedure TTreeNodeTreeNodes_R(Self: TTreeNode; var T: TTreeNodes); begin T := Self.TreeNodes; end;
procedure TTreeNodeTreeView_R(Self: TTreeNode; var T: TTreeView); begin T := TTreeView(Self.TreeView); end;
procedure TTreeNodeVisible_R(Self: TTreeNode; var T: Boolean); begin T := Self.Visible; end;
procedure TTreeNodeVisible_W(Self: TTreeNode; T: Boolean); begin Self.Visible := T; end;

procedure TTreeNodesCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.Count; end;
procedure TTreeNodesItem_R(Self: TTreeNodes; var T: TTreeNode; I: Integer); begin T := Self.Item[I]; end;
procedure TTreeNodesKeepCollapsedNodes_R(Self: TTreeNodes; var T: Boolean); begin T := Self.KeepCollapsedNodes; end;
procedure TTreeNodesKeepCollapsedNodes_W(Self: TTreeNodes; T: Boolean); begin Self.KeepCollapsedNodes := T; end;
procedure TTreeNodesOwner_R(Self: TTreeNodes; var T: TTreeView); begin T := TTreeView(Self.Owner); end;
procedure TTreeNodesSelectionCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.SelectionCount; end;
procedure TTreeNodesTopLvlCount_R(Self: TTreeNodes; var T: Integer); begin T := Self.TopLvlCount; end;
procedure TTreeNodesTopLvlItems_R(Self: TTreeNodes; var T: TTreeNode; I: Integer); begin T := Self.TopLvlItems[I]; end;

procedure TTreeViewSelected_R(Self: TTreeView; var T: TTreeNode); begin T := Self.Selected; end;
procedure TTreeViewSelected_W(Self: TTreeView; T: TTreeNode); begin Self.Selected := T; end;
procedure TTreeViewSelectionCount_R(Self: TTreeView; var T: Cardinal); begin T := Self.SelectionCount; end;
procedure TTreeViewSelections_R(Self: TTreeView; var T: TTreeNode; I: Integer); begin T := Self.Selections[I]; end;

procedure RIRegister_TreeView(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TTreeNode) do
  begin
    //RegisterVirtualConstructor(@TTreeNode.Create, 'Create');
    RegisterMethod(@TTreeNode.AlphaSort, 'AlphaSort');
    RegisterMethod(@TTreeNode.Bottom, 'Bottom');
    RegisterMethod(@TTreeNode.BottomExpanded, 'BottomExpanded');
    RegisterMethod(@TTreeNode.CustomSort, 'CustomSort');
    RegisterMethod(@TTreeNode.DefaultTreeViewSort, 'DefaultTreeViewSort');
    RegisterMethod(@TTreeNode.DisplayExpandSignLeft, 'DisplayExpandSignLeft');
    RegisterMethod(@TTreeNode.DisplayExpandSignRect, 'DisplayExpandSignRect');
    RegisterMethod(@TTreeNode.DisplayExpandSignRight, 'DisplayExpandSignRight');
    RegisterMethod(@TTreeNode.DisplayIconLeft, 'DisplayIconLeft');
    RegisterMethod(@TTreeNode.DisplayRect, 'DisplayRect');
    RegisterMethod(@TTreeNode.DisplayStateIconLeft, 'DisplayStateIconLeft');
    RegisterMethod(@TTreeNode.DisplayTextLeft, 'DisplayTextLeft');
    RegisterMethod(@TTreeNode.DisplayTextRight, 'DisplayTextRight');
    RegisterMethod(@TTreeNode.EditText, 'EditText');
    RegisterMethod(@TTreeNode.FindNode, 'FindNode');
    RegisterMethod(@TTreeNode.GetFirstChild, 'GetFirstChild');
    RegisterMethod(@TTreeNode.GetFirstVisibleChild, 'GetFirstVisibleChild');
    RegisterMethod(@TTreeNode.GetHandle, 'GetHandle');
    RegisterMethod(@TTreeNode.GetLastChild, 'GetLastChild');
    RegisterMethod(@TTreeNode.GetLastSibling, 'GetLastSibling');
    RegisterMethod(@TTreeNode.GetLastSubChild, 'GetLastSubChild');
    RegisterMethod(@TTreeNode.GetLastVisibleChild, 'GetLastVisibleChild');
    RegisterMethod(@TTreeNode.GetNext, 'GetNext');
    RegisterMethod(@TTreeNode.GetNextChild, 'GetNextChild');
    RegisterMethod(@TTreeNode.GetNextExpanded, 'GetNextExpanded');
    RegisterMethod(@TTreeNode.GetNextMultiSelected, 'GetNextMultiSelected');
    RegisterMethod(@TTreeNode.GetNextSibling, 'GetNextSibling');
    RegisterMethod(@TTreeNode.GetNextSkipChildren, 'GetNextSkipChildren');
    RegisterMethod(@TTreeNode.GetNextVisible, 'GetNextVisible');
    RegisterMethod(@TTreeNode.GetNextVisibleSibling, 'GetNextVisibleSibling');
    RegisterMethod(@TTreeNode.GetParentNodeOfAbsoluteLevel, 'GetParentNodeOfAbsoluteLevel');
    RegisterMethod(@TTreeNode.GetPrev, 'GetPrev');
    RegisterMethod(@TTreeNode.GetPrevChild, 'GetPrevChild');
    RegisterMethod(@TTreeNode.GetPrevExpanded, 'GetPrevExpanded');
    RegisterMethod(@TTreeNode.GetPrevMultiSelected, 'GetPrevMultiSelected');
    RegisterMethod(@TTreeNode.GetPrevSibling, 'GetPrevSibling');
    RegisterMethod(@TTreeNode.GetPrevVisible, 'GetPrevVisible');
    RegisterMethod(@TTreeNode.GetPrevVisibleSibling, 'GetPrevVisibleSibling');
    RegisterMethod(@TTreeNode.GetTextPath, 'GetTextPath');
    RegisterMethod(@TTreeNode.HasAsParent, 'HasAsParent');
    RegisterMethod(@TTreeNode.IndexOf, 'IndexOf');
    RegisterMethod(@TTreeNode.IndexOfText, 'IndexOfText');
    RegisterMethod(@TTreeNode.Collapse, 'Collapse');
    RegisterMethod(@TTreeNode.Delete, 'Delete');
    RegisterMethod(@TTreeNode.DeleteChildren, 'DeleteChildren');
    RegisterMethod(@TTreeNode.EndEdit, 'EndEdit');
    RegisterMethod(@TTreeNode.Expand, 'Expand');
    RegisterMethod(@TTreeNode.ExpandParents, 'ExpandParents');
    RegisterMethod(@TTreeNode.FreeAllNodeData, 'FreeAllNodeData');
    RegisterMethod(@TTreeNode.MakeVisible, 'MakeVisible');
    RegisterVirtualMethod(@TTreeNode.MoveTo, 'MoveTo');
    RegisterMethod(@TTreeNode.MultiSelectGroup, 'MultiSelectGroup');
    RegisterMethod(@TTreeNode.Update, 'Update');

    RegisterPropertyHelper(@TTreeNodeAbsoluteIndex_R, nil, 'AbsoluteIndex');
    RegisterPropertyHelper(@TTreeNodeCount_R, nil, 'Count');
    RegisterPropertyHelper(@TTreeNodeCut_R, @TTreeNodeCut_W, 'Cut');
    RegisterPropertyHelper(@TTreeNodeData_R, @TTreeNodeData_W, 'Data');
    RegisterPropertyHelper(@TTreeNodeDeleting_R, nil, 'Deleting');
    RegisterPropertyHelper(@TTreeNodeExpanded_R, @TTreeNodeExpanded_W, 'Expanded');
    RegisterPropertyHelper(@TTreeNodeFocused_R, @TTreeNodeFocused_W, 'Focused');
    RegisterPropertyHelper(@TTreeNodeHandle_R, nil, 'Handle');
    RegisterPropertyHelper(@TTreeNodeHasChildren_R, @TTreeNodeHasChildren_W, 'HasChildren');
    RegisterPropertyHelper(@TTreeNodeHeight_R, @TTreeNodeHeight_W, 'Height');
    RegisterPropertyHelper(@TTreeNodeImageIndex_R, @TTreeNodeImageIndex_W, 'ImageIndex');
    RegisterPropertyHelper(@TTreeNodeIndex_R, @TTreeNodeIndex_W, 'Index');
    RegisterPropertyHelper(@TTreeNodeIsFullHeightVisible_R, nil, 'IsFullHeightVisible');
    RegisterPropertyHelper(@TTreeNodeIsVisible_R, nil, 'IsVisible');
    RegisterPropertyHelper(@TTreeNodeItems_R, @TTreeNodeItems_W, 'Items');
    RegisterPropertyHelper(@TTreeNodeLevel_R, nil, 'Level');
    RegisterPropertyHelper(@TTreeNodeMultiSelected_R, @TTreeNodeMultiSelected_W, 'MultiSelected');
    RegisterPropertyHelper(@TTreeNodeNodeEffect_R, @TTreeNodeNodeEffect_W, 'NodeEffect');
    RegisterPropertyHelper(@TTreeNodeOverlayIndex_R, @TTreeNodeOverlayIndex_W, 'OverlayIndex');
    RegisterPropertyHelper(@TTreeNodeOwner_R, nil, 'Owner');
    RegisterPropertyHelper(@TTreeNodeParent_R, nil, 'Parent');
    RegisterPropertyHelper(@TTreeNodeSelected_R, @TTreeNodeSelected_W, 'Selected');
    RegisterPropertyHelper(@TTreeNodeSelectedIndex_R, @TTreeNodeSelectedIndex_W, 'SelectedIndex');
    RegisterPropertyHelper(@TTreeNodeStateIndex_R, @TTreeNodeStateIndex_W, 'StateIndex');
    RegisterPropertyHelper(@TTreeNodeStates_R, nil, 'States');
    RegisterPropertyHelper(@TTreeNodeSubTreeCount_R, nil, 'SubTreeCount');
    RegisterPropertyHelper(@TTreeNodeText_R, @TTreeNodeText_W, 'Text');
    RegisterPropertyHelper(@TTreeNodeTop_R, nil, 'Top');
    RegisterPropertyHelper(@TTreeNodeTreeNodes_R, nil, 'TreeNodes');
    RegisterPropertyHelper(@TTreeNodeTreeView_R, nil, 'TreeView');
    RegisterPropertyHelper(@TTreeNodeVisible_R, @TTreeNodeVisible_W, 'Visible');
  end;

  with Cl.Add(TTreeNodes) do
  begin
    //RegisterConstructor(@TTreeNodes.Create, 'Create');
    RegisterMethod(@TTreeNodes.Add, 'Add');
    RegisterMethod(@TTreeNodes.AddChild, 'AddChild');
    RegisterMethod(@TTreeNodes.AddChildFirst, 'AddChildFirst');
    RegisterMethod(@TTreeNodes.AddChildObject, 'AddChildObject');
    RegisterMethod(@TTreeNodes.AddChildObjectFirst, 'AddChildObjectFirst');
    RegisterMethod(@TTreeNodes.AddFirst, 'AddFirst');
    RegisterMethod(@TTreeNodes.AddObject, 'AddObject');
    RegisterMethod(@TTreeNodes.AddObjectFirst, 'AddObjectFirst');
    RegisterMethod(@TTreeNodes.FindNodeWithData, 'FindNodeWithData');
    RegisterMethod(@TTreeNodes.FindNodeWithText, 'FindNodeWithText');
    RegisterMethod(@TTreeNodes.FindNodeWithTextPath, 'FindNodeWithTextPath');
    RegisterMethod(@TTreeNodes.FindTopLvlNode, 'FindTopLvlNode');
    RegisterMethod(@TTreeNodes.GetFirstNode, 'GetFirstNode');
    RegisterMethod(@TTreeNodes.GetFirstVisibleNode, 'GetFirstVisibleNode');
    RegisterMethod(@TTreeNodes.GetLastExpandedSubNode, 'GetLastExpandedSubNode');
    RegisterMethod(@TTreeNodes.GetLastNode, 'GetLastNode');
    RegisterMethod(@TTreeNodes.GetLastSubNode, 'GetLastSubNode');
    RegisterMethod(@TTreeNodes.GetLastVisibleNode, 'GetLastVisibleNode');
    RegisterMethod(@TTreeNodes.GetSelections, 'GetSelections');
    RegisterMethod(@TTreeNodes.Insert, 'Insert');
    RegisterMethod(@TTreeNodes.InsertBehind, 'InsertBehind');
    RegisterMethod(@TTreeNodes.InsertObject, 'InsertObject');
    RegisterMethod(@TTreeNodes.InsertObjectBehind, 'InsertObjectBehind');
    RegisterMethod(@TTreeNodes.IsMultiSelection, 'IsMultiSelection');
    RegisterMethod(@TTreeNodes.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TTreeNodes.Clear, 'Clear');
    RegisterMethod(@TTreeNodes.ClearMultiSelection, 'ClearMultiSelection');
    RegisterMethod(@TTreeNodes.Delete, 'Delete');
    RegisterMethod(@TTreeNodes.EndUpdate, 'EndUpdate');
    RegisterMethod(@TTreeNodes.SelectionsChanged, 'SelectionsChanged');
    RegisterMethod(@TTreeNodes.SelectOnlyThis, 'SelectOnlyThis');
    RegisterMethod(@TTreeNodes.SortTopLevelNodes, 'SortTopLevelNodes');
    RegisterPropertyHelper(@TTreeNodesCount_R, nil, 'Count');
    RegisterPropertyHelper(@TTreeNodesItem_R, nil, 'Item');
    RegisterPropertyHelper(@TTreeNodesKeepCollapsedNodes_R, @TTreeNodesKeepCollapsedNodes_W, 'KeepCollapsedNodes');
    RegisterPropertyHelper(@TTreeNodesOwner_R, nil, 'Owner');
    RegisterPropertyHelper(@TTreeNodesSelectionCount_R, nil, 'SelectionCount');
    RegisterPropertyHelper(@TTreeNodesTopLvlCount_R, nil, 'TopLvlCount');
    RegisterPropertyHelper(@TTreeNodesTopLvlItems_R, nil, 'TopLvlItems');
  end;

  with Cl.Add(TTreeView) do
  begin
    RegisterMethod(@TTreeView.AlphaSort, 'AlphaSort');
    RegisterVirtualMethod(@TTreeView.ClearSelection, 'ClearSelection');
    RegisterMethod(@TTreeView.CustomSort, 'CustomSort');
    RegisterMethod(@TTreeView.DefaultTreeViewSort, 'DefaultTreeViewSort');
    RegisterMethod(@TTreeView.GetHitTestInfoAt, 'GetHitTestInfoAt');
    RegisterMethod(@TTreeView.GetNodeAt, 'GetNodeAt');
    RegisterMethod(@TTreeView.GetInsertMarkAt, 'GetInsertMarkAt');
    RegisterMethod(@TTreeView.SetInsertMark, 'SetInsertMark');
    RegisterVirtualMethod(@TTreeView.SetInsertMarkAt, 'SetInsertMarkAt');
    RegisterMethod(@TTreeView.IsEditing, 'IsEditing');
    RegisterMethod(@TTreeView.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TTreeView.EndUpdate, 'EndUpdate');
    RegisterMethod(@TTreeView.FullCollapse, 'FullCollapse');
    RegisterMethod(@TTreeView.FullExpand, 'FullExpand');
    RegisterMethod(@TTreeView.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TTreeView.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TTreeView.SavetoFile, 'SaveToFile');
    RegisterMethod(@TTreeView.SavetoStream, 'SaveToStream');
    RegisterMethod(@TTreeView.LockSelectionChangeEvent, 'LockSelectionChangeEvent');
    RegisterMethod(@TTreeView.UnLockSelectionChangeEvent, 'UnLockSelectionChangeEvent');
    RegisterMethod(@TTreeView.GetFirstMultiSelected, 'GetFirstMultiSelected');
    RegisterMethod(@TTreeView.GetLastMultiSelected, 'GetLastMultiSelected');
    RegisterMethod(@TTreeView.Select, 'Select');
    RegisterMethod(@TTreeView.SelectionVisible, 'SelectionVisible');
    RegisterMethod(@TTreeView.MakeSelectionVisible, 'MakeSelectionVisible');
    RegisterMethod(@TTreeView.ClearInvisibleSelection, 'ClearInvisibleSelection');
    RegisterMethod(@TTreeView.StoreCurrentSelection, 'StoreCurrentSelection');
    RegisterMethod(@TTreeView.ApplyStoredSelection, 'ApplyStoredSelection');
    RegisterMethod(@TTreeView.MoveToNextNode, 'MoveToNextNode');
    RegisterMethod(@TTreeView.MoveToPrevNode, 'MoveToPrevNode');
    RegisterPropertyHelper(@TTreeViewSelected_R, @TTreeViewSelected_W, 'Selected');
    RegisterPropertyHelper(@TTreeViewSelectionCount_R, nil, 'SelectionCount');
    RegisterPropertyHelper(@TTreeViewSelections_R, nil, 'Selections');
  end;
end;

procedure TParamListValues_R(Self: TParamList; var T: Variant; I: String); begin T := Self.Values[I]; end;
procedure TParamListValues_W(Self: TParamList; T: Variant; I: String); begin Self.Values[I] := T; end;
procedure TParamListObjects_R(Self: TParamList; var T: TObject; I: String); begin T := Self.Objects[I]; end;
procedure TParamListObjects_W(Self: TParamList; T: TObject; I: String); begin Self.Objects[I] := T; end;
procedure TParamListNames_R(Self: TParamList; var T: String; I: Integer); begin T := Self.Names[I]; end;
procedure TParamListValueFromIndex_R(Self: TParamList; var T: Variant; I: Integer); begin T := Self.ValueFromIndex[I]; end;
procedure TParamListValueFromIndex_W(Self: TParamList; T: Variant; I: Integer); begin Self.ValueFromIndex[I] := T; end;
procedure TParamListObjectFromIndex_R(Self: TParamList; var T: TObject; I: Integer); begin T := Self.ObjectFromIndex[I]; end;
procedure TParamListObjectFromIndex_W(Self: TParamList; T: TObject; I: Integer); begin Self.ObjectFromIndex[I] := T; end;
procedure TParamListCount_R(Self: TParamList; var T: Integer); begin T := Self.Count; end;
procedure TParamListOnGetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnGetParam; end;
procedure TParamListOnGetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnGetParam := T; end;
procedure TParamListOnSetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnSetParam; end;
procedure TParamListOnSetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnSetParam := T; end;

procedure RIRegister_dxTypes(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TParamList) do
  begin
    RegisterConstructor(@TParamList.Create, 'Create');
    RegisterMethod(@TParamList.Clear, 'Clear');
    RegisterMethod(@TParamList.ParamExists, 'ParamExists');
    RegisterPropertyHelper(@TParamListValues_R, @TParamListValues_W, 'Values');
    RegisterPropertyHelper(@TParamListObjects_R, @TParamListObjects_W, 'Objects');
    RegisterPropertyHelper(@TParamListNames_R, nil, 'Names');
    RegisterPropertyHelper(@TParamListValueFromIndex_R, @TParamListValueFromIndex_W, 'ValueFromIndex');
    RegisterPropertyHelper(@TParamListObjectFromIndex_R, @TParamListObjectFromIndex_W, 'ObjectFromIndex');
    RegisterPropertyHelper(@TParamListCount_R, nil, 'Count');
    RegisterEventPropertyHelper(@TParamListOnGetParam_R, @TParamListOnGetParam_W, 'OnGetParam');
    RegisterEventPropertyHelper(@TParamListOnSetParam_R, @TParamListOnSetParam_W, 'OnSetParam');
  end;
end;

procedure TTrayIconAnimate_R(Self: TTrayIcon; var T: Boolean); begin T := Self.Animate; end;
procedure TTrayIconAnimate_W(Self: TTrayIcon; T: Boolean); begin Self.Animate := T; end;
procedure TTrayIconAnimateInterval_R(Self: TTrayIcon; var T: Cardinal); begin T := Self.AnimateInterval; end;
procedure TTrayIconAnimateInterval_W(Self: TTrayIcon; T: Cardinal); begin Self.AnimateInterval := T; end;
procedure TTrayIconCanvas_R(Self: TTrayIcon; var T: TCanvas); begin T := Self.Canvas; end;
procedure TTrayIconIcons_R(Self: TTrayIcon; var T: TCustomImageList); begin T := Self.Icons; end;
procedure TTrayIconIcons_W(Self: TTrayIcon; T: TCustomImageList); begin Self.Icons := T; end;
procedure TTrayIconShowIcon_R(Self: TTrayIcon; var T: Boolean); begin T := Self.ShowIcon; end;
procedure TTrayIconShowIcon_W(Self: TTrayIcon; T: Boolean); begin Self.ShowIcon := T; end;

procedure RIRegister_TrayIcon(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TTrayIcon) do
  begin
    RegisterMethod(@TTrayIcon.ShowBalloonHint, 'ShowBalloonHint');
    RegisterPropertyHelper(@TTrayIconAnimate_R, @TTrayIconAnimate_W, 'Animate');
    RegisterPropertyHelper(@TTrayIconAnimateInterval_R, @TTrayIconAnimateInterval_W, 'AnimateInterval');
    RegisterPropertyHelper(@TTrayIconCanvas_R, nil, 'Canvas');
    RegisterPropertyHelper(@TTrayIconIcons_R, @TTrayIconIcons_W, 'Icons');
    RegisterPropertyHelper(@TTrayIconShowIcon_R, @TTrayIconShowIcon_W, 'ShowIcon');
  end;
end;

procedure TCsvFileRowCount_R(Self: TCsvData; var T: Integer); begin T := Self.RowCount; end;
procedure TCsvFileRowCount_W(Self: TCsvData; T: Integer); begin Self.RowCount := T; end;
procedure TCsvFileColCount_R(Self: TCsvData; var T: Integer); begin T := Self.ColCount; end;
procedure TCsvFileColCount_W(Self: TCsvData; T: Integer); begin Self.ColCount := T; end;
procedure TCsvFileCells_R(Self: TCsvData; var T: String; C, R: Integer); begin T := Self.Cells[C, R]; end;
procedure TCsvFileCells_W(Self: TCsvData; const T: String; C, R: Integer); begin Self.Cells[C, R] := T; end;
procedure TCsvFileDelimiter_R(Self: TCsvData; var T: Char); begin T := Self.Delimiter; end;
procedure TCsvFileDelimiter_W(Self: TCsvData; T: Char); begin Self.Delimiter := T; end;

procedure RIRegister_CsvData(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCsvData) do
  begin
    RegisterConstructor(@TCsvData.Create, 'Create');
    RegisterMethod(@TCsvData.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TCsvData.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TCsvData.SaveToFile, 'SaveToFile');
    RegisterMethod(@TCsvData.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TCsvFileRowCount_R, @TCsvFileRowCount_W, 'RowCount');
    RegisterPropertyHelper(@TCsvFileColCount_R, @TCsvFileColCount_W, 'ColCount');
    RegisterPropertyHelper(@TCsvFileCells_R, @TCsvFileCells_W, 'Cells');
    RegisterPropertyHelper(@TCsvFileDelimiter_R, @TCsvFileDelimiter_W, 'Delimiter');
  end;
end;

procedure RIRegister_dxRecordId(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxRecordId) do
  begin

  end;
end;

procedure RIRegister_Functions(Exec: TPSExec);
begin
  Exec.RegisterDelphiFunction(@MessageBox, 'MsgBox', cdRegister);
  Exec.RegisterDelphiFunction(@Debug, 'Debug', cdRegister);

  Exec.RegisterDelphiFunction(@MyUTF8Length, 'Utf8Length', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Pos, 'Utf8Pos', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Copy, 'Utf8Copy', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8Delete, 'Utf8Delete', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8Insert, 'Utf8Insert', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8StringReplace, 'Utf8StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8LowerCase, 'Utf8LowerCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8UpperCase, 'Utf8UpperCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8CompareStr, 'Utf8CompareStr', cdRegister);
  Exec.RegisterDelphiFunction(@MyUtf8CompareText, 'Utf8CompareText', cdRegister);
  Exec.RegisterDelphiFunction(@Utf8ToWinCP, 'Utf8ToWinCP', cdRegister);
  Exec.RegisterDelphiFunction(@WinCPToUtf8, 'WinCPToUtf8', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8ToUTF16, 'Utf8ToUtf16', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF16ToUTF8, 'Utf16ToUtf8', cdRegister);

  Exec.RegisterDelphiFunction(@FileExistsUtf8, 'FileExists', cdRegister);
  Exec.RegisterDelphiFunction(@FileAgeUtf8, 'FileAge', cdRegister);
  Exec.RegisterDelphiFunction(@DirectoryExistsUtf8, 'DirectoryExists', cdRegister);
  Exec.RegisterDelphiFunction(@ExpandFileNameUtf8, 'ExpandFileName', cdRegister);
  Exec.RegisterDelphiFunction(@FileSetDateUtf8, 'FileSetDate', cdRegister);
  Exec.RegisterDelphiFunction(@FileGetAttrUtf8, 'FileGetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@FileSetAttrUtf8, 'FileSetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@DeleteFileUtf8, 'DeleteFile', cdRegister);
  Exec.RegisterDelphiFunction(@RenameFileUtf8, 'RenameFile', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDirUtf8, 'GetCurrentDir', cdRegister);
  Exec.RegisterDelphiFunction(@CreateDirUtf8, 'CreateDir', cdRegister);
  Exec.RegisterDelphiFunction(@RemoveDirUtf8, 'RemoveDir', cdRegister);
  Exec.RegisterDelphiFunction(@ForceDirectoriesUtf8, 'ForceDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyCopyFile, 'CopyFile', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllFiles, 'FindAllFiles', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllDirectories, 'FindAllDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileName, 'ExtractFileName', cdRegister);
  Exec.RegisterDelphiFunction(@ExtractFileNameOnly, 'ExtractFileNameOnly', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileExt, 'ExtractFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFilePath, 'ExtractFilePath', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDrive, 'ExtractFileDrive', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDir, 'ExtractFileDir', cdRegister);
  Exec.RegisterDelphiFunction(@MyChangeFileExt, 'ChangeFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyIncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyExcludeLeadingPathDelimiter, 'ExcludeLeadingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempFilename, 'GetTempFileName', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempDir, 'GetTempDir', cdRegister);
  Exec.RegisterDelphiFunction(@ShellExec, 'ShellExecute', cdRegister);
  Exec.RegisterDelphiFunction(@FileDateToDateTime, 'FileDateToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@DateTimeToFileDate, 'DateTimeToFileDate', cdRegister);
  Exec.RegisterDelphiFunction(@FileSizeUtf8, 'FileSize', cdRegister);

  //Exec.RegisterDelphiFunction(@CreateForm, 'CreateForm', cdRegister);
  //Exec.RegisterDelphiFunction(@DestroyForm, 'DestroyForm', cdRegister);
  Exec.RegisterDelphiFunction(@GetForms, 'GetForms', cdRegister);

  //Exec.RegisterDelphiFunction(@FreeObj, 'FreeObj', cdRegister);
  Exec.RegisterDelphiFunction(@MyRandom, 'Random', cdRegister);

  Exec.RegisterDelphiFunction(@MsgDlg, 'MessageDlg', cdRegister);
  Exec.RegisterDelphiFunction(@InputBox, 'InputBox', cdRegister);
  Exec.RegisterDelphiFunction(@EvalExpr, 'EvalExpr', cdRegister);

  Exec.RegisterDelphiFunction(@DCount, 'DCount', cdRegister);
  Exec.RegisterDelphiFunction(@DSum, 'DSum', cdRegister);
  Exec.RegisterDelphiFunction(@DAvg, 'DAvg', cdRegister);
  Exec.RegisterDelphiFunction(@DMax, 'DMax', cdRegister);
  Exec.RegisterDelphiFunction(@DMin, 'DMin', cdRegister);
  Exec.RegisterDelphiFunction(@DMerge, 'DMerge', cdRegister);
  Exec.RegisterDelphiFunction(@ToWordsRu, 'ToWords', cdRegister);
  Exec.RegisterDelphiFunction(@RurToWords, 'RurToWords', cdRegister);
  Exec.RegisterDelphiFunction(@Nz, 'Nz', cdRegister);
  Exec.RegisterDelphiFunction(@MathRound, 'RoundTo', cdRegister);
  Exec.RegisterDelphiFunction(@_MyFrac, 'Frac', cdRegister);
  Exec.RegisterDelphiFunction(@Power, 'Power', cdRegister);
  Exec.RegisterDelphiFunction(@YearsBetweenEx, 'YearsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MonthsBetweenEx, 'MonthsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@WeeksBetweenEx, 'WeeksBetween', cdRegister);
  Exec.RegisterDelphiFunction(@DaysBetweenEx, 'DaysBetween', cdRegister);
  Exec.RegisterDelphiFunction(@HoursBetweenEx, 'HoursBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MinutesBetweenEx, 'MinutesBetween', cdRegister);
  Exec.RegisterDelphiFunction(@SecondsBetweenEx, 'SecondsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MilliSecondsBetween, 'MilliSecondsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@IncYear, 'AddYear', cdRegister);
  Exec.RegisterDelphiFunction(@IncMonth, 'AddMonth', cdRegister);
  Exec.RegisterDelphiFunction(@IncWeek, 'AddWeek', cdRegister);
  Exec.RegisterDelphiFunction(@IncDay, 'AddDay', cdRegister);
  Exec.RegisterDelphiFunction(@IncHour, 'AddHour', cdRegister);
  Exec.RegisterDelphiFunction(@IncMinute, 'AddMinute', cdRegister);
  Exec.RegisterDelphiFunction(@IncSecond, 'AddSecond', cdRegister);
  Exec.RegisterDelphiFunction(@YearOf, 'YearOf', cdRegister);
  Exec.RegisterDelphiFunction(@MonthOf, 'MonthOf', cdRegister);
  Exec.RegisterDelphiFunction(@WeekOf, 'WeekOf', cdRegister);
  Exec.RegisterDelphiFunction(@DayOf, 'DayOf', cdRegister);
  Exec.RegisterDelphiFunction(@HourOf, 'HourOf', cdRegister);
  Exec.RegisterDelphiFunction(@MinuteOf, 'MinuteOf', cdRegister);
  Exec.RegisterDelphiFunction(@SecondOf, 'SecondOf', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToTime, 'StrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTimeToStr, 'TimeToStr', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDate, 'TryStrToDate', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToTime, 'TryStrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDateTime, 'TryStrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@TryStrToInt, 'TryStrToInt', cdRegister);
  Exec.RegisterDelphiFunction(@TryStrToInt64, 'TryStrToInt64', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToFloat, 'TryStrToFloat', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToDateTime, 'StrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyIntToHex, 'IntToHex', cdRegister);

  Exec.RegisterDelphiFunction(@GetMonthName, 'GetMonthName', cdRegister);
  Exec.RegisterDelphiFunction(@GetWeekName, 'GetWeekName', cdRegister);
  Exec.RegisterDelphiFunction(@DayOfTheWeek, 'DayOfTheWeek', cdRegister);
  Exec.RegisterDelphiFunction(@FmtDate, 'FmtDate', cdRegister);
  Exec.RegisterDelphiFunction(@SetZeros, 'FillZeros', cdRegister);
  Exec.RegisterDelphiFunction(@CalcPeriod, 'CalcPeriod', cdRegister);
  Exec.RegisterDelphiFunction(@BeginYear, 'BeginYear', cdRegister);
  Exec.RegisterDelphiFunction(@BeginQuarter, 'BeginQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@BeginMonth, 'BeginMonth', cdRegister);
  Exec.RegisterDelphiFunction(@BeginWeek, 'BeginWeek', cdRegister);
  Exec.RegisterDelphiFunction(@EndYear, 'EndYear', cdRegister);
  Exec.RegisterDelphiFunction(@EndQuarter, 'EndQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@EndMonth, 'EndMonth', cdRegister);
  Exec.RegisterDelphiFunction(@EndWeek, 'EndWeek', cdRegister);
  Exec.RegisterDelphiFunction(@QuarterOf, 'QuarterOf', cdRegister);
  Exec.RegisterDelphiFunction(@GetUser, 'GetCurrentUser', cdRegister);
  Exec.RegisterDelphiFunction(@GetRole, 'GetCurrentRole', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDatabase, 'GetCurrentDatabase', cdRegister);
  Exec.RegisterDelphiFunction(@GetTemplatesDir, 'GetTemplatesDir', cdRegister);
  Exec.RegisterDelphiFunction(@GetOutDir, 'GetOutputDir', cdRegister);
  Exec.RegisterDelphiFunction(@MyFormat, 'Format', cdRegister);
  Exec.RegisterDelphiFunction(@MyStringReplace, 'StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@SplitStr, 'SplitStr', cdRegister);

  Exec.RegisterDelphiFunction(@MyClipboard, 'Clipboard', cdRegister);

	Exec.RegisterDelphiFunction(@SQLSelect, 'SQLSelect', cdRegister);
  Exec.RegisterDelphiFunction(@SQLExecute, 'SQLExecute', cdRegister);

  Exec.RegisterDelphiFunction(@ColorToString, 'ColorToString', cdRegister);
  Exec.RegisterDelphiFunction(@MyStringToColor, 'StringToColor', cdRegister);
  Exec.RegisterDelphiFunction(@RGBToColor, 'RGBToColor', cdRegister);
  Exec.RegisterDelphiFunction(@ColorToRGB, 'ColorToRGB', cdRegister);
  Exec.RegisterDelphiFunction(@RedGreenBlue, 'RedGreenBlue', cdRegister);
  Exec.RegisterDelphiFunction(@MyFormatFloat, 'FormatFloat', cdRegister);

  Exec.RegisterDelphiFunction(@EncodeMD5, 'EncodeMD5', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeSHA1, 'EncodeSHA1', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeBase64, 'EncodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@DecodeBase64, 'DecodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@HMACMD5, 'HMACMD5', cdRegister);
  Exec.RegisterDelphiFunction(@HMACSHA1, 'HMACSHA1', cdRegister);

  Exec.RegisterDelphiFunction(@VarToStr, 'VarToStr', cdRegister);
  Exec.RegisterDelphiFunction(@VarIsNothing, 'VarIsNothing', cdRegister);
  Exec.RegisterDelphiFunction(@VarCast, 'VarCast', cdRegister);
  Exec.RegisterDelphiFunction(@VarAsType, 'VarAsType', cdRegister);

  Exec.RegisterDelphiFunction(@ShowPrintErrors, 'ShowPrintErrors', cdRegister);
  Exec.RegisterDelphiFunction(@MySameValue, 'SameValue', cdRegister);

  Exec.RegisterDelphiFunction(@Point, 'Point', cdRegister);
  Exec.RegisterDelphiFunction(@Rect, 'Rect', cdRegister);

  Exec.RegisterDelphiFunction(@GetId, 'GetComponentId', cdRegister);
  Exec.RegisterDelphiFunction(@GetFieldName, 'GetComponentFieldName', cdRegister);
  Exec.RegisterDelphiFunction(@ShowExprEditor, 'ShowExprEditor', cdRegister);
  Exec.RegisterDelphiFunction(@GetFormatSettings, 'GetFormatSettings', cdRegister);
  Exec.RegisterDelphiFunction(@SetFormatSettings, 'SetFormatSettings', cdRegister);

  Exec.RegisterDelphiFunction(@VarArrayOf, 'VarArrayOf', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayDimCount, 'VarArrayDimCount', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayLowBound, 'VarArrayLowBound', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayHighBound, 'VarArrayHighBound', cdRegister);

  Exec.RegisterDelphiFunction(@GetBuildDate, 'GetBuildDate', cdRegister);

  Exec.RegisterDelphiFunction(@SetPropertyValue, 'SetPropValue', cdRegister);
  Exec.RegisterDelphiFunction(@GetPropertyValue, 'GetPropValue', cdRegister);

  Exec.RegisterDelphiFunction(@ReadXmlFromFile, 'ReadXmlFromFile', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlFromStream, 'ReadXmlFromStream', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlFromString, 'ReadXmlFromString', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlNodeFromString, 'ReadXmlNodeFromString', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToFile, 'WriteXmlToFile', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToStream, 'WriteXmlToStream', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToString, 'WriteXmlToString', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlNodeToString, 'WriteXmlNodeToString', cdRegister);

  Exec.RegisterDelphiFunction(@ReadJSONFromString, 'ReadJSONFromString', cdRegister);
  Exec.RegisterDelphiFunction(@ReadJSONFromStream, 'ReadJSONFromStream', cdRegister);
  Exec.RegisterDelphiFunction(@ReadJSONFromFile, 'ReadJSONFromFile', cdRegister);
  Exec.RegisterDelphiFunction(@JSONStringToString, 'JSONStringToString', cdRegister);
  Exec.RegisterDelphiFunction(@StringToJSONString, 'StringToJSONString', cdRegister);

  Exec.RegisterDelphiFunction(@Utf8CharToString, 'Utf8CharToString', cdRegister);
  Exec.RegisterDelphiFunction(@StringToUtf8Char, 'StringToUtf8Char', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeURLElement, 'EncodeURLElement', cdRegister);
  Exec.RegisterDelphiFunction(@DecodeURLElement, 'DecodeURLElement', cdRegister);
  Exec.RegisterDelphiFunction(@DebugFile, 'DebugFile', cdRegister);

  Exec.RegisterDelphiFunction(@ScaleToScreen, 'Scale96ToScreen', cdRegister);
  Exec.RegisterDelphiFunction(@IIF, 'IIF', cdRegister);
  Exec.RegisterDelphiFunction(@CreateGUIDString, 'CreateGUIDString', cdRegister);

  Exec.RegisterDelphiFunction(@SetVar, 'SetExprVar', cdRegister);
  Exec.RegisterDelphiFunction(@GetVar, 'GetExprVar', cdRegister);
end;

end.

