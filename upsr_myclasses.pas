{
This file is a modified version of the file included in the PascalScript package.
Modified the file by Pavel Duborkin (e-mail: 7bit@list.ru, mydataexpress@mail.ru).
The changes made are marked with comments that begin with "// 7bit" and end with "//"
}
unit uPSR_MyClasses;

interface
uses
  uPSRuntime, uPSUtils, LazUtf8;


procedure RIRegisterTStrings(cl: TPSRuntimeClassImporter; Streams: Boolean);
procedure RIRegisterTStringList(cl: TPSRuntimeClassImporter);
procedure RIRegisterTSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFILESTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSTRINGSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCOLLECTION(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCOLLECTIONITEM(Cl: TPSRuntimeClassImporter);

procedure RIRegister_Classes(Cl: TPSRuntimeClassImporter; Streams: Boolean{$IFDEF D4PLUS}=True{$ENDIF});

implementation
uses
  Classes, scriptfuncs, mytypes;

procedure TStringsCapacityR(Self: TStrings; var T: Longint); begin T := Self.Capacity; end;
procedure TStringsCapacityW(Self: TStrings; T: Longint); begin Self.Capacity := T; end;
procedure TStringsDelimiterR(Self: TStrings; var T: char); begin T := Self.Delimiter; end;
procedure TStringsDelimiterW(Self: TStrings; T: char); begin Self.Delimiter:= T; end;
procedure TStringsDelimitedTextR(Self: TStrings; var T: string); begin T := Self.DelimitedText; end;
procedure TStringsDelimitedTextW(Self: TStrings; T: string); begin Self.DelimitedText:= T; end;
procedure TStringsNameValueSeparatorR(Self: TStrings; var T: char); begin T := Self.NameValueSeparator; end;
procedure TStringsNameValueSeparatorW(Self: TStrings; T: char); begin Self.NameValueSeparator:= T; end;
procedure TStringsQuoteCharR(Self: TStrings; var T: char); begin T := Self.QuoteChar; end;
procedure TStringsQuoteCharW(Self: TStrings; T: char); begin Self.QuoteChar:= T; end;

procedure TStringsCountR(Self: TStrings; var T: Longint); begin T := Self.Count; end;

procedure TStringsTextR(Self: TStrings; var T: string); begin T := Self.Text; end;
procedure TStringsTextW(Self: TStrings; T: string); begin Self.Text:= T; end;

procedure TStringsCommaTextR(Self: TStrings; var T: string); begin T := Self.CommaText; end;
procedure TStringsCommaTextW(Self: TStrings; T: string); begin Self.CommaText:= T; end;

procedure TStringsObjectsR(Self: TStrings; var T: TObject; I: Longint);
begin
T := Self.Objects[I];
end;
procedure TStringsObjectsW(Self: TStrings; const T: TObject; I: Longint);
begin
  Self.Objects[I]:= T;
end;

procedure TStringsStringsR(Self: TStrings; var T: string; I: Longint);
begin
T := Self.Strings[I];
end;
procedure TStringsStringsW(Self: TStrings; const T: string; I: Longint);
begin
  Self.Strings[I]:= T;
end;

procedure TStringsNamesR(Self: TStrings; var T: string; I: Longint);
begin
T := Self.Names[I];
end;
procedure TStringsValuesR(Self: TStrings; var T: string; const I: string);
begin
T := Self.Values[I];
end;
procedure TStringsValuesW(Self: TStrings; Const T, I: String);
begin
  Self.Values[I]:= T;
end;

procedure TStringsValueFromIndexR(Self: TStrings; var T: string; const I: Longint);
begin
T := Self.ValueFromIndex[I];
end;
procedure TStringsValueFromIndexW(Self: TStrings; Const T: String; I: Longint);
begin
  Self.ValueFromIndex[I]:= T;
end;

procedure RIRegisterTStrings(cl: TPSRuntimeClassImporter; Streams: Boolean); // requires TPersistent
begin
  with Cl.Add(TStrings) do
  begin

    RegisterVirtualMethod(@TStrings.Add, 'Add');
    RegisterVirtualMethod(@TStrings.AddStrings, 'AddStrings');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Clear, 'Clear');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Delete, 'Delete');
    RegisterVirtualMethod(@TStrings.IndexOf, 'IndexOf');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Insert, 'Insert');
    RegisterPropertyHelper(@TStringsCapacityR, @TStringsCapacityW, 'Capacity');
    RegisterPropertyHelper(@TStringsDelimiterR, @TStringsDelimiterW, 'DELIMITER');
    RegisterPropertyHelper(@TStringsDelimitedTextR, @TStringsDelimitedTextW, 'DelimitedText');
    RegisterPropertyHelper(@TStringsNameValueSeparatorR, @TStringsNameValueSeparatorW, 'NameValueSeparator');
    RegisterPropertyHelper(@TStringsQuoteCharR, @TStringsQuoteCharW, 'QuoteChar');
    RegisterPropertyHelper(@TStringsCountR, nil, 'Count');
    RegisterPropertyHelper(@TStringsTextR, @TStringsTextW, 'Text');
    RegisterPropertyHelper(@TStringsCommaTextR, @TStringsCommatextW, 'CommaText');
    if Streams then
    begin
      RegisterMethod(@TStrings.LoadFromFile, 'LoadFromFile');
      RegisterMethod(@TStrings.SaveToFile, 'SaveToFile');
    end;
    RegisterPropertyHelper(@TStringsStringsR, @TStringsStringsW, 'Strings');
    RegisterPropertyHelper(@TStringsObjectsR, @TStringsObjectsW, 'Objects');

    RegisterMethod(@TStrings.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TStrings.EndUpdate, 'EndUpdate');
    RegisterMethod(@TStrings.Equals,  'Equals');
    RegisterVirtualMethod(@TStrings.Exchange, 'Exchange');
    RegisterMethod(@TStrings.IndexOfName, 'IndexOfName');
    if Streams then
      RegisterVirtualMethod(@TStrings.LoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TStrings.Move, 'Move');
    if Streams then
      RegisterVirtualMethod(@TStrings.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TStringsNamesR, nil, 'Names');
    RegisterPropertyHelper(@TStringsValuesR, @TStringsValuesW, 'Values');
    RegisterPropertyHelper(@TStringsValueFromIndexR, @TStringsValueFromIndexW, 'ValueFromIndex');
    RegisterVirtualMethod(@TSTRINGS.ADDOBJECT, 'AddObject');
    RegisterMethod(@TSTRINGS.INDEXOFOBJECT, 'IndexOfObject');
    RegisterMethod(@TSTRINGS.INSERTOBJECT, 'InsertObject');
  end;
end;

procedure TSTRINGLISTCASESENSITIVE_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.CASESENSITIVE; end;
procedure TSTRINGLISTCASESENSITIVE_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.CASESENSITIVE := T; end;
procedure TSTRINGLISTDUPLICATES_R(Self: TSTRINGLIST; var T: TDUPLICATES); begin T := Self.DUPLICATES; end;
procedure TSTRINGLISTDUPLICATES_W(Self: TSTRINGLIST; const T: TDUPLICATES); begin Self.DUPLICATES := T; end;
procedure TSTRINGLISTSORTED_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.SORTED; end;
procedure TSTRINGLISTSORTED_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.SORTED := T; end;
procedure TSTRINGLISTONCHANGE_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT);
begin
T := Self.ONCHANGE; end;
procedure TSTRINGLISTONCHANGE_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT);
begin
Self.ONCHANGE := T; end;
procedure TSTRINGLISTONCHANGING_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT); begin T := Self.ONCHANGING; end;
procedure TSTRINGLISTONCHANGING_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT); begin Self.ONCHANGING := T; end;
procedure RIRegisterTSTRINGLIST(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGLIST) do
  begin
    RegisterConstructor(@TStringList.Create, 'Create');
    RegisterVirtualMethod(@TSTRINGLIST.FIND, 'Find');
    RegisterVirtualMethod(@TSTRINGLIST.SORT, 'Sort');
    RegisterPropertyHelper(@TSTRINGLISTCASESENSITIVE_R, @TSTRINGLISTCASESENSITIVE_W, 'CaseSensitive');
    RegisterPropertyHelper(@TSTRINGLISTDUPLICATES_R, @TSTRINGLISTDUPLICATES_W, 'Duplicates');
    RegisterPropertyHelper(@TSTRINGLISTSORTED_R, @TSTRINGLISTSORTED_W, 'Sorted');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGE_R, @TSTRINGLISTONCHANGE_W, 'OnChange');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGING_R, @TSTRINGLISTONCHANGING_W, 'OnChanging');
  end;
  with Cl.Add(TSTRINGLISTUTF8) do
  begin

  end;
end;

{procedure TBITSBITS_W(Self: TBITS; T: BOOLEAN; t1: INTEGER); begin Self.BITS[t1] := T; end;
procedure TBITSBITS_R(Self: TBITS; var T: BOOLEAN; t1: INTEGER); begin T := Self.Bits[t1]; end;
procedure TBITSSIZE_R(Self: TBITS; T: INTEGER); begin Self.SIZE := T; end;
procedure TBITSSIZE_W(Self: TBITS; var T: INTEGER); begin T := Self.SIZE; end;

procedure RIRegisterTBITS(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TBITS) do
  begin
    RegisterMethod(@TBITS.OPENBIT, 'OpenBit');
    RegisterPropertyHelper(@TBITSBITS_R, @TBITSBITS_W, 'Bits');
    RegisterPropertyHelper(@TBITSSIZE_R, @TBITSSIZE_W, 'Size');
  end;
end;       }

procedure TSTREAMPOSITION_R(Self: TSTREAM; var T: LONGINT); begin t := Self.POSITION; end;
procedure TSTREAMPOSITION_W(Self: TSTREAM; T: LONGINT); begin Self.POSITION := t; end;
procedure TSTREAMSIZE_R(Self: TSTREAM; var T: LONGINT); begin t := Self.SIZE; end;

procedure RIRegisterTSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTREAM) do
  begin
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.READ, 'Read');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.WRITE, 'Write');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.SEEK, 'Seek');
    RegisterMethod(@TSTREAM.READBUFFER, 'ReadBuffer');
    RegisterMethod(@TSTREAM.WRITEBUFFER, 'WriteBuffer');
    RegisterMethod(@TSTREAM.COPYFROM, 'CopyFrom');
    RegisterPropertyHelper(@TSTREAMPOSITION_R, @TSTREAMPOSITION_W, 'Position');
    RegisterPropertyHelper(@TSTREAMSIZE_R, nil, 'Size');
  end;
end;

// mh: because FPC doesn't handle pointers to overloaded functions
{function TFileStreamCreate(Self: TClass; CreateNewInstance: Boolean; filename: string; mode: word): TObject;
begin
  Result:= TFilestream.Create(Utf8ToSys(filename), mode);
end; }

procedure TFILESTREAMHANDLE_R(Self: THANDLESTREAM; var T: INTEGER); begin T := Self.HANDLE; end;

procedure RIRegisterTFILESTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFILESTREAM) do
  begin
    RegisterConstructor(@TFileStream.Create, 'Create');
    RegisterPropertyHelper(@TFILESTREAMHANDLE_R, nil, 'Handle');
  end;
end;

procedure RIRegisterTSTRINGSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGSTREAM) do
  begin
    RegisterConstructor(@TSTRINGSTREAM.CREATE, 'Create');
  end;
end;

procedure RIRegisterTMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMEMORYSTREAM) do
  begin
    RegisterMethod(@TMEMORYSTREAM.CLEAR, 'Clear');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMSTREAM, 'LoadFromStream');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMFILE, 'LoadFromFile');
    RegisterMethod(@TMEMORYSTREAM.SAVETOSTREAM, 'SaveToStream');
    RegisterMethod(@TMEMORYSTREAM.SAVETOFILE, 'SaveToFile');
    RegisterMethod(@TMEMORYSTREAM.SETSIZE, 'SetSize');
  end;
end;

procedure TCOLLECTIONITEMS_W(Self: TCOLLECTION; const T: TCOLLECTIONITEM; const t1: INTEGER);
begin Self.ITEMS[t1] := T; end;

procedure TCOLLECTIONITEMS_R(Self: TCOLLECTION; var T: TCOLLECTIONITEM; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

procedure TCOLLECTIONCOUNT_R(Self: TCOLLECTION; var T: INTEGER);
begin T := Self.COUNT; end;

procedure TCOLLECTIONITEMINDEX_W(Self: TCOLLECTIONITEM; const T: INTEGER);
begin Self.INDEX := T; end;

procedure TCOLLECTIONITEMINDEX_R(Self: TCOLLECTIONITEM; var T: INTEGER);
begin T := Self.INDEX; end;

procedure TCOLLECTIONITEMCOLLECTION_W(Self: TCOLLECTIONITEM; const T: TCOLLECTION);
begin Self.COLLECTION := T; end;

procedure TCOLLECTIONITEMCOLLECTION_R(Self: TCOLLECTIONITEM; var T: TCOLLECTION);
begin T := Self.COLLECTION; end;


procedure RIRegisterTCOLLECTION(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TCOLLECTION) do
  begin
  RegisterConstructor(@TCOLLECTION.CREATE, 'Create');
  RegisterMethod(@TCOLLECTION.ADD, 'Add');
  RegisterVirtualMethod(@TCOLLECTION.BEGINUPDATE, 'BeginUpdate');
  RegisterMethod(@TCOLLECTION.CLEAR, 'Clear');
  RegisterVirtualMethod(@TCOLLECTION.ENDUPDATE, 'EndUpdate');
  RegisterPropertyHelper(@TCOLLECTIONCOUNT_R,nil,'Count');
  RegisterPropertyHelper(@TCOLLECTIONITEMS_R,@TCOLLECTIONITEMS_W,'Items');
  end;
end;

procedure RIRegisterTCOLLECTIONITEM(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TCOLLECTIONITEM) do
  begin
  RegisterVirtualConstructor(@TCOLLECTIONITEM.CREATE, 'Create');
  RegisterPropertyHelper(@TCOLLECTIONITEMCOLLECTION_R,@TCOLLECTIONITEMCOLLECTION_W,'Collection');
  RegisterPropertyHelper(@TCOLLECTIONITEMINDEX_R,@TCOLLECTIONITEMINDEX_W,'Index');
  end;
end;

procedure RIRegister_Classes(Cl: TPSRuntimeClassImporter; Streams: Boolean);
begin
  if Streams then
    RIRegisterTSTREAM(Cl);
  RIRegisterTStrings(cl, Streams);
  RIRegisterTStringList(cl);
  if Streams then
  begin
    RIRegisterTFILESTREAM(Cl);
    RIRegisterTSTRINGSTREAM(Cl);
    RIRegisterTMEMORYSTREAM(Cl);
  end;
  RIRegisterTCOLLECTIONITEM(Cl);
  RIRegisterTCOLLECTION(Cl);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.
