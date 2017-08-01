unit mysqldblib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb;

Type

  { TMySQLDBLibraryLoader }

  TMySQLDBLibraryLoader = Class(TComponent)
  private
    FCType: String;
    FEnabled: Boolean;
    FLibraryName: String;
    procedure CheckDisabled;
    procedure SetCType(AValue: String);
    procedure SetEnabled(AValue: Boolean);
    procedure SetLibraryName(AValue: String);
  Protected
    Function GetConnectionDef : TConnectionDef;
    Procedure Loaded; override;
    Procedure SetDefaultLibraryName; virtual;
  Public
    Procedure LoadLibrary;
    Procedure UnloadLibrary;
  Published
    Property Enabled : Boolean Read FEnabled Write SetEnabled;
    Property ConnectionType : String Read FCType Write SetCType;
    Property LibraryName : String Read FLibraryName Write SetLibraryName;
  end;

implementation

Resourcestring
   SErrConnnected = 'This operation is not allowed while the datatabase is loaded';
   SErrInvalidConnectionType = 'Invalid connection type : "%s"';
{ TMySQLDBLibraryLoader }

procedure TMySQLDBLibraryLoader.CheckDisabled;

begin
  If Enabled then
    DatabaseError(SErrConnnected,Self);
end;

procedure TMySQLDBLibraryLoader.SetCType(AValue: String);
begin
  if FCType=AValue then Exit;
  CheckDisabled;
  FCType:=AValue;
  if (FCType<>'') then
    SetDefaultLibraryName;
end;

procedure TMySQLDBLibraryLoader.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  if (csLoading in ComponentState) then
    FEnabled:=AValue
  else
    If AValue then
      LoadLibrary
    else
      UnloadLibrary;
end;

procedure TMySQLDBLibraryLoader.SetLibraryName(AValue: String);
begin
  if FLibraryName=AValue then Exit;
  CheckDisabled;
  FLibraryName:=AValue;
end;

function TMySQLDBLibraryLoader.GetConnectionDef: TConnectionDef;
begin
  Result:=sqldb.GetConnectionDef(ConnectionType);
  if (Result=Nil) then
    DatabaseErrorFmt(SErrInvalidConnectionType,[FCType],Self)
end;

procedure TMySQLDBLibraryLoader.Loaded;
begin
  inherited;
  If FEnabled and (FCType<>'') and (FLibraryName<>'') then
    LoadLibrary;
end;

procedure TMySQLDBLibraryLoader.SetDefaultLibraryName;
Var
  D : TConnectionDef;
begin
  D:=GetConnectionDef;
  LibraryName:=D.DefaultLibraryName;
end;

procedure TMySQLDBLibraryLoader.LoadLibrary;

Var
  D : TConnectionDef;
  l : TLibraryLoadFunction;

begin
  D:=GetConnectionDef;
  L:=D.LoadFunction();
  if (L<>Nil) then
    L(LibraryName);
  FEnabled:=True;
end;

procedure TMySQLDBLibraryLoader.UnloadLibrary;

Var
  D : TConnectionDef;
  l : TLibraryUnLoadFunction;

begin
  D:=GetConnectionDef;
  L:=D.UnLoadFunction;
  if L<>Nil then
    L;
  FEnabled:=False;
end;

end.

