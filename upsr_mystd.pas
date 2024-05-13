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

    Based on the original uPSR_Std.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

unit uPSR_MyStd;
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTObject(CL: TPSRuntimeClassImporter);
procedure RIRegisterTPersistent(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTComponent(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Std(Cl: TPSRuntimeClassImporter);

implementation

uses
  Classes, dxctrls, scriptfuncs;

procedure TObjectFree(Self: TObject);
begin
  if Self is TdxForm then TObjectDestroyForm(TdxForm(Self))
  else Self.Free;
end;

procedure TObjectClassName_R(Self: TObject; var T: String); begin T := Self.ClassName; end;

procedure RIRegisterTObject(CL: TPSRuntimeClassImporter); 
begin
  with cl.Add(TObject) do
  begin
    RegisterConstructor(@TObject.Create, 'Create');
    RegisterMethod(@TObjectFree, 'Free');
    RegisterPropertyHelper(@TObjectClassName_R, nil, 'ClassName');
  end;
end;

procedure RIRegisterTPersistent(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPersistent) do
  begin
    RegisterVirtualMethod(@TPersistent.Assign, 'Assign');
  end;
end;

procedure TComponentOwnerR(Self: TComponent; var T: TComponent); begin T := Self.Owner; end;


procedure TCOMPONENTCOMPONENTS_R(Self: TCOMPONENT; var T: TCOMPONENT; t1: INTEGER); begin T := Self.COMPONENTS[t1]; end;
procedure TCOMPONENTCOMPONENTCOUNT_R(Self: TCOMPONENT; var T: INTEGER); begin t := Self.COMPONENTCOUNT; end;
procedure TCOMPONENTCOMPONENTINDEX_R(Self: TCOMPONENT; var T: INTEGER); begin t := Self.COMPONENTINDEX; end;
procedure TCOMPONENTCOMPONENTINDEX_W(Self: TCOMPONENT; T: INTEGER); begin Self.COMPONENTINDEX := t; end;


procedure RIRegisterTComponent(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TComponent) do
  begin
    RegisterMethod(@TComponent.FindComponent, 'FindComponent');
    RegisterVirtualConstructor(@TComponent.Create, 'Create');
    RegisterPropertyHelper(@TComponentOwnerR, nil, 'Owner');

    RegisterMethod(@TCOMPONENT.DESTROYCOMPONENTS, 'DestroyComponents');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTS_R, nil, 'Components');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTCOUNT_R, nil, 'ComponentCount');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTINDEX_R, @TCOMPONENTCOMPONENTINDEX_W, 'ComponentIndex');
  end;
end;







procedure RIRegister_Std(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTObject(CL);
  RIRegisterTPersistent(Cl);
  RIRegisterTComponent(Cl);
end;
// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.





