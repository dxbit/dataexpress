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

    Based on the original uPSR_ExtCtrls.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

unit uPSR_MyExtctrls;

interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
//procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
{procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);

procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter); }

implementation

uses
  ExtCtrls, Graphics;

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSHAPE) do
  begin
  end;
end;

//procedure TIMAGECANVAS_R(Self: TIMAGE; var T: TCANVAS); begin T := Self.CANVAS; end;

{procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIMAGE) do
  begin
    RegisterPropertyHelper(@TIMAGECANVAS_R, nil, 'Canvas');
  end;
end;  }

procedure TPAINTBOXCANVAS_R(Self: TPAINTBOX; var T: TCanvas); begin T := Self.CANVAS; end;

procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPAINTBOX) do
  begin
    RegisterPropertyHelper(@TPAINTBOXCANVAS_R, nil, 'Canvas');
  end;
end;

procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBEVEL);
end;

procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TTIMER);
end;

procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMPANEL);
end;

procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPANEL);
end;

{procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPAGE);
end;

procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TNOTEBOOK);
end;

procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMRADIOGROUP);
end;

procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TRADIOGROUP);
end;     }

procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSHAPE(Cl);
  //RIRegisterTIMAGE(Cl);
  RIRegisterTPAINTBOX(Cl);
  RIRegisterTBEVEL(Cl);
  RIRegisterTTIMER(Cl);
  RIRegisterTCUSTOMPANEL(Cl);
  RIRegisterTPANEL(Cl);
  //RIRegisterTPAGE(Cl);
	//RIRegisterTNOTEBOOK(Cl);
  //RIRegisterTCUSTOMRADIOGROUP(Cl);
  //RIRegisterTRADIOGROUP(Cl);
end;

end.


