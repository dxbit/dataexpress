{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit ShapeEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TShapeTypeEx = (steNone, steVertLine, steHorzLine, steBDiagonal, steFDiagonal,
    steCross, steDiagCross);

  TShapeEx = class(TShape)
  private
    FShapeEx: TShapeTypeEx;
    procedure SetShapeEx(AValue: TShapeTypeEx);
  protected

  public
    procedure Paint; override;
  published
    property ShapeEx: TShapeTypeEx read FShapeEx write SetShapeEx;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I shapeex_icon.lrs}
  RegisterComponents('dxComponents',[TShapeEx]);
end;

procedure TShapeEx.SetShapeEx(AValue: TShapeTypeEx);
begin
  if FShapeEx=AValue then Exit;
  FShapeEx:=AValue;
  Invalidate;
end;

procedure TShapeEx.Paint;
var
  x, y: Integer;
  C: TCanvas;
begin
  if FShapeEx = steNone then
    inherited Paint
  else
  begin
    C := Canvas;
    C.Brush := Self.Brush;
    C.Pen := Self.Pen;
    case FShapeEx of
      steVertLine:
        begin
          x := Width div 2;
          C.Line(x, 0, x, Height);
        end;
      steHorzLine:
        begin
          y := Height div 2;
          C.Line(0, y, Width, y);
        end;
      steBDiagonal:
        begin
          C.Line(0, Height, Width, 0);
        end;
      steFDiagonal:
        begin
          C.Line(0, 0, Width, Height);
        end;
      steCross:
        begin
          x := Width div 2;
          C.Line(x, 0, x, Height);
          y := Height div 2;
          C.Line(0, y, Width, y);
        end;
      steDiagCross:
        begin
          C.Line(0, Height, Width, 0);
          C.Line(0, 0, Width, Height);
        end;
    end;

    if Assigned(OnPaint) then OnPaint(Self);
  end;
end;

end.
