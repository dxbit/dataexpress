unit SizeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, strconsts;

type

  { TSizeFm }

  TSizeFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    TopEdt: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PointLbl: TLabel;
    WidthEdt: TSpinEdit;
    HeightEdt: TSpinEdit;
    LeftEdt: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private

  public
    function ShowForm(C: TControl): Integer;
  end;

var
  SizeFm: TSizeFm;

function ShowSizeForm(C: TControl): Integer; overload;

implementation

uses
  apputils, dxctrls, formdesigner, designerframe;

function ShowSizeForm(C: TControl): Integer;
begin
  if SizeFm = nil then
    SizeFm := TSizeFm.Create(Application);
  Result := SizeFm.ShowForm(C);
end;

{$R *.lfm}

{ TSizeFm }

procedure TSizeFm.FormCreate(Sender: TObject);
begin
  Caption := rsPositionSize;
  Label3.Caption := 'X';
  Label4.Caption := 'Y';
  Label1.Caption := rsWidth;
  Label2.Caption := rsHeight;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
end;

function TSizeFm.ShowForm(C: TControl): Integer;
begin
  if C is TdxForm then
  begin
    LeftEdt.Enabled := False;
    TopEdt.Enabled := False;
    WidthEdt.MinValue := ScaleToScreen(20);
    HeightEdt.MinValue := ScaleToScreen(20);
  end
  else
  begin
    LeftEdt.Enabled := True;
    TopEdt.Enabled := True;
    WidthEdt.MinValue := ScaleToScreen(1);
    HeightEdt.MinValue := ScaleToScreen(1);
  end;
  LeftEdt.Value := C.Left;
  TopEdt.Value := C.Top;
  WidthEdt.Value := C.Width;
  HeightEdt.Value := C.Height;
  Result := ShowModal;
  if Result = mrOk then
  begin
    if C is TdxForm then
    begin
      {$ifdef linux}
      DesignFr.DesignBox.DesignFm.Width := WidthEdt.Value;
      DesignFr.DesignBox.DesignFm.Height := HeightEdt.Value;
      {$endif}
    end
    else if (C.Left <> LeftEdt.Value) or (C.Top <> TopEdt.Value) or
    (C.Width <> WidthEdt.Value) or (C.Height <> HeightEdt.Value) then
      FormDesign.DoChangingComponents;

    C.Left := LeftEdt.Value;
    C.Top := TopEdt.Value;
    C.Width := WidthEdt.Value;
    C.Height := HeightEdt.Value;
  end;
end;

end.

