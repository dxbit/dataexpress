unit HierarchyForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin, strconsts, dxctrls;

type

  { THierarchyFm }

  THierarchyFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label2: TLabel;
    ParentField: TComboBox;
    Label1: TLabel;
    LevelCount: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillFields;
  public
    { public declarations }
    procedure ShowForm(aFm: TdxForm);
  end;

var
  HierarchyFm: THierarchyFm;

procedure ShowHierarchyForm(aFm: TdxForm);

implementation

uses
  helpmanager, mytypes, apputils;

procedure ShowHierarchyForm(aFm: TdxForm);
begin
  if HierarchyFm = nil then
  	HierarchyFm := THierarchyFm.Create(Application);
  HierarchyFm.ShowForm(aFm);
end;

{$R *.lfm}

{ THierarchyFm }

procedure THierarchyFm.FormCreate(Sender: TObject);
begin
  Caption := rsHierarchy;
  Label1.Caption := rsParentField;
  Label2.Caption := rsLevelCount;
  //Optimize.Caption := rsOptimizeSelection;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure THierarchyFm.FormShow(Sender: TObject);
begin
  ParentField.SetFocus;
end;

procedure THierarchyFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('hierarchy');
end;

procedure THierarchyFm.FillFields;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if (C is TdxLookupComboBox) and IsCorrectParentField(FFm, C) then
	  	SL.AddObject(GetFieldName(C), C);
  end;
  SL.Sort;
  ParentField.Items := SL;
  SL.Free;
end;

procedure THierarchyFm.ShowForm(aFm: TdxForm);
var
  C: TComponent;
begin
  FFm := aFm;
  FillFields;
  C := FindById(FFm, FFm.ParentField);
  with ParentField do
    ItemIndex := Items.IndexOfObject(C);
  LevelCount.Value := FFm.LevelCount;
  {if C <> nil then
    Optimize.Checked := GetRequired(C)
  else
    Optimize.Checked := False;  }

  if ShowModal = mrOk then
  begin
    with ParentField do
      if ItemIndex > 0 then
      begin
        C := TComponent(Items.Objects[ItemIndex]);
        FFm.ParentField := GetId(C);
        //SetRequired(C, Optimize.Checked);
      end
      else FFm.ParentField := 0;
    FFm.LevelCount := LevelCount.Value;
  end;
end;

end.

