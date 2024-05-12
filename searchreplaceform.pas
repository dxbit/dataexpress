unit SearchReplaceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, strconsts, Buttons;

type

  { TSearchReplaceFm }

  TSearchReplaceFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Options: TCheckGroup;
    SearchText: TComboBox;
    ReplaceText: TComboBox;
    Label1: TLabel;
    Start: TRadioGroup;
    Where: TRadioGroup;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(const aText: String; IsReplace, InSelection: Boolean): Integer;
    procedure SetFind;
    procedure SetReplace;
  end;

var
  SearchReplaceFm: TSearchReplaceFm;

function ShowSearchReplaceForm(const aText: String; IsReplace, InSelection: Boolean): Integer;

implementation

function ShowSearchReplaceForm(const aText: String; IsReplace,
  InSelection: Boolean): Integer;
begin
  if SearchReplaceFm = nil then
  	SearchReplaceFm := TSearchReplaceFm.Create(Application);
  Result := SearchReplaceFm.ShowForm(aText, IsReplace, InSelection);
end;

{$R *.lfm}

{ TSearchReplaceFm }

procedure TSearchReplaceFm.CheckBox1Change(Sender: TObject);
begin
  ReplaceText.Enabled := CheckBox1.Checked;
  if CheckBox1.Checked then
  begin
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel, pbClose];
    ButtonPanel1.OkButton.Caption := rsReplace;
    //ButtonPanel1.CloseButton.Caption := rsReplaceAll;
  end
  else
  begin
    ButtonPanel1.OkButton.Caption := rsFind;
    ButtonPanel1.ShowButtons:=[pbOk, pbCancel];
  end;
end;

procedure TSearchReplaceFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  S: String;
begin
  if ModalResult in [mrOk, mrYesToAll] then
  begin
    with SearchText do
    begin
      S := Trim(Text);
      if (S <> '') and (Items.IndexOf(S) < 0) then
        Items.Add(S);
    end;
    with ReplaceText do
    begin
      S := Trim(Text);
      if (S <> '') and (Items.IndexOf(S) < 0) then
        Items.Add(S);
    end;
  end;
end;

procedure TSearchReplaceFm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult = mrYesToAll then
  begin
    if MessageDlg(rsWarning, rsReplaceAllMsg, mtWarning,
      [mbYes, mbNo], 0) = mrNo then CanClose := False;
  end;
end;

procedure TSearchReplaceFm.FormCreate(Sender: TObject);
begin
  Caption := rsFindReplace;
  Label1.Caption := rsSearchedText;
  CheckBox1.Caption := rsReplace;
  Options.Caption := rsSearchOptions;
  Options.Items[0] := rsWholeWords;
  Options.Items[1] := rsCaseSensitive;
  Start.Caption:=rsStart;
  Start.Items[0] := rsFromBegin;
  Start.Items[1] := rsFromCursor;
  Where.Caption := rsSearchArea;
  Where.Items[0] := rsEntireScope;
  Where.Items[1] := rsInSelected;
  ButtonPanel1.OkButton.Caption := rsFind;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.CloseButton.Kind := bkYesToAll;
  ButtonPanel1.CloseButton.Caption := rsReplaceAll;
end;

procedure TSearchReplaceFm.FormShow(Sender: TObject);
begin
  SearchText.SetFocus;
end;

function TSearchReplaceFm.ShowForm(const aText: String; IsReplace,
  InSelection: Boolean): Integer;
begin
  SearchText.Text := aText;
  CheckBox1.Checked := IsReplace;
  if InSelection then
  	Where.ItemIndex := 1
  else
  	Where.ItemIndex := 0;
  Result := ShowModal;
end;

procedure TSearchReplaceFm.SetFind;
begin
  CheckBox1.Checked := False;
end;

procedure TSearchReplaceFm.SetReplace;
begin
  CheckBox1.Checked := True;
end;

end.

