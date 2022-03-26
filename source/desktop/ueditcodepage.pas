unit uEditCodePage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  PasExt, PUIExt, StdCtrls, Buttons, ActnList,
  uAppNLS, uAppCfg, uLog, Icons, FDKit;

type

  { TfEditCodePage }

  TfEditCodePage = class(TForm)
    bbCancel: TBitBtn;
    bbOK: TBitBtn;
    ilButtons: TImageList;
    iFontState: TImage;
    ilDOSFont: TImageList;
    iDOS: TImage;
    lblAscii: TLabel;
    leUTF8: TLabeledEdit;
    leHTML: TLabeledEdit;
    lbFontState: TLabel;
    lvEditCP: TListView;
    pEditValues: TPanel;
    pBtnSeperator: TPanel;
    pButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure leHTMLEditingDone(Sender: TObject);
    procedure leUTF8EditingDone(Sender: TObject);
    procedure lvEditCPChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvEditCPClick(Sender: TObject);
  private
    EditItem, DOSFont : integer;
    FCodepage: string;
    procedure SetCodepage(AValue: string);
    procedure SelectEdit(Index : integer);
    procedure SaveChanges;
  public
    property Codepage : string read FCodepage write SetCodepage;
    procedure Clear;
    procedure Populate;
  end;

var
  fEditCodePage: TfEditCodePage;

implementation

{$R *.lfm}

const
  NullCodepage = '000';
  FontFileExt  = '.FNT';
  DefaultFont  = '437' + FontFileExt;

{ TfEditCodePage }

procedure TfEditCodePage.FormCreate(Sender: TObject);
begin
  EditItem := -1;
  DOSFont := -1;
  GetPropertyState(Self);
  ilButtons.AddLazarusResource(IconUI[7]);
  ilButtons.AddLazarusResource(IconUI[8]);
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  with lvEditCP.Columns.Add do Caption := lvh_EditCpDOS;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpValue;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpUTF8;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpHTML;
  lblAscii.Caption :=Format(lbl_EditCpValue, ['']);
  leUTF8.EditLabel.Caption :=lbl_EditCpUTF8;
  leHTML.EditLabel.Caption :=lbl_EditCpHTML;
  Clear;
end;

procedure TfEditCodePage.FormHide(Sender: TObject);
var
  I : integer;
begin
  SetPropertyState(Self);
  if ModalResult = mrOK then begin
    Log(Self,'Update codepage ' + FCodePage);
    SaveChanges;
  end else begin
    Log(Self, 'Discard changes to codepage ' + FCodepage);
  end;
  for I := 0 to lvEditCP.Columns.Count - 1 do begin
    SetPropertyState(lvEditCP, 'COLUMN_' + IntToStr(I),
      lvEditCP.Columns.Items[I].Width);
  end;
end;

procedure TfEditCodePage.FormShow(Sender: TObject);
var
  I : integer;
begin
  Position:=poDesigned;
  for I := 0 to lvEditCP.Columns.Count - 1 do begin
      lvEditCP.Columns.Items[I].Width:=
        GetPropertyState(lvEditCP, 'COLUMN_' + IntToStr(I), 75);
  end;
end;

procedure TfEditCodePage.leHTMLEditingDone(Sender: TObject);
begin
  if Trim(leHTML.Caption) <> '' then
    leHTML.Caption := Trim(leHTML.Caption);
  if EditItem <> -1 then
    lvEditCP.Items[EditItem].SubItems[2]:=leHTML.Caption
  else
    leHTML.Caption :='';
end;

procedure TfEditCodePage.leUTF8EditingDone(Sender: TObject);
var
  Both : boolean;
begin
  Both := EditItem <> -1;
  if Both then
    with lvEditCP.Items[EditItem] do Both := SubItems[1] = SubItems[2];

  if Trim(leUTF8.Caption) <> '' then
    leUTF8.Caption := Trim(leUTF8.Caption);
  if EditItem <> -1 then
    lvEditCP.Items[EditItem].SubItems[1]:=leUTF8.Caption
  else
    leUTF8.Caption :='';
  if Both then leHTML.Caption := leUTF8.Caption;
end;

procedure TfEditCodePage.lvEditCPChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  {$IF defined(linux)}
  {$ELSE}
    SelectEdit(Item.Index);
  {$ENDIF}
end;

procedure TfEditCodePage.lvEditCPClick(Sender: TObject);
begin
{$IF defined(linux)}
  if Assigned(lvEditCP.Selected) then
    SelectEdit(lvEditCP.Selected.Index);
{$ENDIF}
end;

procedure TfEditCodePage.SetCodepage(AValue: string);
var
  I : integer;
begin
  DOSFont := -1;
  EditItem := -1;
  if not Assigned(FDNLS) then exit;
  if (FCodepage=AValue) and (AValue <> NullCodepage) then Exit;
  try
    FCodepage:=ZeroPad(StrToInt(AValue),3);
  except
    FCodePage := NullCodepage;
  end;
  Caption := Format(dlg_EditCodePage, [FCodePage]);
  FDNLS.Fonts.Reload;
  I := FDNLS.Fonts.IndexOfFile(FCodePage + FontFileExt);
  if I <> -1 then begin
     iFontState.Picture.LoadFromLazarusResource(IconUI[14]);
     lbFontState.Caption:=Format(lbl_UsingCodepageFont,
       [FCodePage + FontFileExt,FCodePage]);
     lbFontState.Font.Color:=clDefault;
  end else begin
    iFontState.Picture.LoadFromLazarusResource(IconUI[15]);
    lbFontState.Font.Color:=clErrorText;
    I := FDNLS.Fonts.IndexOfFile(DefaultFont);
    if I <> -1 then
      lbFontState.Caption:=Format(lbl_UsingCodepageFont,
      [DefaultFont,FCodePage])
    else
      lbFontState.Caption:=lbl_NoCodepageFonts;
  end;
  DOSFont := I;
  ilDOSFont.Clear;
  if I <> -1 then
    FDNLS.Fonts.ToImageList(I, ilDOSFont);
  Populate;
end;

procedure TfEditCodePage.SelectEdit(Index: integer);
begin
  EditItem := Index;
  pEditValues.Enabled:=Index <> -1;
  leUTF8.Enabled:=Index <> -1;
  leHTML.Enabled:=Index <> -1;
  if Index = -1 then begin
    iDOS.Picture.Clear;
    lblAscii.Caption :=Format(lbl_EditCpValue, ['']);
    leUTF8.Caption:='';
    leHTML.Caption:='';
  end else begin
    leUTF8.Caption:=lvEditCP.Items[EditItem].SubItems[1];
    leHTML.Caption:=lvEditCP.Items[EditItem].SubItems[2];
    if DOSFont = -1 then
      iDOS.Picture.Clear
    else
      FDNLS.Fonts.ToImage(DOSFont, EditItem, iDOS, clBlack, clForm);
    lblAscii.Caption :=Format(lbl_EditCpValue, [IntToStr(EditItem)]);
  end;
end;

procedure TfEditCodePage.SaveChanges;
var
  I, C : integer;
begin
  C := FDNLS.CodePages.IndexOfIdentifier(FCodePage);
  if C = -1 then begin
    C := FDNLS.CodePages.Add;
    FDNLS.CodePages.Filename[C] := FCodePage + '.xml';
    FDNLS.CodePages.SetValue(C, 'IDENTIFIER', FCodePage);
  end;
  for I := 0 to 255 do begin
    FDNLS.Codepages.SetValue(C, 'ASCII_' + IntToStr(I) + '/UTF8', StrToInts(
      lvEditCP.Items[I].SubItems[1]));
    FDNLS.Codepages.SetValue(C, 'ASCII_' + IntToStr(I) + '/HTML', StrToInts(
      lvEditCP.Items[I].SubItems[2]));
  end;
  FDNLS.CodePages.Reload;
end;

procedure TfEditCodePage.Clear;
begin
  SetCodepage(NullCodepage);
end;

procedure TfEditCodePage.Populate;
var
  CP : TCodePageData;
  I : integer;
  LI : TListItem;
begin
  lvEditCP.BeginUpdate;
  lvEditCP.Clear;
  try
    I := FDNLS.CodePages.IndexOfIdentifier(FCodePage);
    if I = -1 then
      I := FDNLS.CodePages.IndexOfIdentifier('437');
    if I = -1 then
      CP := nil
    else
      CP := FDNLS.CodePages.Data[I];
    for I := 0 to 255 do begin
        LI := lvEditCP.Items.Add;
        LI.Caption:='';
        LI.ImageIndex:=I;
        LI.SubItems.Add(IntToStr(I));
        if Assigned(CP) then begin
          LI.SubItems.Add(CP.UTF8[I]);
          LI.SubItems.Add(CP.HTML[I]);
        end else begin
          LI.SubItems.Add('');
          LI.SubItems.Add('');
        end;
    end;
  finally
  end;
  lvEditCP.EndUpdate;
  SelectEdit(-1);
end;

end.

