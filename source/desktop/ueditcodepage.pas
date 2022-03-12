unit uEditCodePage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  PasExt, PUIExt, StdCtrls, Buttons, ActnList, XMLPropStorage,
  uAppNLS, uLog, Icons, FDKit;

type

  { TfEditCodePage }

  TfEditCodePage = class(TForm)
    bbCancel: TBitBtn;
    bbOK: TBitBtn;
    ilButtons: TImageList;
    iFontState: TImage;
    ilDOSFont: TImageList;
    lbFontState: TLabel;
    lvEditCP: TListView;
    pBtnSeperator: TPanel;
    pButtons: TPanel;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCodepage: string;
    FRepository: TFDNLS;
    procedure SetCodepage(AValue: string);
    procedure SetRepository(AValue: TFDNLS);

  public
    property Codepage : string read FCodepage write SetCodepage;
    property Repository : TFDNLS read FRepository write SetRepository;
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
  xProperties.FileName := AppCfgFile;
  xProperties.RootNodePath := DisplayNamePath(Self);
  ilButtons.AddLazarusResource(IconUI[7]);
  ilButtons.AddLazarusResource(IconUI[8]);
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  with lvEditCP.Columns.Add do Caption := lvh_EditCpDOS;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpValue;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCPUnicode;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpUTF8;
  with lvEditCP.Columns.Add do Caption :=lvh_EditCpHTML;
  Clear;
end;

procedure TfEditCodePage.FormHide(Sender: TObject);
var
  I : integer;
begin
  for I := 0 to lvEditCP.Columns.Count - 1 do begin
    xProperties.WriteInteger(lvEditCP.Name + '/COLUMN_' + IntToStr(I),
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
        xProperties.ReadInteger(lvEditCP.Name + '/COLUMN_' + IntToStr(I), 50);
  end;
end;

procedure TfEditCodePage.SetCodepage(AValue: string);
var
  I : integer;
begin
  if not Assigned(Repository) then exit;
  if (FCodepage=AValue) and (AValue <> NullCodepage) then Exit;
  try
    FCodepage:=ZeroPad(StrToInt(AValue),3);
  except
    FCodePage := NullCodepage;
  end;
  Caption := Format(dlg_EditCodePage, [FCodePage]);
  Repository.Fonts.Reload;
  I := Repository.Fonts.IndexOfFile(FCodePage + FontFileExt);
  if I <> -1 then begin
     iFontState.Picture.LoadFromLazarusResource(IconUI[14]);
     lbFontState.Caption:=Format(lbl_UsingCodepageFont,
       [FCodePage + FontFileExt,FCodePage]);
     lbFontState.Font.Color:=clDefault;
  end else begin
    iFontState.Picture.LoadFromLazarusResource(IconUI[15]);
    lbFontState.Font.Color:=clErrorText;
    I := Repository.Fonts.IndexOfFile(FCodePage + FontFileExt);
    if I <> -1 then
      lbFontState.Caption:=Format(lbl_UsingCodepageFont,
      [DefaultFont,FCodePage])
    else
      lbFontState.Caption:=lbl_NoCodepageFonts;
  end;
  if I <> -1 then
    Repository.Fonts.ToImageList(I, ilDOSFont)
  else
    ilDOSFont.Clear;
  Populate;
end;

procedure TfEditCodePage.SetRepository(AValue: TFDNLS);
begin
  if FRepository=AValue then Exit;
  FRepository:=AValue;
end;

procedure TfEditCodePage.Clear;
begin
  SetCodepage(NullCodepage);
end;

procedure TfEditCodePage.Populate;
var
  CP : TCodePageData;
  I, X : integer;
  LI : TListItem;
begin
  lvEditCP.BeginUpdate;
  lvEditCP.Clear;
  I := Repository.CodePages.IndexOfIdentifier(FCodePage);
  if I = -1 then
    CP := nil
  else
    CP := Repository.CodePages.Data[I];
  for I := 0 to 255 do begin
      LI := lvEditCP.Items.Add;
      LI.Caption:='';
      LI.ImageIndex:=I;
      X := LI.SubItems.Add(IntToStr(I));
      LI.SubItemImages[0] := I;
      LI.SubItems.Add(Char(I));
      LI.SubItems.Add(Char(I));
      LI.SubItems.Add(Char(I));
  end;
  lvEditCP.EndUpdate;
end;

end.

