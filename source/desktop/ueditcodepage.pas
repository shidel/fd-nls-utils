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
    lbFontState: TLabel;
    lvEditCP: TListView;
    pBtnSeperator: TPanel;
    pButtons: TPanel;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCodepage: string;
    FRepository: TFDNLS;
    procedure SetCodepage(AValue: string);
    procedure SetRepository(AValue: TFDNLS);

  public
    property Codepage : string read FCodepage write SetCodepage;
    property Repository : TFDNLS read FRepository write SetRepository;
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
  xProperties.RootNodePath := FormNodePath(Self);
  FCodePage := NullCodepage;
  Caption := Format(dlg_EditCodePage, [FCodePage]);
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  ilButtons.AddLazarusResource(IconUI[7]);
  ilButtons.AddLazarusResource(IconUI[8]);
end;

procedure TfEditCodePage.FormShow(Sender: TObject);
begin
  Position:=poDesigned;
end;

procedure TfEditCodePage.SetCodepage(AValue: string);
begin
  if not Assigned(Repository) then exit;
  if FCodepage=AValue then Exit;
  try
    FCodepage:=ZeroPad(StrToInt(AValue),3);
  except
    FCodePage := NullCodepage;
  end;
  Caption := Format(dlg_EditCodePage, [FCodePage]) + ':' + Repository.Fonts.Filename[0];
  Repository.Fonts.Reload;
  if Repository.Fonts.IndexOfFile(FCodePage + FontFileExt) <> -1 then begin
     iFontState.Picture.LoadFromLazarusResource(IconUI[14]);
     lbFontState.Caption:=Format(lbl_UsingCodepageFont,
       [FCodePage + FontFileExt,FCodePage]);
     lbFontState.Font.Color:=clDefault;
  end else begin
    iFontState.Picture.LoadFromLazarusResource(IconUI[15]);
    lbFontState.Font.Color:=clErrorText;
    if Repository.Fonts.IndexOfFile(DefaultFont) <> -1 then
      lbFontState.Caption:=Format(lbl_UsingCodepageFont,
      [DefaultFont,FCodePage])
    else
      lbFontState.Caption:=lbl_NoCodepageFonts;
  end;
end;

procedure TfEditCodePage.SetRepository(AValue: TFDNLS);
begin
  if FRepository=AValue then Exit;
  FRepository:=AValue;
end;

end.

