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

{ TfEditCodePage }

procedure TfEditCodePage.FormCreate(Sender: TObject);
begin
  xProperties.FileName := AppCfgFile;
  xProperties.RootNodePath := FormNodePath(Self);
  FCodePage := '000';
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
    FCodePage := '000';
  end;
  Caption := Format(dlg_EditCodePage, [FCodePage]);
end;

procedure TfEditCodePage.SetRepository(AValue: TFDNLS);
begin
  if FRepository=AValue then Exit;
  FRepository:=AValue;
end;

end.

