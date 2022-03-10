unit uEditCodePage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  PasExt, PUIExt, StdCtrls, Buttons, ActnList, XMLPropStorage,
  uAppNLS, uLog, Icons;

type

  { TfEditCodePage }

  TfEditCodePage = class(TForm)
    bbCancel: TBitBtn;
    bbOK: TBitBtn;
    ilButtons: TImageList;
    pBtnSeperator: TPanel;
    pButtons: TPanel;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

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
  Caption := dlg_PickFlag;
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  ilButtons.AddLazarusResource(IconUI[7]);
  ilButtons.AddLazarusResource(IconUI[8]);
end;

procedure TfEditCodePage.FormShow(Sender: TObject);
begin
  Position:=poDesigned;
end;

end.

