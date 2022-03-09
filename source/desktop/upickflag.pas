unit uPickFlag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, uAppNLS, uLog, Icons;

type

  { TfPickFlag }

  TfPickFlag = class(TForm)
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    ilButtons: TImageList;
    ilFlags: TImageList;
    lvFlags: TListView;
    pButtons: TPanel;
    pBtnSeperator: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  fPickFlag: TfPickFlag;

implementation

{$R *.lfm}

{ TfPickFlag }

procedure TfPickFlag.FormCreate(Sender: TObject);
var
  I : integer;
begin
  Caption := dlg_PickFlag;
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  ilButtons.AddLazarusResource(IconUI[7]);
  ilButtons.AddLazarusResource(IconUI[8]);
  lvFlags.BeginUpdate;
  for I := Low(IconFlags) to High(IconFlags) do begin
      ilFlags.AddLazarusResource(IconFlags[I]);
      with lvFlags.Items.Add do begin
          ImageIndex := I;
          Caption := FlagNames[I];
      end;
  end;
  Log(Self, IntToStr(lvFlags.Items.Count) + ' flag images');
  lvFlags.ViewStyle:=vsIcon;
  lvFlags.EndUpdate;
end;

end.

