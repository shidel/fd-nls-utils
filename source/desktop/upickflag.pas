unit uPickFlag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  PasExt, PUIExt, Buttons, ActnList, uAppNLS, uAppCfg, uLog, Icons;

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
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetIndex: integer;
    procedure SetIndex(AValue: integer);

  public
    property Index : integer read GetIndex write SetIndex;
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
  GetPropertyState(Self);
  Caption := dlg_PickFlag;
  bbOK.Caption:=btn_OK;
  bbCancel.Caption:=btn_Cancel;
  ilButtons.AddLazarusResource(IconUI[icon_Done]);
  ilButtons.AddLazarusResource(IconUI[icon_Close]);
  lvFlags.BeginUpdate;
  for I := Low(IconFlags) to High(IconFlags) do begin
      ilFlags.AddLazarusResource(IconFlags[I]);
      with lvFlags.Items.Add do begin
          ImageIndex := I;
          Caption := FlagNames[I];
      end;
  end;
  Log(Self, IntToStr(lvFlags.Items.Count) + ' flag images');
  lvFlags.EndUpdate;
end;

procedure TfPickFlag.FormHide(Sender: TObject);
begin
  SetPropertyState(Self);
end;

procedure TfPickFlag.FormShow(Sender: TObject);
begin
  if Assigned(lvFLags.Selected) then begin
    lvFlags.Selected.MakeVisible(false);
  end;
  Position:=poDesigned;
end;

function TfPickFlag.GetIndex: integer;
begin
  if Assigned(lvFlags.Selected) then
    Result := lvFlags.Selected.Index
  else
    Result := -1;
end;

procedure TfPickFlag.SetIndex(AValue: integer);
begin
  if GetIndex=AValue then Exit;
  if (AValue < 0) or (AValue >= lvFlags.Items.Count) then begin
    lvFlags.Selected := nil;
  end else begin
    lvFlags.Selected := lvFlags.Items.Item[AValue];
  end;
end;

end.

