unit uPkgPreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, ExtCtrls, PasExt, FDKit,
  DosView, uPkgDetails, uAppNLS;

type

  { TframePkgPreview }

  TframePkgPreview = class(TFrame)
    iPreview: TImage;
    pSpacer: TPanel;
    sbPreview: TScrollBox;
    tRender: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure tRenderTimer(Sender: TObject);
  private
    FDosView : TDosScreen;
    FDetails : TframePkgDetails;
    FPkgGroup,
    FPkgId,
    FPkgDesc,
    FPkgTitle,
    FPkgVer,
    FPkgAuthor,
    FPkgLicense,
    FPkgSummary,
    FPkgPlatforms,
    FPkgKeywords : String;
    procedure SetFieldValue(ID : String; var Field : String; DefaultValue : String = '');
    procedure Render;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Preview(Details : TframePkgDetails); virtual;
  end;

implementation

{$R *.lfm}

{ TframePkgPreview }

procedure TframePkgPreview.FrameResize(Sender: TObject);
begin
  tRender.Interval:=500;
  tRender.Enabled:=True;
end;

procedure TframePkgPreview.tRenderTimer(Sender: TObject);
begin
  Render;
end;

procedure TframePkgPreview.SetFieldValue(ID: String; var Field: String; DefaultValue : String = '');
var
  I : integer;
begin
  Field := DefaultValue;
  if Assigned(FDetails) then begin
    I := FDetails.IndexOfLabel(ID);
    if I < 0 then exit;
    Field := FDetails.Detail[I];
    if FDetails.CodePageIndex <> -1 then
      Field := FDNLS.CodePages.UTF8toDOS(FDetails.CodePageIndex,Field);
  end;
end;

procedure TframePkgPreview.Render;
var
  I, X, R : integer;
  S : String;
begin
  tRender.Enabled:=False;
  FDosView.ClearScreen;
  FDosView.TextColor:=clWhite;
  for I := 1 to FDosView.ScreenMax.Y do begin
    FDosView.GotoXY(1, I);
    FDosView.PutChar(#$b3);
    FDosView.GotoXY(80, I);
    FDosView.PutChar(#$b3);
  end;
  FDosView.GotoXY(25, 1);
  FDosView.PutChar(#$b3);
  FDosView.GotoXY(2, 2);
  FDosView.PutString(RightPad(LeftPad(#$c1, 24, #$c4), 78, #$c4));
  FDosView.TextColor:=clLime;
  FDosView.GotoXY(3, 1);
  FDosView.PutString('[X] ' + FPkgGroup);
  FDosView.GotoXY(27, 1);
  FDosView.PutString('[X] ' + Uppercase(FPkgID));
  FDosView.GotoXY(41, 1);
  FDosView.PutString(Copy(FPkgDesc,1,39));
  FDosView.GotoXY(3, 3);
  FDosView.PutString(Copy(FPkgTitle + ' (' + FPkgVer + ')' ,1,76));
  FDosView.TextColor:=clAqua;
  FDosView.GotoXY(3, 4);
  FDosView.PutString(Copy(FPkgAuthor,1,76));
  FDosView.GotoXY(3, 5);
  FDosView.PutString(Copy(FPkgLicense,1,76));
  FDosView.TextColor:=clWhite;
  I := 6;
  S := StringReplace(WhenTrue(FPkgSummary, FPkgSummary, FPkgDesc), '|', LF,
    [rfReplaceAll]) + LF;
  S := StringReplace(S, CRLF, LF, [rfReplaceAll]);
  while (S <> '') and (I < FDosView.ScreenMax.Y) do begin
    inc(I);
    FDosView.GotoXY(3,I);
    R := LastPos(LF, S);
    if (R < 77) and (R > 0) then
      X := R
    else begin
      X := LastPos(SPACE, Copy(S, 1, 77));
      if X < 1 then X := 76;
    end;
    FDosView.WriteText(Copy(S, 1,X));
    Delete(S, 1, X);
  end;

  iPreview.Height:=iPreview.Width * FDosView.Bitmap.Height div FDosView.Bitmap.Width;
  iPreview.Picture.Assign(FDosView.Bitmap);
end;

constructor TframePkgPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDosView := TDosScreen.Create(80,14);
  FDosView.Background:=clBlue;
  FDosView.Border:=2;
  FPkgGroup := dos_PkgGroup;
  FPkgVer   := dos_PkgVersion;
  FPkgAuthor := dos_PkgAuthor;
end;

destructor TframePkgPreview.Destroy;
begin
  FreeAndNil(FDosView);
  inherited Destroy;
end;

procedure TframePkgPreview.Preview(Details: TframePkgDetails);
begin
  FDetails := Details;
  if Details.FontIndex = -1 then
    FDosView.Font := FDNLS.Fonts.Data[0].FileData
  else
    FDosView.Font := FDNLS.Fonts.Data[Details.FontIndex].FileData;
  FPkgID := Uppercase(Copy(Details.Identity, 1,8));
  SetFieldValue('version', FPkgVer, dos_PkgVersion);
  SetFieldValue('author', FPkgAuthor, dos_PkgAuthor);
  SetFieldValue('group', FPkgAuthor, dos_PkgAuthor);
  SetFieldValue('platforms', FPkgPlatforms, dos_PkgPlatforms);
  SetFieldValue('title', FPkgTitle);
  SetFieldValue('description', FPkgDesc);
  SetFieldValue('summary', FPkgSummary);
  SetFieldValue('keywords', FPkgKeywords);
  SetFieldValue('copying-policy', FPkgLicense);
  Render;
end;

end.

