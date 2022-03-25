unit uPkgPreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, ExtCtrls, PasExt, FDKit,
  DosView, uAppNLS, uLog;

type

  { TframePkgPreview }

  TframePkgPreview = class(TFrame)
    iPreview: TImage;
    sbPreview: TScrollBox;
    tRender: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure tRenderTimer(Sender: TObject);
  private
    FDosView : TDosScreen;
    FDetails : TControl;
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
    FDataUpdate : boolean;
    procedure SetFieldValue(ID : String; var Field : String; DefaultValue : String = '');
    procedure Render;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Preview(Details : TControl);
    procedure UpdateRequest(Sender: TObject);
  end;

implementation

uses uPkgDetails;

{$R *.lfm}

{ TframePkgPreview }

procedure TframePkgPreview.FrameResize(Sender: TObject);
begin
  tRender.Interval:=500;
  tRender.Enabled:=True;
end;

procedure TframePkgPreview.tRenderTimer(Sender: TObject);
begin
  if FDataUpdate then
    Preview(FDetails)
  else
    Render;
end;

procedure TframePkgPreview.SetFieldValue(ID: String; var Field: String; DefaultValue : String = '');
var
  I : integer;
begin
  Field := DefaultValue;
  if Assigned(FDetails) then begin
    I := TframePkgDetails(FDetails).IndexOfLabel(ID);
    if I < 0 then exit;
    Field := TframePkgDetails(FDetails).Detail[I];
    if TframePkgDetails(FDetails).CodePageIndex <> -1 then
      Field := FDNLS.CodePages[TframePkgDetails(FDetails).CodePageIndex].UTF8toDOS(Field);
  end else
    Field := '';
end;

procedure TframePkgPreview.Render;
var
  I, X, R, WX : integer;
  S : String;
begin
  tRender.Enabled:=False;
  FDataUpdate := False;
  FDosView.ClearScreen;
  FDosView.TextColor:=clWhite;
  iPreview.Height:=iPreview.Width * FDosView.Bitmap.Height div FDosView.Bitmap.Width;
  Constraints.MaxHeight:=iPreview.Height + 2;
  if not Assigned(FDetails) then begin
    iPreview.Picture.Assign(FDosView.Bitmap);
    exit;
  end;

  for I := 1 to FDosView.ScreenMax.Y do begin
    if I = 15 then FDosView.TextColor:=clGray;
    FDosView.GotoXY(1, I);
    FDosView.PutChar(#$b3);
    FDosView.GotoXY(80, I);
    FDosView.PutChar(#$b3);
  end;
  FDosView.TextColor:=clWhite;
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
  FDosView.Window(3, 7, FDosView.ScreenMax.X -1, FDosView.ScreenMax.Y);
  S := StringReplace(WhenTrue(FPkgSummary, FPkgSummary, FPkgDesc), '|', LF,
    [rfReplaceAll]) + LF;
  S := StringReplace(S, CRLF, LF, [rfReplaceAll]);
  while (S <> '') and
  (FDosView.WhereY < FDosView.WindMax.Y - FDosView.WindMin.Y) do begin
    WX := FDosView.WindMax.X - FDosView.WindMin.X + 1;
    X := Pos(LF, Copy(S, 1, WX));
    if FDosView.WhereY > 8 then
      FDosView.TextColor:=clGray
    else
      FDosView.TextColor:=clWhite;
    if (X < 1) then begin
      X := WX;
      R := LastPos(SPACE, Copy(S, 1, WX));
      if (R > 1) then X := R;
      WX := FDosView.WhereY;
      FDosView.WriteText(Copy(S, 1,X));
      FDosView.GotoXY(1,WX + 1);
    end else
      FDosView.WriteText(Copy(S, 1,X));
    Delete(S, 1, X);
  end;
  FDosView.WriteText(S);
  iPreview.Picture.Assign(FDosView.Bitmap);
end;

constructor TframePkgPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:=Name + '_DosView';
  FDosView := TDosScreen.Create(80,25);
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

procedure TframePkgPreview.Preview(Details: TControl);
begin
  FDetails := Details;
  if not Assigned(FDetails) then begin
    Log(self, 'Preview upate with (null)' );
    FDosView.Font := FDNLS.Fonts.Data[0].FileData;
    FPkgID := '';
  end else begin
    Log(self, 'Preview upate with ' + FDetails.Name);
    if TframePkgDetails(FDetails).FontIndex = -1 then
      FDosView.Font := FDNLS.Fonts.Data[0].FileData
    else
      FDosView.Font := FDNLS.Fonts.Data[TframePkgDetails(FDetails).FontIndex].FileData;
    FPkgID := Uppercase(Copy(TframePkgDetails(FDetails).Identity, 1,8));
  end;
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

procedure TframePkgPreview.UpdateRequest(Sender: TObject);
begin
  tRender.Interval:=250;
  tRender.Enabled:=True;
  FDataUpdate := True;
end;

end.

