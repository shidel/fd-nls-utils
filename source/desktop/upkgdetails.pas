unit uPkgDetails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, PasExt,
  FDKit, Icons, uAppNLS;

type

  { TframePkgDetails }

  TframePkgDetails = class(TFrame)
    iFlag: TImage;
    iCodePage: TImage;
    lCodepage: TLabel;
    pButtons: TPanel;
    pLanguage: TLabel;
    pTopSpace : TLabel;
    pDetails: TPanel;
    pLabels: TPanel;
    pFrame: TPanel;
    pFlag: TPanel;
    sDetails: TSplitter;
    sbTransfer: TSpeedButton;
    procedure pDetailsResize(Sender: TObject);
  private
    procedure SetRows(AValue: integer);
  private
    FAllowEdit: boolean;
    FCodePageIndex: integer;
    FDetailsIndex: integer;
    FFontIndex: integer;
    FIdentity: String;
    FLanguage: String;
    FLanguageIndex: integer;
    FModified: boolean;
    FRows: integer;
    FLabels : array of TLabel;
    FDatum : array of TControl;
    FViewer: TControl;
    function GetDetail(Index : integer): String;
    function GetFlag: TImage;
    procedure SetDetail(Index : integer; AValue: String);
    procedure SetIdentity(AValue: String);
    procedure SetViewer(AValue: TControl);
    property Rows : integer read FRows write SetRows;
    procedure UpdateViewer(Sender: TObject);
    procedure RefreshViewer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALanguage : String; AEditor : boolean = false); virtual; overload;
    destructor Destroy; override;
    property AllowEdit : boolean read FAllowEdit;
    property Modified : boolean read FModified;
    property Flag : TImage read GetFlag;
    property Viewer : TControl read FViewer write SetViewer;
    property Language : String read FLanguage;
    property LanguageIndex : integer read FLanguageIndex;
    property CodePageIndex : integer read FCodePageIndex;
    property FontIndex : integer read FFontIndex;
    property DetailsIndex : integer read FDetailsIndex;
    property Identity : String read FIdentity write SetIdentity;
    procedure SetLabels(List : TStringList);
    procedure SetDetails(Identifier : String; List : TStringList);
    procedure RowsAdjust;
    function  IndexOfLabel(S : String) : integer;
    property  Detail [Index : integer] : String read GetDetail write SetDetail;
  end;

implementation

uses uMain, uPkgPreview;

{$R *.lfm}

{ TframePkgDetails }

procedure TframePkgDetails.pDetailsResize(Sender: TObject);
begin
    RowsAdjust;
end;

procedure TframePkgDetails.UpdateViewer(Sender: TObject);
begin
  if Assigned(Viewer) then TframePkgPreview(Viewer).Preview(Self);
end;

procedure TframePkgDetails.RefreshViewer(Sender: TObject);
begin
  if Assigned(Viewer) then TframePkgPreview(Viewer).UpdateRequest(Self);
end;

procedure TframePkgDetails.SetRows(AValue: integer);
var
  I : integer;
begin
  if FRows=AValue then Exit;
  if AValue < FRows then begin
    for I := AValue  + 1 to FRows do begin
       FreeAndNil(FLabels[I - 1]);
       FreeAndNil(FDatum[I - 1]);
    end;
  end;
  SetLength(FLabels, AValue);
  SetLength(FDatum, AValue);
  if Not Assigned(pTopSpace) then begin
    pTopSpace := TLabel.Create(Self);
    with pTopSpace do begin
      Parent := pDetails;
      Caption := '';
      Constraints.MinHeight := 8;
      AutoSize := True;
      Align := alTop;
    end;
  end;
  if AValue > FRows then begin
    for I := Rows + 1 to AValue do begin
      if FAllowEdit then begin
        FDatum[I - 1] := TEdit.Create(Self);
        TEdit(FDatum[I - 1]).OnChange:=@RefreshViewer;
      end else begin
        FDatum[I - 1] := TLabel.Create(Self);
        TLabel(FDatum[I - 1]).WordWrap:=True;
      end;
      FDatum[I - 1].Name:=Name + '_Details'+IntToStr(I-1);
      FDatum[I - 1].Parent := pDetails;
      FDatum[I - 1].Top := (I) * 16 + 8;
      FDatum[I - 1].Constraints.MinHeight:=16;
      FDatum[I - 1].AutoSize:=True;
      FDatum[I - 1].Align:=alTop;
      FDatum[I - 1].Caption:='';
      FDatum[I - 1].OnClick := OnClick;

      FLabels[I - 1] := TLabel.Create(Self);
      FLabels[I - 1].Name:=Name + '_PkgLabel'+IntToStr(I-1);
      FLabels[I - 1].Parent := pLabels;
      FLabels[I - 1].Alignment := taRightJustify;
      FLabels[I - 1].Left := pLabels.Width - FLabels[I - 1].Width - 1;
      FLabels[I - 1].Top := (I -1) * 16 + 8;
      FLabels[I - 1].WordWrap:=False;
      Flabels[I - 1].Anchors:=[akRight, akTop];
      FLabels[I - 1].AutoSize := True;
      FLabels[I - 1].OnClick := OnClick;
    end;
  end;
  FRows:=AValue;
  FModified := False;
end;

function TframePkgDetails.GetFlag: TImage;
begin
  Result := iFlag;
end;

function TframePkgDetails.GetDetail(Index : integer): String;
begin
  Result := FDatum[Index].Caption;
end;

procedure TframePkgDetails.SetDetail(Index : integer; AValue: String);
begin
  FDatum[Index].Caption:=AValue;
  FModified := True;
end;

procedure TframePkgDetails.SetIdentity(AValue: String);
begin
  if FIdentity=AValue then Exit;
  FIdentity:=AValue;
end;

procedure TframePkgDetails.SetViewer(AValue: TControl);
begin
  if FViewer=AValue then Exit;
  FViewer:=AValue;
end;

constructor TframePkgDetails.Create(AOwner: TComponent; ALanguage: String;
  AEditor: boolean = false);
begin
  inherited Create(AOwner);
  FDetailsIndex := -1;
  OnClick := @UpdateViewer;
  pFrame.OnClick:= OnClick;
  pLabels.OnClick := OnClick;
  pFlag.OnClick := OnClick;
  sDetails.OnClick := OnClick;
  iFlag.OnClick := OnClick;
  pLanguage.OnClick := OnClick;
  pButtons.OnClick := OnClick;
  lCodePage.OnClick := OnClick;
  FAllowEdit := AEditor;

  FLanguage := ALanguage;
  pLanguage.Caption:=ALanguage;
  Name:=Name + '_' + FLanguage;
  pLabels.Width :=
    fMain.xProperties.ReadInteger(GetNamePath + '/WIDTH', pLabels.Width);
  FLanguageIndex:=FDNLS.FindLanguage(Language);
  FCodePageIndex:=FDNLS.FindCodepage(Language);
  sbTransfer.Enabled:=FAllowEdit and (FCodePageIndex <> -1);
  sbTransfer.Visible:=FAllowEdit and (FCodePageIndex <> -1);
  if FCodePageIndex = -1 then begin
    lCodepage.Visible := False;
    iCodepage.Visible := True;
    iCodePage.Picture.LoadFromLazarusResource(IconUI[12]);
  end else begin
    lCodepage.Visible := True;
    iCodepage.Visible := False;
    lCodePage.Caption :=
      Format(lbl_DOSCP, [FDNLS.CodePages.Identifier[FCodePageIndex]]);
  end;
  FFontIndex:=FDNLS.FindFont(Language);
  FDetailsIndex := FDNLS.PackageLists.Language(ALanguage);
  iFlag.Picture.LoadFromLazarusResource(IconFlags[FDNLS.FindFlag(Language)]);
  SetLabels(FDNLS.PackageLists.Fields);
end;

destructor TframePkgDetails.Destroy;
begin
  fMain.xProperties.WriteInteger(GetNamePath + '/WIDTH', pLabels.Width);
  inherited Destroy;
end;

procedure TframePkgDetails.SetLabels(List: TStringList);
var
  I : integer;
begin
  Rows := List.Count;
  for I := 0 to List.Count - 1 do begin
    FLabels[I].Caption:=List[I];
  end;
  FModified := False;
end;

procedure TframePkgDetails.SetDetails(Identifier : String; List : TStringList);
var
  I : integer;
begin
  FModified := False;
  FIdentity := Identifier;
  Rows := List.Count;
  for I := 0 to List.Count - 1 do begin
    FDatum[I].Caption:=List[I];
    FDatum[I].Enabled:=lCodepage.Visible; // only enabled with valid codepage
  end;
  RowsAdjust;
end;

procedure TframePkgDetails.RowsAdjust;
var
  I : integer;
begin
  if Length(FDatum) > 0 then begin
    Height := FDatum[Length(FDatum) - 1].Top +
      FDatum[Length(FDatum) - 1].Height + pTopSpace.Height;
  end;
  for I := 0 to Length(FDatum) - 1 do begin
    FLabels[I].Top:=FDatum[I].Top;
  end;
end;

function TframePkgDetails.IndexOfLabel(S: String): integer;
var
  I : integer;
begin
  S := LowerCase(Trim(S));
  Result := -1;
  for I := 0 to Length(FLabels) - 1 do
    if Lowercase(FLabels[I].Caption) = S then begin
      Result := I;
      Break;
    end;
end;

end.

