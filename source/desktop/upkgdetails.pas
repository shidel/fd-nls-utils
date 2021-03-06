unit uPkgDetails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, PasExt,
  FDKit, Icons, uAppCfg, uAppNLS, uLog;

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
    procedure sbTransferClick(Sender: TObject);
  private
    procedure SetRows(AValue: integer);
  private
    FAllowEdit: boolean;
    FCodePageIndex: integer;
    FDetailsIndex: integer;
    FFontIndex: integer;
    FIdentity: String;
    FItemIndex: integer;
    FEditIndex : integer;
    FLanguage: String;
    FLanguageIndex: integer;
    FModified: boolean;
    FRows: integer;
    FLabels : array of TLabel;
    FDatum : array of TControl;
    FViewer: TControl;
    FFontSize : integer;
    function GetDetail(Index : integer): String;
    function GetFlag: TImage;
    procedure SetDetail(Index : integer; AValue: String);
    procedure SetDetailsIndex(AValue: integer);
    procedure SetIdentity(AValue: String);
    procedure SetViewer(AValue: TControl);
    property Rows : integer read FRows write SetRows;
    procedure UpdateViewer(Sender: TObject);
    procedure RefreshViewer(Sender: TObject);
    procedure Editing(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditDone(Sender: TObject);
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
    property DetailsIndex : integer read FDetailsIndex write SetDetailsIndex;
    property ItemIndex : integer read FItemIndex;
    property EditIndex : integer read FEditIndex;
    property Identity : String read FIdentity write SetIdentity;
    procedure SetLabels(List : TStringList);
    procedure SetDetails(Identifier : String; Index : integer; List : TStringList; ReplaceAll : boolean = True);
    procedure RowsAdjust;
    function  IndexOfLabel(S : String) : integer;
    property  Detail [Index : integer] : String read GetDetail write SetDetail;
    procedure CommitChanges;
  end;

implementation

uses uPkgPreview, uPkgListEdit;

{$R *.lfm}

{ TframePkgDetails }

procedure TframePkgDetails.pDetailsResize(Sender: TObject);
begin
    RowsAdjust;
end;

procedure TframePkgDetails.sbTransferClick(Sender: TObject);
begin
  if FItemIndex <> -1 then begin
    SetDetails(FIdentity, FItemIndex, FDNLS.PackageLists.MasterDetails[FItemIndex], False);
    FEditIndex := FItemIndex;
    FModified := True;
    CommitChanges;
    OnClick(Sender);
  end;
end;

procedure TframePkgDetails.UpdateViewer(Sender: TObject);
begin
  if Assigned(Viewer) then TframePkgPreview(Viewer).Preview(Self);
end;

procedure TframePkgDetails.RefreshViewer(Sender: TObject);
begin
  if Assigned(Viewer) then TframePkgPreview(Viewer).UpdateRequest(Self);
end;

procedure TframePkgDetails.Editing(Sender: TObject);
begin
  FModified := True;
  RefreshViewer(Sender);
end;

procedure TframePkgDetails.EditExit(Sender: TObject);
begin
  FModified := True;
  CommitChanges;
end;

procedure TframePkgDetails.EditDone(Sender: TObject);
begin
  FModified := True;
  CommitChanges;
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
        TEdit(FDatum[I - 1]).OnChange:=@Editing;
        TEdit(FDatum[I - 1]).OnExit:=@EditExit;
        TEdit(FDatum[I - 1]).OnEditingDone:=@EditDone;
      end else begin
        FDatum[I - 1] := TLabel.Create(Self);
        TLabel(FDatum[I - 1]).WordWrap:=True;
        TLabel(FDatum[I - 1]).ShowAccelChar:=False;
      end;
      FDatum[I - 1].Name:=Name + '_Details'+IntToStr(I-1);
      FDatum[I - 1].Parent := pDetails;
      FDatum[I - 1].AutoSize:=True;
      FDatum[I - 1].Caption:='Xy/|';
      {$if defined(windows)}
        if FFontSize <> 0 then begin
          FDatum[I - 1].Font.Height:=FFontSize;
        end;
        FDatum[I - 1].Top := (I * FDatum[I - 1].Height * 2) + 8;
        FDatum[I - 1].Constraints.MinHeight:=FDatum[I - 1].Height * 2;
      {$else}
        if FFontSize <> 0 then begin
          FDatum[I - 1].Font.Height:=FFontSize;
          FDatum[I - 1].Top := (I * FDatum[I - 1].Height * 3 div 2) + 8;
          FDatum[I - 1].Constraints.MinHeight:=FDatum[I - 1].Height * 3 div 2;
        end else begin
          FDatum[I - 1].Top := (I) * 16 + 8;
          FDatum[I - 1].Constraints.MinHeight:=16;
        end;
      {$endif}
      FDatum[I - 1].Align:=alTop;
      FDatum[I - 1].Caption:='';
      FDatum[I - 1].OnClick := OnClick;

      if FFontSize <> 0 then begin
        FDatum[I - 1].Font.Height:=FFontSize;
      end;

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

      if FFontSize <> 0 then begin
        FLabels[I - 1].Font.Height:=FFontSize;
      end;

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

procedure TframePkgDetails.SetDetailsIndex(AValue: integer);
begin
  if FDetailsIndex=AValue then Exit;
  FDetailsIndex:=AValue;
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
var
  I : integer;
begin
  inherited Create(AOwner);
  FDetailsIndex := -1;
  FItemIndex := -1;
  FEditIndex := -1;
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
  pLabels.Width := GetPropertyState(Name + '_LABELS', 'WIDTH', pLabels.Width);
  FLanguageIndex:=FDNLS.FindLanguage(Language);
  FCodePageIndex:=FDNLS.FindCodepage(Language);
  pButtons.Visible:=FAllowEdit and (FCodePageIndex <> -1);
  sbTransfer.Enabled:=FAllowEdit and (FCodePageIndex <> -1);

  if Not FAllowEdit then
    FFontSize := GetSetting('DETAILS/MASTER/FONT/SIZE', 0);

  if FCodePageIndex = -1 then begin
    lCodepage.Visible := False;
    iCodepage.Visible := True;
    iCodePage.Picture.LoadFromLazarusResource(IconUI[icon_NoImage]);
  end else begin
    lCodepage.Visible := True;
    iCodepage.Visible := False;
    lCodePage.Caption :=
      Format(lbl_DOSCP, [FDNLS.CodePages.Identifier[FCodePageIndex]]);
  end;
  FFontIndex:=FDNLS.FindFont(Language);
  FDetailsIndex := FDNLS.PackageLists.Language(Language);
  I := FDNLS.FindFlag(Language);
  if I >= 0 then
    iFlag.Picture.LoadFromLazarusResource(IconFlags[I]);
  SetLabels(FDNLS.PackageLists.Fields);
  FFontSize := 0;
end;

destructor TframePkgDetails.Destroy;
begin
  CommitChanges;
  SetPropertyState(Name + '_LABELS', 'WIDTH', pLabels.Width);
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

procedure TframePkgDetails.SetDetails(Identifier: String; Index: integer;
  List: TStringList; ReplaceAll : boolean = True);
var
  I : integer;
begin
  if FEditIndex <> -1 then
     CommitChanges;
  FIdentity := Identifier;
  FItemIndex := Index;
  FEditIndex := Index;
  Rows := List.Count;
  for I := 0 to List.Count - 1 do begin
    FDatum[I].Enabled:=lCodepage.Visible; // only enabled with valid codepage
    if ReplaceAll or // Not true when transfer button clicked
    (FDatum[I].Enabled and (trim(FDatum[I].Caption) = '')) then // Is Empty and Active
      FDatum[I].Caption:=List[I];
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

procedure TframePkgDetails.CommitChanges;
var
  I : integer;
  List : TStringList;
begin
  if FModified = False then exit;
  FModified := False;
  if (FEditIndex = -1) or (DetailsIndex = -1) then exit;
  Log(Self, 'save changes to item ' + IntToStr(DetailsIndex) + '/' + IntToStr(FEditIndex));
  List := TStringList.Create;
  for I := 0 to Length(FDatum) - 1 do
    List.Add(FDatum[I].Caption);
  FDNLS.PackageLists.SetLangDetails(DetailsIndex, FEditIndex, List);
  FreeAndNil(List);
  TframePkgListEdit(Owner).DetailsModified(Self);
end;

end.

