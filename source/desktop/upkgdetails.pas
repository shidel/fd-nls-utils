unit uPkgDetails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, FDKit, Icons;

type

  { TframePkgDetails }

  TframePkgDetails = class(TFrame)
    iFlag: TImage;
    pLanguage: TLabel;
    pTopSpace : TLabel;
    pDetails: TPanel;
    pLabels: TPanel;
    pFrame: TPanel;
    pFlag: TPanel;
    sDetails: TSplitter;
    procedure pDetailsResize(Sender: TObject);
  private
    procedure SetRows(AValue: integer);
  private
    FAllowEdit: boolean;
    FCodePageIndex: integer;
    FFontIndex: integer;
    FIdentity: String;
    FLanguage: String;
    FLanguageIndex: integer;
    FModified: boolean;
    FRows: integer;
    FLabels : array of TLabel;
    FDetails : array of TLabel;
    function GetDetail(Index : integer): String;
    function GetFlag: TImage;
    procedure SetAllowEdit(AValue: boolean);
    procedure SetCodePageIndex(AValue: integer);
    procedure SetDetail(Index : integer; AValue: String);
    procedure SetFontIndex(AValue: integer);
    procedure SetIdentity(AValue: String);
    procedure SetLanguageIndex(AValue: integer);
    property Rows : integer read FRows write SetRows;
  public
    constructor Create(AOwner: TComponent; ALanguage : String); virtual; overload;
    property AllowEdit : boolean read FAllowEdit write SetAllowEdit;
    property Modified : boolean read FModified;
    property Flag : TImage read GetFlag;
    property Language : String read FLanguage;
    property LanguageIndex : integer read FLanguageIndex write SetLanguageIndex;
    property CodePageIndex : integer read FCodePageIndex write SetCodePageIndex;
    property FontIndex : integer read FFontIndex write SetFontIndex;
    property Identity : String read FIdentity write SetIdentity;
    procedure SetLabels(List : TStringList);
    procedure SetDetails(Identifier : String; List : TStringList);
    procedure RowsAdjust;
    function  IndexOfLabel(S : String) : integer;
    property  Detail [Index : integer] : String read GetDetail write SetDetail;
  end;

implementation

{$R *.lfm}

{ TframePkgDetails }

procedure TframePkgDetails.pDetailsResize(Sender: TObject);
begin
    RowsAdjust;
end;

procedure TframePkgDetails.SetRows(AValue: integer);
var
  I : integer;
begin
  if FRows=AValue then Exit;
  if AValue < FRows then begin
    for I := AValue  + 1 to FRows do begin
       FreeAndNil(FLabels[I - 1]);
       FreeAndNil(FDetails[I - 1]);
    end;
  end;
  SetLength(FLabels, AValue);
  SetLength(FDetails, AValue);
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
      FDetails[I - 1] := TLabel.Create(Self);
      FDetails[I - 1].Parent := pDetails;
      FDetails[I - 1].Top := (I) * 16 + 8;
      FDetails[I - 1].WordWrap:=True;
      FDetails[I - 1].Constraints.MinHeight:=16;
      FDetails[I - 1].AutoSize:=True;
      FDetails[I - 1].Align:=alTop;

      FLabels[I - 1] := TLabel.Create(Self);
      FLabels[I - 1].Parent := pLabels;
      FLabels[I - 1].Alignment := taRightJustify;
      FLabels[I - 1].Left := pLabels.Width - FLabels[I - 1].Width - 1;
      FLabels[I - 1].Top := (I -1) * 16 + 8;
      FLabels[I - 1].WordWrap:=False;
      Flabels[I - 1].Anchors:=[akRight, akTop];
      FLabels[I - 1].AutoSize := True;
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
  Result := FDetails[Index].Caption;
end;

procedure TframePkgDetails.SetAllowEdit(AValue: boolean);
begin
  if FAllowEdit=AValue then Exit;
  if Length(FDetails) <> 0 then exit;
  FAllowEdit:=AValue;
end;

procedure TframePkgDetails.SetCodePageIndex(AValue: integer);
begin
  if FCodePageIndex=AValue then Exit;
  FCodePageIndex:=AValue;
end;

procedure TframePkgDetails.SetDetail(Index : integer; AValue: String);
begin
  FDetails[Index].Caption:=AValue;
  FModified := True;
end;

procedure TframePkgDetails.SetFontIndex(AValue: integer);
begin
  if FFontIndex=AValue then Exit;
  FFontIndex:=AValue;
end;

procedure TframePkgDetails.SetIdentity(AValue: String);
begin
  if FIdentity=AValue then Exit;
  FIdentity:=AValue;
end;

procedure TframePkgDetails.SetLanguageIndex(AValue: integer);
begin
  if FLanguageIndex=AValue then Exit;
  FLanguageIndex:=AValue;
end;

constructor TframePkgDetails.Create(AOwner: TComponent; ALanguage: String);
begin
  inherited Create(AOwner);
  FLanguage := ALanguage;
  pLanguage.Caption:=ALanguage;
  Name:=Name + '_' + FLanguage;
  FLanguageIndex:=FDNLS.FindLanguage(Language);
  FCodePageIndex:=FDNLS.FindCodepage(Language);
  FFontIndex:=FDNLS.FindFont(Language);
  iFlag.Picture.LoadFromLazarusResource(IconFlags[FDNLS.FindFlag(Language)]);
  SetLabels(FDNLS.PackageLists.Fields);
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
    FDetails[I].Caption:=List[I];
  end;
  RowsAdjust;
end;

procedure TframePkgDetails.RowsAdjust;
var
  I : integer;
begin
  if Length(FDetails) > 0 then begin
    Height := FDetails[Length(FDetails) - 1].Top +
      FDetails[Length(FDetails) - 1].Height + pTopSpace.Height;
  end;
  for I := 0 to Length(FDetails) - 1 do begin
    FLabels[I].Top:=FDetails[I].Top;
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

