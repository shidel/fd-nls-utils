unit uPkgDetails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TframePkgDetails }

  TframePkgDetails = class(TFrame)
    iFlag: TImage;
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
    FRows: integer;
    FPrevWidth : integer;
    FLabels : array of TLabel;
    FDetails : array of TLabel;
    function GetFlag: TImage;
    property Rows : integer read FRows write SetRows;

  public
    property Flag : TImage read GetFlag;
    procedure SetLabels(List : TStringList);
    procedure SetDetails(List : TStringList);
    procedure RowsAdjust;
  end;

implementation

{$R *.lfm}

{ TframePkgDetails }

procedure TframePkgDetails.pDetailsResize(Sender: TObject);
begin
  // if Width <> FPrevWidth then
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
end;

function TframePkgDetails.GetFlag: TImage;
begin
  Result := iFlag;
end;

procedure TframePkgDetails.SetLabels(List: TStringList);
var
  I : integer;
begin
  Rows := List.Count;
  for I := 0 to List.Count - 1 do begin
    FLabels[I].Caption:=List[I];
  end;
end;

procedure TframePkgDetails.SetDetails(List: TStringList);
var
  I : integer;
begin
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
  // FPrevWidth := Width;
  if Length(FDetails) > 0 then begin
    Height := FDetails[Length(FDetails) - 1].Top +
      FDetails[Length(FDetails) - 1].Height + pTopSpace.Height;
  end;
  for I := 0 to Length(FDetails) - 1 do begin
    FLabels[I].Top:=FDetails[I].Top;
  end;
end;

end.

