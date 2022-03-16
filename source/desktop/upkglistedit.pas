unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids,
  StdCtrls, FDKit, uLog, Icons, uPkgDetails;

type

  { TframePkgListEdit }

  TframePkgListEdit = class(TFrame)
    lvPackages: TListView;
    sPkgEditSplitter: TSplitter;
    procedure lvPackagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FNeedRefresh : boolean;
    FMasterDetails : TframePkgDetails;
    FPkgView : TFrame;
  public
    procedure Initialize;
    procedure Clear;
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
    procedure SelectEdit(Item : TListItem); overload;
  end;

implementation

uses uMain;

{$R *.lfm}

{ TframePkgListEdit }

procedure TframePkgListEdit.lvPackagesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if lvPackages.Selected = Item then
    SelectEdit(lvPackages.Selected);
end;

procedure TframePkgListEdit.Initialize;
begin
  if not Assigned(FMasterDetails) then begin
    FPkgView := TFrame.Create(Self);
    FPkgView.Parent := Self;
    FPkgView.Align:=alClient;
    FMasterDetails := TframePkgDetails.Create(Self);
    FMasterDetails.Parent := FPkgView; // pEditorOuter;
    FMasterDetails.Align:=alTop;
    FMasterDetails.Flag.Picture.LoadFromLazarusResource(IconPrefix + 'usa');
  end;
  FMasterDetails.SetLabels(FDNLS.PackageLists.Fields);
  Clear;
end;

procedure TframePkgListEdit.Clear;
begin
  FNeedRefresh := True;
end;

procedure TframePkgListEdit.Refresh;
var
  I : integer;
  LI : TListItem;
begin
  if not Assigned(FMasterDetails) then Initialize;
  if not FNeedRefresh then Exit;
  FNeedRefresh := False;
  log(Self, IntToStr(FDNLS.PackageLists.PackageCount) + ' master packages');
  lvPackages.Clear;
  for I := 0 to FDNLS.PackageLists.PackageCount - 1 do begin
    LI := lvPackages.Items.Add;
    LI.Caption := FDNLS.PackageLists.PackageID[I];
  end;

end;

constructor TframePkgListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNeedRefresh := True;
end;

procedure TframePkgListEdit.SelectEdit(Item: TListItem);
begin
  FMasterDetails.SetDetails(FDNLS.PackageLists.MasterDetails[Item.Index]);
end;

end.

