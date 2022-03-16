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
    procedure Reload;
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
  // Log(Self, 'list item change ');
  if lvPackages.Selected = Item then
    SelectEdit(Item);
end;

procedure TframePkgListEdit.Initialize;
begin
  Clear;
  Log(Self, 'Initialize');
  if not Assigned(FPkgView) then
    FPkgView := TFrame.Create(Self);
  FPkgView.Parent := Self;
  FPkgView.Align:=alClient;
  // I changed some things. Now, all of a sudden, Lazarus started not
  // setting these properties. \O/ 8-(
  lvPackages.OnChange:=@lvPackagesChange;
  lvPackages.ReadOnly:=True;
  lvPackages.RowSelect:=True;

  if not Assigned(FMasterDetails) then begin
    FMasterDetails := TframePkgDetails.Create(Self);
    FMasterDetails.Parent := FPkgView; // pEditorOuter;
    FMasterDetails.Align:=alTop;
    FMasterDetails.Flag.Picture.LoadFromLazarusResource(IconPrefix + 'usa');
    FMasterDetails.SetLabels(FDNLS.PackageLists.Fields);
  end;
end;

procedure TframePkgListEdit.Clear;
begin
  Log(Self, 'Clear');
  exit;
  lvPackages.Clear;
  FreeAndNil(FMasterDetails);
end;

procedure TframePkgListEdit.Reload;
var
  I : integer;
  LI : TListItem;
begin
  if not Assigned(FMasterDetails) then Initialize;
  log(Self, IntToStr(FDNLS.PackageLists.PackageCount) + ' master packages');
  for I := 0 to FDNLS.PackageLists.PackageCount - 1 do begin
    LI := lvPackages.Items.Add;
    LI.Caption := FDNLS.PackageLists.PackageID[I];
  end;

end;

procedure TframePkgListEdit.Refresh;
begin
  if not Assigned(FMasterDetails) then Reload;
  Log(Self, 'Refresh');
end;

constructor TframePkgListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNeedRefresh := True;
end;

procedure TframePkgListEdit.SelectEdit(Item: TListItem);
begin
  Log(Self, 'select item ' + IntToStr(Item.Index) + ', ' + Item.Caption);
  Log(Self, 'set master details');
  FMasterDetails.SetDetails(FDNLS.PackageLists.MasterDetails[Item.Index]);
end;

end.

