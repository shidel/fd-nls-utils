unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids,
  StdCtrls, FDKit, uLog, Icons, uPkgDetails, uPkgPreview;

type

  { TframePkgListEdit }

  TframePkgListEdit = class(TFrame)
    lvPackages: TListView;
    sPkgEditSplitter: TSplitter;
    procedure lvPackagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FNeedRefresh : boolean;
    FPkgView : TFrame;
    FMasterDetails : TframePkgDetails;
    FPreview : TframePkgPreview;
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
  if not Assigned(FPkgView) then begin
    FPkgView := TFrame.Create(Self);
    FPkgView.Parent := Self;
    FPkgView.Align:=alClient;
  end;
  // I changed some things. Now, all of a sudden, Lazarus started not
  // setting the properties for lvPackages. \O/ 8-(
  with lvPackages do begin
       OnChange:=@lvPackagesChange;
       ReadOnly:=True;
       RowSelect:=True;
       ScrollBars:=ssVertical;
       ViewStyle:=vsReport;
       SmallImages := fMain.ilToolsSmall;
  end;

  if not Assigned(FMasterDetails) then begin
    FMasterDetails := TframePkgDetails.Create(Self);
    FMasterDetails.Parent := FPkgView;
    FMasterDetails.AllowEdit:= False;
    FMasterDetails.Align:=alTop;
    FMasterDetails.LanguageIndex:=FDNLS.FindLanguage(MasterCSVLanguage);
    FMasterDetails.CodePageIndex:=FDNLS.FindCodepage(MasterCSVLanguage);
    FMasterDetails.FontIndex:=FDNLS.FindFont(MasterCSVLanguage);
    FMasterDetails.Flag.Picture.LoadFromLazarusResource(IconPrefix + 'usa');
    FMasterDetails.SetLabels(FDNLS.PackageLists.Fields);
  end;

  if not Assigned(FPreview) then begin
    FPreview := TframePkgPreview.Create(Self);
    FPreview.Parent := FPkgView;
    FPreview.Top := FMasterDetails.Top + FMasterDetails.Height;
    FPreview.Align:=alTop;
  end;
end;

procedure TframePkgListEdit.Clear;
begin
  lvPackages.Clear;
  FreeAndNil(FMasterDetails);
  FreeAnDNil(FPreview);
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
  FMasterDetails.SetDetails(Item.Caption, FDNLS.PackageLists.MasterDetails[Item.Index]);
  FPreview.Preview(FMasterDetails);
  lvPackages.Items[5].ImageIndex:=16;
  lvPackages.Items[14].ImageIndex:=15;
end;

end.

