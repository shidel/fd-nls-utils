unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids, PasExt,
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
    FEditors : array of TframePkgPreview;
    function MakeViewer(Language:String; AllowEdit : boolean) : TframePkgDetails;
    procedure MakeEditors;
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

function TframePkgListEdit.MakeViewer(Language: String; AllowEdit: boolean
  ): TframePkgDetails;
begin
  log(Self, 'Create Details ' + WhenTrue(AllowEdit, 'Editor', 'Viewer') + ' for ' + Language);
  Result := TframePkgDetails.Create(Self);
  Result.Parent := FPkgView;
  Result.AllowEdit:= AllowEdit;
  Result.Align:=alTop;
  Result.LanguageIndex:=FDNLS.FindLanguage(Language);
  Result.CodePageIndex:=FDNLS.FindCodepage(Language);
  Result.FontIndex:=FDNLS.FindFont(Language);
  Result.Flag.Picture.LoadFromLazarusResource(IconFlags[FDNLS.FindFlag(Language)]);
  Result.SetLabels(FDNLS.PackageLists.Fields);
end;

procedure TframePkgListEdit.MakeEditors;
begin

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
    FMasterDetails := MakeViewer(MasterCSVLanguage, False);
  end;

  if not Assigned(FPreview) then begin
    FPreview := TframePkgPreview.Create(Self);
    FPreview.Parent := FPkgView;
    FPreview.Top := FMasterDetails.Top + FMasterDetails.Height;
    FPreview.Align:=alTop;
  end;
end;

procedure TframePkgListEdit.Clear;
var
  I : integer;
begin
  lvPackages.Clear;
  FreeAndNil(FMasterDetails);
  FreeAnDNil(FPreview);
  for I := 0 to length(FEditors) - 1 do
    FreeAndNil(FEditors[I]);
  SetLength(FEditors, 0);
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
  if Length(FEditors) = 0 then MakeEditors;

end;

end.

