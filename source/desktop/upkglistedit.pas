unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, PasExt,
  StdCtrls, Dialogs, FDKit, uAppCfg, uLog, Icons, uPkgDetails, uPkgPreview;

type

  { TframePkgListEdit }

  TframePkgListEdit = class(TFrame)
    lvPackages: TListView;
    sPkgEditSplitter: TSplitter;
    procedure lvPackagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure sSplitterMoved(Sender: TObject);
  private
    FNeedRefresh : boolean;
    FPkgView : TFrame;
    FMasterDetails : TframePkgDetails;
    FPreview : TframePkgPreview;
    FPreviewSplitter : TSplitter;
    FScroll : TScrollBox;
    FEditors : array of TframePkgDetails;
    FActive  : TStringArray;
    FStayWithLang : boolean;
    function MakeViewer(Language:String; AllowEdit : boolean; ATop : integer) : TframePkgDetails;
    procedure MakeEditors;
  public
    procedure Initialize;
    procedure Clear;
    procedure Reload;
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
    procedure SelectEdit(Item : TListItem); overload;
    procedure SelectPreview(Details : TframePkgDetails);
    procedure UpdateStatus; overload;
    procedure UpdateStatus(Index : integer); overload;
    procedure UpdateStatus(var Item : TListItem); overload;
    procedure DetailsModified(Sender : TObject);
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

procedure TframePkgListEdit.sSplitterMoved(Sender: TObject);
begin
  if Assigned(FPreview) then
    FPreview.FrameResize(Sender);
end;

function TframePkgListEdit.MakeViewer(Language: String; AllowEdit: boolean;
  ATop: integer): TframePkgDetails;
begin
  if FDNLS.FindLanguage(Language) = -1 then begin
    Result := nil;
    exit;
  end;
  log(Self, 'Create Details ' + WhenTrue(AllowEdit, 'Editor', 'Viewer') + ' for ' + Language);
  Result := TframePkgDetails.Create(Self, Language, AllowEdit);
  if AllowEdit then begin
    Result.Parent := FScroll;
    if Result.DetailsIndex = -1 then begin
      Result.DetailsIndex := FDNLS.PackageLists.CreateLanguage(Language);
      Log(Self, 'created as index ' + IntToStr(Result.DetailsIndex));
    end;
  end  else
    Result.Parent := FPkgView;
  Result.Top := ATop;
  Result.Align:=alTop;
end;

procedure TframePkgListEdit.MakeEditors;
var
  I, Y : integer;
begin
  Y := 0;
  SetLength(FEditors, Length(FActive));
  for I := 0 to Length(FEditors) - 1 do begin
    FEditors[I] := MakeViewer(FActive[I], True, Y);
    Y := FEditors[I].Top + FEditors[I].Height;
    FEditors[I].Viewer:=FPreview;
  end;
  UpdateStatus;
end;

procedure TframePkgListEdit.UpdateStatus;
var
  I : integer;
begin
  log(Self, IntToStr(FDNLS.PackageLists.PackageCount) + ' package status update ' + IntToStr(length(FEditors)));
  for I := 0 to lvPackages.Items.Count - 1 do begin
    // Log(Self, 'Status ' + IntToStr(I));
    UpdateStatus(I);
   end;
end;

procedure TframePkgListEdit.UpdateStatus(var Item: TListItem);
var
  I : integer;
  ps, ts : TPackageState;
begin
   if not Assigned(Item) then exit;
   ps := psGood;
   for I := 0 to length(FEditors) - 1 do begin
     if assigned(FEditors[I]) and (FEditors[I].DetailsIndex >= 0) then begin
       ts := FDNLS.PackageLists.StatusDetails[FEditors[I].DetailsIndex, Item.Index];
       if ts = psNew then
         ps := psNew
       else if not (ps in [psNew, psError, psInvalid]) then
         ps := ts;
     end;
   end;
   case ps of
     psNew      : Item.ImageIndex:=17;
     psWarning  : Item.ImageIndex:=15;
     psInvalid,
     psError    : Item.ImageIndex:=16;
    else
      Item.ImageIndex := -1;
   end;
end;

procedure TframePkgListEdit.DetailsModified(Sender: TObject);
begin
  UpdateStatus(TframePkgDetails(Sender).EditIndex);
end;

procedure TframePkgListEdit.UpdateStatus(Index: integer);
var
  Item : TListItem;
begin
  if Index = -1 then exit;
  Item := lvPackages.Items[Index];
  UpdateStatus(Item);
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

  if not Assigned(FPreview) then begin
    FPreview := TframePkgPreview.Create(Self);
    FPreview.Parent := FPkgView;
    FPreview.Top := 0;
    FPreview.Align:=alTop;
    // FPreview.Height := 300;
  end;

  if not Assigned(FPreviewSplitter) then begin
    FPreviewSplitter := TSplitter.Create(Self);
    FPreviewSplitter.Name:='PkgPreviewSplitter';
    FPreviewSplitter.Parent := FPkgView;
    FPreviewSplitter.Top := FPreview.Top + FPreview.Height;
    FPreviewSplitter.Align := alTop;
    FPreviewSplitter.OnMoved:=@sSplitterMoved;
  end;

  if not Assigned(FMasterDetails) then begin
    FMasterDetails := MakeViewer(MasterCSVLanguage, False,
      FPreviewSplitter.Top + FPreviewSplitter.Height);
    if Assigned(FMasterDetails) then
      FMasterDetails.Viewer:=FPreview;
  end;

  if not Assigned(FScroll) then begin
    FScroll := TScrollBox.Create(Self);
    FScroll.Name:='PkgDetailScroll';
    FScroll.Parent := FPkgView;
    FScroll.Top := FPreviewSplitter.Top + FPreviewSplitter.Height;
    FScroll.Align := alClient;
    FScroll.BorderStyle:=bsNone;
    FScroll.AutoScroll:=True;
    FScroll.HorzScrollBar.Visible:=False;
    FScroll.VertScrollBar.Page:=1;
  end;

  FActive := fMain.ActiveLanguages(True);
end;

procedure TframePkgListEdit.Clear;
var
  I : integer;
begin
  Log(Self, 'Reset/Clear');
  FStayWithLang := not GetSetting('PREVIEW/ENGLISH', false);
  SelectEdit(nil);
  lvPackages.Selected:=nil;
  lvPackages.Clear;
  FreeAndNil(FMasterDetails);
  FreeAndNil(FPreviewSplitter);
  for I := 0 to length(FEditors) - 1 do
    FreeAndNil(FEditors[I]);
  FreeAndNil(FScroll);
  FreeAnDNil(FPreview);
  SetLength(FEditors, 0);
end;

procedure TframePkgListEdit.Reload;
var
  I : integer;
  LI : TListItem;
begin
  if not Assigned(FMasterDetails) then Initialize;
  if not Assigned(FMasterDetails) then Exit;
  FStayWithLang := not GetSetting('PREVIEW/ENGLISH', false);
  log(Self, IntToStr(FDNLS.PackageLists.PackageCount) + ' master packages');
  for I := 0 to FDNLS.PackageLists.PackageCount - 1 do begin
    LI := lvPackages.Items.Add;
    LI.Caption := FDNLS.PackageLists.PackageID[I];
  end;
  if Length(FEditors) = 0 then MakeEditors;
end;

procedure TframePkgListEdit.Refresh;
begin
  if not Assigned(FMasterDetails) then Reload;
  FStayWithLang := not GetSetting('PREVIEW/ENGLISH', false);
  Log(Self, 'Refresh');
end;

constructor TframePkgListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNeedRefresh := True;
  FStayWithLang := not GetSetting('PREVIEW/ENGLISH', false);
end;

procedure TframePkgListEdit.SelectEdit(Item: TListItem);
var
  I : integer;
  P : TframePkgDetails;
begin
  if not Assigned(FMasterDetails) then Exit;
  if not Assigned(Item) then begin
    SelectPreview(nil);
    exit;
  end;
  Log(Self, 'select item ' + IntToStr(Item.Index) + ', ' + Item.Caption);
  FMasterDetails.SetDetails(Item.Caption, Item.Index, FDNLS.PackageLists.MasterDetails[Item.Index]);
  if not FStayWithLang then begin
     Log(self, 'view master details');
     SelectPreview(FMasterDetails);
  end;
  if Length(FEditors) = 0 then MakeEditors;
  for I := 0 to Length(FEditors) - 1 do
      FEditors[I].SetDetails(Item.Caption, Item.Index, FDNLS.PackageLists.LangDetails[FEditors[I].DetailsIndex,Item.index]);
  FDNLS.PackageLists.SaveChanges;
  if FStayWithLang then begin
    P := nil;
    if Assigned(FPreview) then P := TframePkgDetails(FPreview.Recent);
    Log(Self, 'View language details ' + WhenTrue(Assigned(P), 'true', 'false'));
    SelectPreview(P);
  end;
end;

procedure TframePkgListEdit.SelectPreview(Details: TframePkgDetails);
begin
  if not Assigned(Details) then
    Details := FMasterDetails;
  if Assigned(FPreview) then
    FPreview.Preview(Details);
end;

end.

