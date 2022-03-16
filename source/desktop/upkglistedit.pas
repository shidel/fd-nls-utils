unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids, PairSplitter,
  StdCtrls, FDKit, uLog;

type

  { TMasterDetailsPanel }

  TMasterDetailsPanel = class(TPanel)
  private
  protected
    procedure  Configure; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AParent : TWinControl); virtual;
    destructor Destroy; override;
  end;

  { TframePkgListEdit }

  TframePkgListEdit = class(TFrame)
    lvPackages: TListView;
    pEditorOuter: TPanel;
    pEditorInner: TPanel;
    sPkgEditSplitter: TSplitter;
  private
    FNeedRefresh : boolean;
    FMasterDetails : TMasterDetailsPanel;
  public
    procedure Initialize;
    procedure Clear;
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses uMain;

{$R *.lfm}

{ TMasterDetailsPanel }

procedure TMasterDetailsPanel.Configure;
begin
  Height := 50;
  Align := alTop;
  Visible := True;
end;

constructor TMasterDetailsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Configure;
end;

constructor TMasterDetailsPanel.Create(AOwner: TComponent; AParent: TWinControl
  );
begin
  inherited Create(AOwner);
  Parent := AParent;
  Configure;
end;

destructor TMasterDetailsPanel.Destroy;
begin
  inherited Destroy;
end;

{ TframePkgListEdit }

procedure TframePkgListEdit.Initialize;
begin
  if not Assigned(FMasterDetails) then begin
    FMasterDetails := TMasterDetailsPanel.Create(Self, pEditorInner);
  end;
  FMasterDetails.Caption:=FDNLS.PackageLists.Fields.Text;
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

destructor TframePkgListEdit.Destroy;
begin
  FreeAndNil(FMasterDetails);
  inherited Destroy;
end;

end.

