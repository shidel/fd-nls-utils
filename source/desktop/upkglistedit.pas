unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids,
  FDKit, uLog;

type

  { TframePkgListEdit }

  TframePkgListEdit = class(TFrame)
    lvPackages: TListView;
    pEditor: TPanel;
    sPkgEditSplitter: TSplitter;
  private
    FNeedRefresh : boolean;
  public
    procedure Initialize;
    procedure Clear;
    procedure Refresh;
  end;

implementation

uses uMain;

{$R *.lfm}

{ TframePkgListEdit }

procedure TframePkgListEdit.Initialize;
begin
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
  if not FNeedRefresh then Exit;
  FNeedRefresh := False;
  log(Self, IntToStr(FDNLS.PackageLists.PackageCount) + ' master packages');
  lvPackages.Clear;
  for I := 0 to FDNLS.PackageLists.PackageCount - 1 do begin
    LI := lvPackages.Items.Add;
    LI.Caption := FDNLS.PackageLists.PackageID[I];
  end;

end;

end.
