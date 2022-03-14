unit uPkgListEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  FDKit;

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
begin
  if not FNeedRefresh then Exit;
  FNeedRefresh := False;
end;

end.

