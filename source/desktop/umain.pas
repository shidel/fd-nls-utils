unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, Menus, ActnList, XMLConf;

type

  { TmForm }

  TmForm = class(TForm)
    actAppleAbout: TAction;
    actApplePrefs: TAction;
    actMenuHelp: TAction;
    actPrefs: TAction;
    actMenuOpts: TAction;
    actMenuFile: TAction;
    alMain: TActionList;
    mMain: TMainMenu;
    xConfig: TXMLConfig;
    xProperties: TXMLPropStorage;
    procedure actMenuOptsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function AddMenuItem(ToItem : TMenuItem; ActionItem : TBasicAction) : TMenuItem; overload;
    function AddMenuItem(ToItem : TMenuItem; CaptionText : TCaption) : TMenuItem; overload;
    procedure CreateMainMenu;
  public

  end;

var
  mForm: TmForm;

implementation

{$R *.lfm}

{ TmForm }

procedure TmForm.FormCreate(Sender: TObject);
var
   Displays : String;
   I : integer;
begin
   // Create a unique ID for monitor count and resolutions
   Displays := IntToHex(Screen.MonitorCount, 2) +
     IntToHex(Screen.PrimaryMonitor.MonitorNum, 2) +
     IntToHex(Screen.PrimaryMonitor.PixelsPerInch, 4);
   for I := 0 to Screen.MonitorCount - 1 do
     Displays := Displays +
     IntToHex(Screen.Monitors[I].Width,4) +
     IntToHex(Screen.Monitors[I].Height,4);
   // Assign the config application files
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := 'DISPLAYS/UID_' + Displays + '/STATE';
   xConfig.Filename:=AppCfgPath + 'userdata.xml';
   CreateMainMenu;

end;

procedure TmForm.actMenuOptsExecute(Sender: TObject);
begin

end;

function TmForm.AddMenuItem(ToItem: TMenuItem; ActionItem: TBasicAction): TMenuItem;
begin
   Result := TMenuItem.Create(Self);
   Result.Action := ActionItem;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

function TmForm.AddMenuItem(ToItem: TMenuItem; CaptionText: TCaption): TMenuItem;
begin
   Result := TMenuItem.Create(Self);
   Result.Caption := CaptionText;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

procedure TmForm.CreateMainMenu;
var
   aMenu   : TMenuitem;
   I, J, G : integer;
   Cat     : String;
begin
   {$IFDEF MacOS}
     // Create Aplication menu for Apple macOS
     aMenu := AddMenuItem(nil, #$EF#$A3#$BF); { Unicode Apple Logo }
     AddMenuItem(aMenu, actAppleAbout);
     AddMenuItem(aMenu, '-');
     AddMenuItem(aMenu, actApplePrefs);
     AddMenuItem(aMenu, '-');
   {$ENDIF}
   for I := 0 to alMain.ActionCount - 1 do begin
      Cat := Uppercase(alMain.Actions[I].Category);
      if Cat <> 'MENU' then continue;
      aMenu := AddMenuItem(nil, alMain.Actions[I]);
      if not (alMain.Actions[I] is TAction) then Continue;
      G := TAction(alMain.Actions[I]).GroupIndex;
      for J := I + 1 to alMain.ActionCount - 1 do begin
        if not (alMain.Actions[J] is TAction) then Continue;
        if G = TAction(alMain.Actions[J]).GroupIndex then
           AddMenuItem(aMenu, alMain.Actions[J]);
      end;
   end;
end;

end.

