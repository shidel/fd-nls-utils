unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, Forms, Controls, Graphics, Dialogs, XMLPropStorage,
  StdCtrls, Menus, ActnList, ComCtrls, ExtCtrls, PairSplitter, XMLConf;

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
    imgAbout: TImage;
    memoAbout: TMemo;
    mMain: TMainMenu;
    pSeparatorAbout: TPanel;
    pcPrefs: TPageControl;
    pMain: TPanel;
    pcMain: TPageControl;
    pControlArea: TPanel;
    pSeparatorUpper: TPanel;
    pSeperatorLower: TPanel;
    sbMain: TStatusBar;
    sPrefs: TSplitter;
    tsGeneral: TTabSheet;
    tsAbout: TTabSheet;
    tvPrefs: TTreeView;
    tsPrefs: TTabSheet;
    xConfig: TXMLConfig;
    xProperties: TXMLPropStorage;
    procedure actAppleAboutExecute(Sender: TObject);
    procedure actApplePrefsExecute(Sender: TObject);
    procedure actPrefsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvPrefsChange(Sender: TObject; Node: TTreeNode);
  private
    function AddMenuItem(ToItem : TMenuItem; ActionItem : TBasicAction) : TMenuItem; overload;
    function AddMenuItem(ToItem : TMenuItem; CaptionText : TCaption) : TMenuItem; overload;
    procedure AddPrefsTree(ParentNode : TTreeNode; Pages : TPageControl);
    procedure SelectPrefsPage(Tab : TTabSheet);
    procedure CreateMainMenu;
    procedure CreatePrefsTree;
    procedure CreateAboutText;
  public

  end;

var
  mForm: TmForm;

implementation

{$I version.inc}

{$R *.lfm}

{ TmForm }

procedure TmForm.FormCreate(Sender: TObject);
var
   Displays : String;
   I : integer;
begin
   // Hide some design time elements
   pcMain.ShowTabs := False;
   pcPrefs.ShowTabs := False;
   // set Program configuration file
   xConfig.Filename:= AppCfgPath + 'userdata.xml';
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
   // Populate UI elements
   CreateMainMenu;
   CreatePrefsTree;
   CreateAboutText;
end;

procedure TmForm.actAppleAboutExecute(Sender: TObject);
begin
   SelectPrefsPage(tsAbout);
end;

procedure TmForm.actApplePrefsExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TmForm.actPrefsExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TmForm.tvPrefsChange(Sender: TObject; Node: TTreeNode);
var
   I : integer;
begin
   for I := 0 to pcPrefs.PageCount - 1 do begin
     if Node.Text = pcPrefs.Pages[I].Caption then begin
       pcPrefs.ActivePage := pcPrefs.Pages[I];
       exit;
     end;
   end;
end;

function TmForm.AddMenuItem(ToItem: TMenuItem; ActionItem: TBasicAction): TMenuItem;
begin
   // Add an item to the main menu based on it's action
   Result := TMenuItem.Create(Self);
   Result.Action := ActionItem;
   if Assigned(ToItem) then
      ToItem.Add(Result)
   else
      mMain.Items.Add(Result);
end;

function TmForm.AddMenuItem(ToItem: TMenuItem; CaptionText: TCaption): TMenuItem;
begin
   // Add an item to the main menu based on it's caption
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
   // Add ActionList items to Application MainMenu
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

procedure TmForm.AddPrefsTree(ParentNode: TTreeNode; Pages: TPageControl);
var
   I : integer;
begin
   // Add the pages of an option to a node inthe preferences tree
  for I := 0 to Pages.PageCount - 1 do begin
    tvPrefs.Items.AddChild(ParentNode, Pages.Pages[I].Caption);
  end;
end;

procedure TmForm.SelectPrefsPage(Tab: TTabSheet);
var
   N : TTreeNode;
begin
  N := tvPrefs.Items.GetFirstNode;
  while Assigned(N) do begin
    if N.Text = Tab.Caption then begin
      N.Selected:=True;
      Break;
    end;
    N := N.GetNext;
  end;
  pcMain.ActivePage := tsPrefs;
end;

procedure TmForm.CreatePrefsTree;
begin
   // Clear the prefeences tree and populate the root pages
   tvPrefs.Items.Clear;
   AddPrefsTree(nil, pcPrefs);
   tvPrefs.FullExpand;
   tvPrefs.Items.GetFirstNode.Selected:=True;
end;

procedure TmForm.CreateAboutText;
var
   Rows : integer;
begin
  // Create the text for the preferences about page
  memoAbout.Clear;
  memoAbout.Lines.Add('');
  memoAbout.Lines.Add(APP_FILEDESCRIPTION);
  memoAbout.Lines.Add('Version ' + APP_VERSION + ' (r' + SOURCE_REVISION + ')');
  memoAbout.Lines.Add('BSD 3-Clause License');
  memoAbout.Lines.Add('Copyright (c) ' + APP_LEGALCOPYRIGHT);
  memoAbout.Lines.Add('');
  memoAbout.Lines.Add('Created with the Lazarus IDE');
  memoAbout.Lines.Add('and the Free Pascal Compiler');
  Rows := memoAbout.Lines.Count + 1;
  if Rows > 10 then Rows := 10;
  memoAbout.Height:=memoAbout.Font.GetTextHeight(APP_LEGALCOPYRIGHT) * Rows;
end;

end.

