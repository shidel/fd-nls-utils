unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, FDKit, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, Menus, ActnList, ComCtrls, ExtCtrls, Buttons,
  XMLConf, opensslsockets, fphttpclient, uLog;

type

  { TmForm }

  TmForm = class(TForm)
    actAppleAbout: TAction;
    actApplePrefs: TAction;
    actCheckForUpdate: TAction;
    actLocalRepoDir: TAction;
    actMenuHelp: TAction;
    actPrefs: TAction;
    actMenuOpts: TAction;
    actMenuFile: TAction;
    alMain: TActionList;
    bbLocalRepo: TBitBtn;
    bbSoftwareUpdate: TButton;
    cbSoftwareUpdate: TComboBox;
    edLocalRepo: TEdit;
    imgAbout: TImage;
    lbSoftwareUpdate: TLabel;
    lbLocalRepo: TLabel;
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
    sdLocalRepo: TSelectDirectoryDialog;
    sPrefs: TSplitter;
    tsLanguages: TTabSheet;
    tsGeneral: TTabSheet;
    tsAbout: TTabSheet;
    tvPrefs: TTreeView;
    tsPrefs: TTabSheet;
    xConfig: TXMLConfig;
    xProperties: TXMLPropStorage;
    procedure actAppleAboutExecute(Sender: TObject);
    procedure actApplePrefsExecute(Sender: TObject);
    procedure actCheckForUpdateExecute(Sender: TObject);
    procedure actLocalRepoDirExecute(Sender: TObject);
    procedure actPrefsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvPrefsChange(Sender: TObject; Node: TTreeNode);
  private
    Repository: TFDNLS;
    function AddMenuItem(ToItem : TMenuItem; ActionItem : TBasicAction) : TMenuItem; overload;
    function AddMenuItem(ToItem : TMenuItem; CaptionText : TCaption) : TMenuItem; overload;
    procedure AddPrefsTree(ParentNode : TTreeNode; Pages : TPageControl);
    procedure SelectPrefsPage(Tab : TTabSheet);
    procedure CreateMainMenu;
    procedure CreatePrefsTree;
    procedure CreateAboutText;
    procedure OpenRepository(Location : String);
    procedure SoftwareUpdate(Silent : boolean);
  public

  end;

var
  mForm: TmForm;

implementation

{$I version.inc}

{$R *.lfm}

{ TmForm }

procedure TmForm.FormCreate(Sender: TObject);
begin
   // Hide some design time elements
   pcMain.ShowTabs := False;
   pcPrefs.ShowTabs := False;
   // set Program configuration file
   xConfig.Filename:= AppCfgPath + 'userdata.xml';
   // configure local repository
   OpenRepository(xConfig.GetValue('LOCAL/REPO',  ''));
   // Assign the config application files
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := FormNodePath(Self);
   // Populate UI elements
   CreateMainMenu;
   CreatePrefsTree;
   CreateAboutText;
   SoftwareUpdate(True);
end;

procedure TmForm.actAppleAboutExecute(Sender: TObject);
begin
   SelectPrefsPage(tsAbout);
end;

procedure TmForm.actApplePrefsExecute(Sender: TObject);
begin
  SelectPrefsPage(tsGeneral);
end;

procedure TmForm.actCheckForUpdateExecute(Sender: TObject);
var
  Query : String;
  SS: TStringStream;
  Client: TFPHTTPClient;
begin
  Query := 'https://up.lod.bz/' +
    StringReplace(APP_PRODUCTNAME, '-', '', [rfReplaceAll]) +
    '/' + PlatformID + '-' + APP_VERSION;
  Log(Self, 'Check for update ' + Query);
  SS := TStringStream.Create('');
  try
    Client := TFPHTTPClient.Create(nil);
    try
      try
        Client.Get(Query, SS);
      except
        on E: Exception do
           Log(Self, E.Message);
      end;
      Log(Self, SS.DataString);
    finally
      FreeAndNil(Client);
    end;
  finally
    SS.Free;
  end;
end;

procedure TmForm.actLocalRepoDirExecute(Sender: TObject);
begin
  if Repository.Path = '' then
     sdLocalRepo.InitialDir:=UserHomePath
  else
     sdLocalRepo.InitialDir:=Repository.Path;
  if sdLocalRepo.Execute then
     OpenRepository(sdLocalRepo.FileName);
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
   mMain.Items.Clear;
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

procedure TmForm.OpenRepository(Location : String);
begin
    Repository.Path:= Location;
    edLocalRepo.Text:= Repository.Path;
    xConfig.SetValue('LOCAL/REPO', Repository.Path);
    xConfig.Flush;
end;

procedure TmForm.SoftwareUpdate(Silent: boolean);
begin

end;


end.

