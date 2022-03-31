(*

The icons used are not redistributable and Copyright Icons8, http://icons8.com

A collection of the needed icons can be freely downloaded from their website
at https://icons8.com/icons/share-collections/uRjmCz4CbAnb at 100px resolution

Once downloaded, extract the files into a subdirectory called icons8 and run
the shell script icons.sh to create the resource file need to compile

*)

unit Icons;

{$I defines.inc}

interface

uses SysUtils, StrUtils, LResources, PasExt;

const

  { 3rd Party Icon and Copyright Information }

  IconProviderCopyright = '2022 Icons8';
  IconProviderURL       = 'https://icons8.com';
  IconCollectionName    = 'APP-FDNLS';
  IconCollectionURL     = 'https://icons8.com/icons/share-collections/uRjmCz4CbAnb';
  IconPrefix            = 'icon-';

  icon_Null             = -1;
  icon_Done             = 7;
  icon_Close            = 8;
  icon_NoImage          = 12;
  icon_Good             = 14;
  icon_Warning          = 15;
  icon_Error            = 16;
  icon_New              = 17;


  IconUI : array of string = (
    'bug',                { 0 }
    'gear',               { 1 }
    'scroll',             { 2 }
    'hierarchy',          { 3 }
    'list',               { 4 }
    'add',                { 5 }
    'remove',             { 6 }
    'done',               { 7 - icon_Done }
    'close',              { 8 - icon_Close }
    'cancel',             { 9 }
    'no-edit',            { 10 }
    'edit',               { 11 }
    'no-image',           { 12 - icon_NoImage}
    'image',              { 13 }
    'check-circle',       { 14 - icon_Good }
    'error',              { 15 - icon_Warning }
    'high-priority',      { 16 - icon_Error }
    'new',                { 17 - icon_New }
    'bang',               { 18 }
    'transfer',           { 19 }
    'edit-property',
    'edit-file',
    'notification',
    'save',
    'save-all',
    'save-as',
    'form',
    'category',
    'git',
    'github',
    'gitlab',
    'project',
    'project-setup',
    'group-of-projects',
    'folder-tree',
    'hint',
    'bug',
    'dos',
    'trash-can',
    'repository',
    'commit-git',
    'compare-git',
    'merge-git',
    'language',
    'source-code',
    'fold',
    'unfold',
    'document-header',
    'document-body',
    'footer',
    'unavailable',
    'tv-off',
    'tv-on',
    'console',
    'command-line',
    'star',
    'star-filled',
    'internal',
    'process',
    'input',
    'replace',
    'download',
    'import',
    'export'

  );

  IconFlags : array of string = ();
  FlagNames : array of string = ();

implementation

procedure Initialize;
var
  I : integer;
begin
  {$I icons.lrs}
  for I := Low(IconUI) to High(IconUI) do
      IconUI[I] := IconPrefix + IconUI[I];
  SetLength(IconFlags, Length(CountryData));
  SetLength(FlagNames, Length(CountryData));
  for I := Low(IconFlags) to High(IconFlags) do begin
      IconFlags[I] := IconPrefix + FieldStr(CountryData[I],3,',');
      FlagNames[I] := FieldStr(CountryData[I],4,-1,',');
      if FlagNames[I] = '' then
        FlagNames[I] := AnsiProperCase(
          StringReplace(
            FieldStr(CountryData[I],3,','), '-', ' ', [rfReplaceAll]
          ), [SPACE]);
  end;
end;

initialization

   Initialize;

end.

