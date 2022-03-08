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

uses LResources, PasExt;

const

  { 3rd Party Icon and Copyright Information }

  IconProviderCopyright = '2022 Icons8';
  IconProviderURL       = 'https://icons8.com';
  IconCollectionName    = 'APP-FDNLS';
  IconCollectionURL     = 'https://icons8.com/icons/share-collections/uRjmCz4CbAnb';
  IconPrefix            = 'icon100-';

  IconUI : array of string = (
    'bug',                { 0 }
    'gear',               { 1 }
    'scroll',             { 2 }
    'hierarchy',          { 3 }
    'list',               { 4 }
    'add',                { 5 }
    'remove',
    'error',
    'notification',
    'check-circle',
    'save',
    'save-all',
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
    'language'
  );

  IconFlags : array of string = ();

implementation

procedure Initialize;
var
  I : integer;
begin
  {$I icons.lrs}
  for I := Low(IconUI) to High(IconUI) do
      IconUI[I] := 'icons100-' + IconUI[I];
  SetLength(IconFlags, Length(CountryData));
  for I := Low(IconFlags) to High(IconFlags) do
      IconFlags[I] := 'icons100-' + FieldStr(CountryData[I],3,',');
end;

initialization

   Initialize;

end.

