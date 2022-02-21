unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uSettings;

{$I version.inc}

type
  TfMain = class(TForm)
  private

  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

initialization
    WriteLn(GetEnvironmentVariable('HOME') + '/Library/Application Support/' + APP_COMPANYNAME);
end.

