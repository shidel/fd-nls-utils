unit uMain;

{$I defines.inc}

interface

uses
  Classes, SysUtils, PasExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, XMLConf;

type

  { TfMain }

  TfMain = class(TForm)
    XMLConfig1: TXMLConfig;
    XMLPropStorage: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
   XMLPropStorage.FileName := AppCfgFile;
end;

end.

