unit uLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PasExt, PUIExt, Forms, Controls, Graphics, Dialogs,
  XMLPropStorage, StdCtrls, IDEWindowIntf;

type

  { TfLog }

  TfLog = class(TForm)
    Memo1: TMemo;
    xProperties: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  fLog: TfLog;

procedure Log(Sender : TForm; Message : String); overload;

implementation

procedure Log(Sender: TForm; Message: String);
begin
  if not fLog.Visible then fLog.Show;
  fLog.Memo1.Lines.Add(Message);
end;

{$R *.lfm}

{ TfLog }


procedure TfLog.FormCreate(Sender: TObject);
begin
   xProperties.FileName := AppCfgFile;
   xProperties.RootNodePath := FormNodePath(Self);
   Memo1.Clear;
end;

end.

