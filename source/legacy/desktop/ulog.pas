unit uLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PasExt, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfLog }

  TfLog = class(TForm)
    LogText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAutoShow: boolean;
    procedure SetAutoShow(AValue: boolean);

  public
    procedure Add(AMessage : String; TimeStamp : boolean = true);
  published
    property AutoShow : boolean read FAutoShow write SetAutoShow;
  end;

var
  fLog: TfLog;

procedure Log(Sender : TPersistent; Message : String); overload;

implementation

uses uAppCfg;

var
  PreLog : TStringList;

procedure Log(Sender: TPersistent; Message: String);
begin
  if Assigned(Sender) then
    Message := Sender.GetNamePath + ': ' + Message;
  if not Assigned(fLog) then begin
     if Assigned(PreLog) then begin
          PreLog.Append(Message);
     end;
     exit;
  end;
  with fLog do begin
       if (not Visible) and (AutoShow) then Show;
       Add(Message);
  end;
end;

{$R *.lfm}

{ TfLog }

procedure TfLog.FormCreate(Sender: TObject);
begin
   GetPropertyState(Self);
   LogText.Clear;
   Add('Log started on ' + FormatDateTime('yyyy-mm-dd', Now));
   Add('');
   if Assigned(PreLog) then begin
      LogText.Append(PreLog.Text);
      Add('');
      FreeAndNil(PreLog);
   end;
   FAutoShow := False;
end;

procedure TfLog.FormHide(Sender: TObject);
begin
  SetPropertyState(Self);
end;

procedure TfLog.FormShow(Sender: TObject);
begin
  Position:=poDesigned;
end;

procedure TfLog.SetAutoShow(AValue: boolean);
begin
  if FAutoShow=AValue then Exit;
  FAutoShow:=AValue;
end;

procedure TfLog.Add(AMessage: String; TimeStamp : boolean = true);
begin
   if TimeStamp and (AMessage <> '') then
     LogText.Append(FormatDateTime('hh:mm:ss.zzz', Now) + TAB + ' ' + AMessage)
   else
     LogText.Append(AMessage);
end;

initialization
  PreLog := TStringList.Create;

end.

