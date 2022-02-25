unit FormConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, AppConfig;

type
  TFormConfig = class(TAppConfig)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I formconfig_icon.lrs}
  RegisterComponents('AppKit',[TFormConfig]);
end;

end.
