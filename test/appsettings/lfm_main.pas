unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  u_app_settings,handykar_const;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    fOptions: THandykarSettings;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  fOptions:= THandykarSettings.Create;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Database name: '+fOptions.Databasename);
  Memo1.Lines.Add('Backup name: '+fOptions.Backupname);
  Memo1.Lines.Add('Perform batchupdates: '+BooleanText[fOptions.BatchUpdates]);
  fOptions.Free;
end;

end.

