program PNasajon;

uses
  Vcl.Forms,
  ULogin in 'ULogin.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_Login, Form_Login);
  Application.Run;
end.
