unit ULogin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, System.JSON,
  System.Net.HttpClient,System.Net.HttpClientComponent, System.Net.URLClient;

type
  TForm_Login = class(TForm)
    Edit_Emai: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MaskEdit_Password: TMaskEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);

    procedure PegarAccessToken;
  private
    { Private declarations }

  public
    { Public declarations }
    ACCESS_TOKEN : string;
    const
    Const_ApiKey : string = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Im15bnhsdWJ5a3lsbmNpbnR0Z2d1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjUxODg2NzAsImV4cCI6MjA4MDc2NDY3MH0.Z-zqiD6_tjnF2WLU167z7jT5NzZaG72dWH0dpQW1N-Y';

  end;

var
  Form_Login: TForm_Login;


implementation

{$R *.dfm}

uses
  Unit2;

procedure TForm_Login.PegarAccessToken;
var
  Client       : THTTPClient;
  Resp         : IHTTPResponse;
  JSONObj      : TJSONObject;
  Body         : TStringStream;
  ResponseJSON : TJSONObject;
begin
  Client := THTTPClient.Create;
  try
    // Corpo do POST em JSON
    JSONObj := TJSONObject.Create;
    JSONObj.AddPair('email', Edit_Emai.Text);
    JSONObj.AddPair('password', MaskEdit_Password.Text);

    Body := TStringStream.Create(JSONObj.ToJSON, TEncoding.UTF8);

    // Headers
    Client.CustomHeaders['Content-Type'] := 'application/json';
    Client.CustomHeaders['apikey'] := Const_ApiKey;

    // Enviar POST
    Resp := Client.Post(
      'https://mynxlubykylncinttggu.supabase.co/auth/v1/token?grant_type=password',
      Body
    );

    // Converter string para JSON
    ResponseJSON := TJSONObject.ParseJSONValue(Resp.ContentAsString) as TJSONObject;
    try
      // Extrair o access_token ✔
      ACCESS_TOKEN := ResponseJSON.GetValue<string>('access_token');
      ShowMessage('access_token carregado com sucesso');
    finally
      ResponseJSON.Free;
      Body.Free;
      JSONObj.Free;
    end;

  finally
    Client.Free;
  end;
end;

procedure TForm_Login.Button1Click(Sender: TObject);
begin
   PegarAccessToken;
   if ACCESS_TOKEN <> '' then
   begin
     Application.CreateForm(TForm_Principal, Form_Principal);
     Form_Principal.Show;
     Form_Login.Hide;  // esconde a tela de login
   end
   else
    ShowMessage('Erro ao Geral Token')
end;

end.
