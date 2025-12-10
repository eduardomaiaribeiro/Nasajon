unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.JSON,
  System.Net.HttpClient, System.Net.URLClient, System.Generics.Collections,
  System.Character, System.Math;

type
  TIBGEMunicipio = class
  public
    IdIBGE   : Integer;
    Nome     : string;
    NomeNorm : string;
    UF       : string;
    Regiao   : string;
  end;

  TInputLinha = record
    MunicipioInput : string;
    PopulacaoInput : Int64;
    MunicipioIBGE  : string;
    UF             : string;
    Regiao         : string;
    IdIBGE         : Integer;
    Status         : string;
  end;

  TRegiaoStats = record
    SomaPop : Int64;
    Count   : Integer;
  end;

  TForm_Principal = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);

  private
    procedure CarregarMunicipiosIBGE(AMunicipios: TObjectList<TIBGEMunicipio>);
    procedure LerInputCSV(const FileName: string; ALista: TList<TInputLinha>);
    function Levenshtein(const S1, S2: string): Integer;
    procedure MatchMunicipio(const NomeInput: string;
      AMunicipiosIBGE: TObjectList<TIBGEMunicipio>; out MunicipioIBGE, UF,
      Regiao: string; out IdIBGE: Integer; out Status: string);
    function NormalizeName(const S: string): string;
    procedure Processar;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Principal: TForm_Principal;

implementation

{$R *.dfm}

function TForm_Principal.NormalizeName(const S: string): string;
var
  C: Char;
  R: string;
begin
  R := '';
  for C in S.ToUpper do
  begin
    case C of
      'Á','À','Ã','Â': R := R + 'A';
      'É','È','Ê':     R := R + 'E';
      'Í','Ì','Î':     R := R + 'I';
      'Ó','Ò','Õ','Ô': R := R + 'O';
      'Ú','Ù','Û':     R := R + 'U';
      'Ç':             R := R + 'C';
      'Ä','Å':         R := R + 'A';
      'Ö':             R := R + 'O';
      'Ü':             R := R + 'U';
    else
      if not (C in [#9, #10, #13, ' ']) then
        R := R + C;
    end;
  end;
  Result := R;
end;

function TForm_Principal.Levenshtein(const S1, S2: string): Integer;
var
  Len1, Len2, I, J, Cost: Integer;
  D: array of array of Integer;
begin
  Len1 := Length(S1);
  Len2 := Length(S2);
  SetLength(D, Len1 + 1, Len2 + 1);

  for I := 0 to Len1 do
    D[I, 0] := I;
  for J := 0 to Len2 do
    D[0, J] := J;

  for I := 1 to Len1 do
    for J := 1 to Len2 do
    begin
      if S1[I] = S2[J] then
        Cost := 0
      else
        Cost := 1;

      D[I, J] := Min(
        Min(D[I - 1, J] + 1,      // deleção
            D[I, J - 1] + 1),     // inserção
        D[I - 1, J - 1] + Cost    // substituição
      );
    end;

  Result := D[Len1, Len2];
end;

// Carrega TODOS municípios do IBGE em memória
procedure TForm_Principal.CarregarMunicipiosIBGE(AMunicipios: TObjectList<TIBGEMunicipio>);
var
  Client   : THTTPClient;
  Resp     : IHTTPResponse;
  JSONArr  : TJSONArray;
  Item     : TJSONValue;
  Obj      : TJSONObject;
  Nome, UF, Regiao: string;
  Id       : Integer;
  Microrregiao, Mesorregiao, UFObj, RegiaoObj: TJSONObject;
begin
  Client := THTTPClient.Create;
  try

    Memo1.Lines.Add('Baixando lista geral de municipios do IBGE...');
    Resp := Client.Get('https://servicodados.ibge.gov.br/api/v1/localidades/municipios');
    if Resp.StatusCode <> 200 then
      raise Exception.CreateFmt('Erro na API do IBGE: HTTP %d', [Resp.StatusCode]);

    JSONArr := TJSONObject.ParseJSONValue(Resp.ContentAsString) as TJSONArray;
    try
      for Item in JSONArr do
      begin
        Obj := Item as TJSONObject;
        if Obj = nil then
          Continue;

        Id   := Obj.GetValue<Integer>('id');
        Nome := Obj.GetValue<string>('nome');

        // AQUI: usar cast 'as TJSONObject' em vez de GetValue<TJSONObject>
        try
          Microrregiao := Obj.GetValue('microrregiao') as TJSONObject;
        except

        end;
        if Microrregiao = nil then Continue;

        Mesorregiao  := Microrregiao.GetValue('mesorregiao') as TJSONObject;
        if Mesorregiao = nil then Continue;

        UFObj        := Mesorregiao.GetValue('UF') as TJSONObject;
        if UFObj = nil then Continue;

        RegiaoObj    := UFObj.GetValue('regiao') as TJSONObject;
        if RegiaoObj = nil then Continue;

        UF     := UFObj.GetValue<string>('sigla');
        Regiao := RegiaoObj.GetValue<string>('nome');

        var M := TIBGEMunicipio.Create;
        M.IdIBGE   := Id;
        M.Nome     := Nome;
        M.NomeNorm := NormalizeName(Nome);
        M.UF       := UF;
        M.Regiao   := Regiao;

        AMunicipios.Add(M);
      end;
      Memo1.Lines.Add('Total de municipios IBGE carregados: ' + AMunicipios.Count.ToString);
    finally
      JSONArr.Free;
    end;
  finally
    Client.Free;
  end;
end;


// Lê o arquivo input.csv
procedure TForm_Principal.LerInputCSV(const FileName: string; ALista: TList<TInputLinha>);
var
  SL: TStringList;
  I: Integer;
  Line, Nome, PopStr: string;
  CommaPos: Integer;
  Item: TInputLinha;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName, TEncoding.UTF8);
    if SL.Count = 0 then
      Exit;

    // pular header
    for I := 1 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);
      if Line = '' then
        Continue;

      CommaPos := Line.IndexOf(',');
      if CommaPos <= 0 then
        Continue;

      Nome   := Line.Substring(0, CommaPos);
      PopStr := Line.Substring(CommaPos + 1);

      FillChar(Item, SizeOf(Item), 0);
      Item.MunicipioInput := Nome.Trim;
      Item.PopulacaoInput := StrToInt64Def(Trim(PopStr), 0);
      Item.MunicipioIBGE  := '';
      Item.UF             := '';
      Item.Regiao         := '';
      Item.IdIBGE         := 0;
      Item.Status         := 'NAO_ENCONTRADO';

      ALista.Add(Item);
    end;
  finally
    SL.Free;
  end;
end;

// Faz o matching de um município de entrada com a base IBGE
procedure TForm_Principal.MatchMunicipio(
  const NomeInput: string;
  AMunicipiosIBGE: TObjectList<TIBGEMunicipio>;
  out MunicipioIBGE, UF, Regiao: string;
  out IdIBGE: Integer;
  out Status: string);
var
  NomeNorm: string;
  M: TIBGEMunicipio;
  ListMatches: TList<TIBGEMunicipio>;
  Dist, BestDist: Integer;
begin
  MunicipioIBGE := '';
  UF           := '';
  Regiao       := '';
  IdIBGE       := 0;
  Status       := 'NAO_ENCONTRADO';

  NomeNorm := NormalizeName(NomeInput);

  // 1) primeiro tenta match exato por nome normalizado
  ListMatches := TList<TIBGEMunicipio>.Create;
  try
    for M in AMunicipiosIBGE do
      if M.NomeNorm = NomeNorm then
        ListMatches.Add(M);

    if ListMatches.Count = 1 then
    begin
      M := ListMatches[0];
      MunicipioIBGE := M.Nome;
      UF            := M.UF;
      Regiao        := M.Regiao;
      IdIBGE        := M.IdIBGE;
      Status        := 'OK';
      Exit;
    end
    else if ListMatches.Count > 1 then
    begin
      Status := 'AMBIGUO';
      Exit;
    end;
  finally
    ListMatches.Free;
  end;

  // 2) se não achou exato, tenta Levenshtein (fuzzy)
  BestDist := MaxInt;
  ListMatches := TList<TIBGEMunicipio>.Create;
  try
    for M in AMunicipiosIBGE do
    begin
      Dist := Levenshtein(NomeNorm, M.NomeNorm);
      if Dist < BestDist then
      begin
        BestDist := Dist;
        ListMatches.Clear;
        ListMatches.Add(M);
      end
      else if Dist = BestDist then
        ListMatches.Add(M);
    end;

    // limiar de distância aceitável (2 ou 3 funciona bem)
    if (BestDist <= 3) and (ListMatches.Count = 1) then
    begin
      M := ListMatches[0];
      MunicipioIBGE := M.Nome;
      UF            := M.UF;
      Regiao        := M.Regiao;
      IdIBGE        := M.IdIBGE;
      Status        := 'OK';
    end
    else if (BestDist <= 3) and (ListMatches.Count > 1) then
      Status := 'AMBIGUO'
    else
      Status := 'NAO_ENCONTRADO';
  finally
    ListMatches.Free;
  end;
end;

// Gera resultado.csv e calcula estatísticas
procedure TForm_Principal.Processar;
var
  MunicipiosIBGE : TObjectList<TIBGEMunicipio>;
  LinhasInput    : TList<TInputLinha>;
  I              : Integer;
  Linha          : TInputLinha;
  OutMunicipio, OutUF, OutRegiao: string;
  OutIdIBGE: Integer;
  OutStatus: string;
  SL: TStringList;
  TotalMunicipios,
  TotalOK,
  TotalNaoEncontrado,
  TotalErroAPI: Integer;
  PopTotalOK: Int64;
  RegiaoStats: TDictionary<string, TRegiaoStats>;
  RStat: TRegiaoStats;
  Key: string;
begin
  MunicipiosIBGE := TObjectList<TIBGEMunicipio>.Create(True);
  LinhasInput    := TList<TInputLinha>.Create;
  RegiaoStats    := TDictionary<string, TRegiaoStats>.Create;
  SL             := TStringList.Create;
  try
    try
      // 1) Carregar base IBGE
      CarregarMunicipiosIBGE(MunicipiosIBGE);

      // 2) Ler input.csv
      if OpenDialog1.Execute then
      begin
        LerInputCSV(OpenDialog1.FileName, LinhasInput);
      end
      else
      begin
        ShowMessage('Nenhum arquivo selecionado.');
        Exit;
      end;


     // LerInputCSV('input.csv', LinhasInput);

      // 3) Processar cada linha
      TotalMunicipios    := LinhasInput.Count;
      TotalOK            := 0;
      TotalNaoEncontrado := 0;
      TotalErroAPI       := 0;
      PopTotalOK         := 0;

      for I := 0 to LinhasInput.Count - 1 do
      begin
        Linha := LinhasInput[I];

        try
          MatchMunicipio(
            Linha.MunicipioInput,
            MunicipiosIBGE,
            OutMunicipio,
            OutUF,
            OutRegiao,
            OutIdIBGE,
            OutStatus
          );
        except
          on E: Exception do
          begin
            OutStatus := 'ERRO_API';
            OutMunicipio := '';
            OutUF       := '';
            OutRegiao   := '';
            OutIdIBGE   := 0;
          end;
        end;

        Linha.MunicipioIBGE := OutMunicipio;
        Linha.UF            := OutUF;
        Linha.Regiao        := OutRegiao;
        Linha.IdIBGE        := OutIdIBGE;
        Linha.Status        := OutStatus;
        LinhasInput[I]      := Linha;

        // Estatísticas
        if OutStatus = 'OK' then
        begin
          Inc(TotalOK);
          PopTotalOK := PopTotalOK + Linha.PopulacaoInput;

          if RegiaoStats.TryGetValue(OutRegiao, RStat) then
          begin
            RStat.SomaPop := RStat.SomaPop + Linha.PopulacaoInput;
            Inc(RStat.Count);
          end
          else
          begin
            RStat.SomaPop := Linha.PopulacaoInput;
            RStat.Count   := 1;
          end;
          RegiaoStats.AddOrSetValue(OutRegiao, RStat);
        end
        else if OutStatus = 'NAO_ENCONTRADO' then
          Inc(TotalNaoEncontrado)
        else if OutStatus = 'ERRO_API' then
          Inc(TotalErroAPI);
      end;

      // 4) Gerar resultado.csv
      SL.Add('municipio_input,populacao_input,municipio_ibge,uf,regiao,id_ibge,status');
      for Linha in LinhasInput do
      begin
        SL.Add(
          Format('%s,%d,%s,%s,%s,%d,%s',
            [Linha.MunicipioInput,
             Linha.PopulacaoInput,
             Linha.MunicipioIBGE,
             Linha.UF,
             Linha.Regiao,
             Linha.IdIBGE,
             Linha.Status])
        );
      end;
      SL.SaveToFile('resultado.csv', TEncoding.UTF8);

      // 5) Mostrar estatísticas no console
      Memo1.Lines.Add('total_municipios: ' + TotalMunicipios.ToString );
      Memo1.Lines.Add('total_ok: ' + TotalOK.ToString);
      Memo1.Lines.Add('total_nao_encontrado: ' + TotalNaoEncontrado.ToString);
      Memo1.Lines.Add('total_erro_api: ' + TotalErroAPI.ToString);
      Memo1.Lines.Add('pop_total_ok: ' + PopTotalOK.ToString);

      Memo1.Lines.Add('medias_por_regiao:');
      for Key in RegiaoStats.Keys do
      begin
        RStat := RegiaoStats[Key];
        if RStat.Count > 0 then
          // Writeln(Format('  %s: %.2f', [Key, RStat.SomaPop / RStat.Count]));
      end;

      ShowMessage('Prcesso finalizado, arquivo resultado.csv salvo com sucesso em: ..\Win32\Debug\resultado.csv');

    except
      on E: Exception do
      begin
        ShowMessage('Erro geral: ' + E.Message);
      end;
    end;
  finally
    SL.Free;
    RegiaoStats.Free;
    LinhasInput.Free;
    MunicipiosIBGE.Free;
  end;
end;

procedure TForm_Principal.Button1Click(Sender: TObject);
begin
  try
    Processar;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.
