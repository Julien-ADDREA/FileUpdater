unit FormUpdater;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    LabelAction: TLabel;
    ProgressBarAction: TProgressBar;
    LabelDetails: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure searchUpdate();
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

const
  version = '1.0.1'; // Devra être récupéré via un fichier repère

implementation

uses IdHTTP, JSON, IdHashMessageDigest, idHash;

{$R *.dfm}

procedure TForm1.searchUpdate();
function MemoryStreamToString(M: TMemoryStream): string;
begin
  SetString(Result, PAnsiChar(M.Memory), M.Size);
end;
function isUpToDate(localVersion: String; remoteVersion: String): Boolean;
begin
  Result := false;
  if localVersion = remoteVersion then Result := true;
end;
var
  IdHTTP: TIdHTTP;
  MS: TMemoryStream;
  JSONObj: TJSONObject;
  tempPath: String;
  fragmentsCount: integer;
  i: Integer;
procedure downloadFragment(fragment: TJSONObject);
var
  filename: String;
function MD5(const fileName: String) : String;
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
  hash: T4x4LongWordRecord;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite);
  try
    result := idmd5.HashStreamAsHex(fs);
  finally
    fs.Free;
    idmd5.Free;
  end;
end;
procedure download(fileName: String);
var
  _IdHTTP: TIdHTTP;
  _MS: TMemoryStream;
begin
  _IdHTTP := TIdHTTP.Create(Self);
  _MS := TMemoryStream.Create;
  try
    _IdHTTP.Get('http://updater.to/HASH-HASH-HASH-HASH-HASH/' + fileName, _MS);
    _MS.SaveToFile(tempPath + '\' + fileName);
  finally
    _IdHTTP.Free;
    _MS.Free;
  end;
end;
begin
  filename := fragment.GetValue('part').Value + '.frag';
  if FileExists(tempPath + '\' + filename) then
  begin
    // Vérification du hash
//    if not (MD5(tempPath + '\' + filename) = fragment.GetValue('hash').Value) then download(filename)
  end
  else
  begin
    download(filename);
  end;
end;
begin
  LabelAction.Caption := 'Recherche de mises à jours ...';
  ProgressBarAction.Style := pbstMarquee;
  IdHTTP := TIdHTTP.Create(Self);
  MS := TMemoryStream.Create;
  try
    IdHTTP.Get('http://updater.to/HASH-HASH-HASH-HASH-HASH/update.json', MS);
    JSONObj := TJSONObject.ParseJSONValue(MemoryStreamToString(MS)) as TJSONObject;
    if not isUpToDate(version, JSONObj.GetValue('version').Value) then
    begin
      tempPath := GetEnvironmentVariable('APPDATA') + '\' + JSONObj.GetValue('app').Value;
      fragmentsCount := (JSONObj.GetValue('fragments') as TJSONArray).Count;
      forcedirectories(tempPath);
      for i := 0 to (JSONObj.GetValue('fragments') as TJSONArray).Count - 1 do
      begin
        downloadFragment((JSONObj.GetValue('fragments') as TJSONArray).items[i] as TJSONObject);
      end;
    end
    else
    begin
      LabelAction.Caption := 'Votre client est à jour !';
    end;
    // (JSONObj.GetValue('fragments') as TJSONArray).Count.ToString
    // ShowMessage((JSONObj.GetValue('fragments') as TJSONArray).Count.ToString);
    // ShowMessage((JSONObj.GetValue('file') as TJSONValue).Value);
  finally
    IdHTTP.Free;
    ms.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  searchUpdate();
end;

end.
