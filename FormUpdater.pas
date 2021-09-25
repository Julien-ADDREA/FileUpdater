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

uses IdHTTP, JSON;

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
  i: Integer;
procedure downloadFragment(fragment: TJSONObject);
begin
  if FileExists(tempPath + '\' + fragment.GetValue('name') + '.frag') then
  begin
    // Check file
  end
  else
  begin
    // Download file
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
    if not isUpToDate(version, (JSONObj.GetValue('version') as TJSONValue).Value) then
    begin
      tempPath := GetEnvironmentVariable('APPDATA') + '\' + (JSONObj.GetValue('app') as TJSONValue).Value;
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
