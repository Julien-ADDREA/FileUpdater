unit FormUpdater;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, IdComponent, JSON, uUpdater;

type
  TForm1 = class(TForm)
    LabelAction: TLabel;
    ProgressBarAction: TProgressBar;
    LabelDetails: TLabel;
    ProgressBarGlobal: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure OnError(Error: string);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
  TRoutineThread = class(TThread)
    AppJSON: TJSONObject;
    iii: integer;
  protected
    procedure OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure Execute; override;
    procedure OnError(Error: string);
  end;

var
  Form1: TForm1;
  RoutineThread: TRoutineThread;
  Updater: TUpdater;

const
  version = '1.0.1'; // Devra être récupéré via un fichier repère

implementation

uses IdHTTP, IdHashMessageDigest, idHash, System.Math;

{$R *.dfm}

{$region 'Routine Thread'}

{$region 'Routine Thread : OnWorkBegin'}
procedure TRoutineThread.OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  if AWorkMode = wmRead then
  begin
    Form1.ProgressBarAction.Max := AWorkCountMax;
    Form1.ProgressBarAction.Position := 0;
  end;
end;
{$endregion}

{$region 'Routine Thread : OnWork'}
procedure TRoutineThread.OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if AWorkMode = wmRead then
  begin
    Form1.ProgressBarGlobal.Position := (StrToInt(AppJSON.GetValue('block').Value) * iii) + AWorkCount;
    Form1.ProgressBarAction.Position := AWorkCount ;
  end;
end;
{$endregion}

{$region 'Routine Thread : OnWorkEnd'}
procedure TRoutineThread.OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if not (iii + 1 = Ceil(StrToInt(AppJSON.GetValue('size').Value) / StrToInt(AppJSON.GetValue('block').Value))) then
    Form1.ProgressBarAction.Position := 0;
end;
{$endregion}

{$region 'Routine Thread : Execute'}
procedure TRoutineThread.Execute;
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
  JSONObj: TJSONObject;
  JSONApp: TJSONObject;
  tempPath: String;
  InStream, OutStream: TFileStream;

  fragmentsCount: integer;
  i: Integer;
procedure downloadFragment(fragment: TJSONObject);
var
  filename: String;
function MD5(const fileName: String): String;
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
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
  _IdHTTP := TIdHTTP.Create(nil);
  _MS := TMemoryStream.Create;
  try
    _IdHTTP.OnWorkBegin := OnWorkBegin;
    _IdHTTP.OnWork := OnWork;
    _IdHTTP.OnWorkEnd := OnWorkEnd;
    Synchronize(procedure begin Form1.ProgressBarGlobal.Style := pbstNormal; end);
    Synchronize(procedure begin Form1.ProgressBarAction.Style := pbstNormal; end);
    Form1.ProgressBarGlobal.Position := StrToInt(JSONApp.GetValue('block').Value) * i;
    Form1.LabelAction.Caption := 'Téléchargement des fragments ...';
    Form1.LabelDetails.Caption := fragment.GetValue('part').Value + '/' + IntToStr(fragmentsCount);
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
    if not (Form1.LabelAction.Caption = 'Reprise du téléchargement ...') then
      Form1.LabelAction.Caption := 'Reprise du téléchargement ...';
    if not (MD5(tempPath + '\' + filename) = fragment.GetValue('hash').Value) then
    begin
      // Téléchargement
      download(filename);
    end
    else
    begin
      Form1.ProgressBarGlobal.Position := StrToInt(JSONApp.GetValue('block').Value) * i + 1;
    end;
  end
  else
  begin
    // Téléchargement
    download(filename);
  end;
end;
begin
  Form1.LabelAction.Caption := 'Recherche de mise à jour ...';
  Synchronize(procedure begin Form1.ProgressBarGlobal.Style := Form1.ProgressBarAction.Style := pbstMarquee; end);
//  Synchronize(procedure begin Form1.ProgressBarAction.Style := pbstMarquee; end);
  try
//    JSONObj := TJSONObject.ParseJSONValue(Updater.getJSON) as TJSONObject;
    JSONApp := JSONObj.GetValue('app') as TJSONObject;
    AppJSON := JSONApp;
    if not isUpToDate(version, JSONApp.GetValue('version').Value) then
    begin
      tempPath := GetEnvironmentVariable('APPDATA') + '\' + JSONApp.GetValue('name').Value;
      fragmentsCount := (JSONObj.GetValue('fragments') as TJSONArray).Count;
      forcedirectories(tempPath);
      Form1.ProgressBarGlobal.Max := StrToInt(JSONApp.GetValue('size').Value);
      for i := 0 to (JSONObj.GetValue('fragments') as TJSONArray).Count - 1 do
      begin
        iii := i;
        downloadFragment((JSONObj.GetValue('fragments') as TJSONArray).items[i] as TJSONObject);
      end;
      Form1.LabelAction.Caption := 'Préparation de la mise à jour ...';
      OutStream := TFileStream.Create(tempPath + '\' + 'update.frag', fmCreate);
      for i := 0 to (JSONObj.GetValue('fragments') as TJSONArray).Count - 1 do
      begin
        InStream := TFileStream.Create(tempPath + '\' + ((JSONObj.GetValue('fragments') as TJSONArray).items[i] as TJSONObject).GetValue('part').Value + '.frag', fmOpenRead);
        try
          OutStream.CopyFrom(InStream, InStream.Size);
        finally
          InStream.Free;
        end;
      end;
    end;
    Form1.LabelAction.Caption := 'Terminé !';
    Form1.LabelDetails.Caption := 'Votre client est à jour !';
    Synchronize(procedure begin Form1.ProgressBarGlobal.Style := pbstNormal; end);
    Synchronize(procedure begin Form1.ProgressBarAction.Style := pbstNormal; end);
    Form1.ProgressBarGlobal.Position := Form1.ProgressBarGlobal.Max;
    Form1.ProgressBarAction.Position := Form1.ProgressBarAction.Max;
  finally
    OutStream.Free;
    self.Terminate;
  end;
end;
{$endregion}

{$region 'Routine Thread : OnError'}
procedure TRoutineThread.OnError(Error: string);
begin
//  Application.MessageBox(PChar(Error), 'Erreur', MB_OK + MB_ICONERROR);
//  Form1.LabelAction.Caption := 'Erreur : ' + Error;
end;
{$endregion}

{$endregion}

procedure TForm1.OnError(Error: string);
begin
  Application.MessageBox(PChar(Error), 'Erreur', MB_OK + MB_ICONERROR);
//  Form1.LabelAction.Caption := 'Erreur : ' + Error;
end;
procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
//  RoutineThread := TRoutineThread.Create(True);
//  RoutineThread.Priority := tpHighest;
//  RoutineThread.Start;
  Updater := TUpdater.Create(version);
  Updater.OnError := Form1.OnError;
  Updater.initialize;
  if not Updater.IsUpToDate() then
  begin
    for i := 0 to Length(Updater.Update.Fragments) do
    begin
      if not Updater.IsValidFragment(Updater.Update.Fragments[i]) then
      begin
        //
      end;
    end;
  end;
end;

end.
