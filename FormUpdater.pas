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
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
  TRoutineThread = class(TThread)
  protected
    procedure OnStart;
    procedure OnUpdate;
    procedure OnDiscover(Fragment: TFragment);
    procedure OnDownload(Count: Integer; Total: Integer);
    procedure OnMerge;
    procedure OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure OnError(Error: string);
    procedure OnEnd;
    procedure Execute; override;
//    procedure OnError(Error: string);
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

{$region 'Routine Thread : OnStart'}
procedure TRoutineThread.OnStart;
begin
  Form1.LabelAction.Caption := 'Recherche de mise à jour ...';
  Synchronize(
    procedure
    begin
      Form1.ProgressBarGlobal.Style := pbstMarquee;
      Form1.ProgressBarAction.Style := pbstMarquee;
    end
  );
end;
{$endregion}

{$region 'Routine Thread : OnUpdate'}
procedure TRoutineThread.OnUpdate;
begin
  Form1.ProgressBarGlobal.Max := Updater.Update.App.FileSize;
  Synchronize(
    procedure
    begin
      Form1.ProgressBarGlobal.Style := pbstNormal;
    end
  );
end;
{$endregion}

{$region 'Routine Thread : OnDiscover'}
procedure TRoutineThread.OnDiscover(Fragment: TFragment);
begin
  Form1.LabelAction.Caption := 'Récupération des fragments ...';
  Form1.LabelDetails.Caption := Fragment.Part + '/' + IntToStr(Length(Updater.Update.Fragments));
end;
{$endregion}

{$region 'Routine Thread : OnDownload'}
procedure TRoutineThread.OnDownload(Count: Integer; Total: Integer);
begin
  Form1.LabelAction.Caption := 'Téléchargement des fragments ...';
  Form1.LabelDetails.Caption := IntToStr(Count) + '/' + IntToStr(Total);
  Synchronize(
    procedure
    begin
      Form1.ProgressBarGlobal.Style := pbstNormal;
      Form1.ProgressBarAction.Style := pbstNormal;
    end
  );
end;
{$endregion}

{$region 'Routine Thread : OnMerge'}
procedure TRoutineThread.OnMerge;
begin
  Form1.LabelAction.Caption := 'Fuzion des fragments ...';
//  Form1.LabelDetails.Caption := IntToStr(Count) + '/' + IntToStr(Total);
//  Synchronize(
//    procedure
//    begin
//      Form1.ProgressBarGlobal.Style := pbstNormal;
//      Form1.ProgressBarAction.Style := pbstNormal;
//    end
//  );
end;
{$endregion}

{$region 'Routine Thread : OnWorkBegin'}
procedure TRoutineThread.OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  if AWorkMode = wmRead then
  begin
    Form1.ProgressBarGlobal.Position := Updater.Update.App.BlockSize * (Updater.DownloadedFragments + Updater.VerifiedFragments);
    Form1.LabelDetails.Caption := IntToStr(Updater.DownloadedFragments + 1) + '/' + IntToStr(Length(Updater.Update.Fragments) - Updater.VerifiedFragments);
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
    Form1.ProgressBarGlobal.Position := (Updater.DownloadedFragments + Updater.VerifiedFragments) * Updater.Update.App.BlockSize + AWorkCount;
    Form1.ProgressBarAction.Position := AWorkCount;
  end;
end;
{$endregion}

{$region 'Routine Thread : OnWorkEnd'}
procedure TRoutineThread.OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if not (Updater.DownloadedFragments + Updater.VerifiedFragments = Ceil(Updater.Update.App.FileSize / Updater.Update.App.BlockSize)) then
    Form1.ProgressBarAction.Position := 0;
end;
{$endregion}

{$region 'Routine Thread : OnError'}
procedure TRoutineThread.OnError(Error: string);
begin
  Application.MessageBox(PChar(Error), 'Erreur', MB_OK + MB_ICONERROR);
end;
{$endregion}

{$region 'Routine Thread : OnEnd'}
procedure TRoutineThread.OnEnd();
begin
  //
end;
{$endregion}

{$region 'Routine Thread : Execute'}
procedure TRoutineThread.Execute;
begin
  Updater := TUpdater.Create(version);
  Updater.OnStart := OnStart;
  Updater.OnUpdate := OnUpdate;
  Updater.OnDiscover := OnDiscover;
  Updater.OnDownload := OnDownload;
  Updater.OnMerge := OnMerge;
  Updater.OnWorkBegin := OnWorkBegin;
  Updater.OnWork := OnWork;
  Updater.OnWorkEnd := OnWorkEnd;
  Updater.OnError := OnError;
  Updater.OnEnd := OnEnd;
  Updater.CheckUpdate();
end;
{$endregion}

{$endregion}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RoutineThread := TRoutineThread.Create(True);
  RoutineThread.Priority := tpHighest;
  RoutineThread.Start;
end;

end.
