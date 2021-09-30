unit uUpdater;

interface

uses IdComponent;

type
  // TApp
  TApp = class
    Name: string;
    Version: string;
    FileSize: integer;
    BlockSize: integer;
  end;

  // TFragment
  TFragment = class
    Part: string;
    Hash: string;
    IsValid: Boolean;
  end;

  // TJSON
  TJSON = class
    App: TApp;
    Fragments: array of TFragment;
  end;

  // Events
  TErrorEvent = procedure (Error: string) of object;
  TStartEvent = procedure of object;
  TUpdateEvent = procedure of object;
  TDiscoverEvent = procedure (Fragment: TFragment) of object;
  TDownloadEvent = procedure (Count: Integer; Total: Integer) of object;
  TMergeEvent = procedure of object;
  TWorkBeginEvent = procedure (ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64) of object;
  TWorkEvent = procedure (ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64) of object;
  TWorkEndEvent = procedure (ASender: TObject; AWorkMode: TWorkMode) of object;
  TEndEvent = procedure of object;

  TUpdater = class
    VerifiedFragments: Integer;
    DownloadedFragments: Integer;
    constructor Create(Version: string);
    procedure CheckUpdate;
    function Initialize: Boolean;
    function IsUpToDate: Boolean;
    function IsValidFragment(Fragment: TFragment): Boolean;
    procedure DownloadFragment(Fragment: TFragment);
    procedure DiscoverFragments;
    procedure DownloadFragments;
    procedure MergeFragments;
  private
    FVersion: string;
    FDownloadDir: string;
    FUpdate: TJSON;
    FCurrentFragment: TFragment;
    FOnStart: TStartEvent;
    FOnUpdate: TUpdateEvent;
    FOnDiscover: TDiscoverEvent;
    FOnDownload: TDownloadEvent;
    FOnMerge: TMergeEvent;
    FOnWorkBegin: TWorkBeginEvent;
    FOnWork: TWorkEvent;
    FOnWorkEnd: TworkEndEvent;
    FOnError: TErrorEvent;
    FOnEnd: TEndEvent;
  public
    property Update: TJSON read FUpdate;
    property CurrentFragment: TFragment read FCurrentFragment;
    // Events
    property OnStart: TStartEvent read FOnStart write FOnStart;
    property OnUpdate: TUpdateEvent read FOnUpdate write FOnUpdate;
    property OnDiscover: TDiscoverEvent read FOnDiscover write FOnDiscover;
    property OnDownload: TDownloadEvent read FOnDownload write FOnDownload;
    property OnMerge: TMergeEvent read FOnMerge write FOnMerge;
    property OnWorkBegin: TWorkBeginEvent read FOnWorkBegin write FOnWorkBegin;
    property OnWork: TWorkEvent read FOnWork write FOnWork;
    property OnWorkEnd: TWorkEndEvent read FOnWorkEnd write FOnWorkEnd;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnEnd: TEndEvent read FOnEnd write FOnEnd;
  end;

implementation

uses IdHTTP, System.Classes, System.SysUtils, JSON, System.Generics.Collections,
  IdHashMessageDigest, idHash;

{$region 'TUpdater'}

{$region 'TUpdater : Create'}
constructor TUpdater.Create(Version: string);
begin
  FVersion := Version;
  VerifiedFragments := 0;
  DownloadedFragments := 0;
end;
{$endregion}

{$region 'TUpdater : CheckUpdate'}
procedure TUpdater.CheckUpdate;
begin
  if Initialize then
  begin
    if not IsUpToDate then
    begin
      DiscoverFragments;
      DownloadFragments;
      MergeFragments;
    end;
  end
  else
  begin
    //
  end;
end;
{$endregion}

{$region 'TUpdater : Initialize'}
function TUpdater.Initialize: Boolean;
var
  IDHTTP: TIdHTTP;
  MS: TMemoryStream;
  JSONResponse: string;
  JSONObject: TJSONObject;
  JSONApp: TJSONObject;
  JSONFragments: TJSONArray;
  JSONFragment: TJSONObject;
  i: integer;
begin
  Result := False;
  IdHTTP := TIdHTTP.Create(nil);
  MS := TMemoryStream.Create;
  FUpdate := TJSON.Create;
  try
    try
      // OnStart
      if Assigned(FOnStart) then FOnStart;
      // HTTP
      IdHTTP.Get('http://updater.to/HASH-HASH-HASH-HASH-HASH/update.json', MS);
      SetString(JSONResponse, PAnsiChar(MS.Memory), MS.Size);
      JSONObject := TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;
      // App
      JSONApp := JSONObject.GetValue('app') as TJSONObject;
      FUpdate.App := TApp.Create;
      FUpdate.App.Name := JSONApp.GetValue('name').Value;
      FUpdate.App.Version := JSONApp.GetValue('version').Value;
      FUpdate.App.FileSize := StrToInt(JSONApp.GetValue('fileSize').Value);
      FUpdate.App.BlockSize := StrToInt(JSONApp.GetValue('blockSize').Value);
      // Fragments
      JSONFragments := JSONObject.GetValue('fragments') as TJSONArray;
      SetLength(FUpdate.Fragments, JSONFragments.Count);
      for i := 0 to JSONFragments.Count - 1 do
      begin
        JSONFragment := JSONFragments.items[i] as TJSONObject;
        FUpdate.Fragments[i] := TFragment.Create;
        FUpdate.Fragments[i].Part := JSONFragment.GetValue('part').Value;
        FUpdate.Fragments[i].Hash := JSONFragment.GetValue('hash').Value;
        FUpdate.Fragments[i].IsValid := False;
      end;
      FDownloadDir := GetEnvironmentVariable('APPDATA') + '\' + FUpdate.App.Name;
      ForceDirectories(FDownloadDir);
      Result := True;
      if Assigned(FOnUpdate) then FOnUpdate();
    except on E: Exception do
      if Assigned(FOnError) then FOnError(E.Message);
    end;
  finally
    IdHTTP.Free;
    MS.Free;
  end;
end;
{$endregion}

{$region 'TUpdater : IsUpToDate'}
function TUpdater.IsUpToDate: Boolean;
begin
  Result := False;
  if FVersion = FUpdate.App.Version then Result := True;
end;
{$endregion}

{$region 'TUpdater : DiscoverFragments'}
procedure TUpdater.DiscoverFragments;
var
  i: Integer;
begin
  for i := 0 to Length(FUpdate.Fragments) - 1 do
  begin
    if Assigned(FOnDiscover) then FOnDiscover(FUpdate.Fragments[i]);
    if IsValidFragment(FUpdate.Fragments[i]) then
    begin
      FUpdate.Fragments[i].IsValid := True;
      Inc(VerifiedFragments);
    end;
  end;
end;
{$endregion}

{$region 'TUpdater : IsValidFragment'}
function Tupdater.IsValidFragment(Fragment: TFragment): Boolean;
var
  FilePath: string;
function MD5(const Path: String): String;
var
  IdMD5: TIdHashMessageDigest5;
  FS: TFileStream;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  FS := TFileStream.Create(Path, fmOpenRead OR fmShareDenyWrite);
  try
    result := IdMD5.HashStreamAsHex(FS);
  finally
    IdMD5.Free;
    FS.Free;
  end;
end;
begin
  Result := False;
  FilePath := FDownloadDir + '\' + Fragment.Part + '.frag';
  if FileExists(FilePath) then
    if MD5(FilePath) = Fragment.Hash then Result := True;
end;
{$endregion}

{$region 'TUpdater : DownloadFragments'}
procedure TUpdater.DownloadFragments;
var
  i: Integer;
begin
  for i := 0 to Length(FUpdate.Fragments) - 1 do
  begin
    if not FUpdate.Fragments[i].IsValid then
    begin
      if Assigned(FOnDownload) then FOnDownload(DownloadedFragments + 1, Length(Update.Fragments) - VerifiedFragments);
      DownloadFragment(FUpdate.Fragments[i]);
    end;
  end;
end;
{$endregion}

{$region 'TUpdater : DownloadFragment'}
procedure TUpdater.DownloadFragment(Fragment: TFragment);
var
  IdHTTP: TIdHTTP;
  MS: TMemoryStream;
begin
  IdHTTP := TIdHTTP.Create(nil);
  MS := TMemoryStream.Create;
  try
    if Assigned(FOnWorkBegin) then IdHTTP.OnWorkBegin := FOnWorkBegin;
    if Assigned(FOnWork) then IdHTTP.OnWork := FOnWork;
    if Assigned(FOnWorkEnd) then IdHTTP.OnWorkEnd := FOnWorkEnd;
    IdHTTP.Get('http://updater.to/HASH-HASH-HASH-HASH-HASH/' + Fragment.Part + '.frag', MS);
    MS.SaveToFile(FDownloadDir + '\' + Fragment.Part + '.frag');
    Inc(DownloadedFragments);
  finally
    IdHTTP.Free;
    MS.Free;
  end;
end;
{$endregion}

{$region 'TUpdater : MergeFragments'}
procedure TUpdater.MergeFragments;
var
  InStream, OutStream: TFileStream;
  i: Integer;
begin
  if Assigned(FOnMerge) then FOnMerge;
  OutStream := TFileStream.Create(FDownloadDir + '\' + 'packed.frag', fmCreate);
  for i := 0 to Length(FUpdate.Fragments) - 1 do
  begin
    InStream := TFileStream.Create(FDownloadDir + '\' + FUpdate.Fragments[i].Part + '.frag', fmOpenRead);
    try
      OutStream.CopyFrom(InStream, InStream.Size);
    finally
      InStream.Free;
    end;
  end;
  OutStream.Free;
end;
{$endregion}

{$endregion}

end.
