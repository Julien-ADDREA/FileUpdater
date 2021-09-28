unit uUpdater;

interface

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
  end;

  // TJSON
  TJSON = class
    App: TApp;
    Fragments: array of TFragment;
  end;

  // TErrorEvent
  TErrorEvent = procedure(Error: string) of object;

  TUpdater = class
    Update: TJSON;
    procedure Initialize;
  private
    FOnError: TErrorEvent;
  public
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

//var
//  _JSON: TJSON;

implementation

uses IdHTTP, System.Classes, System.SysUtils, JSON, System.Generics.Collections;

procedure TUpdater.Initialize;
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
  IdHTTP := TIdHTTP.Create(nil);
  MS := TMemoryStream.Create;
  Update := TJSON.Create;
  try
    try
      IdHTTP.Get('http://updater.to/HASH-HASH-HASH-HASH-HASH/update.json', MS);
      SetString(JSONResponse, PAnsiChar(MS.Memory), MS.Size);
      JSONObject := TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;
      // App
      JSONApp := JSONObject.GetValue('app') as TJSONObject;
      Update.App := TApp.Create;
      Update.App.Name := JSONApp.GetValue('name').Value;
      Update.App.Version := JSONApp.GetValue('version').Value;
      Update.App.FileSize := StrToInt(JSONApp.GetValue('fileSize').Value);
      Update.App.BlockSize := StrToInt(JSONApp.GetValue('blockSize').Value);
      // Fragments
      JSONFragments := JSONObject.GetValue('fragments') as TJSONArray;
      SetLength(Update.Fragments, JSONFragments.Count);
      for i := 0 to JSONFragments.Count - 1 do
      begin
        JSONFragment := JSONFragments.items[i] as TJSONObject;
        Update.Fragments[i] := TFragment.Create;
        Update.Fragments[i].Part := JSONFragment.GetValue('part').Value;
        Update.Fragments[i].Hash := JSONFragment.GetValue('hash').Value;
      end;
    except on E: Exception do
      if Assigned(FOnError) then
       FOnError(E.Message);
    end;
  finally
    IdHTTP.Free;
    MS.Free;
  end;
end;

end.
