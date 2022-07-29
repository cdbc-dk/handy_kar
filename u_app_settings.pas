{****************************************************
* 19.04.2015 /bc simple app settings persistence    *
* 29.07.2022 /bc update added gridcolumnswidtharray *
*                and gridcolumnwidth properties     *
****************************************************}
unit u_app_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, handykar_const;
type
  { THandykarSettings }
  TGridColArray = array[0..8] of integer;
  THandykarSettings = class
  protected
    fBatchCount: integer;
    fBatchUpdates: boolean;
    fIni: TIniFile;
    fBackupname: string;
    fDbname: string;
    fGridColumnsWidth: TGridColArray;
    function get_GridColumnWidth(Index: integer): integer; virtual;
    function ReadInifile: boolean; virtual;
    procedure set_GridColumnWidth(Index: integer; AValue: integer); virtual;
    function WriteInifile: boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update; virtual;
    property Databasename: string read fDbname write fDbname;
    property BackupPath: string read fBackupname write fBackupname;
    property BatchUpdates: boolean read fBatchUpdates write fBatchUpdates;
    property BatchCount: integer read fBatchCount write fBatchCount;
    property GridColumnsWidthArray: TGridColArray read fGridColumnsWidth;
    property GridColumnWidth[Index: integer]: integer read get_GridColumnWidth write set_GridColumnWidth; default;
  end;

function AppSettings: THandykarSettings;

implementation
var
  Singleton: THandykarSettings;

function AppSettings: THandykarSettings;
begin
  if not assigned(Singleton) then Singleton:= THandykarSettings.Create;
  Result:= Singleton;
end;

{ two nifty little utility functions }
function ArrayToString(const anArray: TGridColArray): string;
var I: integer;
begin
  Result:= '';
  for I in anArray do Result:= Result + I.ToString +',';
  system.Delete(Result,length(Result),1);
end;

procedure StringToArray(const aStr: string;var anArray: TGridColArray);
var I: integer;
begin
  for I:= 0 to 8 do begin
    anArray[I]:= HK_GetFieldToken(I+1,aStr,',').ToInteger;
  end;
end;

{ THandykarSettings }
function THandykarSettings.ReadInifile: boolean;
var S: string;
begin
  if not FileExists(fIni.FileName) then WriteInifile; { create the darn thing }
  fDbname:= fIni.ReadString('Files','Databasename',HK_Databasename); { defaults to ./db/handykar.db3 }
  fBackupname:= fIni.ReadString('Files','BackupName','Not defined'); { could be empty }
  fBatchUpdates:= fIni.ReadBool('Engine','BatchUpdates',false); { cache updates in db-engine }
  fBatchCount:= fIni.ReadInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  S:= fIni.ReadString('Grid','GridColumnsWidth','115,190,120,135,175,280,115,75,64');
  StringToArray(S,fGridColumnsWidth);
  Result:= true;
end;

{ AppSettings[Index]:= grdData.Columns.Items[Index].Width; // testing default directive => IT WORKS! }
function THandykarSettings.get_GridColumnWidth(Index: integer): integer;
begin
  Result:= fGridColumnsWidth[Index];
end;

procedure THandykarSettings.set_GridColumnWidth(Index: integer;aValue: integer);
begin
  fGridColumnsWidth[Index]:= aValue;
end;

function THandykarSettings.WriteInifile: boolean;
var S: string;
begin
  if fDbname <> '' then fIni.WriteString('Files','DatabaseName',fDbname)
  else fIni.WriteString('Files','DatabaseName',HK_Databasename); { failsafe }
  if fBackupname <> '' then fIni.WriteString('Files','BackupName',fBackupname)
  else fIni.WriteString('Files','BackupName','Not defined');
  fIni.WriteBool('Engine','BatchUpdates',fBatchUpdates); { initialized on 1.st run to false }
  fIni.WriteInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  S:= ArrayToString(fGridColumnsWidth);
  if S <> '' then fIni.WriteString('Grid','GridColumnsWidth',S)
  else fIni.WriteString('Grid','GridColumnsWidth','115,190,120,135,175,280,115,75,64'); { failsafe }
  fIni.UpdateFile;
  Result:= true;;
end;

constructor THandykarSettings.Create;
begin
  inherited Create;
  fIni:= TIniFile.Create(HK_Inifilename); { reads the ini if it exists... }
  fIni.CacheUpdates:= false; { update file immediately }
  ReadInifile;
end;

destructor THandykarSettings.Destroy;
begin
  FreeAndNil(fIni);
  inherited Destroy;
end;

procedure THandykarSettings.Update;
begin
  WriteInifile;
end;

initialization
  Singleton:= nil;
finalization
  if assigned(Singleton) then FreeAndNil(Singleton);
end.

