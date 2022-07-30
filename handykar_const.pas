unit handykar_const;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MainTitle = 'Handy Kartotheke';
  Version = '5.29.07.2022';
  cdsQuit = -999;
  { sql statements }
  CreateDb = 'CREATE TABLE handykar(id_hk integer primary key, firstname_hk varchar(128), surname_hk varchar(128), phone_hk varchar(20), mobile_hk varchar(20), email_hk varchar(128), address_hk varchar(128), town_hk varchar(128), amount_hk varchar(11), flag integer, reserved varchar(128));';
  InsSql = 'INSERT INTO handykar(id_hk,firstname_hk,surname_hk,phone_hk,mobile_hk,email_hk,address_hk,town_hk,amount_hk,flag,reserved) VALUES(null,"%s","%s","%s","%s","%s","%s","%s","%s",%d,"%s");';
  UpdSql = 'UPDATE handykar set firstname_hk="%s", surname_hk="%s", phone_hk="%s", mobile_hk="%s", email_hk="%s", address_hk="%s", town_hk="%s", amount_hk="%s", flag=%d, reserved="%s" where id_hk=%d;';
  DelSql = 'DELETE FROM handykar WHERE id_hk = %d;';
  SelSql = 'SELECT * FROM handykar;';

  { modification states }
  mNone = 0;
  mAdded = 3;
  mAltered = 5;
  mDelete = 7;

  GridHeaders: array[0..8]of string = ('Vorname','Familiename','Telefon','Handy','Email','Addresse','Stadt','Betrag','Kunde');
  GridColumnsWidth: array[0..8] of integer = (115,190,120,135,175,280,115,75,64);
  BooleanText: array[boolean] of string = ('Nein','Ja');

  { keyboard codes }
  cBckYellow = $00C0FFFF;
  cEditYellow = $00C0FFFF; // yellowish
  cEnter = $0D;
  cEsc = $1B;
  cIns = $2D;
  cF2 = $71;
  cEdit = cF2;
  cDel = $2E;
  cCr = #13;

  { dialog results }
  DlgYes = 6;
  DlgNo = 7;

function HK_Databasename: string;
function HK_Inifilename: string;
function HK_AdjustTrailingSlash(const S: string): string;
function HK_GetFieldToken(const FieldNo: ptruint;
                          const S: string;
                          const Separator: char): string; { fieldno is 1-based }

implementation

function HK_Databasename: string;
begin
  Result:= ExtractFilePath(paramstr(0))+'db'+DirectorySeparator+'handykar.db3'; // *nix ~ /, binbows ~ \
end;

function HK_Inifilename: string;
begin
  Result:= ExtractFilePath(paramstr(0))+'handykar.ini'
end;

function HK_AdjustTrailingSlash(const S: string): string; { 29.07.2015 bc }
var Len: integer;
begin
  Result:= S;
  if S <> '' then begin
    Len:= length(S);
    while S[Len] = '/' do dec(Len);
    SetLength(Result,Len+1); { we want just the first slash }
  end;
end;

{ usage: Surname:= GetFieldToken(2,'Benny|Christensen','|') => 'Christensen'  or }
{        Firstname:= GetFieldToken(1,'Benny Christensen',' ') => 'Benny' }
function HK_GetFieldToken(const FieldNo: ptruint;
                          const S: string;
                          const Separator: char): string;
var
  B,E,C,I,Len: ptrint;
  InField: boolean;
begin
  Len:= system.length(S);
  if (Len > 0) and (FieldNo > 0) then begin                 { save clockcycles }
    I:= 0; C:= 1; InField:= false; B:= -1; E:= -1;            { initialization }
    while (C <= Len) do begin
      if (system.copy(S,C,1) = Separator) or
         (C = Len) then inc(I);        { check for separator and end of string }
      if (I = FieldNo-1) and not InField then begin        { 0-based by nature }
        B:= C;                             { point b to beginning of substring }
        InField:= true;               { flag field found, now look for the end }
      end;
      if (I = FieldNo) and InField then begin
        E:= C;                                   { point e to end of substring }
        break;                                  { Field found, we're done here }
      end;
      inc(C);                                               { increment cursor }
    end;                                                          { continue ? }
    if (B <> -1) and (E <> -1) then begin
      if E = Len then Result:= system.copy(S,B+1,E-B)       { special cases at }
      else if B = 1 then Result:= system.copy(S,B,E-B)     { beginning and end }
      else Result:= system.copy(S,B+1,E-B-1);
    end else Result:= '';                       { return empty string on error }
  end else Result:= '';           { if fed an empty string return it untouched }
end; { getfieldtoken }

(*
Definition:
           Handy-Record =
             Id: integer;
             FirstName: string;
             SurName: string;
             Phone: string;
             Mobile: string;
             Email: string;
             Address: string;
             Town: string;
             Amount: string;
             Flag: boolean;
             Reserved: string;
           End;

// 27.07.2022 /bc
{ writing: insert, update & delete, works on simple tables ONLY! }
procedure TLiteDb.RunSQL(const aStatement: string); { writing: insert, update & delete }
begin
 if not fTrans.Active then fTrans.StartTransaction;
 try
   fQuery.Close;
   fQuery.SQL.Text:= aStatement;
//    fQuery.Prepare; // unnescesary if no parameters etc. advice from 'rvk'
   fQuery.ExecSQL;
//    fQuery.Close;  // unnescesary, doesn't return a dataset. advice from 'rvk'
 finally fTrans.Commit; end;
end;



*)
end.

