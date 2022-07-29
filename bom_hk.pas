unit bom_hk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  bc_mtlinklist,
  bc_liteDb;
type
  { THKCollectionItem }
  THKCollectionItem = class(TCollectionItem)
  private
    fHK_Id: integer;
    fFirstName: string;
    fModified: integer;
    fSurName: string;
    fPhone: string;
    fMobile: string;
    fEmail: string;
    fAddress: string;
    fTown: string;
    fAmount: string;
    fFlag: boolean;
    fReserved: string;
  protected
    function get_Address: string;
    function get_Amount: string;
    function get_Email: string;
    function get_FirstName: string;
    function get_Flag: boolean;
    function get_HK_Id: integer;
    function get_Mobile: string;
    function get_Phone: string;
    function get_Reserved: string;
    function get_SurName: string;
    function get_Town: string;
    procedure set_Address(AValue: string);
    procedure set_Amount(AValue: string);
    procedure set_Email(AValue: string);
    procedure set_FirstName(AValue: string);
    procedure set_Flag(AValue: boolean);
    procedure set_HK_Id(AValue: integer);
    procedure set_Mobile(AValue: string);
    procedure set_Phone(AValue: string);
    procedure set_Reserved(AValue: string);
    procedure set_SurName(AValue: string);
    procedure set_Town(AValue: string);
    procedure AssignData(aSource: THKCollectionItem);
  public
    property HK_Id: integer read get_HK_Id write set_HK_Id;
    property FirstName: string read get_FirstName write set_FirstName;
    property SurName: string read get_SurName write set_SurName;
    property Phone: string read get_Phone write set_Phone;
    property Mobile: string read get_Mobile write set_Mobile;
    property Email: string read get_Email write set_Email;
    property Address: string read get_Address write set_Address;
    property Town: string read get_Town write set_Town;
    property Amount: string read get_Amount write set_Amount;
    property Flag: boolean read get_Flag write set_Flag;
    property Reserved: string read get_Reserved write set_Reserved;
    property Modified: integer read fModified write fModified;
  end;

  { THKQueue }

  THKQueue = class(TbcQueue)
  public
    function CreateNew: THKCollectionItem;
    procedure Enqueue(anItem: THKCollectionItem);
    function Dequeue: THKCollectionItem;
  end;

  THKCollection = class(TCollection)
  private
    fBatch: boolean;
    fSortOrder: integer;
    function get_DbName: string;
    function get_EngineVersion: string;
    procedure set_DbName(aValue: string);
  protected
    fDb: TLiteDb;
    fDeltaQueue: THKQueue;
    fUpdateCount: integer;
    function BuildInsertSql(anItem: THKCollectionItem): string;
    function BuildUpdateSql(anItem: THKCollectionItem): string;
    function BuildDeleteSql(anItem: THKCollectionItem): string;
    procedure DoUpdate; { refactored 29.07.2015 bc }
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function CheckTable: boolean; { creates a new table in db-file }
    function AddNew: THKCollectionItem;
    procedure BackupDb; { 19.04.2015 bc }
    procedure AppendToDelta(anItem: THKCollectionItem);
    function IndexOf(anItem: THKCollectionItem): longint; { 11.05.2015 bc, returns -1 on not found else collection ID }
    function UpdateDb(const UpdateNow: boolean): boolean; { refactored 29.07.2015 bc }
    function ReadDb: boolean;
    property UpdateCount: integer read fUpdateCount write fUpdateCount;
    property DbName: string read get_DbName write set_DbName;
    property BatchUpdate: boolean read fBatch write fBatch;
    property SortOrder: integer read fSortOrder write fSortOrder;
    property EngineVersion: string read get_EngineVersion;
  end;

{ Bom - factory }
function CreateBom: THKCollection;

implementation
uses handykar_const,bc_memdataset,u_app_settings,bc_DateTime;

function CreateBom: THKCollection;
begin
  Result:= THKCollection.Create(THKCollectionItem);
end;

{ ================== THKQueue ================== }
function THKQueue.CreateNew: THKCollectionItem;
begin
  Result:= THKCollectionItem.Create(nil); // no collection, ie. not appended yet
end;

procedure THKQueue.Enqueue(anItem: THKCollectionItem);
begin
  En_Queue(pointer(anItem));
end;

function THKQueue.Dequeue: THKCollectionItem;
begin
  Result:= THKCollectionItem(De_Queue);
end;

{ ================== THKQueue ================== }

{ compares first names ie,:
    Result = 1 -> Item1 is greater than Item2
    Result = -1 -> Item1 is smaller than Item2
    else they are equal -> 0 }
function HKCompareFirstName(Item1, Item2: TCollectionItem): Integer;
begin
  if THKCollectionItem(Item1).FirstName > THKCollectionItem(Item2).FirstName then Result:= 1
  else if THKCollectionItem(Item1).FirstName < THKCollectionItem(Item2).FirstName then Result:= -1
  else Result:= 0;
end;

function HKCompareSurName(Item1, Item2: TCollectionItem): Integer;
begin
  if THKCollectionItem(Item1).SurName > THKCollectionItem(Item2).SurName then Result:= 1
  else if THKCollectionItem(Item1).SurName < THKCollectionItem(Item2).SurName then Result:= -1
  else Result:= 0;
end;

function HKCompareTownName(Item1, Item2: TCollectionItem): Integer;
begin
  if THKCollectionItem(Item1).Town > THKCollectionItem(Item2).Town then Result:= 1
  else if THKCollectionItem(Item1).Town < THKCollectionItem(Item2).Town then Result:= -1
  else Result:= 0;
end;

function HKCompareAmount(Item1, Item2: TCollectionItem): Integer; // TODO...
begin
  if StrToFloat(THKCollectionItem(Item1).Amount) > StrToFloat(THKCollectionItem(Item2).Amount) then Result:= -1
  else if StrToFloat(THKCollectionItem(Item1).Amount) < StrToFloat(THKCollectionItem(Item2).Amount) then Result:= 1
  else Result:= 0;
(* 26.03.2015 bc
  if StrToCurr(THKCollectionItem(Item1).Amount) > StrToCurr(THKCollectionItem(Item2).Amount) then Result:= -1
  else if StrToCurr(THKCollectionItem(Item1).Amount) < StrToCurr(THKCollectionItem(Item2).Amount) then Result:= 1
  else Result:= 0;
*)
end;

function THKCollection.get_DbName: string;
begin
  Result:= fDb.DbName;
end;

function THKCollection.get_EngineVersion: string;
begin
  Result:= handykar_const.Version + ', Lib: '+fDb.LibVersion;
end;

procedure THKCollection.set_DbName(aValue: string);
begin
  fDb.DbName:= aValue;
end;

function THKCollection.CheckTable: boolean; { creates a new db if not existing }
begin
  Result:= true;
  try fDb.RunSQL(CreateDb); except Result:= false; end;
end;

function THKCollection.BuildInsertSql(anItem: THKCollectionItem): string;
begin
  Result:= format(InsSql,[anItem.FirstName,
                          anItem.SurName,
                          anItem.Phone,
                          anItem.Mobile,
                          anItem.Email,
                          anItem.Address,
                          anItem.Town,
                          anItem.Amount,
                          integer(anItem.Flag),
                          anItem.Reserved]);
end;

function THKCollection.BuildUpdateSql(anItem: THKCollectionItem): string;
begin
  Result:= format(UpdSql,[anItem.FirstName,
                          anItem.SurName,
                          anItem.Phone,
                          anItem.Mobile,
                          anItem.Email,
                          anItem.Address,
                          anItem.Town,
                          anItem.Amount,
                          integer(anItem.Flag),
                          anItem.Reserved,
                          anItem.HK_Id]);
end;

function THKCollection.BuildDeleteSql(anItem: THKCollectionItem): string;
begin
  Result:= format(DelSql,[anItem.HK_Id]);
end;

procedure THKCollection.DoUpdate; { refactored 29.07.2015 bc }
var
  Tmp: THKCollectionItem;
begin
  while not fDeltaQueue.IsEmpty do begin
    Tmp:= fDeltaQueue.Dequeue;
    case Tmp.Modified of
      mAdded: fDb.RunSQL(BuildInsertSql(Tmp));
      mAltered: fDb.RunSQL(BuildUpdateSql(Tmp));
      mDelete: fDb.RunSQL(BuildDeleteSql(Tmp));
    end;
    FreeAndNil(Tmp);
  end;
end;

constructor THKCollection.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  fDb:= TLiteDb.Create;
  fDb.DbName:= AppSettings.Databasename; { 19.04.2015 bc }
  fDb.Connect;
  CheckTable; // create table if non existing
  if fDb.Connected then fDb.DisConnect; // no idle connections
  fDeltaQueue:= THKQueue.Create;
  fBatch:= AppSettings.BatchUpdates; { 19.04.2015 bc }
  fUpdateCount:= AppSettings.BatchCount; { 19.04.2015 bc }
end;

destructor THKCollection.Destroy;
begin
  if not fDeltaQueue.IsEmpty then UpdateDb(true);
  fDeltaQueue.Free;
  if fDb.Connected then fDb.DisConnect;
  fDb.Free;
  inherited Destroy;
end;

function THKCollection.AddNew: THKCollectionItem;
begin
  Result:= fDeltaQueue.CreateNew;
end;

procedure THKCollection.BackupDb; { ok }
var
  BackupFilename: string;
  Buffer: array[0..4095] of byte; { 4 Kb buffer }
  InStream,OutStream: TFileStream;
  Cnt,I,Res: Int64;
begin
  BackupFilename:= AppSettings.BackupPath;
  FillChar(Buffer,4096,0); { 11.05.2015 bc }
  if BackupFilename <> 'Not defined' then begin
    { now construct the actual backupname with a date and .bak extension }
    BackupFilename:= ExtractFilePath(AppSettings.BackupPath)+ExtractFileName(AppSettings.Databasename); ;
    system.insert('_',BackupFilename,length(BackupFilename)-3); // +_
    system.insert(bcDateToStr(now),BackupFilename,length(BackupFilename)-3); // +19.04.2015
    BackupFilename:= ChangeFileExt(BackupFilename,'.bak');  // *.bak
    if FileExists(BackupFilename) then DeleteFile(BackupFilename); { 09.05.2015 bc }
    if fDb.Connected then fDb.DisConnect; { sanity check }
    InStream:= TFileStream.Create(AppSettings.Databasename,fmOpenRead);
    try
      InStream.Seek(0,fsFromBeginning);
      OutStream:= TFileStream.Create(BackupFilename,fmCreate);
      try
        OutStream.Seek(0,fsFromBeginning);
        Cnt:= InStream.Size; Res:= 0;
        { implemented by hand }
        while Cnt > 0 do begin
          if Cnt > 4096 then I:= 4096 else I:= Cnt;
          InStream.ReadBuffer(Buffer,I);
          OutStream.WriteBuffer(Buffer,I);
          dec(Cnt,I);
          inc(Res,I);
        end;
        if Res <> InStream.Size then raise Exception.Create('Backup failed! Db-file and backup-file differs in size!');
        { implemented in TStream, uses much bigger buffer }
//        OutStream.CopyFrom(InStream,InStream.Size);
      finally
        FreeAndNil(OutStream);
      end;
    finally
      FreeAndNil(InStream);
    end;
  end;
end;

procedure THKCollection.AppendToDelta(anItem: THKCollectionItem);
begin
  if assigned(anItem) then begin
    fDeltaQueue.Enqueue(anItem); // add to delta for db persistence
    { in case of delete, force updatenow 29.07.2015 bc }
    if anItem.Modified = mDelete then UpdateDb(true)
    else UpdateDb(not fBatch); // updates now or every n-th record
  end;
end;

function THKCollection.IndexOf(anItem: THKCollectionItem): longint;
var
  Idx: longint;
  Tmp: THKCollectionItem;
begin
  Result:= -1; { not found }
  for Idx:= 0 to Count-1 do begin
    Tmp:= THKCollectionItem(Items[Idx]);
    if ((Tmp.Address = anItem.Address) and (Tmp.Amount = anItem.Amount) and
       (Tmp.Email = anItem.Email) and (Tmp.FirstName = anItem.FirstName) and
       (Tmp.Flag = anItem.Flag) and (Tmp.Mobile = anItem.Mobile) and
       (Tmp.Phone = anItem.Phone) and (Tmp.SurName = anItem.SurName) and
       (Tmp.Town = anItem.Town)) then begin
      Result:= Idx;
      Break;
    end;
  end;
end;

function THKCollection.UpdateDb(const UpdateNow: boolean): boolean; // db writes
begin { original code moved to "cutaway.txt" }
  Result:= false;
  if not fDb.Connected then fDb.Connect;         { connect to physical db-file }
  if not UpdateNow then begin { cater for batch updates }
    if fDeltaQueue.Count >= fUpdateCount then DoUpdate;
  end else DoUpdate;
  if fDb.Connected then fDb.DisConnect;     { disconnect from physical db-file }
  Result:= true;
end; { refactored 29.07.2015 bc -> actual writes moved to "DoUpdate" }

function THKCollection.ReadDb: boolean; // db reads
var
  Ds: TMemDataset;
  Hk: THKCollectionItem;
begin
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  BeginUpdate;
  Ds:= TMemDataset.Create(nil);
  try
    fDb.QuerySQL(SelSql,Ds); // fills the dataset with fielddefs and data
    Ds.First;
    while not Ds.EOF do begin
      Hk:= THKCollectionItem(Add);
      Hk.HK_Id:= Ds.FieldByName('id_hk').AsInteger;
      Hk.FirstName:= Ds.FieldByName('firstname_hk').AsString;
      Hk.SurName:= Ds.FieldByName('surname_hk').AsString;
      Hk.Phone:= Ds.FieldByName('phone_hk').AsString;
      Hk.Mobile:= Ds.FieldByName('mobile_hk').AsString;
      Hk.Email:= Ds.FieldByName('email_hk').AsString;
      Hk.Address:= Ds.FieldByName('address_hk').AsString;
      Hk.Town:= Ds.FieldByName('town_hk').AsString;
      Hk.Amount:= Ds.FieldByName('amount_hk').AsString;
      case Ds.FieldByName('flag').AsInteger of
        0: Hk.Flag:= false;
        1: Hk.Flag:= true;
      end;
      Hk.Reserved:= Ds.FieldByName('reserved').AsString;
      Ds.Next;
    end;
    Result:= true;
  finally Ds.Free; end;
  { now sort the collection, according to user preference }
  case fSortOrder of
    0: Sort(@HKCompareFirstName); // sort by firstname
    1: Sort(@HKCompareSurName); // sort by surname
    6: Sort(@HKCompareTownName); // sort by town
    7: Sort(@HKCompareAmount); // sort by amount 30.04.2015 bc
  end;
  EndUpdate;
  if fDb.Connected then fDb.DisConnect;
  FPONotifyObservers(Self,ooCustom,pointer(Self.Count)); { fpc built-in observer pattern }
end;

{ THKCollectionItem }

function THKCollectionItem.get_FirstName: string;
begin
  Result:= fFirstName;
end;

function THKCollectionItem.get_Flag: boolean;
begin
  Result:= fFlag;
end;

function THKCollectionItem.get_HK_Id: integer;
begin
  Result:= fHK_Id;
end;

function THKCollectionItem.get_Address: string;
begin
  Result:= fAddress;
end;

function THKCollectionItem.get_Amount: string;
begin
  Result:= fAmount;
end;

function THKCollectionItem.get_Email: string;
begin
  Result:= fEmail;
end;

function THKCollectionItem.get_Mobile: string;
begin
  Result:= fMobile;
end;

function THKCollectionItem.get_Phone: string;
begin
  Result:= fPhone;
end;

function THKCollectionItem.get_Reserved: string;
begin
  Result:= fReserved;
end;

function THKCollectionItem.get_SurName: string;
begin
  Result:= fSurName;
end;

function THKCollectionItem.get_Town: string;
begin
  Result:= fTown;
end;

procedure THKCollectionItem.set_Address(AValue: string);
begin
  fAddress:= AValue; // quick and simple
end;

procedure THKCollectionItem.set_Amount(AValue: string);
begin
  fAmount:= AValue;
end;

procedure THKCollectionItem.set_Email(AValue: string);
begin
  fEmail:= AValue;
end;

procedure THKCollectionItem.set_FirstName(AValue: string);
begin
  if AValue <> fFirstName then fFirstName:= AValue;
end;

procedure THKCollectionItem.set_Flag(AValue: boolean);
begin
  fFlag:= AValue;
end;

procedure THKCollectionItem.set_HK_Id(AValue: integer);
begin
  if AValue <> fHK_Id then fHK_Id:= AValue;
end;

procedure THKCollectionItem.set_Mobile(AValue: string);
begin
  if AValue <> fMobile then fMobile:= AValue;
end;

procedure THKCollectionItem.set_Phone(AValue: string);
begin
  if AValue <> fPhone then fPhone:= AValue;
end;

procedure THKCollectionItem.set_Reserved(AValue: string);
begin
  fReserved:= AValue;
end;

procedure THKCollectionItem.set_SurName(AValue: string);
begin
  if AValue <> fSurName then fSurName:= AValue;
end;

procedure THKCollectionItem.set_Town(AValue: string);
begin
  fTown:= AValue;
end;

procedure THKCollectionItem.AssignData(aSource: THKCollectionItem);
begin
  fAddress:= aSource.Address;
  fAmount:= aSource.Amount;
  fEmail:= aSource.Email;
  fFirstName:= aSource.FirstName;
  fFlag:= aSource.Flag;
  fHK_Id:= aSource.HK_Id;
  fMobile:= aSource.Mobile;
  fModified:= aSource.Modified;
  fPhone:= aSource.Phone;
  fReserved:= aSource.Reserved;
  fSurName:= aSource.SurName;
  fTown:= aSource.Town;
end;

end.

