unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Grids, Buttons, Printers, LazUTF8,
  bom_hk,
  u_app_settings;

type
  {*** THKObserver ***}
  THKObserver = class(TInterfacedObject,IFPObserver)
  private
    fDs: THKCollection;
    fGrid: TStringGrid;
  public
    constructor Create(aGrid: TStringGrid);
    procedure ClearGrid;
    procedure CreateGridHeaders;
    procedure PopulateGrid;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  end;

  { TfmMain }
  TfmMain = class(TForm)
    btnSaveSettings: TBitBtn;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    cbClient: TCheckBox;
    cbCachedUpdates: TCheckBox;
    edtSearch: TEdit;
    edtBackupname: TEdit;
    edtDbname: TEdit;
    edtEmail: TLabeledEdit;
    edtAdress: TLabeledEdit;
    edtTown: TLabeledEdit;
    edtAmount: TLabeledEdit;
    edtPhone: TLabeledEdit;
    edtMobile: TLabeledEdit;
    gbxBackupname: TGroupBox;
    gbxButtons: TGroupBox;
    gbxData: TGroupBox;
    edtFirstname: TLabeledEdit;
    edtSurname: TLabeledEdit;
    gbxDbname: TGroupBox;
    gbxEngine: TGroupBox;
    gbxFiles: TGroupBox;
    HKImages: TImageList;
    Label1: TLabel;
    edtCacheCount: TLabeledEdit;
    lblSearch: TLabel;
    lblInfo: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    btnExit: TSpeedButton;
    btnNew: TSpeedButton;
    btnSearch: TSpeedButton;
    btnHelp: TSpeedButton;
    btnBackup: TSpeedButton;
    pnlSearch: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    btnFind: TSpeedButton;
    btnSearchClose: TSpeedButton;
    Status: TStatusBar;
    grdData: TStringGrid;
    tabNew: TTabSheet;
    tabOverview: TTabSheet;
    tabOptions: TTabSheet;
    procedure btnBackupClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchCloseClick(Sender: TObject);
    procedure edtBackupnameClick(Sender: TObject);
    procedure edtDbnameClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdDataHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure grdDataHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure grdDataKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure StatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure tabNewShow(Sender: TObject);
    procedure tabOptionsShow(Sender: TObject);
    procedure tabOverviewShow(Sender: TObject);
  private
    fBom: THKCollection;
    fTmp: THKCollectionItem;
    fObserver: THKObserver;
    fRowSelected: integer; { 21.03.2015 bc: added cursor position support }
  protected
    procedure HideControlsOnNewTab;
    procedure ShowControlsOnNewTab;
    procedure DeleteItem(anItem: THKCollectionItem);
    function EditItem(anItem: THKCollectionItem): boolean; { bc 20.12.2014 }
    procedure btnEditSaveClick(Sender: TObject);
    procedure ShowInfo(anItem: THKCollectionItem);
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation
uses handykar_const,bc_graphics;

{ ==================== THKObserver ==================== }
constructor THKObserver.Create(aGrid: TStringGrid);
begin
  inherited Create;
  fGrid:= aGrid;
  CreateGridHeaders;
end;

procedure THKObserver.ClearGrid;
var x,y: integer;
begin
  fGrid.BeginUpdate;
  for X:= 0 to fGrid.ColCount-1 do
    for Y:= 1 to fGrid.RowCount-1 do begin
      fGrid.Cells[X,Y]:= '';
      fGrid.Objects[X,Y]:= nil;
    end;
  fGrid.RowCount:= 1;
  fGrid.EndUpdate;
end;

procedure THKObserver.CreateGridHeaders;
var I: integer;
begin
  fGrid.BeginUpdate;
  fGrid.Columns.Clear; // remove what's already there
  for I:= 0 to 8 do with fGrid.Columns.Add do begin
    Color:= $00C0FFFF; // $00C0FFFF Yellowish // $00D9EAD9 greenish
    Font.Color:= clBlue;
    Title.Caption:= GridHeaders[I];
    Title.Color:= $00D9EAD9; // clBtnFace
    Title.Font.Color:= clBlue;
    Title.Font.Style:= [fsBold];
    Width:= AppSettings.GridColumnsWidthArray[I]; // refactored 29.07.2022 /bc
    if I = 7 then Alignment:= taRightJustify;
  end;
  fGrid.EndUpdate;
end;

procedure THKObserver.PopulateGrid;
var
  Hk: TCollectionItem;
  Y: integer;
begin
  ClearGrid;
  CreateGridHeaders;
  fGrid.BeginUpdate;
  Y:= fGrid.RowCount;
  for Hk in fDs do begin
    fGrid.RowCount:= fGrid.RowCount+1; // add a row to the grid
    fGrid.Cells[0,Y]:= THKCollectionItem(Hk).FirstName;
    fGrid.Cells[1,Y]:= THKCollectionItem(Hk).SurName;
    fGrid.Cells[2,Y]:= THKCollectionItem(Hk).Phone;
    fGrid.Cells[3,Y]:= THKCollectionItem(Hk).Mobile;
    fGrid.Cells[4,Y]:= THKCollectionItem(Hk).Email;
    fGrid.Cells[5,Y]:= THKCollectionItem(Hk).Address;
    fGrid.Cells[6,Y]:= THKCollectionItem(Hk).Town;
    fGrid.Cells[7,Y]:= THKCollectionItem(Hk).Amount;
    fGrid.Cells[8,Y]:= BooleanText[THKCollectionItem(Hk).Flag];
    fGrid.Objects[0,Y]:= Hk; // save reference to object in case we need it
    inc(Y);
  end;
  fGrid.EndUpdate(true);
end;

{   TFPObservedOperation = (ooChange,ooFree,ooAddItem,ooDeleteItem,ooCustom);  }
procedure THKObserver.FPOObservedChanged(ASender: TObject;Operation: TFPObservedOperation; Data: Pointer);
var
  HkCount: ptrint;
begin
//  if Data <> nil then HkItem:= THKCollectionItem(Data);
  case Operation of
    ooChange: ; //todo
    ooFree: ; //todo
    ooAddItem: {showmessage('Observer notified: Item added...')}; // todo
    ooDeleteItem: ; // todo
    ooCustom: begin // triggered after EndUpdate...
                HkCount:= ptrint(Data);
                fmMain.Caption:= MainTitle+' - [Kontakte in Database: '+inttostr(HkCount)+']';
                fDs:= THKCollection(ASender);
                PopulateGrid;
              end;
  end;

end;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.btnFindClick(Sender: TObject);
begin
  pnlSearch.Visible:= true;
  edtSearch.SetFocus;
//  tabOverviewShow(Self);
end;

procedure TfmMain.btnCancelClick(Sender: TObject);
var Idx: integer;
begin
  if assigned(fTmp) then begin { we were in "edit" mode }
    fTmp:= nil;
    tabNew.Caption:= 'Neue Datei';
    btnSave.OnClick:= @btnSaveClick; { restore normal "save" button operation }
  end;
  // now clear all fields
  for Idx:= 0 to gbxData.ControlCount-1 do
    if gbxData.Controls[Idx] is TLabeledEdit then TLabeledEdit(gbxData.Controls[Idx]).Text:= '';
  cbClient.Checked:= false;
  HideControlsOnNewTab;
  PageControl1.ActivePage:= tabOverview;
end;

procedure TfmMain.btnBackupClick(Sender: TObject);
begin
  lblInfo.Visible:= true;
  Application.ProcessMessages;
  fBom.BackupDb;
  lblInfo.Visible:= false;
end;

procedure TfmMain.btnHelpClick(Sender: TObject);
begin
  ShowMessage('Handy Kartotek'+#10+'Version: '+fBom.EngineVersion+#10+'Copyright (C) 2014-2022 cdbc.dk');
end;

procedure TfmMain.btnNewClick(Sender: TObject);
begin
  fRowSelected:= grdData.Row; { remember cursor position, 21.03.2015 bc }
  PageControl1.ActivePage:= tabNew;
  ShowControlsOnNewTab;
end;

procedure TfmMain.btnSaveClick(Sender: TObject);
var
  Idx: integer;
  aNewItem: THKCollectionItem;
begin
  aNewItem:= fBom.AddNew; // creates a new record to fill, handled by the deltaqueue
  aNewItem.FirstName:= edtFirstname.Text;
  aNewItem.SurName:= edtSurname.Text;
  aNewItem.Phone:= edtPhone.Text;
  aNewItem.Mobile:= edtMobile.Text;
  aNewItem.Email:= edtEmail.Text;
  aNewItem.Address:= edtAdress.Text;
  aNewItem.Town:= edtTown.Text;
  aNewItem.Amount:= edtAmount.Text;
  aNewItem.Flag:= cbClient.Checked; // existing  client
  aNewItem.Modified:= mAdded;
  { now check for duplicates }
  if fBom.IndexOf(aNewItem) = -1 then begin
    { put in queue for persistence }
    fBom.AppendToDelta(aNewItem);
    aNewItem:= nil; // remove reference to object in deltaQ
  end else begin
    FreeAndNil(aNewItem); { throw it away }
    ShowMessage('FEHLER! - Kontakte ist schon ins database gespeichert!'); { 11.05.2015 bc }
  end;
  // now clear all fields in case user wants to add another contact
  for Idx:= 0 to gbxData.ControlCount-1 do
    if gbxData.Controls[Idx] is TLabeledEdit then TLabeledEdit(gbxData.Controls[Idx]).Text:= '';
  cbClient.Checked:= false;
  ActiveControl:= edtFirstname;
end;

procedure TfmMain.btnSaveSettingsClick(Sender: TObject);
begin
  AppSettings.Databasename:= edtDbname.Text;
  AppSettings.BackupPath:= HK_AdjustTrailingSlash(edtBackupname.Text + DirectorySeparator);
  AppSettings.BatchUpdates:= cbCachedUpdates.Checked;
  AppSettings.BatchCount:= strtoint(edtCacheCount.Text);
  AppSettings.Update;
  { in case of a new database }
  if not FileExists(AppSettings.Databasename) then begin
    fBom.DbName:= AppSettings.Databasename;
    fBom.CheckTable;
  end;
  { in case of batch change }
  fBom.BatchUpdate:= AppSettings.BatchUpdates; { 29.07.2016 bc }
  PageControl1.ActivePage:= tabOverview;
  // TODO
end;

procedure TfmMain.btnSearchClick(Sender: TObject);
begin
  PageControl1.ActivePage:= tabOverview;
end;

procedure TfmMain.btnSearchCloseClick(Sender: TObject);
begin
  fRowSelected:= grdData.Row;
  edtSearch.Text:= '';
  pnlSearch.Visible:= false;
  grdData.Row:= fRowSelected;
end;

procedure TfmMain.edtBackupnameClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then begin
    edtBackupname.Text:= SelectDirectoryDialog1.FileName + DirectorySeparator;
  end;
end;

procedure TfmMain.edtDbnameClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    edtDbname.Text:= OpenDialog1.FileName;
    fBom.DbName:= edtDbname.Text;
  end;
end;

procedure TfmMain.edtSearchChange(Sender: TObject);
var
  Mask,GridText: string;
  Cnt,Len: integer;
begin
  Mask:= LowerCase(edtSearch.Text);
  Len:= Length(Mask);
  for Cnt:= 1 to grdData.RowCount-1 do begin
    case fBom.SortOrder of
      0: GridText:= LowerCase(grdData.Cells[0,Cnt]);
      1: GridText:= LowerCase(grdData.Cells[1,Cnt]);
      6: GridText:= LowerCase(grdData.Cells[6,Cnt]);
      7: ; // for now do nothing
    end;
    if Mask = copy(GridText,1,Len) then begin
      grdData.Row:= Cnt; // position on the first match
      break;
    end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  fBom:= CreateBom;
  fObserver:= THKObserver.Create(grdData);
  fBom.FPOAttachObserver(fObserver);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  fBom.FPODetachObserver(fObserver);
  fObserver.Free;
  fBom.Free;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage:= tabOverview; // force this on startup
end;

procedure TfmMain.grdDataHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  case Index of
    0: fBom.SortOrder:= 0; // firstname
    1: fBom.SortOrder:= 1; // surname
    6: fBom.SortOrder:= 6; // town
    7: fBom.SortOrder:= 7; // amount :-)
  end;
  Self.tabOverviewShow(Self); { invokes the bom and the observer to update gui }
  grdData.Row:= 1;
end;

procedure TfmMain.grdDataHeaderSized(Sender: TObject;IsColumn: Boolean;Index: Integer);
begin
  AppSettings.GridColumnWidth[Index]:= grdData.Columns.Items[Index].Width;
  AppSettings.Update; { persist grid column width to ini-file }
end;

procedure TfmMain.grdDataKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin { set fmMain.KeyPreview = true }
  case Key of
    cIns: btnNewClick(Self);
    cEdit,cEnter: EditItem(THKCollectionItem(grdData.Objects[0,grdData.Row]));
    cDel: DeleteItem(THKCollectionItem(grdData.Objects[0,grdData.Row]));
    cEsc: btnExitClick(Self);
  end; // case
end;

{ uses bc_graphics; }
procedure TfmMain.StatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin { setup panels in designer and panel.style = psownerdraw simplepanel = false}
  case panel.index of
    0: CustomizeStatusbar(Status,
                          Rect,
                          nil,
                          ' [Esc]',
                          ' = Beenden',
                          clRed,
                          clBlue,
                          BackGroundYellow);
    1: CustomizeStatusbar(Status,
                          Rect,
                          nil,
                          ' [Ins]',
                          ' = Zufügen',
                          clRed,
                          clBlue,
                          BackGroundYellow);
    2: CustomizeStatusbar(Status,
                          Rect,
                          nil,
                          ' [F2]',
                          ' = Redigieren',
                          clRed,
                          clBlue,
                          BackGroundYellow);
    3: CustomizeStatusbar(Status,
                          Rect,
                          nil,
                          ' [Del]',
                          ' = Löschen',
                          clRed,
                          clBlue,
                          BackGroundYellow);
    4: CustomizeStatusbar(Status,
                          Rect,
                          nil,
                          ' (C)',
                          ' 2014-2022 cdbc.dk, version: '+ fBom.EngineVersion,
                          clRed,
                          clBlue,
                          BackGroundYellow);
  end; // case                { ' [Enter]', ' = vælg', }

end;

procedure TfmMain.tabNewShow(Sender: TObject);
begin
  ShowControlsOnNewTab;
end;

procedure TfmMain.tabOptionsShow(Sender: TObject);
begin
  edtDbname.Color:= $00C0FFFF; // yellowish
  edtDbname.Font.Color:= clBlue;
  edtDbname.Text:= AppSettings.Databasename;
  edtBackupname.Color:= $00C0FFFF; // yellowish
  edtBackupname.Font.Color:= clBlue;
  edtBackupname.Text:= AppSettings.BackupPath;
  cbCachedUpdates.Checked:= AppSettings.BatchUpdates;
  edtCacheCount.Color:= $00C0FFFF; // yellowish
  edtCacheCount.Font.Color:= clBlue;
  edtCacheCount.Text:= inttostr(AppSettings.BatchCount);
  ActiveControl:= btnSaveSettings; { 11.05.2015 bc }
end;

procedure TfmMain.tabOverviewShow(Sender: TObject);
begin
  fBom.ReadDb; { triggers our observer object }
  Self.ActiveControl:= grdData;
  if fRowSelected <> 0 then grdData.Row:= fRowSelected; { place cursor in the vicinity of where we left it, 21.03.2015 bc }
end;

procedure TfmMain.HideControlsOnNewTab;
var
  Idx: integer;
begin
  for Idx:= 0 to gbxData.ControlCount-1 do
    if gbxData.Controls[Idx] is TLabeledEdit then gbxData.Controls[Idx].Visible:= false;
  cbClient.Visible:= false;
end;

procedure TfmMain.ShowControlsOnNewTab;
var
  Idx: integer;
begin
  for Idx:= 0 to gbxData.ControlCount-1 do
    if gbxData.Controls[Idx] is TLabeledEdit then begin
      gbxData.Controls[Idx].Visible:= true;
      gbxData.Controls[Idx].Color:= cEditYellow; { 11.05.2015 bc }
//      gbxData.Controls[Idx].Color:= $00C0FFFF; // yellowish
      gbxData.Controls[Idx].Font.Color:= clBlue;
    end;
  cbClient.Visible:= true;
  ActiveControl:= edtFirstname;
end;

procedure TfmMain.DeleteItem(anItem: THKCollectionItem);
const sDel = 'Möchten sie %s løschen?';
begin
  if messagedlg('Handy Kartotek - löschen...',                     //caption
                format(sDel,[anItem.FirstName+' '+anItem.SurName]),//message
                mtConfirmation,                                    //dialogtype
                mbYesNo,                                           //buttons
                -1) = DlgYes then begin
    fRowSelected:= grdData.Row; // remember cursor position
    anItem.Modified:= mDelete;
    fBom.AppendToDelta(anItem);
    fBom.ReadDb; // observer handles the rest :-)
    if fRowSelected <> 1 then grdData.Row:= fRowSelected; // reposition the cursor
  end;
end;

function TfmMain.EditItem(anItem: THKCollectionItem): boolean;
begin
  Result:= false;
  fTmp:= anItem; // save reference to item for saving
  fRowSelected:= grdData.Row; { remember cursor position, 21.03.2015 bc }
  btnSave.OnClick:= @btnEditSaveClick; // redirect eventhandler
  btnNewClick(Self);
  tabNew.Caption:= 'Datei redigieren';
  edtFirstname.Text:= fTmp.FirstName;
  edtSurname.Text:= fTmp.SurName;
  edtPhone.Text:= fTmp.Phone;
  edtMobile.Text:= fTmp.Mobile;
  edtEmail.Text:= fTmp.Email;
  edtAdress.Text:= fTmp.Address;
  edtTown.Text:= fTmp.Town;
  edtAmount.Text:= fTmp.Amount;
  cbClient.Checked:= fTmp.Flag;
  Result:= true;
end;

procedure TfmMain.btnEditSaveClick(Sender: TObject);
var
  Idx: integer;
begin
  if assigned(fTmp) then try
    fTmp.FirstName:= edtFirstname.Text;
    fTmp.SurName:= edtSurname.Text;
    fTmp.Phone:= edtPhone.Text;
    fTmp.Mobile:= edtMobile.Text;
    fTmp.Email:= edtEmail.Text;
    fTmp.Address:= edtAdress.Text;
    fTmp.Town:= edtTown.Text;
    fTmp.Amount:= edtAmount.Text;
    fTmp.Flag:= cbClient.Checked; // existing  client
    fTmp.Modified:= mAltered;
    fBom.AppendToDelta(fTmp);
    // now clear all fields
    for Idx:= 0 to gbxData.ControlCount-1 do
      if gbxData.Controls[Idx] is TLabeledEdit then TLabeledEdit(gbxData.Controls[Idx]).Text:= '';
    cbClient.Checked:= false;
  finally fTmp:= nil; end;
  tabNew.Caption:= 'Neue Datei';
  btnSave.OnClick:= @btnSaveClick; { restore normal "save" button operation }
  btnSearchClick(Sender); { return to overview tab to show changes }
end;

procedure TfmMain.ShowInfo(anItem: THKCollectionItem);
begin
  // TODO!
end;

end.

