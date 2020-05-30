unit FMainForm;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes, System.Actions, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ValEdit, 
  Vcl.Grids, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, 
  Vcl.ExtCtrls, Vcl.ActnList, Vcl.ImgList,
  
  GLHeightTileFile;

type
   TSrc = record
      fs : TFileStream;
      x, y, w, h : Integer;
      format : Integer;
      FlipRotate : Integer;
   end;
   PSrc = ^TSrc;

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    StringGrid: TStringGrid;
    File1: TMenuItem;
    ActionList: TActionList;
    ImageList: TImageList;
    ACOpen: TAction;
    ACSave: TAction;
    ACExit: TAction;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EDHTFName: TEdit;
    EDDEMPath: TEdit;
    BUDEMPath: TButton;
    BUPickHTF: TButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    DEMs1: TMenuItem;
    ACNewDEM: TAction;
    ACRemoveDEM: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    AddDEMsource1: TMenuItem;
    RemoveDEMsource1: TMenuItem;
    SDHTF: TSaveDialog;
    PopupMenu: TPopupMenu;
    AddDEMsource2: TMenuItem;
    RemoveDEMsource2: TMenuItem;
    MIAbout: TMenuItem;
    CBType: TComboBox;
    CBFile: TComboBox;
    Label3: TLabel;
    EDSizeX: TEdit;
    EDSizeY: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EDDefaultZ: TEdit;
    ODTerrainPack: TOpenDialog;
    SDTerrainPack: TSaveDialog;
    ToolButton6: TToolButton;
    ACProcess: TAction;
    ToolButton7: TToolButton;
    N2: TMenuItem;
    Process1: TMenuItem;
    PanelFoot: TPanel;
    ProgressBar: TProgressBar;
    EDTileSize: TEdit;
    Label6: TLabel;
    ToolButton8: TToolButton;
    ACViewer: TAction;
    N3: TMenuItem;
    HTFViewer1: TMenuItem;
    ToolButton9: TToolButton;
    ODPath: TOpenDialog;
    Label7: TLabel;
    EDTileOverlap: TEdit;
    Label8: TLabel;
    EDZFilter: TEdit;
    Label9: TLabel;
    EDZScale: TEdit;
    CBWholeOnly: TCheckBox;
    CBFlipRotate: TComboBox;
    procedure ACExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BUDEMPathClick(Sender: TObject);
    procedure BUPickHTFClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ACNewDEMExecute(Sender: TObject);
    procedure ACRemoveDEMExecute(Sender: TObject);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure CBTypeChange(Sender: TObject);
    procedure ACSaveExecute(Sender: TObject);
    procedure ACOpenExecute(Sender: TObject);
    procedure EDDEMPathChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDDefaultZChange(Sender: TObject);
    procedure ACProcessExecute(Sender: TObject);
    procedure ACViewerExecute(Sender: TObject);
    procedure EDZFilterChange(Sender: TObject);
    procedure EDZScaleChange(Sender: TObject);
  private
     
    sources : array of TSrc;
    defaultZ : SmallInt;
    filterZ : SmallInt;
    zScale : Single;

    procedure Parse;
    procedure Cleanup;
    procedure SrcExtractFlip(src : PSrc; relX, relY, len : Integer; dest : PSmallInt);
    procedure SrcExtract(src : PSrc; relX, relY, len : Integer; dest : PSmallInt; DiagFlip:boolean=false);
    procedure WorldExtract(x, y, len : Integer; dest : PSmallInt);

  public
     
  end;

var
  MainForm: TMainForm;

implementation

uses
  FViewerForm;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
   i : Integer;
begin
   with ActionList do
      for i:=0 to ActionCount-1 do with TAction(Actions[i]) do
         Hint:=Caption;
   with StringGrid do begin
      Cells[0, 0]:='File Name';           ColWidths[0]:=140;
      Cells[1, 0]:='World Offset';        ColWidths[1]:=80;
      Cells[2, 0]:='Size (rotated)';      ColWidths[2]:=80;
      Cells[3, 0]:='Data type';           ColWidths[3]:=120;
      Cells[4, 0]:='Flip and Rotate';     ColWidths[4]:=110;
      Row:=0;
   end;
   zScale:=1;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
   Cleanup;
end;

procedure TMainForm.ACExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TMainForm.BUDEMPathClick(Sender: TObject);
begin
   ODPath.InitialDir:=EDDEMPath.Text;
   ODPath.FileName:=EDDEMPath.Text+'pick a dummy.file';
   if ODPath.Execute then
      EDDEMPath.Text:=ExtractFilePath(ODPath.FileName);
end;

procedure TMainForm.BUPickHTFClick(Sender: TObject);
begin
   SDHTF.FileName:=EDHTFName.Text;
   if SDHTF.Execute then
      EDHTFName.Text:=SDHTF.FileName;
end;

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
   ShowMessage(Caption+#13#10#13#10
               +'HTF Generation Utility'#13#10
               +'Part of GLScene library.'#13#10#13#10
               +'http://glscene.org');
end;

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
   ACRemoveDEM.Enabled:=(StringGrid.RowCount>2);
end;

procedure TMainForm.ACNewDEMExecute(Sender: TObject);
begin
   StringGrid.RowCount:=StringGrid.RowCount+1;
end;

procedure TMainForm.ACRemoveDEMExecute(Sender: TObject);
var
   i : Integer;
begin
   with StringGrid do begin
      i:=Row;
      if i<RowCount-1 then begin
         while i<RowCount-1 do begin
            Rows[i]:=Rows[i+1];
            Inc(i);
         end;
      end else Row:=i-1;
      RowCount:=RowCount-1;
   end;
end;

procedure TMainForm.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);

   procedure SetCB(const cb : TComboBox);
   var
      r : TRect;
      i : Integer;
   begin
      r:=StringGrid.CellRect(ACol, ARow);
      cb.Left:=r.Left+StringGrid.Left;
      cb.Top:=r.Top+StringGrid.Top;
      cb.Width:=r.Right+1-r.Left;
      i:=cb.Items.IndexOf(StringGrid.Cells[ACol, ARow]);
      if i>=0 then
         cb.ItemIndex:=i
      else cb.Text:=StringGrid.Cells[ACol, ARow];
      if Visible then
         cb.SetFocus;
   end;

begin
   if ARow>0 then begin
      if ACol=0 then begin
         CBFile.Visible:=True;
         SetCB(CBFile);
      end else CBFile.Visible:=False;
      if ACol=3 then begin
         CBType.Visible:=True;
         SetCB(CBType);
      end else CBType.Visible:=False;
      if ACol=4 then begin
         CBFlipRotate.Visible:=True;
         SetCB(CBFlipRotate);
      end else CBFlipRotate.Visible:=False;
      CanSelect:=True;
   end;
end;

procedure TMainForm.CBTypeChange(Sender: TObject);
begin
   with StringGrid do
      Cells[Col, Row]:=(Sender as TComboBox).Text;
end;

procedure TMainForm.ACSaveExecute(Sender: TObject);
var
   i : Integer;
   sl, sg : TStringList;
begin
   if SDTerrainPack.Execute then begin
      sl:=TStringList.Create;
      with sl do begin
         Values['HTFName']:=EDHTFName.Text;
         Values['WorldSizeX']:=EDSizeX.Text;
         Values['WorldSizeY']:=EDSizeY.Text;
         Values['TileSize']:=EDTileSize.Text;
         Values['TileOverlap']:=EDTileOverlap.Text;
         Values['DefaultZ']:=EDDefaultZ.Text;
         Values['FilterZ']:=EDZFilter.Text;
         Values['ZScale']:=EDZScale.Text;
         Values['DEMPath']:=EDDEMPath.Text;
         Values['WholeTiles']:=IntToStr(Integer(CBWholeOnly.Checked));
         sg:=TStringList.Create;
         for i:=1 to StringGrid.RowCount-1 do
            sg.Add(StringGrid.Rows[i].CommaText);
         Values['DEMs']:=sg.CommaText;
         sg.Free;
      end;
      sl.SaveToFile(SDTerrainPack.FileName);
      sl.Free;
   end;
end;

procedure TMainForm.ACOpenExecute(Sender: TObject);
var
   i : Integer;
   sl, sg : TStringList;
begin
   if ODTerrainPack.Execute then begin
      sl:=TStringList.Create;
      sl.LoadFromFile(ODTerrainPack.FileName);
      with sl do begin
         EDHTFName.Text:=Values['HTFName'];
         EDSizeX.Text:=Values['WorldSizeX'];
         EDSizeY.Text:=Values['WorldSizeY'];
         EDTileSize.Text:=Values['TileSize'];
         EDTileOverlap.Text:=Values['TileOverlap'];
         EDDefaultZ.Text:=Values['DefaultZ'];
         EDZFilter.Text:=Values['FilterZ'];
         EDZScale.Text:=Values['ZScale'];
         EDDEMPath.Text:=Values['DEMPath'];
         CBWholeOnly.Checked:=(Values['WholeTiles']='1');
         sg:=TStringList.Create;
         sg.CommaText:=Values['DEMs'];
         StringGrid.RowCount:=sg.Count+1;
         for i:=0 to sg.Count-1 do
            StringGrid.Rows[i+1].CommaText:=sg[i];
         sg.Free;
      end;
      sl.Free;
      SDTerrainPack.FileName:=ODTerrainPack.FileName;
   end;
end;

procedure TMainForm.EDDEMPathChange(Sender: TObject);
var
   f : TSearchRec;
   r : Integer;
begin
   CBFile.Items.Clear;
   r:=FindFirst(EDDEMPath.Text+'\*.*', faAnyFile, f);
   while r=0 do begin
      if (f.Attr and faDirectory)=0 then
         CBFile.Items.Add(f.Name);
      r:=FindNext(f);
   end;
   FindClose(f);
end;

procedure TMainForm.EDDefaultZChange(Sender: TObject);
begin
   defaultZ:=StrToIntDef(EDDefaultZ.Text, 0);
   if EDZFilter.Text='' then
      filterZ:=defaultZ;
end;

procedure TMainForm.EDZFilterChange(Sender: TObject);
begin
   filterZ:=StrToIntDef(EDZFilter.Text, defaultZ);
end;

procedure TMainForm.EDZScaleChange(Sender: TObject);
begin
   zScale:=StrToFloatDef(EDZScale.Text, 1.0);
end;

procedure TMainForm.Parse;
var
   i, p : Integer;
   row : TStrings;
begin
   Cleanup;
   SetLength(sources, StringGrid.RowCount-1);
   for i:=0 to High(sources) do begin
      row:=StringGrid.Rows[i+1];
      sources[i].fs:=TFileStream.Create(EDDEMPath.Text+'\'+row[0], fmOpenRead+fmShareDenyNone);
      p:=Pos(',', row[1]);
      sources[i].x:=StrToInt(Copy(row[1], 1, p-1));
      sources[i].y:=StrToInt(Copy(row[1], p+1, MaxInt));
      p:=Pos('x', row[2]);
      sources[i].w:=StrToInt(Copy(row[2], 1, p-1));
      sources[i].h:=StrToInt(Copy(row[2], p+1, MaxInt));
      sources[i].format:=CBType.Items.IndexOf(row[3]);           //File format
      sources[i].FlipRotate:=CBFlipRotate.Items.IndexOf(row[4]); //Flip and Rotate
   end;
end;

procedure TMainForm.Cleanup;
var
   i : Integer;
begin
   for i:=0 to High(sources) do
      sources[i].fs.Free;
   SetLength(sources, 0);
end;

procedure TMainForm.SrcExtractFlip(src : PSrc; relX, relY, len : Integer; dest : PSmallInt);
var i:integer;
    val:SmallInt;
begin
  if src.FlipRotate<=0 then SrcExtract(src,relX, relY, len, dest)  //None
  else begin
    for i:=0 to len-1 do begin
      case src.FlipRotate of
      //0 : SrcExtract(src,relX, relY+i,1,@val);                   //No change                  (    )
        1 : SrcExtract(src,src.w-(relX+i),relY,1,@val);            //H-Flip                     (Flip)
        2 : SrcExtract(src,relY,src.w-(relX+i),1,@val,true);       //DiagFlip + H-Flip          (90deg)
        3 : SrcExtract(src,src.w-(relX+i),src.h-relY,1,@val);      //H-Flip   + V-Flip          (180deg)
        4 : SrcExtract(src,src.h-relY,(relX+i),1,@val,true);       //DiagFlip + V-Flip          (270deg)
        5 : SrcExtract(src,src.h-relY,src.w-(relX+i),1,@val,true); //DiagFlip + V-Flip + H-Flip (Flip-90deg)
        6 : SrcExtract(src,relX+i,src.h-relY,1,@val);              //V-FLIP                     (Flip-180deg)
        7 : SrcExtract(src,relY, relX+i,1,@val,true);              //DiagFlip                   (Flip-270deg)
      end;
      PSmallIntArray(dest)[i]:=val;
    end;
  end;
end;


procedure TMainForm.SrcExtract(src : PSrc; relX, relY, len : Integer; dest : PSmallInt; DiagFlip:boolean=false);
var
   i, c : Integer;
   wd : Word;
   buf : array of Single;
   bmp : TBitmap;
   rw    : integer; //rotated width
begin
   if DiagFlip then rw:=src.h else rw:=src.w;

   with src^ do begin
      case format of
         0 : begin // 16bits Intel
            fs.Position:=(relX+relY*rw)*2;
            fs.Read(dest^, len*2);
         end;
         1 : begin // 16bits unsigned Intel
            fs.Position:=(relX+relY*rw)*2;
            fs.Read(dest^, len*2);
            for i:=0 to len-1 do begin
               wd:=PWord(Integer(dest)+i*2)^;
               PSmallInt(Integer(dest)+i*2)^:=Integer(wd)-32768;
            end;
         end;
         2 : begin // 16bits non-Intel
            fs.Position:=(relX+relY*rw)*2;
            fs.Read(dest^, len*2);
            for i:=0 to len-1 do begin
               wd:=PWord(Integer(dest)+i*2)^;
               PWord(Integer(dest)+i*2)^:=((wd and 255) shl 8)+(wd shr 8);
            end;
         end;
         3 : begin // VTP's BT single
            fs.Position:=(relX+relY*rw)*4+256;
            SetLength(buf, len);
            fs.Read(buf[0], len*4);
            for i:=0 to len-1 do
               PSmallInt(Integer(dest)+i*2)^:=Round(buf[i]);
         end;
         4 : begin // windows BMP
            bmp:=TBitmap.Create;
            try
               fs.Position:=0;
               bmp.LoadFromStream(fs);
               if DiagFlip then rw:=bmp.Width else rw:=bmp.Height;
               for i:=0 to len-1 do begin
                  c:=bmp.Canvas.Pixels[relX+i, rw-relY-1];
                  PSmallInt(Integer(dest)+i*2)^:=(GetGValue(c)-128) shl 7;
               end;
            finally
               bmp.Free;
            end;
         end;
         5 : begin // 32bits FP Intel
            fs.Position:=(relX+relY*rw)*4;
            SetLength(buf, len);
            fs.Read(buf[0], len*4);
            for i:=0 to len-1 do
               PSmallInt(Integer(dest)+i*2)^:=Round((buf[i]-0.5)*32000);
         end;
         6 : begin // DTED
            fs.Position:=3434+(relX+relY*rw)*2 +(relY*12);
            fs.Read(dest^, len*2);
            for i:=0 to len-1 do begin
               wd:=PWord(Integer(dest)+i*2)^;
               PWord(Integer(dest)+i*2)^:=((wd and 255) shl 8)+(wd shr 8);
            end;
         end;
      end;
   end;
end;

procedure TMainForm.WorldExtract(x, y, len : Integer; dest : PSmallInt);
var
   i, n, rx, ry : Integer;
   src : PSrc;
begin
   while len>0 do begin
      src:=nil;
      for i:=0 to High(sources) do begin
         if (sources[i].x<=x) and (sources[i].y<=y)
               and (x<sources[i].x+sources[i].w)
               and (y<sources[i].y+sources[i].h) then begin
            src:=@sources[i];
            Break;
         end;
      end;
      if Assigned(src) then begin
         rx:=x-src.x;
         ry:=y-src.y;
         n:=len;
         if rx+n>src.w then
            n:=src.w-rx;
         SrcExtractFlip(src, rx, ry, n, dest);
         if filterZ<>defaultZ then begin
            for i:=0 to n-1 do
               if PSmallIntArray(dest)[i]=filterZ then
                  PSmallIntArray(dest)[i]:=defaultZ;
         end;
         if zScale<>1 then begin
            for i:=0 to n-1 do
               PSmallIntArray(dest)[i]:=Round(PSmallIntArray(dest)[i]*zScale);
         end;
         Dec(len, n);
         Inc(dest, n);
         Inc(x, n);
      end else begin
         dest^:=defaultZ;
         Inc(dest);
         Dec(len);
         Inc(x);
      end;
   end;
end;

procedure TMainForm.ACProcessExecute(Sender: TObject);
var
   x, y, wx, wy, ts, tx, ty, i, j, overlap : Integer;
   n, maxN : Cardinal;
   htf : TGLHeightTileFile;
   buf : array of SmallInt;
   f : file of Byte;
begin
   Screen.Cursor:=crHourGlass;

   wx:=StrToInt(EDSizeX.Text);
   wy:=StrToInt(EDSizeY.Text);
   ts:=StrToInt(EDTileSize.Text);
   overlap:=StrToInt(EDTileOverlap.Text);
   Parse;
   SetLength(buf, ts*ts);
   htf:=TGLHeightTileFile.CreateNew(EDHTFName.Text, wx, wy, ts);
   htf.DefaultZ:=defaultZ;
   ProgressBar.Max:=1000;
   maxN:=Ceil(wx/ts)*Ceil(wy/ts);
   n:=0;
   ProgressBar.Position:=0;
   y:=0; while y<wy do begin
      ty:=wy+overlap-y;
      if ty>ts then
         ty:=ts;
      x:=0; while x<wx do begin
         tx:=wx+overlap-x;
         if (not CBWholeOnly.Checked) or ((tx>=ts) and ((wy-y)>=ts)) then begin
            if tx>ts then
               tx:=ts;
            for i:=0 to ty-1 do begin
               WorldExtract(x, y+i, tx, @buf[i*ts]);
               if overlap>0 then begin
                  for j:=tx to ts-1 do
                     buf[i*ts+j]:=buf[i*ts+tx-1];
               end else begin
                  for j:=tx to ts-1 do
                     buf[i*ts+j]:=defaultZ;
               end;
            end;
            if overlap>0 then begin
               for i:=ty to ts-1 do for j:=0 to ts-1 do
                  buf[i*ts+j]:=buf[(i-1)*ts+j];
            end else begin
               for i:=ty to ts-1 do for j:=0 to ts-1 do
                  buf[i*ts+j]:=defaultZ;
            end;
            htf.CompressTile(x, y, ts, ts, @buf[0]);
         end;
         Inc(x, ts-overlap);
         Inc(n);
         ProgressBar.Position:=(n*1000) div maxN;
         if (n and 15)=0 then begin
            Application.ProcessMessages;
         end;
      end;
      Inc(y, ts-overlap);
   end;
   htf.Free;
   Cleanup;

   Screen.Cursor:=crDefault;

   AssignFile(f, EDHTFName.Text);
   Reset(f);
   i:=FileSize(f);
   CloseFile(f);

   ShowMessage( 'HTF file created.'#13#10#13#10
               +IntToStr(i)+' bytes in file'#13#10
               +'('+IntToStr(wx*wy*2)+' raw bytes)');
end;

procedure TMainForm.ACViewerExecute(Sender: TObject);
var
   viewer : TViewerForm;
begin
   viewer:=TViewerForm.Create(nil);
   try
      Viewer.htf:=TGLHeightTileFile.Create(EDHTFName.Text);                   //R
      Viewer.Caption:='HTFViewer - '+ExtractFileName(EDHTFName.Text);       //R

      viewer.ShowModal;
   finally
      viewer.Free;
   end;
end;

end.
