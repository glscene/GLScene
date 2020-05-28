// FRMMSnapShot
{: Eric Grange<p>

   Part of RecyclerMM, this form can display memory allocation and usage
   statistics at runtime.
}
unit FRMMSnapShot;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RecyclerMM, ExtCtrls, ComCtrls, Buttons, ImgList;

type
  TRMMSnapShot = class(TForm)
    PageControl: TPageControl;
    TSSummary: TTabSheet;
    TSMemoryMap: TTabSheet;
    ScrollBox: TScrollBox;
    Image: TImage;
    LVStats: TListView;
    TSSMBStats: TTabSheet;
    LVSMB: TListView;
    BBRefresh: TBitBtn;
    PBLegend: TPaintBox;
    ImageList1: TImageList;
    procedure BBRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PBLegendPaint(Sender: TObject);
    procedure LVStatsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure Display(const aSnapShot : TRMMUsageSnapShot);
  end;

function RMMSnapShot : TRMMSnapShot;
procedure ReleaseRMMSnapShot;

implementation

{$R *.DFM}

{$ifdef VER150}   // of course it's unsafe, so no warnings plz
   {$WARN UNSAFE_CODE OFF}
   {$WARN UNSAFE_TYPE OFF}
{$endif}

var
	vRMMSnapShot : TRMMSnapShot;

function RMMSnapShot : TRMMSnapShot;
begin
	if not Assigned(vRMMSnapShot) then
	   vRMMSnapShot:=TRMMSnapShot.Create(nil);
	Result:=vRMMSnapShot;
end;

procedure ReleaseRMMSnapShot;
begin
	if Assigned(vRMMSnapShot) then begin
	   vRMMSnapShot.Release; vRMMSnapShot:=nil;
	end;
end;

type
   TRGB24 = packed record
      b, g, r : Byte;
   end;
   TRGB24Array = packed array [0..MaxInt shr 5] of TRGB24;
   PRGB24Array = ^TRGB24Array;

const
   cColUnallocated   : TRGB24 = (b: 255; g: 255; r: 255);
   cColReserved      : TRGB24 = (b:   0; g: 255; r:   0);
   cColSysAllocated  : TRGB24 = (b: 128; g: 128; r: 255);
   cColSysReserved   : TRGB24 = (b: 200; g: 200; r: 255);
   cColZeroUsed      : TRGB24 = (b: 255; g: 192; r: 192);
   cColAllUsed       : TRGB24 = (b:  63; g:   0; r:   0);

function RGB24ToColor(const rgb24 : TRGB24) : TColor;
begin
   Result:=RGB(rgb24.r, rgb24.g, rgb24.b); 
end;

function LerpRGB24(const c1, c2 : TRGB24; f : Single) : TRGB24;
var
   invf : Single;
begin
   invf:=1-f;
   Result.b:=Round(c1.b*invf+c2.b*f);
   Result.g:=Round(c1.g*invf+c2.g*f);
   Result.r:=Round(c1.r*invf+c2.r*f);
end;

function SizeToString(size : Int64; const unitStr : String) : String;
begin
   if size<1024*1024 then
      Result:=Format('%.1f k', [size*(1/1024)])
   else if size<1024*1024*1024 then
      Result:=Format('%.1f M', [size*(1/(1024*1024))])
   else Result:=Format('%.1f G', [size*(1/(1024*1024*1024))]);
   Result:=Result+unitStr;
end;

procedure TRMMSnapShot.FormCreate(Sender: TObject);
begin
   PageControl.ActivePageIndex:=0;
end;

procedure TRMMSnapShot.Display(const aSnapShot : TRMMUsageSnapShot);

   procedure AddSpacer(const name : String);
   begin
      with LVStats.Items.Add do begin
         Caption:=name;
         Data:=Pointer(1);
      end;
   end;

   procedure AddSizeStat(const name : String; value : Int64);
   begin
      with LVStats.Items.Add do begin
         Caption:=name;
         SubItems.Add(SizeToString(value, 'B'));
      end;
   end;

   procedure AddCountStat(const name : String; value : Cardinal);
   begin
      with LVStats.Items.Add do begin
         Caption:=name;
         SubItems.Add(IntToStr(value));
      end;
   end;

   procedure AddBigCountStat(const name : String; value : Int64);
   begin
      with LVStats.Items.Add do begin
         Caption:=name;
         SubItems.Add(SizeToString(value, ''));
      end;
   end;

var
   x, y, i : Integer;
   col : TRGB24;
   bmp : TBitmap;
   scanLine : PRGB24Array;
   smbStat : PRMMSMBStat;
begin
   // Add summary stats
   LVStats.Items.Clear;
   AddSpacer('RecyclerMM Stats');
   AddSizeStat ('Total Virtual Allocated', aSnapShot.TotalVirtualAllocated);
   AddSizeStat ('User Allocated Size', aSnapShot.AllocatedUserSize);
   AddCountStat('Nb Allocated Blocks', aSnapShot.AllocatedBlocks);
   AddSpacer('VM Address Space');
   AddSizeStat ('Total VM Space', aSnapShot.TotalVMSpace);
   AddSizeStat ('System Allocated', aSnapShot.SystemAllocatedVM);
   AddSizeStat ('System Reserved', aSnapShot.SystemReservedVM);
   AddSizeStat ('Largest Contiguous Free', aSnapShot.LargestFreeVM);
   if aSnapShot.BenchRGetMem.NbCalls>0 then begin
      AddSpacer('Usage');
      AddBigCountStat('RGetMem Calls', aSnapShot.BenchRGetMem.NbCalls);
      AddBigCountStat('RReallocMem Calls', aSnapShot.BenchRReallocMem.NbCalls);
      AddBigCountStat('RFreeMem Calls', aSnapShot.BenchRFreeMem.NbCalls);
      AddBigCountStat('RGetMem CPU Ticks', aSnapShot.BenchRGetMem.TotalTime);
      AddBigCountStat('RReallocMem CPU Ticks', aSnapShot.BenchRReallocMem.TotalTime);
      AddBigCountStat('RFreeMem CPU Ticks', aSnapShot.BenchRFreeMem.TotalTime);
   end;
   // Memory map
   bmp:=TBitmap.Create;
   try
      scanLine:=nil;
      bmp.PixelFormat:=pf24bit;
      bmp.Width:=64;
      bmp.Height:=512;
      for i:=0 to 32767 do begin
         x:=(i and 63);
         y:=(i shr 6);
         if x=0 then
            scanLine:=bmp.ScanLine[y];
         with aSnapShot.Map[i] do begin
            case Status of
               rmmsReserved : col:=cColReserved;
               rmmsAllocated :
                  col:=LerpRGB24(cColZeroUsed, cColAllUsed, AllocatedUserSize/Length);
               rmmsSysAllocated : col:=cColSysAllocated;
               rmmsSysReserved : col:=cColSysReserved;
            else
               col:=cColUnallocated;
            end;
         end;
         scanLine[x]:=col;
      end;
      Image.Picture.Graphic:=bmp;
   finally
      bmp.Free;
   end;
   ScrollBox.DoubleBuffered:=True;
   // Add SMB Stats
   LVSMB.Items.Clear;
   for i:=Low(aSnapShot.SMBStats) to High(aSnapShot.SMBStats) do begin
      smbStat:=@aSnapShot.SMBStats[i];
      with LVSMB.Items.Add do begin
         Caption:=IntToStr(smbStat.BlockSize);
         SubItems.Add(SizeToString(smbStat.TotalVirtualAllocated, 'B'));
         SubItems.Add(SizeToString(smbStat.AllocatedUserSize, 'B'));
         SubItems.Add(IntToStr(smbStat.AllocatedBlocks));
      end;
   end;
end;

procedure TRMMSnapShot.BBRefreshClick(Sender: TObject);
begin
   Display(RMMUsageSnapShot);
end;

procedure TRMMSnapShot.PBLegendPaint(Sender: TObject);

   procedure PaintLegend(x, y : Integer; const caption : String; const col : TRGB24);
   begin
      x:=x*95+5;
      y:=y*9+1;
      with PBLegend.Canvas do begin
         Brush.Color:=RGB24ToColor(col);
         Rectangle(x, y+1, x+8, y+9);
         Brush.Color:=clBtnFace;
         TextOut(x+10, y, caption);
      end;
   end;

var
   i : Integer;
begin
   PaintLegend(0, 0, 'Unallocated', cColUnallocated);
   PaintLegend(0, 1, 'SysAllocated', cColSysAllocated);
   PaintLegend(0, 2, 'SysReserved', cColSysReserved);
   PaintLegend(1, 0, 'RecyclerMM Reserved', cColReserved);
   with PBLegend.Canvas do begin
      for i:=0 to 83 do begin
         Brush.Color:=RGB24ToColor(LerpRGB24(cColZeroUsed, cColAllUsed, i*(1/83)));
         FillRect(Rect(100+2*i, 13, 102+2*i, 28));
      end;
      Font.Color:=clWhite;
      Brush.Style:=bsClear;
      TextOut(105, 15, '0%');
      TextOut(150, 15, 'RMM Allocated');
      TextOut(240, 15, '100%');
      Brush.Color:=clWindowText;
      Brush.Style:=bsSolid;
      FrameRect(Rect(100, 12, 268, 29));
   end;
end;

procedure TRMMSnapShot.LVStatsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
   r : TRect;
begin
   if Item.Data=nil then
      DefaultDraw:=True
   else begin
      DefaultDraw:=False;
      r:=Item.DisplayRect(drBounds);
      with Sender.Canvas do begin
         Pen.Color:=clNavy;
         MoveTo(r.Left, (r.Top+r.Bottom) shr 1);
         LineTo(r.Right, (r.Top+r.Bottom) shr 1);
         Font.Style:=[fsBold];
         TextOut(r.Left+4, r.Top+1, ' '+Item.Caption+' ');
      end;
   end;
end;

procedure TRMMSnapShot.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
   i : Cardinal;
   pt : TPoint;
   newHint : String;
begin
   i:=((X shr 2)+(Y shr 2)*64) shl 16;
   newHint:='$'+IntToHex(i, 8);
   if newHint<>Image.Hint then begin
      GetCursorPos(pt);
      Application.HideHint;
      Image.Hint:=newHint;
      Application.ActivateHint(pt);
   end;
end;

end.
