unit fFourier_D;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Mask,

  GLS.Utils,
  GLS.FilePGM,
  GLS.Graphics,
  GLS.VectorTypes,
  CUDA.Utility,
  CUDA.DataAccess,

  uCPUFFT,
  CUDA.Context,
  CUDA.Graphics,
  CUDA.FFTPlan,
  CUDA.APIComps;

type
  TDemoMode = (dmNone, dm1D, dm2D, dmLena);

  TFormFFT = class(TForm)
    Panel1: TPanel;
    But1DFFT: TButton;
    But2DFFT: TButton;
    BLenna: TButton;
    Label4: TLabel;
    CBReorder: TCheckBox;
    Label5: TLabel;
    CBInvFFT: TCheckBox;
    Label3: TLabel;
    RBModule: TRadioButton;
    RBPhase: TRadioButton;
    RBReal: TRadioButton;
    RBImag: TRadioButton;
    Panel2: TPanel;
    Label1: TLabel;
    LDemoMode: TLabel;
    Label2: TLabel;
    Image2: TImage;
    Image1: TImage;
    GLCUDA1: TGLCUDA;
    GLCUDADevice1: TGLCUDADevice;
    Signal1D: TCUDAMemData;
    FFTPlan1D: TCUDAFFTPlan;
    ESize: TLabeledEdit;
    DeviceBox: TComboBox;
    Temp1D: TCUDAMemData;
    Signal2D: TCUDAMemData;
    Temp2D: TCUDAMemData;
    FFTPlan2D: TCUDAFFTPlan;
    procedure But1DFFTClick(Sender: TObject);
    procedure But2DFFTClick(Sender: TObject);
    procedure ExecDemo(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    DemoMode: TDemoMode;
    FTimer: Cardinal;
    procedure Load1DBmp(out buf: TByteDynArray);
    procedure Save1DBmp(buf: TByteDynArray);
    procedure Load2DBmp(out buf: TByte2DArray);
    procedure LoadLenna(out buf: TByte2DArray);
    procedure Save2DBmp(buf: TByte2DArray);
    procedure DrawBmp(bmp: TBitmap; buf: TByteDynArray);
    function GetDisplayMode: TProcessMode;
  end;

var
  FormFFT: TFormFFT;

implementation

{$R *.dfm}
// ============================== 1D CASE ===========================

procedure TFormFFT.But1DFFTClick(Sender: TObject);
var
  cArr: TComplexDynArray;
  j, len: integer;
  buf: TByteDynArray;
  c: TComplex;
  c_: DoubleElement.TVector2;
  v1, v2: IntElement.TVector3;
begin
  DemoMode := dm1D;
  // get input signal in buf
  Load1DBmp(buf);
  len := length(buf);
  SetLength(cArr, len);

  if DeviceBox.ItemIndex = 0 then
  begin
    // copy input buf into work array cArr
    c.Real := 0;
    c.Imag := 0;
    for j := len - 1 downto 0 do
    begin
      c.Real := buf[j];
      cArr[j] := c;
    end;

    cutStartTimer( FTimer );
    // perform fft on cArr; get result back in cArr
    ftu(1, len, cArr);

    // reorder quadrants left<->right
    if CBReorder.Checked then
      Reorder(cArr);

    // perform inverse fft on cArr to restore input signal
    if CBInvFFT.Checked then
      ftu(-1, len, cArr);

    cutStopTimer( FTimer );
  end
  else
  begin
    Signal1D.Map([mmfFastWrite]);
    c_[1] := 0;
    try
      for j := len - 1 downto 0 do
      begin
        c_[0] := buf[j];
        Signal1D.Data<Double>(j).Vector2 := c_;
      end;
    finally
      Signal1D.UnMap;
    end;

    cutStartTimer( FTimer );

    FFTPlan1D.Execute(Signal1D, Signal1D, fftdForward);

    if CBReorder.Checked then
    begin
      v1[0] := 128; v1[1] := 0; v1[2] := 0;
      v2[0] := 0; v2[1] := 0; v2[2] := 0;
      Signal1D.SubCopyTo(Temp1D, v1, v2, v1);
      Signal1D.SubCopyTo(Signal1D, v2, v1, v1);
      Temp1D.SubCopyTo(Signal1D, v2, v2, v1);
    end;

    if CBInvFFT.Checked then
      FFTPlan1D.Execute(Signal1D, Signal1D, fftdInverse);

    // Readback result
    Signal1D.Map;
    try
      // We may move hole data due to types size is equal
      Move(Signal1D.MappedMemoryAddress^, cArr[0], Signal1D.DataSize);
    finally
      Signal1D.UnMap;
    end;
    cutStopTimer( FTimer );
  end;

  // normalize result in cArr to [0,255] and copy it to buf
  Normalize(cArr, buf, GetDisplayMode);

  // display result
  Save1DBmp(buf);

  LDemoMode.Caption := 
    Format('1D FFT Demo, time: %f (ms)', [cutGetTimerValue( FTimer )]);
  cutResetTimer( FTimer );
end;

procedure TFormFFT.Load1DBmp(out buf: TByteDynArray);
var
  h, i, j, w: integer;
  bmp: TBitmap;
begin
  // set up bitmap in Image1
  bmp := Image1.Picture.Bitmap;
  bmp.PixelFormat := pf8bit;
  bmp.Canvas.FillRect(Image1.ClientRect);
  h := bmp.Height; // =256
  // prepare buf to store input data
  SetLength(buf, h);
  FillChar(buf[0], h, 0);
  j := h div 2;
  w := StrToInt(ESize.Text) div 2;
  // place a pulse wide 2*w at the center of buf
  if (w <= 0) or (w > h div 2) then
    raise Exception.Create('Pulse size is out of range [2,255]');
  for i := j - w to j + w do
    buf[i] := 255;
  // draw input date into Image1
  DrawBmp(bmp, buf);
end;

procedure TFormFFT.Save1DBmp(buf: TByteDynArray);
var
  bmp: TBitmap;
begin
  // setup bitmap in Image2
  bmp := Image2.Picture.Bitmap;
  bmp.Canvas.FillRect(Image2.ClientRect);
  // draw result in Image2
  DrawBmp(bmp, buf);
end;

procedure TFormFFT.DrawBmp(bmp: TBitmap; buf: TByteDynArray);
var
  h, i: integer;
begin
  h := bmp.Height;
  bmp.Canvas.MoveTo(0, h - buf[0] - 1);
  for i := 0 to length(buf) - 1 do
    bmp.Canvas.LineTo(i, h - buf[i] - 1);
end;

// ============================== 2D CASE ===========================

procedure TFormFFT.But2DFFTClick(Sender: TObject);
var
  cArr: TComplex2DArray;
  i, j, len: integer;
  buf: TByte2DArray;
  c: TComplex;
  c_: DoubleElement.TVector2 absolute c;
  v1, v2, v3, v4: IntElement.TVector3;
begin
  // load input bmp into buf[rows,cols]
  if Sender = BLenna then
    LoadLenna(buf)
  else
    Load2DBmp(buf);

  len := length(buf);
  // the original fft routine uses cArr[1..rows,1..cols]
  SetLength(cArr, len, len); // cArr[rows,cols]

  if DeviceBox.ItemIndex = 0 then
  begin
    // copy buf[bytes]->cArr[Real,Imag=0]
    c.Real := 0;
    c.Imag := 0;
    for i := 0 to len - 1 do
      for j := 0 to len - 1 do
      begin
        c.Real := buf[i, j];
        cArr[i, j] := c;
      end;

    cutStartTimer( FTimer );
    // apply forward fft
    ftu(1, len, cArr);

    // reorder quadrants
    if CBReorder.Checked then
      Reorder(cArr);
    if CBInvFFT.Checked then
      // apply inverse fft
      ftu(-1, len, cArr);
    cutStopTimer( FTimer );
  end
  else
  begin
    Signal2D.Map([mmfFastWrite]);
    c_[1] := 0;
    try
      for I := len - 1 downto 0 do
        for J := len - 1 downto 0 do
        begin
          c_[0] := buf[i, j];
          Signal2D.Data<Double>(j, i).Vector2 := c_;
        end;
    finally
      Signal2D.UnMap;
    end;

    cutStartTimer( FTimer );

    FFTPlan2D.Execute(Signal2D, Signal2D, fftdForward);

    if CBReorder.Checked then
    begin
      v1[0] := 128; v1[1] := 128; v1[2] := 128;
      v2[0] := 0; v2[1] := 0; v2[2] := 0;
      Signal2D.SubCopyTo(Temp2D, v2, v2, v1);
      Signal2D.SubCopyTo(Signal2D, v1, v2, v1);
      Temp2D.SubCopyTo(Signal2D, v2, v1, v1);

      v3[0] := 128; v3[1] := 0; v3[2] := 0;
      v4[0] := 0; v4[1] := 128; v4[2] := 0;
      Signal2D.SubCopyTo(Temp2D, v3, v2, v1);
      Signal2D.SubCopyTo(Signal2D, v4, v3, v1);
      Temp2D.SubCopyTo(Signal2D, v2, v4, v1);
    end;

    if CBInvFFT.Checked then
      FFTPlan2D.Execute(Signal2D, Signal2D, fftdInverse);

    cutStopTimer( FTimer );

    // Readback result
    Signal2D.Map;
    try
      // We may move hole data due to types size is equal
      for I := len - 1 downto 0 do
        for j := len - 1 downto 0 do
        begin
          c_ := Signal2D.Data<Double>(j, i).Vector2;
          cArr[i, j] := c;
        end;
    finally
      Signal2D.UnMap;
    end;
  end;

  // normalize cArr
  Normalize(cArr, buf, GetDisplayMode);

  // stuff bmp with buf
  Save2DBmp(buf);
  
  if Sender = BLenna then
    LDemoMode.Caption := 
      Format('2D FFT Demo Lenna, time: %f (ms)', [cutGetTimerValue( FTimer )])
  else
    LDemoMode.Caption := 
      Format('2D FFT Demo, time: %f (ms)', [cutGetTimerValue( FTimer )]);

  cutResetTimer( FTimer );
end;

procedure TFormFFT.Load2DBmp(out buf: TByte2DArray);
var
  bmp: TBitmap;
  i, j, k, w, h: integer;
begin
  DemoMode := dm2D;
 
  h := 256;
  // setup buf to store input data into
  SetLength(buf, h, h); // buf[rows,cols]
  for i := 0 to h - 1 do
    FillChar(buf[i, 0], h, 0);
  k := h div 2;
  w := StrToInt(ESize.Text) div 2;
  // place pulse size [2*w,2*w] at the center of buf
  if (w <= 0) or (w > h div 2) then
    raise Exception.Create('Pulse size is out of range [2,255]');
  for i := k - w to k + w do
    for j := k - w to k + w do
      buf[i, j] := 255;
  // prepare  Image1 bitmap
  bmp := Image1.Picture.Bitmap;
  bmp.PixelFormat := pf8bit;
  // move input data from buf to Image1 bitmap
  for i := 0 to h - 1 do
    Move(buf[i, 0], (bmp.ScanLine[i])^, h);
  Image1.Refresh;
end;

procedure TFormFFT.LoadLenna(out buf: TByte2DArray);
var
  pgm: TGLPGMImage;
  img: TGLImage;
  i, j, w, h: integer;
begin
  DemoMode := dmLena;

  pgm := TGLPGMImage.Create;
  img := TGLImage.Create;
  try
    pgm.LoadFromFile('lena_bw.pgm');
    img.Assign(pgm);
    img.DownSampleByFactor2;
    img.VerticalReverseOnAssignFromBitmap := True;
    img.AssignToBitmap(Image1.Picture.Bitmap);
    h := img.Height;
    w := img.Width;
    SetLength(buf, h, w); // buf[rows,cols]
    for I := 0 to h - 1 do
      for J := 0 to w - 1 do
        buf[I, J] := img.ScanLine[i][j].R;  
  finally
    pgm.Destroy;
    img.Destroy;
  end;
end;

procedure TFormFFT.Save2DBmp(buf: TByte2DArray);
var
  i, h: integer;
  bmp: TBitmap;
begin
  bmp := Image2.Picture.Bitmap;
  h := bmp.Height;
  for i := 0 to h - 1 do
    Move(buf[i, 0], (bmp.ScanLine[i])^, h);
  Image2.Refresh;
end;

// ============================== COMMON ===========================

function TFormFFT.GetDisplayMode: TProcessMode;
begin
  if RBModule.Checked then
    Result := module
  else if RBPhase.Checked then
    Result := phase
  else if RBReal.Checked then
    Result := real
  else
    Result := Imag;
end;

procedure Make8BitPal(pal: PLogPalette);
var
  i, j: integer;
  k: array [0 .. 3] of byte absolute j;
begin
  pal^.palVersion := $300;
  pal^.palNumEntries := 256;
  for i := 0 to 255 do
  begin
    j := i;
    k[1] := i;
    k[2] := i;
    pal^.palPalEntry[i] := TPaletteEntry(j);
  end;
end;

procedure TFormFFT.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
  pPal: PLogPalette;
begin
  if not InitCUTIL then
  begin
    MessageDlg('Can''t load cutil32.dll', mtError, [mbOk], 0);
    Application.Terminate;
  end;

  cutCreateTimer( FTimer );

  DemoMode := dmNone;
  GetMem(pPal, 1028);
  // create a gray custom bitmap and assign it to both images
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf8bit;
  Make8BitPal(pPal);
  bmp.Palette := CreatePalette(pPal^);
  bmp.Height := 256;
  bmp.Width := 256;
  Image1.Picture.Bitmap := bmp;
  Image2.Picture.Bitmap := bmp;
  bmp.Free;
  FreeMem(pPal);
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  Image2.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image2.Picture.Bitmap.Canvas.Pen.Color := clWhite;

  // Force CUFFT initialization
  FFTPlan1D.Execute(Signal1D, Signal1D);
end;

procedure TFormFFT.FormDestroy(Sender: TObject);
begin
  cutDeleteTimer( FTimer );
end;

// RadioButton and CheckBox OnClick handler
//
procedure TFormFFT.ExecDemo(Sender: TObject);
begin
  case DemoMode of
    dm1D:
      But1DFFTClick(But1DFFT);
    dm2D:
      But2DFFTClick(But2DFFT);
    dmLena:
      But2DFFTClick(BLenna);
  end;
end;

end.
