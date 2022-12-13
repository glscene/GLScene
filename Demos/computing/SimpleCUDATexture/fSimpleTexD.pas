unit fSimpleTexD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  GLS.Utils,
  GLS.FilePGM,

  CUDA.Context,
  CUDA.APIComps,
  CUDA.Compiler,
  CUDA.Utility,

  GLS.Graphics,
  GLS.TextureFormat;

type
  TForm1 = class(TForm)
    GLCUDA1: TGLCUDA;
    GLCUDADevice1: TGLCUDADevice;
    GLCUDACompiler1: TGLCUDACompiler;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    MainModule: TCUDAModule;
    TurnPicture: TCUDAFunction;
    Image: TCUDATexture;
    TextureArray: TCUDAMemData;
    ResultData: TCUDAMemData;
    pgm: TGLPGMImage;
    procedure TurnPictureParameterSetup(Sender: TObject);
  end;


  TGLBitmap32 = TGLImage;   // comment if supported Graphics32

var
  Form1: TForm1;
  Angle : Single = 0.5;    // angle to rotate image by (in radians)

//-----------------------------------------
implementation
//-----------------------------------------

{$R *.dfm}

const
  TestFileName = 'lena_bw.pgm';
  OutFileName  = 'lena_bw_out.pgm';

procedure TForm1.Button1Click(Sender: TObject);
var
  timer: Cardinal;
  bmp32: TGLBitmap32;
begin
  pgm.LoadFromFile( TestFileName );
  if not InitCUTIL then
  begin
    Memo1.Lines.Add('Can''t load cutil32.dll');
    exit;
  end;
  bmp32 := TGLBitmap32.Create;
  bmp32.Assign( pgm );
  Memo1.Lines.Add(Format('File %s - loaded', [TestFileName]));
  TextureArray.CopyFrom( bmp32 );        // <- error
  Memo1.Lines.Add('Copied from host to device array');
  TurnPicture.Launch;                    // <- failed
  Memo1.Lines.Add('Warmup launch finished');
  cutCreateTimer( timer );
  cutStartTimer( timer );
  TurnPicture.Launch;
  cutStopTimer( timer );
  Memo1.Lines.Add('Launch finished');
  Memo1.Lines.Add(Format('Processing time: %f (ms)', [cutGetTimerValue( timer )] ));
  Memo1.Lines.Add(Format('%.2f Mpixels/sec',
    [(pgm.LevelWidth[0]*pgm.LevelHeight[0] / (cutGetTimerValue( timer) / 1000.0)) / 1e6]));
  cutDeleteTimer( timer );
  ResultData.CopyTo( bmp32 );
  Memo1.Lines.Add('Copied from device global memory to host');
  pgm.Assign( bmp32 );
  pgm.SaveToFile( OutFileName );
  Memo1.Lines.Add(Format('Saving result to %s - done', [OutFileName]));
  bmp32.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDirToAsset();
  pgm := TGLPGMImage.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   pgm.Destroy;
end;

procedure TForm1.TurnPictureParameterSetup(Sender: TObject);
begin
  with TurnPicture do
  begin
    SetParam(ResultData);
    SetParam(pgm.LevelWidth[0]);
    SetParam(pgm.LevelHeight[0]);
    SetParam(Angle);
    SetParam(Image);
  end;
end;

end.
