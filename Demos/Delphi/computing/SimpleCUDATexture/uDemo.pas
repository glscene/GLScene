unit uDemo;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  
  GLUtils,GLSCUDAContext, GLSCUDA, GLSCUDACompiler,  GLFilePGM,
  GLSCUDAUtility, GLGraphics, GLTextureFormat;

type
  TForm1 = class(TForm)
    GLSCUDACompiler1: TGLSCUDACompiler;
    GLSCUDA1: TGLSCUDA;
    GLSCUDADevice1: TGLSCUDADevice;
    MainModule: TCUDAModule;
    Button1: TButton;
    Memo1: TMemo;
    TurnPicture: TCUDAFunction;
    Image: TCUDATexture;
    TextureArray: TCUDAMemData;
    ResultData: TCUDAMemData;
    procedure TurnPictureParameterSetup(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
     
  public
     
    pgm: TGLPGMImage;
  end;

var
  Form1: TForm1;
  Angle : Single = 0.5;    // angle to rotate image by (in radians)

implementation

{$R *.dfm}

const
  TestFileName = 'lena_bw.pgm';
  OutFileName  = 'lena_bw_out.pgm';

procedure TForm1.Button1Click(Sender: TObject);
var
  timer: Cardinal;
  bmp32: TGLBitmap32;
begin
  if not InitCUTIL then
  begin
    Memo1.Lines.Add('Can''t load cutil32.dll');
    exit;
  end;
  pgm.LoadFromFile( TestFileName );
  bmp32 := TGLBitmap32.Create;
  bmp32.Assign( pgm );
  Memo1.Lines.Add(Format('File %s - loaded', [TestFileName]));
  TextureArray.CopyFrom( bmp32 );
  Memo1.Lines.Add('Copied from host to device array');
  TurnPicture.Launch;
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
  SetGLSceneMediaDir();
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
