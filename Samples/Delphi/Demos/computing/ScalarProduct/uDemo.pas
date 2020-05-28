unit uDemo;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  //CUDA
  GLSCUDACompiler, GLSCUDAContext, GLSCUDA, GLSCUDAUtility;


type
  TForm1 = class(TForm)
    GLSCUDA1: TGLSCUDA;
    GLSCUDADevice1: TGLSCUDADevice;
    GLSCUDACompiler1: TGLSCUDACompiler;
    Memo1: TMemo;
    Button1: TButton;
    MainModule: TCUDAModule;
    scalarProdGPU: TCUDAFunction;
    deviceA: TCUDAMemData;
    deviceB: TCUDAMemData;
    deviceC: TCUDAMemData;
    hostC_GPU: TCUDAMemData;
    hostB: TCUDAMemData;
    hostC_CPU: TCUDAMemData;
    hostA: TCUDAMemData;
    _Z13scalarProdGPUPfS_S_ii_d_C: TCUDAFuncParam;
    _Z13scalarProdGPUPfS_S_ii_d_A: TCUDAFuncParam;
    _Z13scalarProdGPUPfS_S_ii_d_B: TCUDAFuncParam;
    _Z13scalarProdGPUPfS_S_ii_vectorN: TCUDAFuncParam;
    _Z13scalarProdGPUPfS_S_ii_elementN: TCUDAFuncParam;
    procedure Button1Click(Sender: TObject);
    procedure scalarProdGPUParameterSetup(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  {: Total number of input vector pairs; arbitrary }
  VECTOR_N = 256;
  {: Number of elements per vector; arbitrary,
     but strongly preferred to be a multiple of warp size
     to meet memory coalescing constraints }
  ELEMENT_N = 4096;

procedure scalarProdCPU(hC, hA, hB: TCUDAMemData; vectorN, elementN: Integer);
var
  vec, pos, vectorBase, vectorEnd: Integer;
  sum: Double;
  A, B: Single;
begin
  for vec := 0 to vectorN - 1 do
  begin
    vectorBase := elementN * vec;
    vectorEnd := vectorBase + elementN;

    sum := 0;
    for pos := vectorBase to vectorEnd - 1 do
    begin
      A := hA.Data<Single>(pos).Scalar;
      B := hB.Data<Single>(pos).Scalar;
      sum := sum + A * B;
    end;
    hC.Data<Single>(vec).Scalar := sum;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  timer: Cardinal;
  sumDelta, sumRef, L1norm: Double;
  val1, val2, delta: Single;
begin
  if not InitCUTIL then
  begin
    Memo1.Lines.Add('Can''t load cutil32.dll');
    exit;
  end;

  cutCreateTimer( Timer );

  Memo1.Lines.Add('Initializing data...');
  hostA.Width := VECTOR_N * ELEMENT_N;
  hostB.Width := VECTOR_N * ELEMENT_N;
  hostC_CPU.Width := VECTOR_N;
  hostC_GPU.Width := VECTOR_N;
  hostA.RawData;
  hostB.RawData;
  hostC_CPU.RawData;
  hostC_GPU.RawData;

  Memo1.Lines.Add('...allocating GPU memory.');
  deviceA.Width := VECTOR_N * ELEMENT_N;
  deviceB.Width := VECTOR_N * ELEMENT_N;
  deviceC.Width := VECTOR_N;
  deviceA.RawData;
  deviceB.RawData;
  deviceC.RawData;

  Memo1.Lines.Add('...generating input data in CPU mem.');
  // Generating input data on CPU
  for I := 0 to VECTOR_N * ELEMENT_N - 1 do
  begin
    hostA.Data<Single>(I).Scalar := Random;
    hostB.Data<Single>(I).Scalar := Random;
  end;

  Memo1.Lines.Add('...copying input data to GPU mem.');
  // Copy options data to GPU memory for further processing
  hostA.CopyTo(deviceA);
  hostB.CopyTo(deviceB);
  Memo1.Lines.Add('Data init done.');

  Memo1.Lines.Add('Executing GPU kernel...');
  cutResetTimer( Timer );

  cutStartTimer( Timer );
  scalarProdGPU.Launch;
  cutStopTimer( Timer );

  Memo1.Lines.Add('Launch finished.');
  Memo1.Lines.Add(Format('GPU time: %f (ms)', [cutGetTimerValue( Timer )]));

  Memo1.Lines.Add('Reading back GPU result...');
  // Read back GPU results to compare them to CPU results
  deviceC.CopyTo(hostC_GPU);
  Memo1.Lines.Add('Checking GPU results...');
  Memo1.Lines.Add('...running CPU scalar product calculation');

  scalarProdCPU(hostC_CPU, hostA, hostB, VECTOR_N, ELEMENT_N);

  Memo1.Lines.Add('...comparing the results');

  // Calculate max absolute difference and L1 distance
  // between CPU and GPU results
  sumDelta := 0;
  sumRef := 0;
  for I := 0 to VECTOR_N - 1 do
  begin
    val1 := hostC_GPU.Data<Single>(I).Scalar;
    val2 := hostC_CPU.Data<Single>(I).Scalar;
    delta := Abs(val1 - val2);
    sumDelta := sumDelta + delta;
    sumRef := sumRef + val2;
  end;

  L1norm := sumDelta / sumRef;
  Memo1.Lines.Add(Format('L1 error: %f', [L1norm]));
  if L1norm < 1e-6 then
    Memo1.Lines.Add('TEST PASSED')
  else
    Memo1.Lines.Add('TEST FAILED');

  cutDeleteTimer( timer );
end;

procedure TForm1.scalarProdGPUParameterSetup(Sender: TObject);
begin
  with scalarProdGPU do
  begin
    SetParam(deviceC);
    SetParam(deviceA);
    SetParam(deviceB);
    SetParam(VECTOR_N);
    SetParam(ELEMENT_N);
  end;
end;

end.

