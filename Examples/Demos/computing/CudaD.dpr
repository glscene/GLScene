program CudaD;

uses
  Vcl.Forms,
  fCudaD in 'fCudaD.pas' {FormCudaD},
  fFastFourierD in 'FastFourierTransformation\fFastFourierD.pas' {FormFFT},
  fPostProcessingD in 'PostProcessing\fPostProcessingD.pas',
  fScalarProductD in 'ScalarProduct\fScalarProductD.pas',
  fSimpleTexD in 'SimpleCUDATexture\fSimpleTexD.pas',
  fFluidsD in 'StableFluids\fFluidsD.pas',
  fVertexGenD in 'VertexDataGeneration\fVertexGenD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCudaD, FormCudaD);
  Application.Run;
end.
