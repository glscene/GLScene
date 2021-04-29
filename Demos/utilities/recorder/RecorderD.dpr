{: This sample demonstrates use of the TGLAVIRecorder to create an AVI file.

   The animation is taken from the "Hierarchy" sample, all the recording takes
   place in Button1Click.

   Be aware that if you use default compression, you will likely get a lossless,
   low compression codec (which may be good if you want the highest quality),
   but you can specify a codec, for instance DiVX (www.divx.com) if you
   installed it, for high compression video.
   The codec can be choosed with the Compressor property of TGLAVIRecorder.
}
program RecorderD;

uses
  Forms,
  fMainD in 'fMainD.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
