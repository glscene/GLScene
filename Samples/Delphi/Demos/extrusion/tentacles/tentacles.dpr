{: Tentacles demo, a weird use for TGLPipe.

   Serves as a test for TGLPipe's ability to have a per-node color with smooth
   interpolation between nodes, and as some kind of sickening modern art...<br>
   Position of the nodes, radius and color are updated for each frame. Note that
   the TGLPipe's ObjectStyle is altered to make it "osDirectDraw": since the geometry
   is constantly altered, it's no use compiling/saving it for the next frame,
   setting the style to "osDirectDraw" tells GLScene the object should be rendered
   directly (faster when geometry constantly changes, slower  when geometry is
   static from one frame to the other).<br>
   Try commenting out that line and see for yourself ;).
}
program tentacles;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
