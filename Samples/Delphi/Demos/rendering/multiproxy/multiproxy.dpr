{: Illustrates the use of TGLMultiProxy to perform discreet LOD.

   The MultiProxy object is used to switch automatically between three models
   of varying resolution (since I'm no good at modelling, these are only
   three resolution levels of a sphere).<br>
   You'll find the MultiProxy under the GLParticles object in DCTarget,
   the TGLParticles object is used to automatically duplicate the MultiProxy
   and its settings (happens in FormCreate).
}
program multiproxy;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
