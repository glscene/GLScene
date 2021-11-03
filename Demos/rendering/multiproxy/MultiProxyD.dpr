{: Illustrates the use of TGLMultiProxy to perform discreet LOD.

   The MultiProxy object is used to switch automatically between three models
   of varying resolution (since I'm no good at modelling, these are only
   three resolution levels of a sphere).
   You'll find the MultiProxy under the GLParticles object in DCTarget,
   the TGLParticles object is used to automatically duplicate the MultiProxy
   and its settings (happens in FormCreate).
}
program MultiProxyD;

uses
  Forms,
  fMultiProxyD in 'fMultiProxyD.pas' {FormMultiProxy};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiProxy, FormMultiProxy);
  Application.Run;
end.
