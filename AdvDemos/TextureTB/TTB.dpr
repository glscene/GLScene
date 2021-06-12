{: A simple utility that allows combining RGB and Alpha channel into a single
   32 bits texture, also allows to view RGB & Alpha channel of a 32 bits texture.<br>
   The implementation isn't high performance, just sufficiently fast for
   interactive use.

   Eric Grange / GLScene<br>
   http://glscene.org
}
program TTB;

uses
  Forms,
  FTTBMain in 'FTTBMain.pas' {TTBMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTTBMain, TTBMain);
  Application.Run;
end.
