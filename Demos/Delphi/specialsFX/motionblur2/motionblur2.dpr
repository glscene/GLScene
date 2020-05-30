{: A Demo of the TGLMotionBlur component
   Version history
    02/03/07 - DaStr - Updated GLSimpleNavigation component
    25/02/07 - Dave Gravel - Initial version
}
program MotionBlur2;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
