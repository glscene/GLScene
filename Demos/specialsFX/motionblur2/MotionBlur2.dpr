{: A Demo of the TGLMotionBlur component
   Version history
    02/03/07 - DaStr - Updated GLSimpleNavigation component
    25/02/07 - Dave Gravel - Initial version
}
program MotionBlur2;

uses
  Forms,
  MotionBlur2Fm in 'MotionBlur2Fm.pas' {FormMotionBlur2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMotionBlur2, FormMotionBlur2);
  Application.Run;
end.
