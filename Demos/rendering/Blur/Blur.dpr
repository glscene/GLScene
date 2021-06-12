{
 Example showing usage of GLBlur

 Adding it to the scene root will blur all the scene.
 Adding a GLBlur to an object will make it blur only that object
 (note that you might need to sort objects to avoid z-order issues
  or you can set GLScene1.ObjectSorting = osRenderFarthestFirst)

 You can choose a GLBlur effect from the "presets" property or
 set the parameters yourself (see GLBlur.pas)

}
program Blur;

uses
  Forms,
  BlurFm in 'BlurFm.pas' {FormBlur};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBlur, FormBlur);
  Application.Run;
end.
