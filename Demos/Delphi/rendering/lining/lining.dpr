{: Demo for using the Outline and the Hiddenline shaders.

   The HiddenLine shader provides some kind of 'technical drawing' rendering
   styles. You can change the settings for front lines and back lines to get
   various looks.

   The Outline shader is useful for displaying objects as 'selected' or for
   toon-style shading.
}
program lining;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
