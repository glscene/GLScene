{: Basic raycast/mesh sample.

   Demonstrating how to find the intersection point between eye-screen ray
   and a simple mesh in orthogonal and perspective views (click on the mushroom
   and intersection point and normal will be calculated).
}
program meshhit;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
