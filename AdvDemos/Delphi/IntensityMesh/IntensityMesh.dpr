{: Shows how to use 1D palettes to visualize "intensity" on a mesh.

   Typical application is interactive representation of FEA (Finite Element
   Analysis), the palette texture can be used with discrete colors
   (use "nearest" filtering) or continuous colors (use "linear" filtering).
   Palette scale and offset can then be adjusted via texture scaling/off.

   Representation uses a simple multipass shader to overlay the model's
   wireframe using smoothed lines.

   Sample Data represents the Von Mises Stress (usually is used to predict
   steel yielding) over the structure of a tubular tower for electrical
   power lines. That stress is the result of the loads from cables,
   transversal wind and self weigth.

   (Sample Data contributed by Carlos Ferreira)
}
program IntensityMesh;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};



{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
