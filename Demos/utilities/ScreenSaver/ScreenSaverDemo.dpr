{: Sample screen-saver for GLScene using TScreenSaver.

	I've kept the OpenGL stuff to a minimum here, the saver is just animating
	a cube on your screen, with 3 colored dancing light source. All movements
	are handled through Inertia behaviours and the Cadencer, this is why you
	won't find any	animation code here :).

	This saver has two forms : this one is the main saver screen, Form2 is the
	properties screen (adjust torus tesselation).
	Apart from dropping a TScreenSaver on Form1 and handling OnpropertiesRequested
	to display Form2, I did these things :<ul>
	<li>changed the extension to "scr" in Project/Options/Application
	<li>removed Form2 from the autocreate list (I moved the code from project1.dpr
		to the OnPropertiesRequest event code
	</ul>
	In most cases, these are the only steps required for making a screen-saver.

	NB : Password stuff does not work under NT.
}
program ScreenSaverDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$E scr}

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
