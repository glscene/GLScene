{: The Simple Paint Program using Gui components from GLScene.

   To use the Gui you must place a TGLGUILayout component on your form,
   this is a storage for Gui layouts within a single texture, each layout
   is identified by a name. All Layouts can be used with any Gui Component,
   however features like a forms title might look awkward on a checkbox...

   For correct usage a Material Library should be used.

   If a BitmapFont or WindowsFont is applied to a layout it is auto set to
   the components, however it is perfectly valid to set it to someother font.

   To Interface correctly, handlers must send the mouse and key events
   to a root gui component, this can be any of the keyboard aware gui
   components or a RootControl(a tramsparent component with no other purpose)
   Plus the root gui component should have its DoChanges invoked, this
   makes form movement and other mouse/key events changes fluintly
   match the rendering process, eg no flicker.

   All other Gui component must be below the root control in the GLScene
   hierachy, this is a rule which allows you to seperate gui from the
   rest of glscene and save some clock cycles on <tab> operations.
   Plus mouse events...

   For a more thorough look on the gui please check out my article on
   the caperaven site: http://caperaven.co.za
   under "Online documentation" called "Gui Interface"

   The two BMP's pen.bmp and brush.bmp have been published by Borland in the
   Doc/GraphEx demo that comes with Delphi.
   The rest is MPL as part of the GLScene project.

   NOTICE IS YOU HAVE A LOW FRAME RATE and you feel the drawing is lagging to
   far behind, try setting GLCanvas.MaxInvalidRenderCount to a lower value in
   the formcreate event.


	<b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Changed FPS output to miFPS.Caption instead of Form1.Caption
      <li>01/05/03 - JAJ - Creation
	</ul></font>
}
program GuiPaintD;

uses
  Forms,
  fGuiPaintD in 'fGuiPaintD.pas' {FormGuiPaint};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGuiPaint, FormGuiPaint);
  Application.Run;
end.
