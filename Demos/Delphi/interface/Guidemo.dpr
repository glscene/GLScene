{: A sample for the Gui interface system.

   To use the Gui you must place a TGLGUILayout component on your form,
   this is a storage for Gui layouts within a single texture, each layout
   is identified by a name. All Layouts can be used with any Gui Component,
   however features like a forms title might look awkward on a checkbox...

   For correct usage a Material Library should be used.
   Both the TGLGUILayout and each gui component should have the same
   material from the material library.

   A BitmapFont or WindowsFont should also be used and applied to both
   layout and components.

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
   the glscene back bone: http://www.ting.it/gls.dll
   in the "Source" - "Core" Section called "An Overview: GLScene GUI"

	<b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added miFPS to output FPS
      <li>27/09/02 - JAJ - Creation
	</ul></font>
}
program Guidemo;

uses
  Forms,
  GuidemoFm in 'GuidemoFm.pas' {FormGuidemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGuidemo, FormGuidemo);
  Application.Run;
end.
