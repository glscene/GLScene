{: Displaying alpha-blended 2D bitmaps with GLScene.

   This sample is a very basic picture viewer, using OpenGL for displaying
   images and maintaining a cursor with alpha-blended trail.

   TGLHUDSprite objects are used to display the bitmap and the cursor/trail. The
   cursor/trail bitmaps share a single material stored in the material library.

   The trail uses a particle-system component to track trail bitmaps, each time
   the mouse is moved on the scene viewer, the HSCursor sprite is moved accordingly
   and a new trail bitmap is created and initialized. Trail bitmaps have a slowly
   decreasing alpha channel value and take 5 seconds to go from fully opaque to
   fully invisible, and when invisibility is reached, they are killed.

   When trails are not active (toggled on/off by a menu item), the status bar
   displays the color (RGB) of the point below the cursor, by using the
   GetPixelColor function.
}
program Cursor;

uses
  Forms,
  CursorFm in 'CursorFm.pas' {FormCursor};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCursor, FormCursor);
  Application.Run;
end.
