{: A crude but effective way of "picking" in an height field.

   This demo implements a simple way to "pick" a clicked sector in an
   heightfield, using it for a basic "3D paint". In "paint" mode,
   the left button will paint in blue and the right one in red.
   The "rotate" mode allows you to move around the height field
   to continue your painting from a different angle.
   No you can't save your art ;)

   The picking is performed by getting the 3D coordinates from the
   2D mouse coordinates via PixelRayToWorld (which reads the depth
   buffer), and converting those absolute 3D coordinates to local
   coordinates of the HeightField, the last steps are then obvious.
   This method is approximate in that its precision highly depends on
   that of the ZBuffer. It will also fail if some objects "obstructs"
   picking (prevent the read of a proper ZBuffer value).

   Some simple improvements left as an exercice to the reader:<ul>
   <li>add a "terraform" mode allowing to raise and lower height values
   <li>add more colors to the paint mode
   <li>allow painting the top and bottom of the heightfield with different colors
   <li>painting to a texture instead of a grid
   </ul>
}
program HFpick;

uses
  Forms,
  HFPickFm in 'HFPickFm.pas' {FormHFPickFm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHFPickFm, FormHFPickFm);
  Application.Run;
end.
