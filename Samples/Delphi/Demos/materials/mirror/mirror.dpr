{: GLMirror demo and sample.

   Depiste its simplistic look, this sample showcases all the dos and don'ts
   of reflections with TGLMirror, this is a powerfull mirroring component,
   but it must be handled with care and knowingly.
   The object that must be mirrored is specified through the "MirrorObject"
   property, you can specify only one, but with proper use of dummy cubes and/or
   proxies, this is not a real limitation, and allows you to select what will be
   reflected (each reflected object must be rendered twice). If no MirrorObject
   is specified, the whole scene will be mirrored.

   If you want your mirror to be transparent, you must respect a rendering order
   and have the non-transparent objects rendered last (this includes the mirror,
   like any other blended object, see materials/transparency for an explanation).<br>
   Also note that some of the options (stenciling, clearZBuffer) <b>require</b>
   a stencil buffer (must be enabled in the viewer's buffer), but stenciling may
   not always be hardware accelerated (modern boards will support it).

   There is also a variety of settings to the right of the screen, those adjust
   internal options that have a direct impact of what the mirror will be able
   to do right, and how much time rendering will take. The scene contains three
   groups of objects of interest:<ul>
   <li>non-reflecting ones: green torus and cylinder, these are behind the mirror,
      there are no particular issues with non-reflecting objects in front of a
      mirror (except objects not reflecting...), but by playing with the settings
      (ClearZBuffer especially) you'll notice they cause some artifacts if improperly
      handled.
   <li>teapot group: those are reflected. See how disabling stencil will cause
      the reflected teapot to be visible outside of the mirror. If your mirror
      is in a wall (opaque on all sides around the mirror), you may not have to
      care about stenciling (the wall will overdraw mirror images).
   <li>lone inclined gray cylinder: this one is a don't, it's an object that is
      reflecting but that goes through the mirror... well, you can do it,
      but you have to activate PlaneClip, otherwise objects on the other side
      of the mirror get reflected on the "wrong" side... (uncheck the option
      and see for yourself). PlaneClip is better avoided, it can make your FPS
      drop significantly on some 3D boards.
   </ul>

   In addition to being opaque, transparent or semi-transparent, the mirror
   can also be textured as usual.

   Final note: T&L boards like the GeForce will be the one taking the most
      performance hit from the PlaneClip because it basicly turns-off hardware T&L.
      The glEval-based teapot also performs (relatively) poorly on those boards,
      while on an old-fashioned TNT2 f.i., plane clipping has a negligible
      performance impact.
}
program mirror;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
