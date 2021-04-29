{: Basic particle system.

   This is a very basic use of the particle systems in GLScene : colored
   alos (sprites) are created randomly with fade-in and fade-out effects, and
   the whole particle system rotates slowly (particles do not move in this
   sample). Particles live for 10 seconds, and are created every 300 ms.
   An inertia behaviour takes care of the rotation and cadencer makes the whole
   thing move.

   TGLParticles works with a "template", this the mother of all particles, and
   it is duplicated when a new particle is requested. The template is the
   first (top) child of TGLParticles, other children are considered to be
   particles (don't temper directly with TGLParticles children !). In this
   sample, a sprite is the only child, and as such make a simple particle
   template, particles can be very complex : if the sprite was having children,
   these would be part of the particle too, and their children and the children
   of their children and... you got it.

   Some eye candy here, but if you don't have a 3D hardware, reduce the window
   size to avoid slowdown. This one could make a nice screen-saver, this is
   left as an exercice to reader (hint : you just need to drop 1 component,
   type in 3 characters and press CTRL+F9).
}
program Particles;

uses
  Forms,
  ParticlesFm in 'ParticlesFm.pas' {FormParticles};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormParticles, FormParticles);
  Application.Run;
end.
