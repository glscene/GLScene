{: Simple motion blur demo.

   This demo illustrates a simple technique to obtain a motion blur: using
   a plane that covers all the viewport that is used to transparently blend
   the previous frame. By adjusting the transparency, you control how many
   frames are taken into account in the blur.
   Since it is a frame-to-frame mechanism, the result is highly dependant
   on framerate, which is illustrated here by turning VSync ON or OFF in the
   demo (hit V or S key). You can control the number of frames with the up
   and down arrow key.
   In a more complex application, you will have to implement a framerate
   control mechanism (relying on VSync isn't such a control mechanism,
   VSync frequency is a user setting that depends on the machine and monitor).

   Original demo by Piotr Szturmaj.
}
program MotionBlur;

uses
  Forms,
  MotionBlurFm in 'MotionBlurFm.pas' {FormMotionBlur};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMotionBlur, FormMotionBlur);
  Application.Run;
end.
