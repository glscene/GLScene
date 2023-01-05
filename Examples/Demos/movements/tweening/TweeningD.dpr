(* Tweening demo.

  Original Tweener (caurina.transitions.Tweener) is a Class used to create tweenings
  and other transitions via ActionScript code for projects built on the Flash platform.

  Current demo is an example project with implementation of
  the tweener library in GLAnimationUtils.pas unit

*)

program TweeningD;

uses
  Forms,
  fTweeningD in 'fTweeningD.pas' {FormTweening};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTweening, FormTweening);
  Application.Run;

end.
