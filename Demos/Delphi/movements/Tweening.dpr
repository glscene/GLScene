{ : Tweening demo.

  Original Tweener (caurina.transitions.Tweener) is a Class used to create tweenings
  and other transitions via ActionScript code for projects built on the Flash platform.

  Current demo is an example project of the GLScene's delphi implementation of
  the tweener library in GLAnimationUtils.pas unit

  <b>History : </b><font size=-1><ul>
  <li>11/10/12 - YP - Created by Yann Papouin
  </ul>
}

program Tweening;

uses
  Forms,
  TweeningFm in 'TweeningFm.pas' {FormTweening};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTweening, FormTweening);
  Application.Run;

end.
