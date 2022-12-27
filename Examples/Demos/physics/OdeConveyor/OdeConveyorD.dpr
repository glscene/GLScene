{ : ODE Conveyor belt demo.

  This demo demontrates how to use the Motion1 parameter of ODE.

  Surface settings applied when a collision occured. In this situation, the ODE
  Manager uses surface settings of both objects:
  eg: muFinal = (muObject1 + muObject2)*0.5

  To choose the direction of the motion, we are changing the FDir-1 parameter of
  the collision contact.

  For a full explanation take a look at:
  http://opende.sourceforge.net/wiki/index.php/Manual_(All)#Contact

  Approximate coefficients of friction (from http://en.wikipedia.org/wiki/Friction)
  ------------------------------------
  Materials 	    Static friction (µs)
  ---------------------------------------------------
  Dry & clean 	Lubricated
  Aluminum 	      Steel 	  0.61
  Copper 	        Steel 	  0.53
  Brass 	        Steel 	  0.51
  Cast iron 	    Copper 	  1.05
  Cast iron 	    Zinc 	    0.85
  Concrete (wet) 	Rubber 	  0.30
  Concrete (dry) 	Rubber 	  1.0
  Concrete 	      Wood 	    0.62[7]
  Copper 	        Glass 	  0.68
  Glass 	        Glass 	  0.94
  Metal 	        Wood 	    0.2-0.6[7] 	  0.2 (wet)[7]
  Polythene 	    Steel 	  0.2[8] 	      0.2[8]
  Steel 	        Steel 	  0.80[8] 	    0.16[8]
  Steel 	        Teflon  	0.04[8] 	    0.04[8]
  Teflon 	        Teflon 	  0.04[8] 	    0.04[8]
  Wood 	          Wood 	    0.25-0.5[7] 	0.2 (wet)[7]

  <b>History : </b><font size=-1><ul>
  <li>19/09/10 - YP - Created by Yann Papouin
  </ul>
}
program OdeConveyorD;

uses
  Forms,
  fOdeConveyorD in 'fOdeConveyorD.pas' {FormOdeConveyor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormOdeConveyor, FormOdeConveyor);
  Application.Run;
end.
