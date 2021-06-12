{ : Newton Game Dynamics Physics Engine demo.

  This exemple show Joints.
  Mouse1 to pick, Mouse2 to move camera.

  When you create Joints with TGLNGD, it's better if one of the two bodies is
  static.
  In debug view (If ShowJoint is true in manager), the blues lines represent
  pins direction, aquamarine dot represent pivot point, and aqua is connections
  between BaseSceneObjects.
  However if you create multiples connected joints
  (ex: FLOOR<--HINGE-->CUBE<--HINGE-->SPHERE),
  the debug view won't match to bodies positions because Joints are
  represented in global space. Debug view was made for design time.


  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - FP - Update for GLNGDManager
  <li>20/09/10 - FP - Created by Franck Papouin
  </ul>
}
program NewtonJoints;

uses
  Forms,
  NewtonJointsFm in 'NewtonJointsFm.pas' {FormNewtonJoints};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonJoints, FormNewtonJoints);
  Application.Run;
end.
