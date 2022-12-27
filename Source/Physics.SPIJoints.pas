//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Physics.SPIJoints;

(*
  This unit isn't used at all at the moment, just putting down some ideas
  for serial-link manipulators
*)

interface

uses
  GLS.VectorTypes,
  GLS.Scene,
  GLS.VectorGeometry;

type

  (*
    //
    //                         Joint(Z1)      Joint(Z3)
    //                          / \            |_| (Wrist)
    //                         / \/\           //
    //                        / / \ \         //
    //                       / /   \ \(1)    //(2)
    //             Link(0)  / /     \ \     //
    //                     / /       \ \   //
    //                    / /         \ \ //
    //                   / /           \/\/
    //         Joint(Z0)/ /             \/
    //               |------|          Joint(Z2)
    //               | Base |
    //     --------------------------------------------
    //     X(n) = Common normal between joint axis Z(n-1) & Z(n)
  *)

  TGLBaseJoint = class(TObject)
  end;

  TGLBaseLink = class(TObject)
  end;

  TGLJoint = class(TGLBaseJoint)
    Link1: TGLBaseLink; // if Link1 is nil, assumed to be base
    Link2: TGLBaseLink; // if Link2 is nil, assumed to be wrist
    // Object1:TGLBaseSceneObject;
    // Object2:TGLBaseSceneObject;
  end;

  // Links are mainly for used for Serial-Link manipulators

  // Direct & Inverse Kinematics algorithms are planned
  TGLLink = class(TGLBaseLink)
    // Link Parameters
    fLinkLength: Real; // Length of common normal which is orthogonal to both
    // joint axes Z[n-1] and Z[n] (a.k.a.  L)
    fTwistAngle: Real; // Twist angle between joint axes, if joint frames were
    // coincident (a.k.a.  Theta?)
    fLinkAngle: Real;
    // Angle between common normals X[n-1] and X[n] (a.k.a.  Alpha?)
    fLinkDistance: Real;
    // Distance along joint axis Z[n-1] between intersection
    // points of common normals X[n-1] and X[n] (a.k.a.  d)
(*
    //
    // A-Matrix
    // [ Cos(Theta)   -Sin(Theta)*Cos(Alpha)    Sin(Theta)*Sin(Alpha)   L*Cos(Theta) ]
    // [ Sin(Theta)    Cos(Theta)*cos(Alpha)   -Cos(Theta)*Sin(Alpha)   L*Sin(Theta) ]
    // [    0            Sin(Alpha)                   Cos(Alpha)           d         ]
    // [    0               0                            0                 1         ]
    //
*)
    A: TGLMatrix;
    constructor Create(LinkLength, TwistAngle, LinkAngle, LinkDistance: Real);
    // constructor Create();virtual;
  end;

  // see html file for description of the different links
(*
  // Type 1 Two-Link Manipulator
  //
  // _ ________
  // / \        \
  // / \/________/
  // /  /
  // /  /
  // /  /
  // /  /
  // /  /
  // \_/
  //
*)
  TGLType1Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
(*
    // A-Matrix
    // [ C1   -S1    0    L1C1 ]
    // [ S1    C1    0    L1S1 ]
    // [  0     0    1      0  ]
    // [  0     0    0      1  ]
    //
*)
    constructor Create(Length, Angle: Real);
  end;
(*
  // Type 2 Two-Link Manipulator
  // ___
  // /   \___________________
  // | + |___________________|
  // \   /
  // | |
  // | |
  // | |
  // | |
  // | |
  // | |
  // __|_|__
  // |___.___|
  //
*)
  TGLType2Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
    constructor Create(Length, Angle: Real);
  end;
(*
  // Type 3 Two-Link Manipulator
  // _______
  // _________|       |__
  // <-|_________|       |__|->
  // |__   __|
  // | |
  // | |
  // | |
  // | |
  // | |
  // _|_|_
  // |__.__|
*)
  TGLType3Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
(*
    // A-Matrix
    // [  1    0    0    0  ]
    // [  0    1    0    0  ]
    // [  0    0    1    d2 ]
    // [  0    0    0    1  ]
    //
*)
    constructor Create(Length, Angle: Real);
  end;

  TGLType4Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
    constructor Create(Length, Angle: Real);
  end;
(*
  // Type 5 Two-Link Manipulator
  //
  // _
  // ________|_|__
  // <-|_____________|->
  // | |
  // | |
  // | |
  // | |
  // | |
  // __| |__
  // |__| |__|
  // |_|
  // |
  // V
  //
*)
  TGLType5Link = class(TGLLink)
    Length: Real; // variable
    constructor Create(Length, Angle: Real);
  end;

  TGLType6Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
    constructor Create(Length, Angle: Real);
  end;

  TGLType7Link = class(TGLLink)
    Length: Real; // fixed
    Angle: Real; // variable
    constructor Create(Length, Angle: Real);
  end;

  TGLType8Link = class(TGLLink)
    Length: Real; // variable
    constructor Create(Length, Angle: Real);
  end;

  TGLPrismaticJoint = class(TGLJoint)
  end;

  TGLRevoluteJoint = class(TGLJoint)
  end;

  TGLBallAndSocketJoint = class(TGLJoint)
  end;

//======================================================================
implementation
//======================================================================

constructor TGLLink.Create(LinkLength, TwistAngle, LinkAngle,
  LinkDistance: Real);
begin
  fLinkLength := LinkLength;
  fTwistAngle := TwistAngle;
  fLinkAngle := LinkAngle;
  fLinkDistance := LinkDistance;
end;

constructor TGLType1Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType2Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType3Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType4Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType5Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType6Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType7Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

constructor TGLType8Link.Create(Length, Angle: Real);
begin
  inherited Create(Length, 0, Angle, 0);
end;

end.
