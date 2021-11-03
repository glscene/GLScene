//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.ODEImport;

(* ************************************************************************
  *                                                                       *
  * Open Dynamics Engine, Copyright (C) 2001,2020 Russell L. Smith.       *
  *                All rights reserved. Web: www.ode.org                  *
  *                                                                       *
  * This library is free software; you can redistribute it and/or         *
  * modify it under the terms of EITHER:                                  *
  *   (1) The GNU Lesser General Public License as published by the Free  *
  *       Software Foundation; either version 2.1 of the License, or (at  *
  *       your option) any later version. The text of the GNU Lesser      *
  *       General Public License is included with this library in the     *
  *       file LICENSE.TXT.                                               *
  *   (2) The BSD-style license that is included with this library in     *
  *       the file LICENSE-BSD.TXT.                                       *
  *                                                                       *
  * This library is distributed in the hope that it will be useful,       *
  * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
  * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
  *                                                                       *
  ************************************************************************ *)

(* ************************************************************************
  Some notes;

  Sometimes it's easier and faster to refer to the members of the objects
  directly, like Body.Pos, Geom.Data or Body.Mass, instead of calling the built
  in routines. Be very careful if you do this, because some of the built in
  routines alter states when called.

  Examples

  bGeomSetBody(Geom, Body); // This method must be used so
  Geom.Body := Body; // DON'T DO THIS

  Setting the mass of a body is another example. Typically, reading members is
  fine, but before writing directly to member fields yourself, you should check
  the c source and see what the function/procedure does. The source can be found
  at http://www.q12.org/ode/

  ************************************************************************ *)

interface

// remove . from line below if you are using a generic ODE DLL
{$DEFINE VanillaODE}
{$IFNDEF VanillaODE}
{$DEFINE PARODE}
{$ENDIF}

uses
  System.Classes,
  GLS.ModuleLoader;

const

  // ********************************************************************
  //
  // ODE precision:
  //
  // ODE can be run in Single or Double precision, Single is less precise,
  // but requires less memory.
  //
  // If you choose to run in Single mode, you must deploy the single precision
  // dll (this is default)
  //
  // If you choose to run in Double mode, you must deploy the double precision
  // dll (named ode_double.dll and located in the dll directory)

{$DEFINE cSINGLE}  // Insert . before "$" to make ODEImport double based
{$IFDEF WIN32}
{$IFDEF cSINGLE}
  ODEDLL = 'ode32s.dll';
{$ELSE}
  ODEDLL = 'ode32d.dll';
{$ENDIF}
{$ENDIF}
{$IFDEF WIN64}
{$IFDEF cSINGLE}
  ODEDLL = 'ode64s.dll';
{$ELSE}
  ODEDLL = 'ode64d.dll';
{$ENDIF}
{$ENDIF}
{$IFDEF UNIX}
  ODEDLL = 'libode.so';
{$ENDIF}
{$IFDEF MACOS}
  ODEDLL = 'libode.dylib';
{$ENDIF}
{$IFDEF DARWIN} // MacOS X
  ODEDLL = 'libode.dylib';
{$ENDIF}

const
  // enum (* TRIMESH_FACE_NORMALS, TRIMESH_LAST_TRANSFORMATION ; *)
  TRIMESH_FACE_NORMALS = 0;
  TRIMESH_LAST_TRANSFORMATION = 1;

  // Just generate any contacts (disables any contact refining).
  CONTACTS_UNIMPORTANT = $80000000;

  // Change: New Type added, syntax enforcement
type
  TJointFlag = Integer;

  // These consts now have defined types
const
  // if this flag is set, the joint was allocated in a joint group
  dJOINT_INGROUP: TJointFlag = 1;
  (* if this flag is set, the joint was attached with arguments (0,body).
    our convention is to treat all attaches as (body,0), i.e. so node[0].body
    is always nonzero, so this flag records the fact that the arguments were swapped *)
  dJOINT_REVERSE: TJointFlag = 2;
  (* if this flag is set, the joint can not have just one body attached to it,
    it must have either zero or two bodies attached *)
  dJOINT_TWOBODIES: TJointFlag = 4;

  // Change: New Type added, syntax enforcement
type
  TdContactType = Integer;

  // These consts now have defined types
const
  dContactMu2: TdContactType = $0001;
  dContactFDir1: TdContactType = $0002;
  dContactBounce: TdContactType = $0004;
  dContactSoftERP: TdContactType = $0008;
  dContactSoftCFM: TdContactType = $0010;
  dContactMotion1: TdContactType = $0020;
  dContactMotion2: TdContactType = $0040;
  dContactMotionN: TdContactType = $0080;
  dContactSlip1: TdContactType = $0100;
  dContactSlip2: TdContactType = $0200;

  dContactApprox0: TdContactType = $0000;
  dContactApprox1_1: TdContactType = $1000;
  dContactApprox1_2: TdContactType = $2000;
  dContactApprox1: TdContactType = $3000;

  // Change: New Type added, syntax enforcement
type
  TBodyFlags = Integer;

  // These consts now have defined types
const
  dxBodyFlagFiniteRotation: TBodyFlags = 1; // use finite rotations
  dxBodyFlagFiniteRotationAxis: TBodyFlags = 2;
  // use finite rotations only along axis
  dxBodyDisabled: TBodyFlags = 4; // body is disabled
  dxBodyNoGravity: TBodyFlags = 8; // body is not influenced by gravity

type
{$IFDEF cSINGLE}
  TdReal = single;
{$ELSE}
  TdReal = double;
{$ENDIF}
  PdReal = ^TdReal;

  { define cODEDebugEnabled } // Debug mode

  (* Pointers to internal ODE structures to reproduce C++ classes in Delphi *)
  PdxJointGroup = ^TdxJointGroup;
  TdJointGroupID = PdxJointGroup;

  TdxJointGroup = record
    num: Integer;
    stack: pointer;
  end;

  PdxJointLimitMotor = ^TdxJointLimitMotor;

  TdxJointLimitMotor = record
    vel, fmax: TdReal; // powered joint: velocity, max force
    lostop, histop: TdReal; // joint limits, relative to initial position
    fudge_factor: TdReal; // when powering away from joint limits
    normal_cfm: TdReal; // cfm to use when not at a stop
    stop_erp, stop_sfm: TdReal; // erp and cfm for when at joint limit
    bounce: TdReal; // restitution factor
    // variables used between getInfo1() and getInfo2()
    limit: Integer; // 0=free, 1=at lo limit, 2=at hi limit
    limit_err: TdReal; // if at limit, amount over limit
  end;

  TdRealArray = array [0 .. 15] of TdReal;
  PdRealArray = ^TdRealArray;

  // typedef dReal dVector33[4];
  TdVector3 = array [0 .. 3] of TdReal;
  // Q: Why isn't TdVector3 = array[0..2] of TdReal? A: Because of SIMD alignment.
  PdVector3 = ^TdVector3;

  Pd3Axis = ^Td3Axis;
  Td3Axis = array [0 .. 2] of TdVector3;

  PdInteger3 = ^TdInteger3;
  TdInteger3 = array [0 .. 2] of Integer;

  PdxJointLimitMotor3 = ^TdxJointLimitMotor3;
  TdxJointLimitMotor3 = array [0 .. 2] of TdxJointLimitMotor;

  // typedef dReal dVector4[4];
  TdVector4 = array [0 .. 3] of TdReal;
  PdVector4 = ^TdVector4;

  // typedef dReal dMatrix3[4*3];
  TdMatrix3 = array [0 .. 4 * 3 - 1] of TdReal;
  PdMatrix3 = ^TdMatrix3;

  TdMatrix3_As3x4 = array [0 .. 2, 0 .. 3] of TdReal;
  PdMatrix3_As3x4 = ^TdMatrix3_As3x4;

  // typedef dReal dMatrix4[4*4];
  TdMatrix4 = array [0 .. 4 * 4 - 1] of TdReal;
  PdMatrix4 = ^TdMatrix4;

  // typedef dReal dMatrix6[8*6];
  TdMatrix6 = array [0 .. 8 * 6 - 1] of TdReal;
  PdMatrix6 = ^TdMatrix6;

  // typedef dReal dQuaternion[4];
  TdQuaternion = TdVector4; // array[0..3] of TdReal;
  PdQuaternion = ^TdQuaternion;

  // No typedef for AABB
  TdAABB = array [0 .. 5] of TdReal;

  TdMass = record
    mass: TdReal; // total mass of the rigid body
    c: TdVector4; // center of gravity position in body frame (x,y,z)
    I: TdMatrix3; // 3x3 inertia tensor in body frame, about POR
  end;

  PdMass = ^TdMass;

  TdxAutoDisable = record
    idle_time: TdReal; // time the body needs to be idle to auto-disable it
    idle_steps: Integer; // steps the body needs to be idle to auto-disable it
    linear_average_threashold: TdReal;
    // linear (squared) average velocity threshold
    angular_average_threashold: TdReal;
    // angular (squared) average velocity threshold
    average_samples: longword;
    // size of the average_lvel and average_avel buffers
  end;

  TdxDampingParameters = record
    linear_scale: TdReal; // multiply the linear velocity by (1 - scale)
    angular_scale: TdReal; // multiply the angular velocity by (1 - scale)
    linear_threahold: TdReal; // linear (squared) average speed threshold
    angular_threashold: TdReal; // angular (squared) average speed threshold
  end;

  TdxContactParameters = record
    max_vel: TdReal; // maximum correcting velocity
    min_depth: TdReal; // thickness of 'surface layer'
  end;

  TdxQuickStepParameters = record
    num_iterations: Integer; // number of SOR iterations to perform
    w: TdReal; // the SOR over-relaxation parameter
  end;

  PdxGeom = ^TdxGeom;
  PPdxGeom = ^PdxGeom;

  // Whenever a body has its position or rotation changed during the
  // timestep, the callback will be called (with body as the argument).
  // Use it to know which body may need an update in an external
  // structure (like a 3D engine).
  TdMovedCallback = procedure(o1: PdxGeom); cdecl;

  // Per triangle callback. Allows the user to say if he wants a collision with
  // a particular triangle.
  TdTriCallback = function(TriMesh, RefObject: PdxGeom;
    TriangleIndex: Integer): Integer;

  // Per object callback. Allows the user to get the list of triangles in 1
  // shot. Maybe we should remove this one.
  TdTriArrayCallback = procedure(TriMesh, RefObject: PdxGeom;
    TriIndices: PIntegerArray; TriCount: Integer);

  // Allows the user to say if a ray collides with a triangle on barycentric
  // coords. The user can for example sample a texture with alpha transparency
  // to determine if a collision should occur.
  TdTriRayCallback = function(TriMesh, Ray: PdxGeom; TriangleIndex: Integer;
    u, v: TdReal): Integer;

  PdxWorld = ^TdxWorld;

  PdObject = ^TdObject;
  PPdObject = ^PdObject;

  TdObject = record
    World: PdxWorld; // world this object is in
    next: PdObject; // next object of this type in list
    tome: PPdObject; // pointer to previous object's next ptr
    userdata: pointer; // user settable data
    tag: Integer; // used by dynamics algorithms
  end;

  PdxBody = ^TdxBody;
  PdxJoint = ^TdxJoint;
  TdJointID = PdxJoint;

{$IFDEF PARODE}
  TdJointBreakCallback = procedure(joint: TdJointID); cdecl;

  TJointBreakMode = Integer;

  PdxJointBreakInfo = ^TdxJointBreakInfo;

  TdxJointBreakInfo = record
    flags: Integer;
    b1MaxF: TdVector3; // maximum force on body 1
    b1MaxT: TdVector3; // maximum torque on body 1
    b2MaxF: TdVector3; // maximum force on body 2
    b2MaxT: TdVector3; // maximum torque on body 2
    callback: TdJointBreakCallback;
    // function that is called when this joint breaks
  end;
{$ENDIF}

  PdxJointNode = ^TdxJointNode;

  TdxJointNode = record
    joint: PdxJoint; // pointer to enclosing dxJoint object
    body: PdxBody; // *other* body this joint is connected to
    next: PdxJointNode; // next node in body's list of connected joints
  end;

  // info returned by getInfo1 function. the constraint dimension is m (<=6).
  // i.e. that is the total number of rows in the jacobian. `nub' is the
  // number of unbounded variables (which have lo,hi = -/+ infinity).
  TJointInfo1 = record
    m, nub: Integer;
  end;

  // info returned by getInfo2 function
  TJointInfo2 = record
    // integrator parameters: frames per second (1/stepsize), default error
    // reduction parameter (0..1).
    fps, erp: TdReal;
    // for the first and second body, pointers to two (linear and angular)
    // n*3 jacobian sub matrices, stored by rows. these matrices will have
    // been initialized to 0 on entry. if the second body is zero then the
    // J2xx pointers may be 0.
    J1l, J1a, J2l, J2a: PdReal;
    // elements to jump from one row to the next in J's
    rowskip: Integer;
    // right hand sides of the equation J*v = c + cfm * lambda. cfm is the
    // "constraint force mixing" vector. c is set to zero on entry, cfm is
    // set to a constant value (typically very small or zero) value on entry.
    c, cfm: PdReal;
    // lo and hi limits for variables (set to -/+ infinity on entry).
    lo, hi: PdReal;
    // findex vector for variables. see the LCP solver interface for a
    // description of what this does. this is set to -1 on entry.
    // note that the returned indexes are relative to the first index of
    // the constraint.
    findex: pinteger;
  end;

  TdxJoint = record
    baseObject: TdObject;
    Info1: TJointInfo1;
    Info2: TJointInfo2;
  end;

  TdxJointNull = TdxJoint;

  PdxJointBall = ^TdxJointBall;

  TdxJointBall = record
    BaseJoint: TdxJoint;
    anchor1: TdVector3; // anchor w.r.t first body
    anchor2: TdVector3; // anchor w.r.t second body
    erp: TdReal; // error reduction
    cfm: TdReal; // constraint force mix in
  end;

  PdxJointHinge = ^TdxJointHinge;

  TdxJointHinge = record
    BaseJoint: TdxJoint;
    anchor1: TdVector3; // anchor w.r.t first body
    anchor2: TdVector3; // anchor w.r.t second body
    axis1: TdVector3; // axis w.r.t first body
    axis2: TdVector3; // axis w.r.t second body
    qrel: TdQuaternion; // initial relative rotation body1 -> body2
    limot: TdxJointLimitMotor; // limit and motor information
  end;

  PdxJointUniversial = ^TdxJointUniversial;

  TdxJointUniversial = record
    BaseJoint: TdxJoint;
    anchor1: TdVector3; // anchor w.r.t first body
    anchor2: TdVector3; // anchor w.r.t second body
    axis1: TdVector3; // axis w.r.t first body
    axis2: TdVector3; // axis w.r.t second body
    qrel1: TdQuaternion;
    // initial relative rotation body1 -> virtual cross piece
    qrel2: TdQuaternion;
    // initial relative rotation virtual cross piece -> body2
    limot1: TdxJointLimitMotor; // limit and motor information for axis1
    limot2: TdxJointLimitMotor; // limit and motor information for axis2
  end;

  PdxJointPR = ^TdxJointPR;

  TdxJointPR = record
    BaseJoint: TdxJoint;

    anchor2: TdVector3;
    /// < @brief Position of the rotoide articulation
    /// <        w.r.t second body.
    /// < @note Position of body 2 in world frame +
    /// < anchor2 in world frame give the position
    /// < of the rotoide articulation
    axisR1: TdVector3;
    /// < axis of the rotoide articulation w.r.t first body.
    /// < @note This is considered as axis1 from the parameter
    /// < view.
    axisR2: TdVector3;
    /// < axis of the rotoide articulation w.r.t second body.
    /// < @note This is considered also as axis1 from the
    /// < parameter view
    axisP1: TdVector3;
    /// < axis for the prismatic articulation w.r.t first body.
    /// < @note This is considered as axis2 in from the parameter
    /// < view
    qrel: TdQuaternion;
    /// < initial relative rotation body1 -> body2.
    offset: TdVector3;
    /// < @brief vector between the body1 and the rotoide
    /// < articulation.
    /// <
    /// < Going from the first to the second in the frame
    /// <  of body1.
    /// < That should be aligned with body1 center along axisP
    /// < This is calculated when the axis are set.
    limotR: TdxJointLimitMotor;
    /// < limit and motor information for the rotoide articulation.
    limotP: TdxJointLimitMotor;
    /// < limit and motor information for the prismatic articulation.
  end;

  PdxJointPiston = ^TdxJointPiston;

  TdxJointPiston = record
    BaseJoint: TdxJoint;

    axis1: TdVector3;
    /// < Axis of the prismatic and rotoide w.r.t first body
    axis2: TdVector3;
    /// < Axis of the prismatic and rotoide w.r.t second body

    qrel: TdQuaternion;
    /// < Initial relative rotation body1 -> body2

    /// Anchor w.r.t first body.
    /// This is the same as the offset for the Slider joint
    /// @note To find the position of the anchor when the body 1 has moved
    /// you must add the position of the prismatic joint
    /// i.e anchor = R1 * anchor1 + dJointGetPistonPosition() * (R1 * axis1)
    anchor1: TdVector3;
    anchor2: TdVector3; // < anchor w.r.t second body

    /// limit and motor information for the prismatic
    /// part of the joint
    limotP: TdxJointLimitMotor;

    /// limit and motor information for the rotoide
    /// part of the joint
    limotR: TdxJointLimitMotor;
  end;

  PdxJointSlider = ^TdxJointSlider;

  TdxJointSlider = record
    BaseJoint: TdxJoint;
    axis1: TdVector3; // axis w.r.t first body
    qrel: TdQuaternion; // initial relative rotation body1 -> body2
    offset: TdVector3; // point relative to body2 that should be
    // aligned with body1 center along axis1
    limot: TdxJointLimitMotor; // limit and motor information
  end;

  PdxJointHinge2 = ^TdxJointHinge2;

  TdxJointHinge2 = record
    BaseJoint: TdxJoint;

    anchor1: TdVector3; // anchor w.r.t first body
    anchor2: TdVector3; // anchor w.r.t second body
    axis1: TdVector3; // axis 1 w.r.t first body
    axis2: TdVector3; // axis 2 w.r.t second body
    c0, s0: TdReal; // cos,sin of desired angle between axis 1,2
    v1, v2: TdVector3; // angle ref vectors embedded in first body
    limot1: TdxJointLimitMotor; // limit+motor info for axis 1
    limot2: TdxJointLimitMotor; // limit+motor info for axis 2
    susp_erp, susp_cfm: TdReal; // suspension parameters (erp,cfm)
  end;

  TdxJointAMotor = record
    BaseJoint: TdxJoint;

    num: Integer; // number of axes (0..3)
    mode: Integer; // a dAMotorXXX constant
    rel: TdInteger3; // what the axes are relative to (global,b1,b2)
    axis: Td3Axis; // three axes
    limot: TdxJointLimitMotor3; // limit+motor info for axes
    angle: TdVector3; // user-supplied angles for axes
    // these vectors are used for calculating euler angles
    reference1: TdVector3; // original axis[2], relative to body 1
    reference2: TdVector3; // original axis[0], relative to body 2
  end;

  TdxJointLMotor = record
    BaseJoint: TdxJoint;

    num: Integer;
    rel: TdInteger3;
    axis: Td3Axis; // three axes
    limot: TdxJointLimitMotor3; // limit+motor info for axes
  end;

  TdxJointPlane2D = record
    BaseJoint: TdxJoint;
    row_motor_x: Integer;
    row_motor_y: Integer;
    row_motor_angle: Integer;
    motor_x: TdxJointLimitMotor;
    motor_y: TdxJointLimitMotor;
    motor_angle: TdxJointLimitMotor;
  end;

  TdxJointFixed = record
    BaseJoint: TdxJoint;
    qrel: TdQuaternion; // initial relative rotation body1 -> body2
    offset: TdVector3; // relative offset between the bodies
    erp: TdReal; // error reduction parameter
    cfm: TdReal; // constraint force mix-in
  end;

  // position vector and rotation matrix for geometry objects that are not
  // connected to bodies.
  PdxPosR = ^TdxPosR;

  TdxPosR = record
    pos: TdVector3;
    R: TdMatrix3;
  end;

  TdxBody = record
    baseObject: TdObject;
{$IFDEF cSINGLE}
    Padding: byte;
{$ENDIF}
    firstjoint: TdJointID; // list of attached joints
    flags: Integer; // some dxBodyFlagXXX flags
    geom: PdxGeom; // first collision geom associated with body
    mass: TdMass; // mass parameters about POR
    invI: TdMatrix3; // inverse of mass.I
    invMass: TdReal; // 1 / mass.mass
    posr: TdxPosR; // position and orientation of point of reference
    q: TdQuaternion; // orientation quaternion
    lvel, avel: TdVector3; // linear and angular velocity of POR
    facc, tacc: TdVector3; // force and torque accululators
    finite_rot_axis: TdVector3; // finite rotation axis, unit length or 0=none
    adis: TdxAutoDisable; // auto-disable parameters
    adis_timeleft: TdReal; // time left to be idle
    adis_stepsleft: Integer; // steps left to be idle
    average_lvel_buffer: PdVector3;
    // buffer for the linear average velocity calculation
    average_avel_buffer: PdVector3;
    // buffer for the angular average velocity calculation
    average_counter: longword; // counter/index to fill the average-buffers
    average_ready: Integer;
    // indicates ( with = 1 ), if the Body's buffers are ready for average-calculations
    moved_callback: TdMovedCallback; // let the user know the body moved
    dampingp: TdxDampingParameters; // damping parameters, depends on flags
    max_angular_speed: TdReal; // limit the angular velocity to this magnitude
  end;

  TBodyList = class(TList)
  private
    function GetItems(I: Integer): PdxBody;
    procedure SetItems(I: Integer; const Value: PdxBody);
  public
    property Items[I: Integer]: PdxBody read GetItems write SetItems; default;
    procedure DeleteAllBodies;
  end;

  (* struct dxWorld : public dBase {
    dxBody *firstbody;		// body linked list
    dxJoint *firstjoint;	// joint linked list
    int nb,nj;			      // number of bodies and joints in lists
    dVector3 gravity;		  // gravity vector (m/s/s)
    dReal global_erp;		  // global error reduction parameter
    dReal global_cfm;		  // global costraint force mixing parameter
    }; *)

  TdxWorld = record // (TdBase)
    firstbody: PdxBody; // body linked list
    firstjoint: PdxJoint; // joint linked list
    nb, nj: Integer; // number of bodies and joints in lists
    gravity: TdVector3; // gravity vector (m/s/s)
    global_erp: TdReal; // global error reduction parameter
    global_cfm: TdReal; // global costraint force mixing parameter
    adis: TdxAutoDisable;
    body_flags: Integer;
    qs: TdxQuickStepParameters;
    contactp: TdxContactParameters;
    dampingp: TdxDampingParameters;
    max_angular_speed: TdReal;
  end;

  TdJointFeedback = record
    f1: TdVector3; // force that joint applies to body 1
    t1: TdVector3; // torque that joint applies to body 1
    f2: TdVector3; // force that joint applies to body 2
    t2: TdVector3; // torque that joint applies to body 2
  end;

  PTdJointFeedback = ^TdJointFeedback;

  TdErrorType = (d_ERR_UNKNOWN, // unknown error
    d_ERR_IASSERT, // user assertion failed */
    d_ERR_UASSERT, // user assertion failed */
    d_ERR_LCP);

  TdJointTypeNumbers = (dJointTypeNone, // or "unknown"
    dJointTypeBall, dJointTypeHinge, dJointTypeSlider, dJointTypeContact,
    dJointTypeUniversal, dJointTypeHinge2, dJointTypeFixed, dJointTypeNull,
    dJointTypeAMotor, dJointTypeLMotor, dJointTypePlane2D, dJointTypePR,
    dJointTypePU, dJointTypePiston);

  TdAngularMotorModeNumbers = (dAMotorUser, dAMotorEuler);

  TdSurfaceParameters = record
    // must always be defined
    mode: Integer;
    mu: TdReal;

    // only defined if the corresponding flag is set in mode
    mu2, bounce, bounce_vel, soft_erp, soft_cfm, motion1, motion2, motionN,
      slip1, slip2: TdReal end;

    TdContactGeom = record pos: TdVector3;
    normal: TdVector3;
    depth: TdReal;
    g1, g2: PdxGeom;
    side1, side2: Integer;
  end;

  PdContactGeom = ^TdContactGeom;

  TdContact = record
    surface: TdSurfaceParameters;
    geom: TdContactGeom;
    fdir1: TdVector3;
  end;

  PdContact = ^TdContact;

  // Collission callback structure
  TdNearCallback = procedure(data: pointer; o1, o2: PdxGeom); cdecl;

  TdColliderFn = function(o1, o2: PdxGeom; flags: Integer;
    contact: PdContactGeom; skip: Integer): Integer; cdecl;
  TdGetColliderFnFn = function(num: Integer): TdColliderFn; cdecl;
  TdGetAABBFn = procedure(g: PdxGeom; var aabb: TdAABB); cdecl;
  TdGeomDtorFn = procedure(o: PdxGeom); cdecl;
  TdAABBTestFn = function(o1, o2: PdxGeom; const aabb2: TdAABB): Integer; cdecl;

  (* typedef struct dGeomClass {
    int bytes;
    dGetColliderFnFn *collider;
    dGetAABBFn *aabb;
    dAABBTestFn *aabb_test;
    dGeomDtorFn *dtor;
    } dGeomClass; *)

  TdGeomClass = record
    bytes: Integer; // extra storage size
    collider: TdGetColliderFnFn; // collider function
    aabb: TdGetAABBFn; // bounding box function
    aabb_test: TdAABBTestFn; // aabb tester, can be 0 for none
    dtor: TdGeomDtorFn; // destructor, can be 0 for none
  end;

  PdGeomClass = ^TdGeomClass;

  (* struct dxSpace : public dBase {
    int type;			// don't want to use RTTI
    virtual void destroy()=0;
    virtual void add (dGeomID)=0;
    virtual void remove (dGeomID)=0;
    virtual void collide (void *data, dNearCallback *callback)=0;
    virtual int query (dGeomID)=0;
    }; *)

  PdxSpace = ^TdxSpace;

  TdRealHugeArray = array [0 .. 65535] of TdReal;
  PdRealHugeArray = ^TdRealHugeArray;

  // Tri-list collider
  TdIntegerArray = array [0 .. 65535] of Integer;
  PdIntegerArray = ^TdIntegerArray;

  TdVector3Array = array [0 .. 65535] of TdVector3;
  PdVector3Array = ^TdVector3Array;

  (* struct dxTriMeshData{
    Model BVTree;
    MeshInterface Mesh;

    dxTriMeshData();
    ~dxTriMeshData();

    void Build(const void* Vertices, int VertexStide, int VertexCount,
    const void* Indices, int IndexCount, int TriStride,
    const void* Normals,
    bool Single);

    /* aabb in model space */
    dVector3 AABBCenter;
    dVector3 AABBExtents;

    /* data for use in collison resolution */
    const void* Normals;
    Matrix4x4   last_trans;
    }; *)
  TdxTriMeshData = record
    unknown: byte; //
  end;

  PdxTriMeshData = ^TdxTriMeshData;

  TdxHeightfieldData = record
    m_fWidth: TdReal;
    m_fDepth: TdReal;
    m_fSampleWidth: TdReal;
    m_fSampleDepth: TdReal;
    m_fInvSampleWidth: TdReal;
    m_fInvSampleDepth: TdReal;
    m_fHalfWidth: TdReal;
    m_fHalfDepth: TdReal;
    m_fMinHeight: TdReal;
    m_fMaxHeight: TdReal;
    m_fThickness: TdReal;
    m_fScale: TdReal;
    m_fOffset: TdReal;
    m_nWidthSamples: Integer;
    m_nDepthSamples: Integer;
    m_bCopyHeightData: Integer;
    m_bWrapMode: Integer;
    m_nGetHeightMode: Integer;
    m_pHeightData: pointer;
    m_pUserData: pointer;
    m_contacts: PdContactGeom;
  end;

  PdxHeightfieldData = ^TdxHeightfieldData;

  (* //simple space - reports all n^2 object intersections
    struct dxSimpleSpace : public dxSpace {
    dGeomID first;
    void destroy();
    void add (dGeomID);
    void remove (dGeomID);
    void collide (void *data, dNearCallback *callback);
    int query (dGeomID);
    }; *)

  PdxSimpleSpace = ^TdxSimpleSpace;

  (* //currently the space 'container' is just a list of the geoms in the space.
    struct dxHashSpace : public dxSpace {
    dxGeom *first;
    int global_minlevel;	// smallest hash table level to put AABBs in
    int global_maxlevel;	// objects that need a level larger than this will be
    // put in a "big objects" list instead of a hash table
    void destroy();
    void add (dGeomID);
    void remove (dGeomID);
    void collide (void *data, dNearCallback *callback);
    int query (dGeomID);
    }; *)

  PdxHashSpace = ^TdxHashSpace;

  (* typedef struct dGeomSpaceData {
    dGeomID next;
    } dGeomSpaceData; *)

  TdGeomSpaceData = record
    next: PdxGeom;
  end;

  (* // common data for all geometry objects. the class-specific data area follows
    // this structure. pos and R will either point to a separately allocated
    // buffer (if body is 0 - pos points to the dxPosR object) or to the pos and
    // R of the body (if body nonzero).
    struct dxGeom {		// a dGeomID is a pointer to this
    dxGeomClass *_class;	// class of this object
    void *data;		// user data pointer
    dBodyID body;		// dynamics body associated with this object (if any)
    dReal *pos;		// pointer to object's position vector
    dReal *R;		// pointer to object's rotation matrix
    dSpaceID spaceid;	// the space this object is in
    dGeomSpaceData space;	// reserved for use by space this object is in
    dReal *space_aabb;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
    }; *)

  TdxGeom = record // a dGeomID is a pointer to this
    _type: Integer; // class of this object

{$IFDEF cSINGLE}
    Padding: byte;
{$ENDIF}
    gflags: Integer;

    data: pointer; // user data pointer
    body: PdxBody; // dynamics body associated with this object (if any)
    body_next: PdxGeom; // next geom in body's linked list of associated geoms
    final_posr: PdxPosR; // final position of the geom in world coordinates
    offset_posr: PdxPosR; // offset from body in local coordinates

    next: PdxGeom;
    tome: PPdxGeom;
    parent_space: PdxSpace;
    aabb: TdAABB;
    category_bits, collide_bits: longword;
  end;

  TGeomList = class(TList)
  private
    function GetItems(I: Integer): PdxGeom;
    procedure SetItems(I: Integer; const Value: PdxGeom);
  public
    property Items[I: Integer]: PdxGeom read GetItems write SetItems; default;
    procedure DeleteAllGeoms(DeleteDataAsObject: boolean = false);
  end;

  TdxSpace = record
    baseGeom: TdxGeom;
    count: Integer;
    first: PdxGeom;
    cleanup: Integer;
    current_index: Integer;
    current_geom: PdxGeom;
    lock_count: Integer;
  end;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxSimpleSpace = TdxSpace;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxHashSpace = record
    BaseSpace: TdxSpace;
    global_minlevel: Integer;
    global_maxlevel: Integer;
  end;

  TdxQuadTreeSpace = record
    BaseSpace: TdxSpace;
    Blocks: pointer;
    DirtyList: pointer;
  end;

  // TJointParams = (
  // parameters for limits and motors
  // Change: New Type added, sintax enforcement
  TJointParams = Integer;

  // These consts now have defined types

{$IFDEF PARODE}

const
  dJOINT_BREAK_UNKNOWN: TJointBreakMode = $0000;
  dJOINT_BROKEN: TJointBreakMode = $0001;
  dJOINT_DELETE_ON_BREAK: TJointBreakMode = $0002;
  dJOINT_BREAK_AT_B1_FORCE: TJointBreakMode = $0004;
  dJOINT_BREAK_AT_B1_TORQUE: TJointBreakMode = $0008;
  dJOINT_BREAK_AT_B2_FORCE: TJointBreakMode = $0010;
  dJOINT_BREAK_AT_B2_TORQUE: TJointBreakMode = $0020;
{$ENDIF}

const
  _priv_dParamLoStop = $000;
  _priv_dParamLoStop2 = $100;
  _priv_dParamLoStop3 = $200;

const
  dParamLoStop: TJointParams = _priv_dParamLoStop;
  dParamHiStop: TJointParams = _priv_dParamLoStop + 1;
  dParamVel: TJointParams = _priv_dParamLoStop + 2;
  dParamFMax: TJointParams = _priv_dParamLoStop + 3;
  dParamFudgeFactor: TJointParams = _priv_dParamLoStop + 4;
  dParamBounce: TJointParams = _priv_dParamLoStop + 5;
  dParamCFM: TJointParams = _priv_dParamLoStop + 6;
  dParamStopERP: TJointParams = _priv_dParamLoStop + 7;
  dParamStopCFM: TJointParams = _priv_dParamLoStop + 8;
  // parameters for suspension
  dParamSuspensionERP: TJointParams = _priv_dParamLoStop + 9;
  dParamSuspensionCFM: TJointParams = _priv_dParamLoStop + 10;
  dParamERP: TJointParams = _priv_dParamLoStop + 11;

  dParamGroup1: TJointParams = $000;
  dParamLoStop1: TJointParams = _priv_dParamLoStop;
  dParamHiStop1: TJointParams = _priv_dParamLoStop + 1;
  dParamVel1: TJointParams = _priv_dParamLoStop + 2;
  dParamFMax1: TJointParams = _priv_dParamLoStop + 3;
  dParamFudgeFactor1: TJointParams = _priv_dParamLoStop + 4;
  dParamBounce1: TJointParams = _priv_dParamLoStop + 5;
  dParamCFM1: TJointParams = _priv_dParamLoStop + 6;
  dParamStopERP1: TJointParams = _priv_dParamLoStop + 7;
  dParamStopCFM1: TJointParams = _priv_dParamLoStop + 8;
  // parameters for suspension
  dParamSuspensionERP1: TJointParams = _priv_dParamLoStop + 9;
  dParamSuspensionCFM1: TJointParams = _priv_dParamLoStop + 10;
  dParamERP1: TJointParams = _priv_dParamLoStop + 11;

  // SECOND AXEL
  // parameters for limits and motors
  dParamGroup2: TJointParams = $100;
  dParamLoStop2: TJointParams = _priv_dParamLoStop2;
  dParamHiStop2: TJointParams = _priv_dParamLoStop2 + 1;
  dParamVel2: TJointParams = _priv_dParamLoStop2 + 2;
  dParamFMax2: TJointParams = _priv_dParamLoStop2 + 3;
  dParamFudgeFactor2: TJointParams = _priv_dParamLoStop2 + 4;
  dParamBounce2: TJointParams = _priv_dParamLoStop2 + 5;
  dParamCFM2: TJointParams = _priv_dParamLoStop2 + 6;
  dParamStopERP2: TJointParams = _priv_dParamLoStop2 + 7;
  dParamStopCFM2: TJointParams = _priv_dParamLoStop2 + 8;
  // parameters for suspension
  dParamSuspensionERP2: TJointParams = _priv_dParamLoStop2 + 9;
  dParamSuspensionCFM2: TJointParams = _priv_dParamLoStop2 + 10;
  dParamERP2: TJointParams = _priv_dParamLoStop2 + 11;

  // THIRD AXEL
  // parameters for limits and motors
  dParamGroup3: TJointParams = $200;
  dParamLoStop3: TJointParams = _priv_dParamLoStop3;
  dParamHiStop3: TJointParams = _priv_dParamLoStop3 + 1;
  dParamVel3: TJointParams = _priv_dParamLoStop3 + 2;
  dParamFMax3: TJointParams = _priv_dParamLoStop3 + 3;
  dParamFudgeFactor3: TJointParams = _priv_dParamLoStop3 + 4;
  dParamBounce3: TJointParams = _priv_dParamLoStop3 + 5;
  dParamCFM3: TJointParams = _priv_dParamLoStop3 + 6;
  dParamStopERP3: TJointParams = _priv_dParamLoStop3 + 7;
  dParamStopCFM3: TJointParams = _priv_dParamLoStop3 + 8;
  // parameters for suspension
  dParamSuspensionERP3: TJointParams = _priv_dParamLoStop3 + 9;
  dParamSuspensionCFM3: TJointParams = _priv_dParamLoStop3 + 10;
  dParamERP3: TJointParams = _priv_dParamLoStop3 + 11;
  dParamGroup: TJointParams = $100;

  // added by PAR
{$IFDEF PARODE}
function dSphereGetClass: Integer; cdecl; external ODEDLL;
function dBoxGetClass: Integer; cdecl; external ODEDLL;
function dCylinderGetClass: Integer; cdecl; external ODEDLL;
function dCapsuleGetClass: Integer; cdecl; external ODEDLL;
function dRayGetClass: Integer; cdecl; external ODEDLL;
function dPlaneGetClass: Integer; cdecl; external ODEDLL;
function dConvexGetClass: Integer; cdecl; external ODEDLL;
function dTriMeshGetClass: Integer; cdecl; external ODEDLL;
function dHeightfieldGetClass: Integer; cdecl; external ODEDLL;
function dGeomTransformGetClass: Integer; cdecl; external ODEDLL;
{$ENDIF}
procedure dInitODE; cdecl; external ODEDLL;
function dInitODE2(uiInitFlags: longword): Integer; cdecl; external ODEDLL;

procedure dCloseODE; cdecl; external ODEDLL;

// ----- dWorld -----
function dWorldCreate: PdxWorld; cdecl; external ODEDLL;
procedure dWorldDestroy(const World: PdxWorld); cdecl; external ODEDLL;
function dWorldGetCFM(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
function dWorldGetERP(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
procedure dWorldGetGravity(const World: PdxWorld; var gravity: TdVector3);
  cdecl; external ODEDLL;
procedure dWorldImpulseToForce(const World: PdxWorld;
  const stepsize, ix, iy, iz: TdReal; var force: TdVector3); cdecl;
  external ODEDLL;
procedure dWorldSetCFM(const World: PdxWorld; cfm: TdReal); cdecl;
  external ODEDLL;
procedure dWorldSetERP(const World: PdxWorld; erp: TdReal); cdecl;
  external ODEDLL;
procedure dWorldSetGravity(const World: PdxWorld; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dWorldSetContactMaxCorrectingVel(const World: PdxWorld;
  const vel: TdReal); cdecl; external ODEDLL;
function dWorldGetContactMaxCorrectingVel(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetContactSurfaceLayer(const World: PdxWorld;
  const depth: TdReal); cdecl; external ODEDLL;
function dWorldGetContactSurfaceLayer(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldExportDIF(const World: PdxWorld; fileHandle: cardinal;
  const world_name: PAnsiChar); cdecl; external ODEDLL;

// Damping
function dWorldGetLinearDampingThreshold(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetLinearDampingThreshold(const World: PdxWorld;
  const threshold: TdReal); cdecl; external ODEDLL;
function dWorldGetAngularDampingThreshold(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetAngularDampingThreshold(const World: PdxWorld;
  const threshold: TdReal); cdecl; external ODEDLL;
function dWorldGetLinearDamping(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetLinearDamping(const World: PdxWorld; const scale: TdReal);
  cdecl; external ODEDLL;
function dWorldGetAngularDamping(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetAngularDamping(const World: PdxWorld; const scale: TdReal);
  cdecl; external ODEDLL;
procedure dWorldSetDamping(const World: PdxWorld;
  const linear_scale, angular_scale: TdReal); cdecl; external ODEDLL;
function dWorldGetMaxAngularSpeed(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetMaxAngularSpeed(const World: PdxWorld;
  const max_speed: TdReal); cdecl; external ODEDLL;
// Step
procedure dWorldStep(const World: PdxWorld; const stepsize: TdReal); cdecl;
  external ODEDLL;
// QuickStep
procedure dWorldQuickStep(const World: PdxWorld; const stepsize: TdReal); cdecl;
  external ODEDLL;
procedure dWorldSetQuickStepNumIterations(const World: PdxWorld;
  const num: Integer); cdecl; external ODEDLL;
function dWorldGetQuickStepNumIterations(const World: PdxWorld): Integer; cdecl;
  external ODEDLL;
procedure dWorldSetQuickStepW(const World: PdxWorld; const param: TdReal);
  cdecl; external ODEDLL;
function dWorldGetQuickStepW(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
// Auto-disable functions
function dWorldGetAutoDisableLinearAverageThreshold(const World: PdxWorld)
  : TdReal; cdecl; external ODEDLL;
procedure dWorldSetAutoDisableLinearAverageThreshold(const World: PdxWorld;
  linear_average_threshold: TdReal); cdecl; external ODEDLL;
function dWorldGetAutoDisableAngularAverageThreshold(const World: PdxWorld)
  : TdReal; cdecl; external ODEDLL;
procedure dWorldSetAutoDisableAngularAverageThreshold(const World: PdxWorld;
  linear_average_threshold: TdReal); cdecl; external ODEDLL;
function dWorldGetAutoDisableAverageSamplesCount(const World: PdxWorld): TdReal;
  cdecl; external ODEDLL;
procedure dWorldSetAutoDisableAverageSamplesCount(const World: PdxWorld;
  linear_average_threshold: TdReal); cdecl; external ODEDLL;

function dWorldGetAutoDisableLinearThreshold(const World: PdxWorld): TdReal;
  cdecl; external ODEDLL;
procedure dWorldSetAutoDisableLinearThreshold(const World: PdxWorld;
  linThreshold: TdReal); cdecl; external ODEDLL;
function dWorldGetAutoDisableAngularThreshold(const World: PdxWorld): TdReal;
  cdecl; external ODEDLL;
procedure dWorldSetAutoDisableAngularThreshold(const World: PdxWorld;
  angThreshold: TdReal); cdecl; external ODEDLL;
function dWorldGetAutoDisableSteps(const World: PdxWorld): Integer; cdecl;
  external ODEDLL;
procedure dWorldSetAutoDisableSteps(const World: PdxWorld; steps: Integer);
  cdecl; external ODEDLL;
function dWorldGetAutoDisableTime(const World: PdxWorld): TdReal; cdecl;
  external ODEDLL;
procedure dWorldSetAutoDisableTime(const World: PdxWorld; time: TdReal); cdecl;
  external ODEDLL;
function dWorldGetAutoDisableFlag(const World: PdxWorld): Integer; cdecl;
  external ODEDLL;
procedure dWorldSetAutoDisableFlag(const World: PdxWorld;
  do_auto_disable: Integer); cdecl; external ODEDLL;

// ----- dBody -----
procedure dBodyAddForce(const body: PdxBody; const fx, fy, fz: TdReal); cdecl;
  external ODEDLL;
procedure dBodyAddForceAtPos(const body: PdxBody;
  const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
procedure dBodyAddForceAtRelPos(const body: PdxBody;
  const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
procedure dBodyAddRelForce(const body: PdxBody; const fx, fy, fz: TdReal);
  cdecl; external ODEDLL;
procedure dBodyAddRelForceAtPos(const body: PdxBody;
  const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
procedure dBodyAddRelForceAtRelPos(const body: PdxBody;
  const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
procedure dBodyAddRelTorque(const body: PdxBody; const fx, fy, fz: TdReal);
  cdecl; external ODEDLL;
procedure dBodyAddTorque(const body: PdxBody; const fx, fy, fz: TdReal); cdecl;
  external ODEDLL;

function dBodyCreate(const World: PdxWorld): PdxBody; cdecl; external ODEDLL;
procedure dBodyDestroy(const body: PdxBody); cdecl; external ODEDLL;
procedure dBodyDisable(const body: PdxBody); cdecl; external ODEDLL;
procedure dBodyEnable(const body: PdxBody); cdecl; external ODEDLL;
function dBodyGetAngularVel(const body: PdxBody): PdVector3; cdecl;
  external ODEDLL;
procedure dBodyGetFiniteRotationAxis(const body: PdxBody;
  var result: TdVector3); cdecl; external ODEDLL;
function dBodyGetFiniteRotationMode(const body: PdxBody): Integer; cdecl;
  external ODEDLL;
function dBodyGetForce(const body: PdxBody): PdVector3; cdecl; external ODEDLL;
function dBodyGetGravityMode(const body: PdxBody): Integer; cdecl;
  external ODEDLL;
function dBodyGetJoint(const body: PdxBody; const index: Integer): TdJointID;
  cdecl; external ODEDLL;
function dBodyGetLinearVel(const body: PdxBody): PdVector3; cdecl;
  external ODEDLL;
procedure dBodyGetMass(const body: PdxBody; var mass: TdMass); cdecl;
  external ODEDLL;
function dBodyGetNumJoints(const body: PdxBody): Integer; cdecl;
  external ODEDLL;
procedure dBodyGetPointVel(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dBodyGetPosRelPoint(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
function dBodyGetPosition(const body: PdxBody): PdVector3; cdecl;
  external ODEDLL;
function dBodyGetQuaternion(const body: PdxBody): PdQuaternion; cdecl;
  external ODEDLL;
procedure dBodyGetRelPointPos(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dBodyGetRelPointVel(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
function dBodyGetRotation(const body: PdxBody): PdMatrix3; cdecl;
  external ODEDLL;
function dBodyGetTorque(const body: PdxBody): PdVector3; cdecl; external ODEDLL;
function dBodyIsEnabled(const body: PdxBody): Integer; cdecl; external ODEDLL;
procedure dBodySetAngularVel(const body: PdxBody; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dBodySetFiniteRotationAxis(const body: PdxBody;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dBodySetFiniteRotationMode(const body: PdxBody; const mode: Integer);
  cdecl; external ODEDLL;
procedure dBodySetForce(const body: PdxBody; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dBodySetGravityMode(const body: PdxBody; const mode: Integer); cdecl;
  external ODEDLL;
procedure dBodySetLinearVel(const body: PdxBody; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dBodySetMass(const body: PdxBody; const mass: PdMass); cdecl;
  external ODEDLL;
procedure dBodySetPosition(const body: PdxBody; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dBodySetQuaternion(const body: PdxBody; const q: TdQuaternion); cdecl;
  external ODEDLL;
procedure dBodySetRotation(const body: PdxBody; const R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dBodySetTorque(const body: PdxBody; const x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dBodyVectorFromWorld(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dBodyVectorToWorld(const body: PdxBody; const px, py, pz: TdReal;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dBodySetData(const body: PdxBody; data: pointer); cdecl;
  external ODEDLL;
function dBodyGetData(const body: PdxBody): pointer; cdecl; external ODEDLL;
procedure dBodySetMovedCallback(const body: PdxBody; callback: TdMovedCallback);
  cdecl; external ODEDLL;
procedure dBodyCopyPosition(const body: PdxBody; const pos: TdVector3); cdecl;
  external ODEDLL;
procedure dBodyCopyRotation(const body: PdxBody; const R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dBodyCopyQuaternion(const body: PdxBody; const quat: TdQuaternion);
  cdecl; external ODEDLL;

// damping functions
procedure dBodySetLinearDamping(const body: PdxBody; scale: TdReal); cdecl;
  external ODEDLL;
function dBodyGetLinearDamping(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetAngularDamping(const body: PdxBody; scale: TdReal); cdecl;
  external ODEDLL;
function dBodyGetAngularDamping(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetDamping(const body: PdxBody;
  linear_scale, angular_scale: TdReal); cdecl; external ODEDLL;
function dBodyGetLinearDampingThreshold(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetLinearDampingThreshold(const body: PdxBody;
  threshold: TdReal); cdecl; external ODEDLL;
function dBodyGetAngularDampingThreshold(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetAngularDampingThreshold(const body: PdxBody;
  threshold: TdReal); cdecl; external ODEDLL;
procedure dBodySetDampingDefaults(const body: PdxBody; threshold: TdReal);
  cdecl; external ODEDLL;
procedure dBodySetMaxAngularSpeed(const body: PdxBody; max_speed: TdReal);
  cdecl; external ODEDLL;
function dBodyGetMaxAngularSpeed(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;

// Auto-disable functions
function dBodyGetAutoDisableLinearThreshold(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetAutoDisableLinearThreshold(const body: PdxBody;
  linThreshold: TdReal); cdecl; external ODEDLL;
function dBodyGetAutoDisableAngularThreshold(const body: PdxBody): TdReal;
  cdecl; external ODEDLL;
procedure dBodySetAutoDisableAngularThreshold(const body: PdxBody;
  angThreshold: TdReal); cdecl; external ODEDLL;
function dBodyGetAutoDisableSteps(const body: PdxBody): Integer; cdecl;
  external ODEDLL;
procedure dBodySetAutoDisableSteps(const body: PdxBody; steps: Integer); cdecl;
  external ODEDLL;
function dBodyGetAutoDisableTime(const body: PdxBody): TdReal; cdecl;
  external ODEDLL;
procedure dBodySetAutoDisableTime(const body: PdxBody; time: TdReal); cdecl;
  external ODEDLL;
function dBodyGetAutoDisableFlag(const body: PdxBody): Integer; cdecl;
  external ODEDLL;
procedure dBodySetAutoDisableFlag(const body: PdxBody;
  do_auto_disable: Integer); cdecl; external ODEDLL;
procedure dBodySetAutoDisableDefaults(const body: PdxBody); cdecl;
  external ODEDLL;
procedure dBodySetAutoDisableAverageSamplesCount(const body: PdxBody;
  average_samples_count: longword); cdecl; external ODEDLL;


// ----- dJoint -----
{$IFDEF PARODE}
// breakable joints
procedure dJointSetBreakable(const dJointID: TdJointID; b: Integer); cdecl;
  external ODEDLL;
procedure dJointSetBreakCallback(const dJointID: TdJointID;
  callbackFunc: TdJointBreakCallback); cdecl; external ODEDLL;
procedure dJointSetBreakMode(const dJointID: TdJointID; mode: Integer); cdecl;
  external ODEDLL;
function dJointGetBreakMode(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
procedure dJointSetBreakForce(const dJointID: TdJointID; body: Integer;
  x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointSetBreakTorque(const dJointID: TdJointID; body: Integer;
  x, y, z: TdReal); cdecl; external ODEDLL;
function dJointIsBreakable(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
procedure dJointGetBreakForce(const dJointID: TdJointID; body: Integer;
  var force: TdVector3); cdecl; external ODEDLL;
procedure dJointGetBreakTorque(const dJointID: TdJointID; body: Integer;
  var torque: TdVector3); cdecl; external ODEDLL;
{$ENDIF}
// normal joints
procedure dJointGroupDestroy(const dJointGroupID: TdJointGroupID); cdecl;
  external ODEDLL;
function dJointGroupCreate(const max_size: Integer): TdJointGroupID; cdecl;
  external ODEDLL;
procedure dJointGroupEmpty(const dJointGroupID: TdJointGroupID); cdecl;
  external ODEDLL;

procedure dJointAttach(const dJointID: TdJointID; const body1, body2: PdxBody);
  cdecl; external ODEDLL;
procedure dJointDestroy(const dJointID: TdJointID); cdecl; external ODEDLL;

function dJointGetData(const dJointID: TdJointID): pointer; cdecl;
  external ODEDLL;
procedure dJointSetData(const dJointID: TdJointID; data: pointer); cdecl;
  external ODEDLL;

// callback routines for feedback of joints
procedure dJointSetFeedback(const dJointID: TdJointID;
  Feedback: PTdJointFeedback); cdecl; external ODEDLL;
function dJointGetFeedback(const dJointID: TdJointID): PTdJointFeedback; cdecl;
  external ODEDLL;

function dJointGetType(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
function dJointGetBody(const dJointID: TdJointID; const index: Integer)
  : PdxBody; cdecl; external ODEDLL;

// Contact
function dJointCreateContact(const World: PdxWorld;
  dJointGroupID: TdJointGroupID; const dContact: PdContact): TdJointID; cdecl;
  external ODEDLL;

// AMotor
function dJointCreateAMotor(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetAMotorAngle(const dJointID: TdJointID; const anum: Integer;
  const angle: TdReal); cdecl; external ODEDLL;
function dJointGetAMotorAngle(const dJointID: TdJointID; const anum: Integer)
  : TdReal; cdecl; external ODEDLL;
procedure dJointSetAMotorAxis(const dJointID: TdJointID;
  const anum, rel: Integer; const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetAMotorAxis(const dJointID: TdJointID; const anum: Integer;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetAMotorNumAxes(const dJointID: TdJointID; const num: Integer);
  cdecl; external ODEDLL;
function dJointGetAMotorNumAxes(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
procedure dJointSetAMotorParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetAMotorParam(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
procedure dJointSetAMotorMode(const dJointID: TdJointID;
  const mode: TdAngularMotorModeNumbers); cdecl; external ODEDLL;
function dJointGetAMotorMode(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
procedure dJointAddAMotorTorques(const dJointID: TdJointID;
  torque1, torque2, torque3: TdReal); cdecl; external ODEDLL;
function dJointGetAMotorAngleRate(const dJointID: TdJointID;
  const anum: Integer): TdReal; cdecl; external ODEDLL;
function dJointGetAMotorAxisRel(const dJointID: TdJointID; const anum: Integer)
  : Integer; cdecl; external ODEDLL;

// LMotor
function dJointCreateLMotor(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetLMotorAxis(const dJointID: TdJointID;
  const anum, rel: Integer; const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetLMotorAxis(const dJointID: TdJointID; const anum: Integer;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetLMotorNumAxes(const dJointID: TdJointID; const num: Integer);
  cdecl; external ODEDLL;
function dJointGetLMotorNumAxes(const dJointID: TdJointID): Integer; cdecl;
  external ODEDLL;
procedure dJointSetLMotorParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetLMotorParam(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;

// Ball
function dJointCreateBall(const World: PdxWorld; dJointGroupID: TdJointGroupID)
  : TdJointID; cdecl; external ODEDLL;
procedure dJointSetBallAnchor(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetBallAnchor(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointGetBallAnchor2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;

// Hinge
function dJointCreateHinge(const World: PdxWorld; dJointGroupID: TdJointGroupID)
  : TdJointID; cdecl; external ODEDLL;
procedure dJointSetHingeAnchor(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetHingeAnchor(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointGetHingeAnchor2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetHingeAxis(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetHingeAxis(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointSetHingeParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetHingeParam(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
function dJointGetHingeAngle(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetHingeAngleRate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
procedure dJointAddHingeTorque(const dJointID: TdJointID; torque: TdReal);
  cdecl; external ODEDLL;

// Hinge2
function dJointCreateHinge2(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetHinge2Anchor(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetHinge2Anchor(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointGetHinge2Anchor2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetHinge2Axis1(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetHinge2Axis1(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetHinge2Axis2(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetHinge2Axis2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetHinge2Param(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetHinge2Param(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
function dJointGetHinge2Angle1(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetHinge2Angle1Rate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetHinge2Angle2Rate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
procedure dJointAddHinge2Torques(const dJointID: TdJointID;
  torque1, torque2: TdReal); cdecl; external ODEDLL;
procedure dJointCorrectHinge2(const dJointID: TdJointID); cdecl;
  external ODEDLL;

// Slider
function dJointCreateSlider(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetSliderAxis(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetSliderAxis(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointSetSliderParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetSliderParam(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
function dJointGetSliderPosition(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetSliderPositionRate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
procedure dJointAddSliderForce(const dJointID: TdJointID; force: TdReal); cdecl;
  external ODEDLL;

// Universal
function dJointCreateUniversal(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointGetUniversalAnchor(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointGetUniversalAnchor2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetUniversalAxis1(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetUniversalAxis1(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetUniversalAxis2(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetUniversalAxis2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetUniversalParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetUniversalParam(const dJointID: TdJointID;
  const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
function dJointGetUniversalAngle1(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetUniversalAngle2(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetUniversalAngle1Rate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetUniversalAngle2Rate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
procedure dJointSetUniversalAnchor(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointAddUniversalTorques(const dJointID: TdJointID;
  torque1, torque2: TdReal); cdecl; external ODEDLL;

// Fixed
function dJointCreateFixed(const World: PdxWorld; dJointGroupID: TdJointGroupID)
  : TdJointID; cdecl; external ODEDLL;
procedure dJointSetFixed(const dJointID: TdJointID); cdecl; external ODEDLL;

// Plane2D
function dJointCreatePlane2D(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetPlane2DXParam(const dJointID: TdJointID;
  const parameter: Integer; const Value: TdReal); cdecl; external ODEDLL;
procedure dJointSetPlane2DYParam(const dJointID: TdJointID;
  const parameter: Integer; const Value: TdReal); cdecl; external ODEDLL;
procedure dJointSetPlane2DAngleParam(const dJointID: TdJointID;
  const parameter: Integer; const Value: TdReal); cdecl; external ODEDLL;

// PR
function dJointCreatePR(const World: PdxWorld; dJointGroupID: TdJointGroupID)
  : TdJointID; cdecl; external ODEDLL;
procedure dJointSetPRAnchor(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointSetPRAxis1(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetPRAxis1(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointSetPRAxis2(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetPRAxis2(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointSetPRParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetPRParam(const dJointID: TdJointID; parameter: Integer)
  : TdReal; cdecl; external ODEDLL;
procedure dJointAddPRTorque(const dJointID: TdJointID; torque: TdReal); cdecl;
  external ODEDLL;

// Piston
function dJointCreatePiston(const World: PdxWorld;
  dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
procedure dJointSetPistonAnchor(const dJointID: TdJointID;
  const x, y, z: TdReal); cdecl; external ODEDLL;
procedure dJointGetPistonAnchor(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointGetPistonAnchor2(const dJointID: TdJointID;
  var result: TdVector3); cdecl; external ODEDLL;
procedure dJointSetPistonAxis(const dJointID: TdJointID; const x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dJointGetPistonAxis(const dJointID: TdJointID; var result: TdVector3);
  cdecl; external ODEDLL;
procedure dJointSetPistonParam(const dJointID: TdJointID;
  const parameter: TJointParams; const Value: TdReal); cdecl; external ODEDLL;
function dJointGetPistonParam(const dJointID: TdJointID; parameter: Integer)
  : TdReal; cdecl; external ODEDLL;
procedure dJointSetPistonAxisDelta(const dJointID: TdJointID;
  const x, y, z, ax, ay, az: TdReal); cdecl; external ODEDLL;
procedure dJointAddPistonForce(const dJointID: TdJointID; force: TdReal); cdecl;
  external ODEDLL;
function dJointGetPistonPosition(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetPistonAngle(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetPistonAngleRate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;
function dJointGetPistonRate(const dJointID: TdJointID): TdReal; cdecl;
  external ODEDLL;

// ----- dGeom -----
function dCreateGeom(classnum: Integer): PdxGeom; cdecl; external ODEDLL;
procedure dGeomDestroy(const geom: PdxGeom); cdecl; external ODEDLL;

function dCreateGeomClass(const classptr: TdGeomClass): Integer; cdecl;
  external ODEDLL;
function dGeomGetClass(const geom: PdxGeom): Integer; cdecl; external ODEDLL;
function dGeomGetClassData(o: PdxGeom): pointer; cdecl; external ODEDLL;

function dGeomGetSpace(const geom: PdxGeom): PdxSpace; cdecl; external ODEDLL;
function dGeomIsSpace(const geom: PdxGeom): Integer; cdecl; external ODEDLL;
procedure dGeomSetBody(const geom: PdxGeom; body: PdxBody); cdecl;
  external ODEDLL;
function dGeomGetBody(const geom: PdxGeom): PdxBody; cdecl; external ODEDLL;

procedure dGeomSetPosition(const geom: PdxGeom; const x, y, z: TdReal); cdecl;
  external ODEDLL;
function dGeomGetPosition(const geom: PdxGeom): PdVector3; cdecl;
  external ODEDLL;
procedure dGeomSetRotation(const geom: PdxGeom; const R: TdMatrix3); cdecl;
  external ODEDLL;
function dGeomGetRotation(const geom: PdxGeom): PdMatrix3; cdecl;
  external ODEDLL;
procedure dGeomSetQuaternion(const geom: PdxGeom; const TdQuaternion); cdecl;
  external ODEDLL;
procedure dGeomGetQuaternion(const geom: PdxGeom; var result: TdQuaternion);
  cdecl; external ODEDLL;
procedure dGeomCopyPosition(const geom: PdxGeom; const pos: TdVector3); cdecl;
  external ODEDLL;
procedure dGeomCopyRotation(const geom: PdxGeom; const R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dGeomCopyQuaternion(const geom: PdxGeom; const quat: TdQuaternion);
  cdecl; external ODEDLL;

procedure dGeomSetData(const geom: PdxGeom; data: pointer); cdecl;
  external ODEDLL;
function dGeomGetData(const geom: PdxGeom): pointer; cdecl; external ODEDLL;
procedure dGeomEnable(const geom: PdxGeom); cdecl; external ODEDLL;
procedure dGeomDisable(const geom: PdxGeom); cdecl; external ODEDLL;
function dGeomIsEnabled(const geom: PdxGeom): Integer; cdecl; external ODEDLL;
procedure dGeomGetAABB(const geom: PdxGeom; var aabb: TdAABB); cdecl;
  external ODEDLL;
procedure dGeomSetCategoryBits(const geom: PdxGeom; bits: cardinal); cdecl;
  external ODEDLL;
function dGeomGetCategoryBits(const geom: PdxGeom): cardinal; cdecl;
  external ODEDLL;
procedure dGeomSetCollideBits(const geom: PdxGeom; bits: cardinal); cdecl;
  external ODEDLL;
function dGeomGetCollideBits(const geom: PdxGeom): cardinal; cdecl;
  external ODEDLL;

procedure dGeomSetOffsetPosition(const geom: PdxGeom; x, y, z: TdReal); cdecl;
  external ODEDLL;
function dGeomGetOffsetPosition(const geom: PdxGeom): PdVector3; cdecl;
  external ODEDLL;
procedure dGeomSetOffsetRotation(const geom: PdxGeom; R: TdMatrix3); cdecl;
  external ODEDLL;
function dGeomGetOffsetRotation(const geom: PdxGeom): PdVector3; cdecl;
  external ODEDLL;
procedure dGeomSetOffsetQuaternion(const geom: PdxGeom; const q: TdQuaternion);
  cdecl; external ODEDLL;
procedure dGeomGetOffsetQuaternion(const geom: PdxGeom; var q: TdQuaternion);
  cdecl; external ODEDLL;
procedure dGeomClearOffset(const geom: PdxGeom); cdecl; external ODEDLL;
procedure dGeomSetOffsetWorldPosition(const geom: PdxGeom; x, y, z: TdReal);
  cdecl; external ODEDLL;
procedure dGeomSetOffsetWorldRotation(const geom: PdxGeom; R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dGeomSetOffsetWorldQuaternion(const geom: PdxGeom;
  const q: TdQuaternion); cdecl; external ODEDLL;
procedure dGeomCopyOffsetPosition(const geom: PdxGeom; var pos: TdVector3);
  cdecl; external ODEDLL;
procedure dGeomCopyOffsetRotation(const geom: PdxGeom; var R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dGeomIsOffset(const geom: PdxGeom); cdecl; external ODEDLL;

// Transform
function dCreateGeomTransform(const Space: PdxSpace): PdxGeom; cdecl;
  external ODEDLL;
procedure dGeomTransformSetGeom(const geom, obj: PdxGeom); cdecl;
  external ODEDLL;
function dGeomTransformGetGeom(const geom: PdxGeom): PdxGeom; cdecl;
  external ODEDLL;
procedure dGeomTransformSetInfo(const geom: PdxGeom; mode: Integer); cdecl;
  external ODEDLL;
function dGeomTransformGetInfo(const geom: PdxGeom): Integer; cdecl;
  external ODEDLL;
procedure dGeomTransformSetCleanup(const geom: PdxGeom; const mode: Integer);
  cdecl; external ODEDLL;
function dGeomTransformGetCleanup(const geom: PdxGeom): Integer; cdecl;
  external ODEDLL;

// Box
function dCreateBox(const Space: PdxSpace; const lx, ly, lz: TdReal): PdxGeom;
  cdecl; external ODEDLL;
procedure dGeomBoxGetLengths(const geom: PdxGeom; var result: TdVector3); cdecl;
  external ODEDLL;
procedure dGeomBoxSetLengths(const geom: PdxGeom; const lx, ly, lz: TdReal);
  cdecl; external ODEDLL;
function dGeomBoxPointDepth(const geom: PdxGeom; const x, y, z: TdReal): TdReal;
  cdecl; external ODEDLL;

// dCylinder (not a capped cylinder).
function dCreateCylinder(const Space: PdxSpace; R, lz: TdReal): PdxGeom; cdecl;
  external ODEDLL;
procedure dGeomCylinderSetParams(const geom: PdxGeom; radius, length: TdReal);
  cdecl; external ODEDLL;
procedure dGeomCylinderGetParams(const geom: PdxGeom;
  var radius, length: TdReal); cdecl; external ODEDLL;

// dCapsule (a capped cylinder).
function dCreateCapsule(const Space: PdxSpace; const radius, length: TdReal)
  : PdxGeom; cdecl; external ODEDLL;
procedure dGeomCapsuleSetParams(const geom: PdxGeom;
  const radius, length: TdReal); cdecl; external ODEDLL;
procedure dGeomCapsuleGetParams(const geom: PdxGeom;
  var radius, length: TdReal); cdecl; external ODEDLL;
function dGeomCapsulePointDepth(const geom: PdxGeom; const x, y, z: TdReal)
  : TdReal; cdecl; external ODEDLL;

// Plane
function dCreatePlane(const Space: PdxSpace; const a, b, c, d: TdReal): PdxGeom;
  cdecl; external ODEDLL;
procedure dGeomPlaneSetParams(const geom: PdxGeom; const a, b, c, d: TdReal);
  cdecl; external ODEDLL;
procedure dGeomPlaneGetParams(const geom: PdxGeom; var result: TdVector4);
  cdecl; external ODEDLL;
function dGeomPlanePointDepth(const geom: PdxGeom; const x, y, z: TdReal)
  : TdReal; cdecl; external ODEDLL;

// Sphere
function dCreateSphere(const Space: PdxSpace; const radius: TdReal): PdxGeom;
  cdecl; external ODEDLL;
procedure dGeomSphereSetRadius(const geom: PdxGeom; const radius: TdReal);
  cdecl; external ODEDLL;
function dGeomSphereGetRadius(const geom: PdxGeom): TdReal; cdecl;
  external ODEDLL;
function dGeomSpherePointDepth(const geom: PdxGeom; const x, y, z: TdReal)
  : TdReal; cdecl; external ODEDLL;

// Convex
function dCreateConvex(const Space: PdxSpace; _planes: PdReal;
  _planecount: longword; _points: PdReal; _pointcount: longword;
  const _polygons: longword): PdxGeom; cdecl; external ODEDLL;
procedure dGeomSetConvex(const geom: PdxGeom; _planes: PdReal;
  _planecount: longword; _points: PdReal; _pointcount: longword;
  const _polygons: longword); cdecl; external ODEDLL;

// Heightfield  (incomplete)
function dCreateHeightfield(const Space: PdxSpace; data: PdxHeightfieldData;
  bPlaceable: Integer): PdxGeom; cdecl; external ODEDLL;
function dGeomHeightfieldDataCreate: PdxHeightfieldData; cdecl; external ODEDLL;
procedure dGeomHeightfieldDataDestroy(data: PdxHeightfieldData); cdecl;
  external ODEDLL;
procedure dGeomHeightfieldSetHeightfieldData(const geom: PdxGeom;
  data: PdxHeightfieldData); cdecl; external ODEDLL;
function dGeomHeightfieldGetHeightfieldData(const geom: PdxGeom)
  : PdxHeightfieldData; cdecl; external ODEDLL;
function dGeomHeightfieldDataSetBounds(data: PdxHeightfieldData;
  minHeight, MaxHeight: TdReal): TdReal; cdecl; external ODEDLL;

// dRay
function dCreateRay(const Space: PdxSpace; length: TdReal): PdxGeom; cdecl;
  external ODEDLL;
procedure dGeomRaySet(const geom: PdxGeom; px, py, pz, dx, dy, dz: TdReal);
  cdecl; external ODEDLL;
procedure dGeomRayGet(const geom: PdxGeom; var start, dir: TdVector3); cdecl;
  external ODEDLL;
procedure dGeomRaySetLength(const geom: PdxGeom; length: TdReal); cdecl;
  external ODEDLL;
function dGeomRayGetLength(const geom: PdxGeom): TdReal; cdecl; external ODEDLL;
procedure dGeomRaySetParams(const geom: PdxGeom;
  FirstContact, BackfacCull: Integer); cdecl; external ODEDLL;
procedure dGeomRayGetParams(const geom: PdxGeom;
  var FirstContact, BackfacCull: Integer); cdecl; external ODEDLL;
procedure dGeomRaySetClosestHit(const geom: PdxGeom; closestHit: Integer);
  cdecl; external ODEDLL;
function dGeomRayGetClosestHit(const geom: PdxGeom): Integer; cdecl;
  external ODEDLL;

// TriMesh
function dCreateTriMesh(const Space: PdxSpace; data: PdxTriMeshData;
  callback: TdTriCallback; ArrayCallback: TdTriArrayCallback;
  RayCallback: TdTriRayCallback): PdxGeom; cdecl; external ODEDLL;

procedure dGeomTriMeshSetData(g: PdxGeom; data: PdxTriMeshData); cdecl;
  external ODEDLL;
function dGeomTriMeshGetData(g: PdxGeom): PdxTriMeshData; cdecl;
  external ODEDLL;
function dGeomTriMeshGetTriMeshDataID(g: PdxGeom): PdxTriMeshData; cdecl;
  external ODEDLL;
procedure dGeomTriMeshDataUpdate(g: PdxTriMeshData); cdecl; external ODEDLL;

function dGeomTriMeshIsTCEnabled(g: PdxGeom; geomClass: Integer): Integer;
  cdecl; external ODEDLL;
procedure dGeomTriMeshEnableTC(g: PdxGeom; geomClass, enable: Integer); cdecl;
  external ODEDLL;
procedure dGeomTriMeshClearTCCache(g: PdxGeom); cdecl; external ODEDLL;

function dGeomTriMeshGetTriangleCount(g: PdxGeom): Integer; cdecl;
  external ODEDLL;
procedure dGeomTriMeshGetTriangle(g: PdxGeom; index: Integer;
  v0, v1, v2: PdVector3); cdecl; external ODEDLL;
procedure dGeomTriMeshGetPoint(g: PdxGeom; index: Integer; u, v: TdReal;
  result: TdVector3); cdecl; external ODEDLL;
function dGeomTriMeshGetArrayCallback(g: PdxGeom): pointer; cdecl;
  external ODEDLL;
function dGeomTriMeshGetRayCallback(g: PdxGeom): pointer; cdecl;
  external ODEDLL;
procedure dGeomTriMeshSetArrayCallback(g: PdxGeom; ArrayCallback: pointer);
  cdecl; external ODEDLL;
procedure dGeomTriMeshSetRayCallback(g: PdxGeom; RayCallback: pointer); cdecl;
  external ODEDLL;
procedure dGeomTriMeshSetCallback(g: PdxGeom; callback: pointer); cdecl;
  external ODEDLL;
function dGeomTriMeshGetCallback(g: PdxGeom): pointer; cdecl; external ODEDLL;

procedure dGeomTriMeshDataDestroy(g: PdxTriMeshData); cdecl; external ODEDLL;
function dGeomTriMeshDataCreate: PdxTriMeshData; cdecl; external ODEDLL;
procedure dGeomTriMeshDataSet(g: PdxTriMeshData; data_id: Integer;
  data: pointer); cdecl; external ODEDLL;

procedure dGeomTriMeshDataBuildSimple(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray;
  IndexCount: Integer); cdecl; external ODEDLL;
procedure dGeomTriMeshDataBuildSimple1(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray;
  IndexCount: Integer; Normals: PdVector3Array); cdecl; external ODEDLL;
procedure dGeomTriMeshDataBuildDouble(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexStride, VertexCount: Integer;
  Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl;
  external ODEDLL;
procedure dGeomTriMeshDataBuildDouble1(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexStride, VertexCount: Integer;
  Indices: PdIntegerArray; IndexCount, TriStride: Integer;
  Normals: PdVector3Array); cdecl; external ODEDLL;
procedure dGeomTriMeshDataBuildSingle(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexStride, VertexCount: Integer;
  Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl;
  external ODEDLL;
procedure dGeomTriMeshDataBuildSingle1(g: PdxTriMeshData;
  Vertices: PdVector3Array; VertexStride, VertexCount: Integer;
  Indices: PdIntegerArray; IndexCount, TriStride: Integer;
  Normals: PdVector3Array); cdecl; external ODEDLL;

procedure dInfiniteAABB(geom: PdxGeom; var aabb: TdAABB); cdecl;
  external ODEDLL;

// ----- dSpace -----
procedure dSpaceDestroy(const Space: PdxSpace); cdecl; external ODEDLL;

function dSimpleSpaceCreate(Space: PdxSpace): PdxSpace; cdecl; external ODEDLL;
function dHashSpaceCreate(Space: PdxSpace): PdxSpace; cdecl; external ODEDLL;
function dQuadTreeSpaceCreate(const Space: PdxSpace;
  const Center, Extents: TdVector3; const depth: Integer): PdxSpace; cdecl;
  external ODEDLL;

procedure dSpaceAdd(const Space: PdxSpace; const geom: PdxGeom); cdecl;
  external ODEDLL;
procedure dSpaceRemove(const Space: PdxSpace; const geom: PdxGeom); cdecl;
  external ODEDLL;
procedure dSpaceClean(const Space: PdxSpace); cdecl; external ODEDLL;
function dSpaceQuery(const Space: PdxSpace; const geom: PdxGeom): Integer;
  cdecl; external ODEDLL;
function dSpaceGetNumGeoms(const Space: PdxSpace): Integer; cdecl;
  external ODEDLL;
function dSpaceGetGeom(const Space: PdxSpace; const I: Integer): PdxGeom; cdecl;
  external ODEDLL;
procedure dHashSpaceSetLevels(const Space: PdxSpace;
  const minlevel, maxlevel: Integer); cdecl; external ODEDLL;
procedure dHashSpaceGetLevels(const Space: PdxSpace;
  var minlevel, maxlevel: Integer); cdecl; external ODEDLL;
procedure dSpaceSetCleanup(Space: PdxSpace; const mode: Integer); cdecl;
  external ODEDLL;
function dSpaceGetCleanup(Space: PdxSpace): Integer; cdecl; external ODEDLL;

function dCollide(o1, o2: PdxGeom; flags: Integer; var contact: TdContactGeom;
  skip: Integer): Integer; cdecl; external ODEDLL;
procedure dSpaceCollide(const Space: PdxSpace; data: pointer;
  callback: TdNearCallback); cdecl; external ODEDLL;
procedure dSpaceCollide2(o1, o2: PdxGeom; data: pointer;
  callback: TdNearCallback); cdecl; external ODEDLL;

// ----- dMass -----
procedure dMassSetParameters(var m: TdMass; themass, cgx, cgy, cgz, I11, I22,
  I33, I12, I13, I23: TdReal); cdecl; external ODEDLL;
procedure dMassAdd(var a, b: TdMass); cdecl; external ODEDLL;
procedure dMassAdjust(var m: TdMass; newmass: TdReal); cdecl; external ODEDLL;
procedure dMassTranslate(var m: TdMass; x, y, z: TdReal); cdecl;
  external ODEDLL;
procedure dMassRotate(var m: TdMass; var R: TdMatrix3); cdecl; external ODEDLL;

procedure dMassSetZero(var m: TdMass); cdecl; external ODEDLL;
procedure dMassSetBox(var m: TdMass; density, lx, ly, lz: TdReal); cdecl;
  external ODEDLL;
procedure dMassSetBoxTotal(var m: TdMass; total_mass, lx, ly, lz: TdReal);
  cdecl; external ODEDLL;
procedure dMassSetCylinder(var m: TdMass; density: TdReal; direction: Integer;
  radius, length: TdReal); cdecl; external ODEDLL;
procedure dMassSetCylinderTotal(var m: TdMass; total_mass: TdReal;
  direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
procedure dMassSetCapsule(var m: TdMass; density: TdReal; direction: Integer;
  radius, length: TdReal); cdecl; external ODEDLL;
procedure dMassSetCapsuleTotal(var m: TdMass; total_mass: TdReal;
  direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
procedure dMassSetSphere(var m: TdMass; density, radius: TdReal); cdecl;
  external ODEDLL;
procedure dMassSetSphereTotal(var m: TdMass; total_mass, radius: TdReal); cdecl;
  external ODEDLL;
procedure dMassSetTrimesh(var m: TdMass; density: TdReal; TriMesh: PdxGeom);
  cdecl; external ODEDLL;
procedure dMassSetTrimeshTotal(var m: TdMass; total_mass: TdReal;
  TriMesh: PdxGeom); cdecl; external ODEDLL;

// ----- Rotation.h -----
procedure dQFromAxisAndAngle(var q: TdQuaternion;
  const ax, ay, az, angle: TdReal); cdecl; external ODEDLL;
procedure dRFromAxisAndAngle(var R: TdMatrix3; const ax, ay, az, angle: TdReal);
  cdecl; external ODEDLL;
procedure dRSetIdentity(var R: TdMatrix3); cdecl; external ODEDLL;
procedure dQSetIdentity(var q: TdQuaternion); cdecl; external ODEDLL;
procedure dRFromEulerAngles(var R: TdMatrix3; const phi, theta, psi: TdReal);
  cdecl; external ODEDLL;
procedure dRFrom2Axes(var R: TdMatrix3; const ax, ay, az, bx, by, bz: TdReal);
  cdecl; external ODEDLL;
procedure dRFromZAxis(var R: TdMatrix3; const ax, ay, az: TdReal); cdecl;
  external ODEDLL;

procedure dMultiply0(const a: PdReal; const b, c: PdReal; p, q, R: Integer);
  cdecl; external ODEDLL;
procedure dMultiply1(const a: PdReal; const b, c: PdReal; p, q, R: Integer);
  cdecl; external ODEDLL;
procedure dMultiply2(const a: PdReal; const b, c: PdReal; p, q, R: Integer);
  cdecl; external ODEDLL;
procedure dQMultiply0(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  external ODEDLL;
procedure dQMultiply1(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  external ODEDLL;
procedure dQMultiply2(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  external ODEDLL;
procedure dQMultiply3(var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl;
  external ODEDLL;
procedure dRfromQ(var R: TdMatrix3; const q: TdQuaternion); cdecl;
  external ODEDLL;
procedure dQfromR(var q: TdQuaternion; const R: TdMatrix3); cdecl;
  external ODEDLL;
procedure dDQfromW(var dq: TdVector4; const w: TdVector3;
  const q: TdQuaternion); cdecl; external ODEDLL;

// ----- Math -----
procedure dNormalize3(var a: TdVector3); cdecl; external ODEDLL;
procedure dNormalize4(var a: TdVector4); cdecl; external ODEDLL;

// ----- Misc -----
procedure dClosestLineSegmentPoints(const a1, a2, b1, b2: TdVector3;
  var cp1, cp2: TdVector3); cdecl; external ODEDLL;

function dBoxTouchesBox(const _p1: TdVector3; const R1: TdMatrix3;
  const side1: TdVector3; const _p2: TdVector3; const R2: TdMatrix3;
  const side2: TdVector3): Integer; cdecl; external ODEDLL;

function dMaxDifference(a, b: PdReal; n, m: Integer): TdReal; cdecl;
  external ODEDLL;
procedure dMakeRandomVector(var n1: TdVector3; a: Integer; f: TdReal); cdecl;
  external ODEDLL;
function dAreConnected(a, b: PdxBody): Integer; cdecl; external ODEDLL;
function dAreConnectedExcluding(a, b: PdxBody; joint_type: TdJointTypeNumbers)
  : Integer; cdecl; external ODEDLL;

procedure dMakeRandomMatrix(a: PdRealArray; n, m: Integer; range: TdReal);
  cdecl; external ODEDLL;
procedure dClearUpperTriangle(a: PdRealArray; n: Integer); cdecl;
  external ODEDLL;

function dRandGetSeed: cardinal; cdecl; external ODEDLL;
procedure dRandSetSeed(const s: cardinal); cdecl; external ODEDLL;
function dRandInt(const n: Integer): Integer; cdecl; external ODEDLL;
function dRandReal: TdReal; cdecl; external ODEDLL;

// return 1 if the random number generator is working.
function dTestRand: Integer; cdecl; external ODEDLL;

procedure dTestMatrixComparison; cdecl; external ODEDLL;
procedure dTestSolveLCP; cdecl; external ODEDLL;

// ----- Recreated -----
function dDot(const a, b: TdVector3): TdReal; overload;
function dDot(const a, b: PdVector3): TdReal; overload;

function dDOT14(const a, b: TdRealArray): TdReal; overload;
function dDOT14(const a, b: PdRealArray): TdReal; overload;

procedure dMULTIPLY0_333(var a: TdMatrix3; const b, c: TdMatrix3);
procedure dMULTIPLY0_331(var a: TdVector3; const b: TdMatrix3;
  const c: TdVector3);

function Vector3ScalarMul(const a: TdVector3; const Scalar: TdReal): TdVector3;
function Vector3ADD(const a, b: TdVector3): TdVector3;
function Vector3SUB(const a, b: TdVector3): TdVector3;
function Vector3Length(const a: TdVector3): TdReal;
function Vector3Cross(const v1, v2: TdVector3): TdVector3;
function Vector3Make(const x, y, z: TdReal): TdVector3;

procedure VerifyDelphiODE(body: PdxBody; geom: PdxGeom);

{ ExportInitODEMarker }

const
  MaxUserClasses = 4;

var
  dSphereClass: Integer = 0;
  dBoxClass: Integer = 1;
  dCapsuleClass: Integer = 2;
  dCylinderClass: Integer = 3;
  dPlaneClass: Integer = 4;
  dRayClass: Integer = 5;
  dConvexClass: Integer = 6;
  dGeomTransformClass: Integer = 7;
  dTriMeshClass: Integer = 8;
  dHeightFieldClass: Integer = 9;
  dFirstSpaceClass: Integer = 10;
  dSimpleSpaceClass: Integer = 10;
  dHashSpaceClass: Integer = 11;
  dSweepAndPruneSpaceClass: Integer = 12;
  dQuadTreeSpaceClass: Integer = 13;
  dLastSpaceClass: Integer = 13;
  dFirstUserClass: Integer = 14;
  dLastUserClass: Integer = 17;
  dGeomNumClasses: Integer = 18;

  IsODEInitialized: boolean = false;
  DisabledDebugGeom: boolean = false;
  DisabledDebugCollision: boolean = false;

{$IFDEF cODEDebugEnabled}

var
  ODEDebugGeomList: TGeomList;
{$ENDIF}
{$IFDEF cODEDebugCollisionEnabled}

var
  ODEDebugCollisionList: array of TdContact;
{$ENDIF}
  // These are made public in the dynamic version MRQZZZ
function InitODE(ADllName: PChar): boolean;
procedure CloseODE;

// -----------------------------------------
implementation

// -----------------------------------------

// ---------------------
// TBodyList
// ---------------------

procedure TBodyList.DeleteAllBodies;
var
  I: Integer;
begin
  for I := 0 to count - 1 do
    dBodyDestroy(Get(I));
  Clear;
end;

function TBodyList.GetItems(I: Integer): PdxBody;
begin
  result := Get(I);
end;

procedure TBodyList.SetItems(I: Integer; const Value: PdxBody);
begin
  Put(I, Value);
end;

// ---------------------------
// TGeomList
// ---------------------------

procedure TGeomList.DeleteAllGeoms(DeleteDataAsObject: boolean = false);
var
  I: Integer;
  geom: PdxGeom;
begin
  for I := 0 to count - 1 do
  begin
    geom := Get(I);
    if DeleteDataAsObject and (geom.data <> nil) then
      TObject(geom.data).Free;
    dGeomDestroy(geom);
  end;
  Clear;
end;

function TGeomList.GetItems(I: Integer): PdxGeom;
begin
  result := Get(I);
end;

procedure TGeomList.SetItems(I: Integer; const Value: PdxGeom);
begin
  Put(I, Value);
end;

// ----- Recreated -----

function dDot(const a, b: PdVector3): TdReal;
begin
  Assert(Assigned(a), 'a not assigned!');
  Assert(Assigned(b), 'b not assigned!');
  result := ((a)[0] * (b)[0] + (a)[1] * (b)[1] + (a)[2] * (b)[2]);
end;

function dDot(const a, b: TdVector3): TdReal;
begin
  result := ((a)[0] * (b)[0] + (a)[1] * (b)[1] + (a)[2] * (b)[2]);
end;

// #define dDOT(a,b)   ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2])
// #define dDOT14(a,b) ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8])
// #define dDOT41(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[1] + (a)[8]*(b)[2])
// #define dDOT44(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[4] + (a)[8]*(b)[8])

function dDOT14(const a, b: TdRealArray): TdReal; overload;
begin
  result := ((a)[0] * (b)[0] + (a)[1] * (b)[4] + (a)[2] * (b)[8]);
end;

function dDOT14(const a, b: PdRealArray): TdReal; overload;
begin
  result := ((a)[0] * (b)[0] + (a)[1] * (b)[4] + (a)[2] * (b)[8]);
end;

procedure dMULTIPLY0_331(var a: TdVector3; const b: TdMatrix3;
  const c: TdVector3);
{ var
  v : PdVector3; }
begin
  // #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)

  // #define dMULTIPLYOP0_331(A,op,B,C) \
  // (A)[0] op dDOT((B),(C)); \
  // (A)[1] op dDOT((B+4),(C)); \
  // (A)[2] op dDOT((B+8),(C));

  a[0] := dDot(PdVector3(@(b[0]))^, c);

  a[1] := dDot(PdVector3(@(b[4]))^, c);
  a[2] := dDot(PdVector3(@(b[8]))^, c); // }
end;

procedure dMULTIPLY0_333(var a: TdMatrix3; const b, c: TdMatrix3);
begin
  // #define dMULTIPLY0_333(A,B,C) dMULTIPLYOP0_333(A,=,B,C)
  // #define dMULTIPLYOP0_333(A,op,B,C) \
  // (A)[0] op dDOT14((B),(C)); \
  // (A)[1] op dDOT14((B),(C+1)); \
  // (A)[2] op dDOT14((B),(C+2)); \
  // (A)[4] op dDOT14((B+4),(C)); \
  // (A)[5] op dDOT14((B+4),(C+1)); \
  // (A)[6] op dDOT14((B+4),(C+2)); \
  // (A)[8] op dDOT14((B+8),(C)); \
  // (A)[9] op dDOT14((B+8),(C+1)); \
  // (A)[10] op dDOT14((B+8),(C+2));

  a[0] := dDOT14(PdRealArray(@(b[0])), PdRealArray(@(c[0])));
  a[1] := dDOT14(PdRealArray(@(b[0])), PdRealArray(@(c[1])));
  a[2] := dDOT14(PdRealArray(@(b[0])), PdRealArray(@(c[2])));

  a[4] := dDOT14(PdRealArray(@(b[4])), PdRealArray(@(c[0])));
  a[5] := dDOT14(PdRealArray(@(b[4])), PdRealArray(@(c[1])));
  a[6] := dDOT14(PdRealArray(@(b[4])), PdRealArray(@(c[2])));

  a[8] := dDOT14(PdRealArray(@(b[8])), PdRealArray(@(c[0])));
  a[9] := dDOT14(PdRealArray(@(b[8])), PdRealArray(@(c[1])));
  a[10] := dDOT14(PdRealArray(@(b[8])), PdRealArray(@(c[2])));
end;

function Vector3ScalarMul(const a: TdVector3; const Scalar: TdReal): TdVector3;
begin
  result[0] := a[0] * Scalar;
  result[1] := a[1] * Scalar;
  result[2] := a[2] * Scalar;
end;

function Vector3ADD(const a, b: TdVector3): TdVector3;
begin
  result[0] := a[0] + b[0];
  result[1] := a[1] + b[1];
  result[2] := a[2] + b[2];
end;

function Vector3SUB(const a, b: TdVector3): TdVector3;
begin
  result[0] := a[0] - b[0];
  result[1] := a[1] - b[1];
  result[2] := a[2] - b[2];
end;

function Vector3Length(const a: TdVector3): TdReal;
begin
  result := sqrt(sqr(a[0]) + sqr(a[1]) + sqr(a[2]));
end;

function Vector3Cross(const v1, v2: TdVector3): TdVector3;
begin
  result[0] := v1[1] * v2[2] - v1[2] * v2[1];
  result[1] := v1[2] * v2[0] - v1[0] * v2[2];
  result[2] := v1[0] * v2[1] - v1[1] * v2[0];
end;

function Vector3Make(const x, y, z: TdReal): TdVector3;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

(*
  procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);
  var
  Body : PdxBody;
  TempList : TList;
  begin
  if not Assigned(WasStillBefore) then
  begin
  WasStillBefore := TList.Create;
  WasStillBeforeOld := TList.Create;
  end;

  Body := World.FirstBody;

  WasStillBefore.Clear;

  // We can't disable bodies just as soon as they're still - that could disable
  // bodies that are just slowed down or titering on an edge. If they've been
  // still for two frames, we consider them truly still.
  while Assigned(Body) do
  begin
  if dBodyIsEnabled(Body)=1 then
  begin
  // Is the body still?
  if (abs(Body.lvel[0])<Threshold) and (abs(Body.lvel[1])<Threshold) and (abs(Body.lvel[2])<Threshold) and
  (abs(Body.avel[0])<Threshold) and (abs(Body.avel[1])<Threshold) and (abs(Body.avel[2])<Threshold) then
  begin
  if WasStillBeforeOld.IndexOf(Body)<>-1 then
  dBodyDisable(Body)
  else
  WasStillBefore.Add(Body);
  end;
  end;

  Body := PdxBody(Body.BaseObject.next);
  end;

  TempList := WasStillBeforeOld;
  WasStillBeforeOld := WasStillBefore;
  WasStillBefore := TempList;
  end; *)

procedure VerifyDelphiODE(body: PdxBody; geom: PdxGeom);
var
  m: TdMass;
  VerificationPointer: pointer;
begin
  VerificationPointer := pointer(-1); // A known pointer
  // Verify Body
  dBodySetData(body, VerificationPointer);
  Assert(dBodyGetData(body) = VerificationPointer, 'Body test 1 fails');
  Assert(body.baseObject.userdata = VerificationPointer, 'Body test 2 fails');

  dBodyGetMass(body, m);

  Assert(body.mass.mass = m.mass, 'Body test 3 fails');

  // Verify Geom
  dGeomSetData(geom, VerificationPointer);
  Assert(dGeomGetData(geom) = VerificationPointer, 'Geom test 1 fails');
  Assert(dGeomGetBody(geom) = geom.body, 'Geom test 2 fails');
  Assert(geom.data = VerificationPointer, 'Geom test 3 fails');
end;

var
  vODEHandle: TModuleHandle;

procedure GetODEClassIDs;
begin
{$IFDEF PARODE}
  dSphereClass := dSphereGetClass;
  dBoxClass := dBoxGetClass;
  dPlaneClass := dPlaneGetClass;
  dCylinderClass := dCylinderGetClass;
  dConvexClass := dConvexGetClass;
  dGeomTransformClass := dGeomTransformGetClass;
  dRayClass := dRayGetClass;
  dTriMeshClass := dTriMeshGetClass;
  dHeightFieldClass := dHeightfieldGetClass;
{$ENDIF}
end;

function InitODE(ADllName: PChar): boolean;
var
  isODELoaded: boolean;
begin
  result := IsODEInitialized;
  if IsODEInitialized then
    exit;

  if ADllName = '' then
    ADllName := ODEDLL;

  isODELoaded := LoadModule(vODEHandle, ADllName);
  if not isODELoaded then
    exit;

  if isODELoaded and not IsODEInitialized then
  begin
    dInitODE2(0);
    GetODEClassIDs;
    IsODEInitialized := True;
  end;
  result := IsODEInitialized;
end;

procedure CloseODE;
begin
  if IsODEInitialized then
    dCloseODE;
  IsODEInitialized := false;
  UnLoadModule(vODEHandle);
end;

// ---------------------------------------
initialization
// ---------------------------------------

InitODE(ODEDLL);

// ---------------------------------------
finalization
// ---------------------------------------

CloseODE;

end.
