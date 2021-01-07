//
// The graphics rendering engine GLScene http://glscene.org
//
unit Imports.NGD_Joints;

(******************************************************************************
                                                                               
      Newton Game Dynamics Delphi-Header translation                            
                                                                               
*******************************************************************************
                                                                               
 License :                                                                     
                                                                               
  The contents of this file are used with permission, subject to               
  the Mozilla Public License Version 1.1 (the "License"); you may              
  not use this file except in compliance with the License. You may             
  obtain a copy of the License at                                              
  http://www.mozilla.org/MPL/MPL-1.1.html                                      
                                                                               
  Software distributed under the License is distributed on an                  
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               
  implied. See the License for the specific language governing                 
  rights and limitations under the License.                                    
                                                                               
*******************************************************************************)


{.$DEFINE NEWTON_DOUBLE_PRECISION}

interface


uses
  System.Classes,
  Imports.NGD;

const
{$IFDEF WIN32}
   JointLibraryDLL = 'dJointLibrary32.dll';
{$ENDIF}
{$IFDEF WIN64}
   JointLibraryDLL = 'dJointLibrary64.dll';
{$ENDIF}

type

// *****************************************************************************************************************************
//
//  JointLibrary Callbacks
//
// *****************************************************************************************************************************

NewtonUserJointDestructorCallback = procedure( const me : Pointer ); cdecl;
PNewtonUserJointDestructorCallback = ^NewtonUserJointDestructorCallback;

NewtonUserJointSubmitConstraintCallback = procedure( const me : Pointer; timestep : dFloat; threadIndex : Integer); cdecl;
PNewtonUserJointSubmitConstraintCallback = ^NewtonUserJointSubmitConstraintCallback;

BlankJointGetInfo = procedure( const me : Pointer; info : PNewtonJointRecord ); cdecl;
PBlankJointGetInfo = ^BlankJointGetInfo;

DGRaycastVehicleTireTransformCallback = procedure( car : Pointer ); cdecl;
PDGRaycastVehicleTireTransformCallback = ^DGRaycastVehicleTireTransformCallback;

// *****************************************************************************************************************************
//
// JointLibrary functions
//
// *****************************************************************************************************************************

// generic joint functions
procedure CustomDestroyJoint(const joint: Pointer); cdecl; external JointLibraryDLL;
function CustomGetNewtonJoint(const joint: Pointer): PNewtonJoint; cdecl; external JointLibraryDLL;
function CustomGetJointID(const joint: Pointer): integer; cdecl; external JointLibraryDLL;
procedure CustomSetJointID(const joint: Pointer; rttI: integer); cdecl; external JointLibraryDLL;
function CustomGetBody0(const joint: Pointer): PNewtonBody; cdecl; external JointLibraryDLL;
function CustomGetBody1(const joint: Pointer): PNewtonBody; cdecl; external JointLibraryDLL;
function CustomGetBodiesCollisionState(const joint: Pointer): integer; cdecl; external JointLibraryDLL;
procedure CustomSetBodiesCollisionState(const joint: Pointer; state: integer); cdecl; external JointLibraryDLL;
function CustomGetUserData(const joint: Pointer): Pointer; cdecl; external JointLibraryDLL;
procedure CustomSetUserData(const joint: Pointer; userData: Pointer); cdecl; external JointLibraryDLL;
procedure CustomSetDestructorCallback(const joint: Pointer; callback: NewtonUserJointDestructorCallback); cdecl; external JointLibraryDLL;
procedure CustomSetSubmitContraintCallback(const joint: Pointer; callback: NewtonUserJointSubmitConstraintCallback); cdecl; external JointLibraryDLL;

(* This is a plain blank joint that can be used by advanced users who want to make their own joints
   but that can only use languages that can only interface with C code.
   we recommend using the CPP library to make the joints and then add a C interface, but this join is here for completion *)
function  CustomCreateBlankJoint( maxDof : integer; const body0 : PNewtonBody; const body1 : PNewtonBody; info : BlankJointGetInfo): Pointer; cdecl; external JointLibraryDLL;

// Kinematic control joint
function CreateCustomKinematicController(const targetBody: PNewtonBody; attachmentPointInGlobalSpace: PdFloat): Pointer; cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetPickMode(const pick: Pointer; mode: integer); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetMaxLinearFriction(const pick: Pointer; accel: dFloat); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetMaxAngularFriction(const pick: Pointer; alpha: dFloat); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetTargetPosit(const pick: Pointer; posit: PdFloat); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetTargetRotation(const pick: Pointer; rotation: PdFloat); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerSetTargetMatrix(const pick: Pointer; matrix: PdFloat); cdecl; external JointLibraryDLL;
procedure CustomKinematicControllerGetTargetMatrix(const pick: Pointer; matrix: PdFloat); cdecl; external JointLibraryDLL;

// Generic 6 degree of Freedom Joint
function  CreateCustomJoint6DOF( const pinsAndPivotChildFrame : PdFloat; const pinsAndPivotParentFrame : PdFloat; const child : PNewtonBody; const parent : PNewtonBody ) : Pointer; cdecl; external JointLibraryDLL;
procedure CustomJoint6DOF_SetLinearLimits( customJoint6DOF : Pointer; const minLinearLimits : PdFloat; const maxLinearLimits : PdFloat ); cdecl; external JointLibraryDLL;
procedure CustomJoint6DOF_SetAngularLimits( customJoint6DOF : Pointer; const minAngularLimits : PdFloat; const maxAngularLimits : PdFloat ); cdecl; external JointLibraryDLL;
procedure CustomJoint6DOF_GetLinearLimits( customJoint6DOF : Pointer; minLinearLimits, maxLinearLimits : PdFloat ); cdecl; external JointLibraryDLL;
procedure CustomJoint6DOF_GetAngularLimits( customJoint6DOF : Pointer; minAngularLimits, maxAngularLimits : PdFloat ); cdecl; external JointLibraryDLL;
procedure CustomJoint6DOF_SetReverseUniversal( customJoint6DOF : Pointer; order : integer ); cdecl; external JointLibraryDLL;

// Interface for a custom BallAndSocket joint with Limits
function CreateCustomBallAndSocket(const pinsAndPivotChildFrame: PdFloat; const child: PNewtonBody; const parent: PNewtonBody): Pointer; cdecl; external JointLibraryDLL;
procedure BallAndSocketSetConeAngle(ballJoint: Pointer; angle: dFloat); cdecl; external JointLibraryDLL;
procedure BallAndSocketSetTwistAngle(ballJoint: Pointer; minAngle, maxAngle: dFloat); cdecl; external JointLibraryDLL;

// Interface for a custom Hinge joint with Limits
function  CreateCustomHinge( const pinsAndPivotChildFrame : PdFloat; const child : PNewtonBody; const parent : PNewtonBody ) : Pointer; cdecl; external JointLibraryDLL;
procedure HingeEnableLimits( hingeJoint : Pointer; state : integer ); cdecl; external JointLibraryDLL;
procedure HingeSetLimits( hingeJoint : Pointer; minAngle, maxAngle : dFloat ); cdecl; external JointLibraryDLL;
// 2.15 - Function added - Sw
function  HingeGetJointAngle (const hingeJoint : Pointer) : dFloat; cdecl; external JointLibraryDLL;
// 2.15 - Procedure added - Sw
procedure HingeGetPinAxis (const hingeJoint : Pointer; Pin : PdFloat); cdecl; external JointLibraryDLL;
// 2.15 - Function added - Sw
function  HingeCalculateJointOmega (const hingeJoint : Pointer) : dFloat; cdecl; external JointLibraryDLL;

// Interface for a custom Slider joint with Limits
function  CreateCustomSlider( const pinsAndPivotChildFrame : PdFloat; const child : PNewtonBody; const parent : PNewtonBody ) : Pointer; cdecl; external JointLibraryDLL;
procedure SliderEnableLimits( sliderJoint : Pointer; state : integer ); cdecl; external JointLibraryDLL;
procedure SliderSetLimits( sliderJoint : Pointer; mindist, maxdist : dFloat ); cdecl; external JointLibraryDLL;

// player controller functions
function  CreateCustomPlayerController( const pins : PdFloat; const player : PNewtonBody; maxStairStepFactor, cushion : dFloat ) : Pointer; cdecl; external JointLibraryDLL;
procedure CustomPlayerControllerSetVelocity( const playerController : Pointer; forwardSpeed, sideSpeed, heading : dFloat ); cdecl; external JointLibraryDLL;
procedure CustomPlayerControllerGetVisualMaTrix( const playerController : Pointer; matrix : PdFloat ); cdecl; external JointLibraryDLL;
function  CustomPlayerControllerGetMaxSlope( const playerController : Pointer ) : dFloat; cdecl; external JointLibraryDLL;
procedure CustomPlayerControllerSetMaxSlope( const playerController : Pointer; maxSlopeAngleIndRadian : dFloat ); cdecl; external JointLibraryDLL;
function  CustomPlayerControllerGetSensorShape( const playerController : Pointer ) : PNewtonCollision; cdecl; external JointLibraryDLL;

// Simple ray cast world vehicle
function DGRaycastVehicleCreate(maxTireCount: integer; const cordenateSytemInLocalSpace: PdFloat; carBody: PNewtonBody): Pointer; cdecl; external JointLibraryDLL;
procedure DGRaycastVehicleAddTire(car: Pointer; userData: Pointer; const localPosition: PdFloat; mass, radius, width, friction, suspensionLength, springConst, springDamper: dFloat; castMode: integer); cdecl; external JointLibraryDLL;
procedure DGRaycastVehicleSetTireTransformCallback(car: Pointer; callback: DGRaycastVehicleTireTransformCallback); cdecl; external JointLibraryDLL;
function DGRaycastVehicleGetTiresCount(car: Pointer): integer; cdecl; external JointLibraryDLL;
function DGRaycastVehicleGetTiresUserData(car: Pointer; tireIndex: integer): Pointer; cdecl; external JointLibraryDLL;
procedure DGRaycastVehicleGetTireMatrix(car: Pointer; tireIndex: integer; tireMatrix: PdFloat); cdecl; external JointLibraryDLL;

procedure DGRaycastVehicleInitNormalizeTireLateralForce( car : Pointer; pointsCount : integer; piceSizeStepAxis : PdFloat; normalizedForceValue : PdFloat ); cdecl; external JointLibraryDLL;
procedure DGRaycastVehicleInitNormalizeTireLongitudinalForce( car : Pointer; pointsCount : integer; piceSizeStepAxis : PdFloat; normalizedForceValue : PdFloat ); cdecl; external JointLibraryDLL;

//====================================================
implementation
//====================================================

end.
