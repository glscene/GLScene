//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Physics.PhysXImport;

interface

uses
  Winapi.Windows;

const
{$IFDEF WIN32}
  PhysXDLL = 'PhysXwrap.dll';
{$ELSE}
  PhysXDLL = 'PhysXwrap.dll';
{$ENDIF}



procedure SDK_Version(major, minor, bugfix: pdword); stdcall; external PhysXDLL;

procedure InitNx; stdcall; external PhysXDLL;
procedure ReleaseNx; stdcall; external PhysXDLL;

procedure SimulateNx(dt: single); stdcall; external PhysXDLL;
procedure GetResultsNx; stdcall; external PhysXDLL;

procedure ActorCount(count: pdword); stdcall; external PhysXDLL;
procedure GetActor(index: dword; obj: pdword); stdcall; external PhysXDLL;
procedure GetActorGlobalPosition(actor: dword; x,y,z: psingle); stdcall; external PhysXDLL;
procedure SetActorGlobalPosition(actor: dword; x,y,z: single); stdcall; external PhysXDLL;
procedure GetActorGlobalOrientation(actor: dword; x,y,z,w: psingle); stdcall; external PhysXDLL;
procedure SetActorGlobalOrientation(actor: dword; x,y,z,w: single); stdcall; external PhysXDLL;
procedure GetActorCMassGlobalPosition(actor: dword; x,y,z: psingle); stdcall; external PhysXDLL;
procedure SetActorCMassGlobalPosition(actor: dword; x,y,z: single); stdcall; external PhysXDLL;
procedure GetActorMass(actor: dword; m: psingle); stdcall; external PhysXDLL;
procedure SetActorMass(actor: dword; m: single); stdcall; external PhysXDLL;

procedure ActorAddForce(actor: dword; x,y,z: single); stdcall; external PhysXDLL;

procedure CreateGroundPlane(actor: pdword); stdcall; external PhysXDLL;
procedure CreateBox(actor: pdword; sx,sy,sz,dens: single); stdcall; external PhysXDLL;
procedure CreateSphere(actor: pdword; rad: single; dens: single); stdcall; external PhysXDLL;
procedure CreateCylinder(actor: pdword; rad: single; height: single; dens: single); stdcall; external PhysXDLL;

//--------------------------------------------
implementation
//--------------------------------------------

end.
