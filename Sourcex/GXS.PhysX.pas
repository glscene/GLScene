//
//
// The graphics engine GXScene https://github.com/glscene
//
//

unit GXS.PhysX;

interface

uses
  Winapi.Windows;

const
  DLL = 'PhysXwrap.dll';

procedure SDK_Version(major, minor, bugfix: pdword); stdcall; external DLL;

procedure InitNx; stdcall; external DLL;
procedure ReleaseNx; stdcall; external DLL;

procedure SimulateNx(dt: single); stdcall; external DLL;
procedure GetResultsNx; stdcall; external DLL;

procedure ActorCount(count: pdword); stdcall; external DLL;
procedure GetActor(index: dword; obj: pdword); stdcall; external DLL;
procedure GetActorGlobalPosition(actor: dword; x,y,z: psingle); stdcall; external DLL;
procedure SetActorGlobalPosition(actor: dword; x,y,z: single); stdcall; external DLL;
procedure GetActorGlobalOrientation(actor: dword; x,y,z,w: psingle); stdcall; external DLL;
procedure SetActorGlobalOrientation(actor: dword; x,y,z,w: single); stdcall; external DLL;
procedure GetActorCMassGlobalPosition(actor: dword; x,y,z: psingle); stdcall; external DLL;
procedure SetActorCMassGlobalPosition(actor: dword; x,y,z: single); stdcall; external DLL;
procedure GetActorMass(actor: dword; m: psingle); stdcall; external DLL;
procedure SetActorMass(actor: dword; m: single); stdcall; external DLL;

procedure ActorAddForce(actor: dword; x,y,z: single); stdcall; external DLL;


procedure CreateGroundPlane(actor: pdword); stdcall; external DLL;
procedure CreateBox(actor: pdword; sx,sy,sz,dens: single); stdcall; external DLL;
procedure CreateSphere(actor: pdword; rad: single; dens: single); stdcall; external DLL;
procedure CreateCylinder(actor: pdword; rad: single; height: single; dens: single); stdcall; external DLL;


implementation

end.
