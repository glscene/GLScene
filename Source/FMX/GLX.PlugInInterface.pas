//
// The graphics platform GLXcene https://github.com/glscene
//
unit GLX.PlugInInterface;

(*
  An interface unit to plug-ins.
  For more information see help file for writing plug-ins.
*)

interface

{$I GLX.Scene.inc}

type
  TPIServiceType = (stRaw, stObject, stBitmap, stTexture, stImport, stExport);
  TPIServices = set of TPIServiceType;

  TEnumCallBack = procedure(Name: PAnsiChar); stdcall;

  TEnumResourceNames = procedure(Service: TPIServiceType;
    Callback: TEnumCallBack); stdcall;
  TGetServices = function: TPIServices; stdcall;
  TGetVendor = function: PAnsiChar; stdcall;
  TGetDescription = function: PAnsiChar; stdcall;
  TGetVersion = function: PAnsiChar; stdcall;

//----------------------------------
implementation
//----------------------------------

end.
