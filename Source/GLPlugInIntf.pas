//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
  An interface unit to plug-ins. 
  For more information see help file for writing plug-ins. 
}

unit GLPlugInIntf;

interface

{$I GLScene.inc}

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

//------------------------------------------------------------
implementation
//------------------------------------------------------------

end.
