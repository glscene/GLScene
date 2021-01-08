//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDAs.Register;

(*  Registration unit for GPU Computing package *)

interface

uses
  System.Classes,
  System.SysUtils,
  DesignIntf,
  DesignEditors,
  ToolsAPI,
  StrEdit,

  GLS.SceneRegister,

  CUDAs.API,
  CUDAs.Context,


  CUDAs.PropEditors;


procedure Register;

//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------

uses
//  Import.CUDARunTime,
  CUDAs.Graphics,
  CUDAs.Compiler,
  CUDAs.FFTPlan,
  CUDAs.Parser;


procedure Register;
begin
  RegisterComponents('GLScene GPU Computing', [TGLCUDA, TGLCUDADevice, TGLCUDACompiler]);
  RegisterComponentEditor(TGLCUDA, TGLCUDAEditor);
  RegisterComponentEditor(TGLCUDACompiler, TGLCUDACompilerEditor);
  RegisterPropertyEditor(TypeInfo(string), TGLCUDACompiler, 'ProjectModule',
    TGLCUDACompilerSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLCUDADevice, 'SelectDevice',
    TGLCUDADeviceProperty);
  RegisterNoIcon([TCUDAModule, TCUDAMemData, TCUDAFunction, TCUDATexture,
    TCUDAFFTPlan, TCUDAImageResource, TCUDAGeometryResource, TCUDAConstant, TCUDAFuncParam]);

  ObjectManager.RegisterSceneObject(TCUDAFeedbackMesh, 'GPU generated mesh', 'GPU Computing', HInstance);
end;


//------------------------------------------------------
initialization
//------------------------------------------------------

  vFindCuFileFunc := FindCuFile;

end.

