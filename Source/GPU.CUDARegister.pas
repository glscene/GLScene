//
// The graphics rendering engine GLScene http://glscene.org
//

unit GPU.CUDARegister;

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

  GPU.CUDA,
  GPU.CUDAContext,


  GPU.CUDAPropEditors;


procedure Register;

//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------

uses
//  Import.CUDARunTime,
  GPU.CUDAGraphics,
  GPU.CUDACompiler,
  GPU.CUDAFFTPlan,
  GPU.CUDAParser;


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

