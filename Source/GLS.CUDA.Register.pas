//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit GLS.CUDA.Register;

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

  GLS.CUDA.APIComps,
  GLS.CUDA.Compiler,
  GLS.CUDA.Context,
  GLS.CUDA.Graphics,
  GLS.CUDA.FFTPlan,
  GLS.CUDA.PropEditors;


procedure Register;

implementation //-------------------------------------------------------------

uses
  CUDA.RunTime,
  CUDA.Parser;


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

  ObjectManager.RegisterSceneObject(TCUDAFeedbackMesh, 'CUDA generated mesh',
     'CUDA Computing', HInstance);
end;

initialization //------------------------------------------------------

  vFindCuFileFunc := FindCuFile;

end.

