object FormVDG: TFormVDG
  Left = 423
  Top = 62
  Caption = 'CUDA fit GLScene'
  ClientHeight = 512
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 512
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 157.897079467773400000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000C03F0000C03F000000400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 2.000000000000000000
      VisibleAtRunTime = True
      object CUDAFeedBackMesh1: TCUDAFeedBackMesh
        Attributes = <
          item
            Name = 'Position'
            GLSLType = GLSLType4F
          end>
        Shader = GLSLShader1
        CommonKernelFunction = MakeDotField
        Visible = False
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Form1 - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 24
    Top = 72
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'GeForce GTX 260 (1)'
    Left = 448
    Top = 16
  end
  object GLCUDA1: TGLCUDA
    ComputingDevice = GLCUDADevice1
    OnOpenGLInteropInit = GLCUDA1OpenGLInteropInit
    Left = 448
    Top = 72
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_13'
        
          #9'// compiled with C:\Program Files\NVIDIA GPU Computing Toolkit\' +
          'CUDA\v3.2\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.2 built on 2010-11-06'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/VPV~1/AppData/Local/Temp/...)'
        #9'//-----------------------------------------------------------'
        ''
        #9'//-----------------------------------------------------------'
        #9'// Options:'
        #9'//-----------------------------------------------------------'
        #9'//  Target:ptx, ISA:sm_13, Endian:little, Pointer Size:32'
        #9'//  -O3'#9'(Optimization level)'
        #9'//  -g0'#9'(Debug level)'
        #9'//  -m2'#9'(Report advisories)'
        #9'//-----------------------------------------------------------'
        ''
       
          #9'.file'#9'2'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLUD' +
          'E\crtdefs.h"'
        
          #9'.file'#9'3'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\crt/device_runtime.h"'
        
          #9'.file'#9'4'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\host_defines.h"'
        
          #9'.file'#9'5'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\builtin_types.h"'
        
          #9'.file'#9'6'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\device_types.h"'
        
          #9'.file'#9'7'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\driver_types.h"'
        
          #9'.file'#9'8'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\surface_types.h"'
        
          #9'.file'#9'9'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\texture_types.h"'
        
          #9'.file'#9'10'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\vector_types.h"'
        
          #9'.file'#9'11'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\builtin_types.h"'
        
          #9'.file'#9'12'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\host_defines.h"'
        
          #9'.file'#9'13'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3' +
          '.2\include\device_launch_parameters.h"'
        
          #9'.file'#9'14'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\crt\storage_class.h"'
        
          #9'.file'#9'15'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLU' +
          'DE\time.h"'
        
          #9'.file'#9'16'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\texture_fetch_functions.h"'
        
          #9'.file'#9'17'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3' +
          '.2\include\common_functions.h"'
        
          #9'.file'#9'18'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_functions.h"'
        
          #9'.file'#9'19'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_constants.h"'
        
          #9'.file'#9'20'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\device_functions.h"'
        
          #9'.file'#9'21'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_11_atomic_functions.h"'
        
          #9'.file'#9'22'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_12_atomic_functions.h"'
        
          #9'.file'#9'23'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_13_double_functions.h"'
        
          #9'.file'#9'24'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_20_atomic_functions.h"'
        
          #9'.file'#9'25'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_20_intrinsics.h"'
        
          #9'.file'#9'26'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\surface_functions.h"'
        
          #9'.file'#9'27'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_functions_dbl_ptx3.h"'
        #9'.file'#9'28'#9'"C:/Users/YARUNA~1/AppData/Local/Temp/temp.cu"'
        ''
        
          #9'.const .align 4 .b8 __cudart_i2opi_f[24] = {65,144,67,60,153,14' +
          '9,98,219,192,221,52,245,209,87,39,252,41,21,68,78,110,131,249,16' +
          '2};'
        ''
        #9'exit;'
        '$LDWend__Z6kernelP6float4jjf:'
        #9'} // _Z6kernelP6float4jjf'
        '')
      Compiler = GLCUDACompiler1
      object MakeDotField: TCUDAFunction
        KernelName = '_Z6kernelP6float4jjf'
        BlockShape.SizeX = 8
        BlockShape.SizeY = 8
        OnParameterSetup = MakeVertexBufferParameterSetup
      end
    end
    object DotFieldMapper: TCUDAGeometryResource
      FeedBackMesh = GLFeedBackMesh1
      Mapping = grmWriteDiscard
      Left = 248
      Top = 264
    end
  end
  object GLCUDACompiler1: TGLCUDACompiler
    NVCCPath = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.2\bin\'
    CppCompilerPath = 'C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\'
    ProjectModule = 'Simple kernel.cu'
    Left = 448
    Top = 128
  end
  object GLSLShader1: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 330'
      'out vec4 FragColor;'
      'void main(void)'
      '{'
      '    FragColor = vec4(1.0, 0.0, 0.0, 1.0);'
      '};')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 330'
      'in vec4 Position;'
      'uniform mat4 ModelViewProjectionMatrix;'
      'void main()'
      '{'
      '    gl_Position = ModelViewProjectionMatrix * Position;'
      '};')
    VertexProgram.Enabled = True
    OnApply = GLSLShader1Apply
    FailedInitAction = fiaSilentDisable
    Left = 24
    Top = 128
  end
end
