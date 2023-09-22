object FormSF: TFormSF
  Left = 0
  Top = 0
  Caption = 'GLScene CUDA Fluids'
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
    VSync = vsmSync
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = csa16xHQ
    FieldOfView = 157.897079467773400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 104
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
    object ParticleRenderer: TCUDAFeedBackMesh
      Attributes = <
        item
          Name = 'Position'
          GLSLType = GLSLType2F
          KernelFunction = advectParticles
          OnBeforeKernelLaunch = BeforeKernelLaunch
        end>
      Shader = FluidShader
      Launching = fblOnePerAtttribute
    end
    object ResetButton: TGLButton
      Autosize = False
      RedrawAtOnce = False
      GuiLayout = GLGuiLayout1
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 45.000000000000000000
      Height = 50.000000000000000000
      Left = 30.000000000000000000
      Top = 20.000000000000000000
      Position.Coordinates = {0000F0410000A041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      DefaultColor = clWhite
      Caption = 'Reset'
      Focused = False
      FocusedColor = clWhite
      Group = -1
      BitBtn.BlendingMode = bmTransparency
      BitBtn.Texture.Image.Picture.Data = {
        07544269746D6170F6070000424DF60700000000000036040000280000002D00
        0000140000000100080000000000C00300000000000000000000000100000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000C0DCC000F0CAA6000020400000206000002080000020A0000020C0000020
        E00000400000004020000040400000406000004080000040A0000040C0000040
        E00000600000006020000060400000606000006080000060A0000060C0000060
        E00000800000008020000080400000806000008080000080A0000080C0000080
        E00000A0000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0
        E00000C0000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0
        E00000E0000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0
        E00040000000400020004000400040006000400080004000A0004000C0004000
        E00040200000402020004020400040206000402080004020A0004020C0004020
        E00040400000404020004040400040406000404080004040A0004040C0004040
        E00040600000406020004060400040606000406080004060A0004060C0004060
        E00040800000408020004080400040806000408080004080A0004080C0004080
        E00040A0000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0
        E00040C0000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0
        E00040E0000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0
        E00080000000800020008000400080006000800080008000A0008000C0008000
        E00080200000802020008020400080206000802080008020A0008020C0008020
        E00080400000804020008040400080406000804080008040A0008040C0008040
        E00080600000806020008060400080606000806080008060A0008060C0008060
        E00080800000808020008080400080806000808080008080A0008080C0008080
        E00080A0000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0
        E00080C0000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0
        E00080E0000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0
        E000C0000000C0002000C0004000C0006000C0008000C000A000C000C000C000
        E000C0200000C0202000C0204000C0206000C0208000C020A000C020C000C020
        E000C0400000C0402000C0404000C0406000C0408000C040A000C040C000C040
        E000C0600000C0602000C0604000C0606000C0608000C060A000C060C000C060
        E000C0800000C0802000C0804000C0806000C0808000C080A000C080C000C080
        E000C0A00000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0
        E000C0C00000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0
        A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFFFFFFFFFF00
        0000FFFFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4FFFFFFF000000FFFF4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFF00
        0000FF4F4F4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF4F4F4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4F4F4FFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4F4F4FFF00
        0000FFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4FFFFF000000FFFFFF4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFFFF00
        0000FFFFFFFFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4FFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000}
      BitBtn.Texture.ImageAlpha = tiaInverseLuminance
      BitBtn.Texture.MagFilter = maNearest
      BitBtn.Texture.MinFilter = miNearest
      BitBtn.Texture.TextureMode = tmReplace
      BitBtn.Texture.Disabled = False
      Pressed = False
      OnButtonClick = ResetButtonButtonClick
      LogicWidth = 50.000000000000000000
      LogicHeight = 21.000000000000000000
      XOffset = 24.000000000000000000
      YOffset = 10.000000000000000000
      AllowUp = False
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 48
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'GeForce GTX 1980 Ti (1)'
    Left = 448
    Top = 48
  end
  object GLCUDA1: TGLCUDA
    ComputingDevice = GLCUDADevice1
    OnOpenGLInteropInit = GLCUDA1OpenGLInteropInit
    Left = 448
    Top = 104
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_13'
        
          #9'// compiled with C:\Program Files\NVIDIA GPU Computing Toolkit\' +
          'CUDA\v3.2\\bin/../open64/lib//be.exe'
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
		
        #9'exit;'
        '$LDWend_advectParticles_k:'
        #9'} // advectParticles_k'
        '')
      Compiler = GLCUDACompiler1
      object TextureOfVelocityField: TCUDATexture
        KernelName = 'texref'
        NormalizedCoord = False
        FilterMode = fmLinear
        Format = ctFloat
        ChannelNum = cnTwo
        MemDataArray = ArrayOfTexture
      end
      object addForces: TCUDAFunction
        KernelName = 'addForces_k'
        OnParameterSetup = addForcesParameterSetup
        object addForces_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object addForces_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_spx: TCUDAFuncParam
          KernelName = 'spx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_spy: TCUDAFuncParam
          KernelName = 'spy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_fx: TCUDAFuncParam
          KernelName = 'fx'
          DataType = float1
          Size = 0
          Reference = False
        end
        object addForces_k_fy: TCUDAFuncParam
          KernelName = 'fy'
          DataType = float1
          Size = 0
          Reference = False
        end
        object addForces_k_r: TCUDAFuncParam
          KernelName = 'r'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
      end
      object advectVelocity: TCUDAFunction
        KernelName = 'advectVelocity_k'
        OnParameterSetup = advectVelocityParameterSetup
        object advectVelocity_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float1
          Size = 0
          Reference = True
        end
        object advectVelocity_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float1
          Size = 0
          Reference = True
        end
        object advectVelocity_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_pdx: TCUDAFuncParam
          KernelName = 'pdx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
      end
      object diffuseProject: TCUDAFunction
        KernelName = 'diffuseProject_k'
        OnParameterSetup = diffuseProjectParameterSetup
        object diffuseProject_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float2
          Size = 0
          Reference = True
        end
        object diffuseProject_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float2
          Size = 0
          Reference = True
        end
        object diffuseProject_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_visc: TCUDAFuncParam
          KernelName = 'visc'
          DataType = float1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
      end
      object updateVelocity: TCUDAFunction
        KernelName = 'updateVelocity_k'
        OnParameterSetup = updateVelocityParameterSetup
        object updateVelocity_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object updateVelocity_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float1
          Size = 0
          Reference = True
        end
        object updateVelocity_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float1
          Size = 0
          Reference = True
        end
        object updateVelocity_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_pdx: TCUDAFuncParam
          KernelName = 'pdx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
        object updateVelocity_k_scale: TCUDAFuncParam
          KernelName = 'scale'
          DataType = float1
          Size = 0
          Reference = False
        end
      end
      object advectParticles: TCUDAFunction
        KernelName = 'advectParticles_k'
        OnParameterSetup = advectParticlesParameterSetup
        object advectParticles_k_part: TCUDAFuncParam
          KernelName = 'part'
          DataType = float2
          Size = 0
          Reference = True
        end
        object advectParticles_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object advectParticles_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object advectParticles_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
      end
    end
    object ArrayOfTexture: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtArray
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object VelocityField: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ComplexVXField: TCUDAMemData
      Width = 131584
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ComplexVYField: TCUDAMemData
      Width = 262144
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object InitialPosition: TCUDAMemData
      Width = 512
      Height = 512
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ForwardFFT: TCUDAFFTPlan
      Width = 512
      Height = 512
    end
    object InverseFFT: TCUDAFFTPlan
      Width = 512
      Height = 512
      Transform = fftComplexToReal
    end
    object ParticleMapper: TCUDAGeometryResource
      FeedBackMesh = ParticleRenderer
      Mapping = grmWriteDiscard
    end
  end
  object GLCUDACompiler1: TGLCUDACompiler
    NVCCPath = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.2\bin\'
    CppCompilerPath = 'C:\Program Files (x86)\Microsoft Visual Studio 2019\VC\bin\'
    ProjectModule = 'Fluids kernels.cu'
    Left = 448
    Top = 160
  end
  object FluidShader: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 330'
      'precision highp float;'
      'out vec4 FragColor;'
      'void main (void)'
      '{'
      '    FragColor = vec4(0.0, 1.0, 0.0, 0.5);'
      '}')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 330'
      'in vec2 Position;'
      'void main(void)'
      '{'
      '    vec2 FlipPos = vec2(Position.x, 1.0 - Position.y);'
      '    gl_Position = vec4(2.0*FlipPos - 1.0, 0.0, 1.0);'
      '}')
    VertexProgram.Enabled = True
    OnApply = FluidShaderApply
    FailedInitAction = fiaSilentDisable
    Left = 40
    Top = 200
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'FluidMaterial'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Shader = FluidShader
      end>
    Left = 40
    Top = 152
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Left = 40
    Top = 256
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = GLWindowsBitmapFont1
    GuiComponents = <>
    Left = 40
    Top = 304
  end
end
