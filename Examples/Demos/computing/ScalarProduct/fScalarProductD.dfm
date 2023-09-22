object FormSP: TFormSP
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 
    'Calculation scalar products of a  given set of input vector pair' +
    's'
  ClientHeight = 365
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 17
  object Memo1: TMemo
    Left = 10
    Top = 10
    Width = 563
    Height = 291
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object Button1: TButton
    Left = 479
    Top = 324
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Run'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLCUDA1: TGLCUDA
    ComputingDevice = GLCUDADevice1
    Left = 80
    Top = 128
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
        '')
      Compiler = GLCUDACompiler1
      object scalarProdGPU: TCUDAFunction
        KernelName = '_Z13scalarProdGPUPfS_S_ii'
        BlockShape.SizeX = 128
        Grid.SizeX = 256
        OnParameterSetup = scalarProdGPUParameterSetup
        object _Z13scalarProdGPUPfS_S_ii_d_C: TCUDAFuncParam
          KernelName = 'd_C'
          DataType = float1
          Size = 0
          Reference = True
        end
        object _Z13scalarProdGPUPfS_S_ii_d_A: TCUDAFuncParam
          KernelName = 'd_A'
          DataType = float1
          Size = 0
          Reference = True
        end
        object _Z13scalarProdGPUPfS_S_ii_d_B: TCUDAFuncParam
          KernelName = 'd_B'
          DataType = float1
          Size = 0
          Reference = True
        end
        object _Z13scalarProdGPUPfS_S_ii_vectorN: TCUDAFuncParam
          KernelName = 'vectorN'
          DataType = int1
          Size = 0
          Reference = False
        end
        object _Z13scalarProdGPUPfS_S_ii_elementN: TCUDAFuncParam
          KernelName = 'elementN'
          DataType = int1
          Size = 0
          Reference = False
        end
      end
    end
    object deviceA: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object deviceB: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object deviceC: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object hostC_GPU: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostB: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostC_CPU: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostA: TCUDAMemData
      ChannelsType = ctFloat
    end
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'GeForce GTX 1050 Ti (1)'
    Left = 408
    Top = 128
  end
  object GLCUDACompiler1: TGLCUDACompiler
    ProjectModule = 'scalarProd_kernel.cu'
    Left = 240
    Top = 128
  end
end