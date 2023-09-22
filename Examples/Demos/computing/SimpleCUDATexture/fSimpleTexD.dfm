object FormST: TFormST
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'CUDA: simple using of texture'
  ClientHeight = 446
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 20
  object Button1: TButton
    Left = 608
    Top = 328
    Width = 94
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 782
    Height = 273
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object GLCUDA1: TGLCUDA
    ComputingDevice = GLCUDADevice1
    Left = 104
    Top = 56
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_10, map_f64_to_f32'
        #9'// compiled with C:\CUDA\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.0 built on 2009-10-29'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/VPV~1/AppData/Local/Temp/...)'
        #9'//-----------------------------------------------------------'
        ''
        #9'//-----------------------------------------------------------'
        #9'// Options:'
        #9'//-----------------------------------------------------------'
        #9'//  Target:ptx, ISA:sm_10, Endian:little, Pointer Size:32'
        #9'//  -O3'#9'(Optimization level)'
        #9'//  -g0'#9'(Debug level)'
        #9'//  -m2'#9'(Report advisories)'
        #9'//-----------------------------------------------------------'
        ''
        #9'exit;'
        '$LDWend_transformKernel:'
        #9'} // transformKernel'
        '')
      object TurnPicture: TCUDAFunction
        KernelName = 'transformKernel'
        BlockShape.SizeX = 8
        BlockShape.SizeY = 8
        Grid.SizeX = 64
        Grid.SizeY = 64
      end
      object Image: TCUDATexture
        KernelName = 'tex'
        AddressModeS = amWrap
        AddressModeT = amWrap
        FilterMode = fmLinear
        Format = ctFloat
        ChannelNum = cnOne
        MemDataArray = TextureArray
      end
    end
    object TextureArray: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtArray
      ChannelsType = ctFloat
    end
    object ResultData: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'GeForce GTX 1050 Ti (1)'
    Left = 336
    Top = 56
  end
  object GLCUDACompiler1: TGLCUDACompiler
    Left = 558
    Top = 59
  end
end
