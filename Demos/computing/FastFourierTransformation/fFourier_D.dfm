object FormFFT: TFormFFT
  Left = 290
  Top = 245
  Caption = 'Fast FT Computing'
  ClientHeight = 464
  ClientWidth = 986
  Color = 2064383
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 777
    Top = 0
    Width = 209
    Height = 464
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    ParentColor = True
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 146
      Width = 65
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Quadrants:'
    end
    object Label5: TLabel
      Left = 8
      Top = 206
      Width = 76
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Restore with:'
    end
    object Label3: TLabel
      Left = 8
      Top = 266
      Width = 77
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Output:'
    end
    object But1DFFT: TButton
      Left = 8
      Top = 79
      Width = 113
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '1D FFT'
      TabOrder = 0
      OnClick = But1DFFTClick
    end
    object But2DFFT: TButton
      Left = 8
      Top = 113
      Width = 53
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '2D FFT'
      TabOrder = 1
      OnClick = But2DFFTClick
    end
    object BLenna: TButton
      Left = 68
      Top = 113
      Width = 53
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Lenna'
      TabOrder = 2
      OnClick = But2DFFTClick
    end
    object CBReorder: TCheckBox
      Left = 8
      Top = 170
      Width = 73
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Reorder'
      TabOrder = 3
      OnClick = ExecDemo
    end
    object CBInvFFT: TCheckBox
      Left = 8
      Top = 230
      Width = 73
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'InvFFT'
      TabOrder = 4
      OnClick = ExecDemo
    end
    object RBModule: TRadioButton
      Left = 8
      Top = 290
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Module'
      Checked = True
      TabOrder = 5
      TabStop = True
      OnClick = ExecDemo
    end
    object RBPhase: TRadioButton
      Left = 8
      Top = 310
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Phase'
      TabOrder = 6
      OnClick = ExecDemo
    end
    object RBReal: TRadioButton
      Left = 8
      Top = 330
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Real'
      TabOrder = 7
      OnClick = ExecDemo
    end
    object RBImag: TRadioButton
      Left = 8
      Top = 350
      Width = 72
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Imag'
      TabOrder = 8
      OnClick = ExecDemo
    end
    object ESize: TLabeledEdit
      Left = 78
      Top = 49
      Width = 43
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      EditLabel.Width = 64
      EditLabel.Height = 16
      EditLabel.Margins.Left = 4
      EditLabel.Margins.Top = 4
      EditLabel.Margins.Right = 4
      EditLabel.Margins.Bottom = 4
      EditLabel.Caption = 'Pulse size:'
      LabelPosition = lpLeft
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 9
      Text = '8'
    end
    object DeviceBox: TComboBox
      Left = 8
      Top = 8
      Width = 113
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 10
      Text = 'CPU'
      OnChange = ExecDemo
      Items.Strings = (
        'CPU'
        'GPU')
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 464
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Constraints.MinHeight = 375
    Constraints.MinWidth = 675
    ParentColor = True
    TabOrder = 1
    object Label1: TLabel
      Left = 140
      Top = 30
      Width = 69
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Input Signal'
    end
    object LDemoMode: TLabel
      Left = 10
      Top = 11
      Width = 85
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Caption = 'Demo mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 460
      Top = 30
      Width = 79
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Output Signal'
    end
    object Image2: TImage
      Left = 340
      Top = 50
      Width = 321
      Height = 321
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    object Image1: TImage
      Left = 10
      Top = 50
      Width = 321
      Height = 321
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
  end
  object GLCUDA1: TGLCUDA
    Left = 152
    Top = 56
    object Signal1D: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctDouble
      ChannelsNum = cnTwo
    end
    object FFTPlan1D: TCUDAFFTPlan
      Transform = fftDoubleComplexToDoubleComplex
    end
    object Temp1D: TCUDAMemData
      Width = 128
      MemoryType = mtDevice
      ChannelsType = ctDouble
      ChannelsNum = cnTwo
    end
    object Signal2D: TCUDAMemData
      Height = 256
      MemoryType = mtDevice
      ChannelsType = ctDouble
      ChannelsNum = cnTwo
    end
    object Temp2D: TCUDAMemData
      Width = 128
      Height = 128
      MemoryType = mtDevice
      ChannelsType = ctDouble
      ChannelsNum = cnTwo
    end
    object FFTPlan2D: TCUDAFFTPlan
      Height = 256
      Transform = fftDoubleComplexToDoubleComplex
    end
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'GeForce GTX 1050 Ti(1)'
    Left = 472
    Top = 56
  end
end
