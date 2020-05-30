object Form1: TForm1
  Left = 290
  Top = 245
  Caption = 'GLScene Computing FFT Demo'
  ClientHeight = 306
  ClientWidth = 646
  Color = 2064383
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 540
    Top = 0
    Width = 106
    Height = 306
    Align = alRight
    ParentColor = True
    TabOrder = 0
    object Label4: TLabel
      Left = 6
      Top = 117
      Width = 52
      Height = 13
      Caption = 'Quadrants:'
    end
    object Label5: TLabel
      Left = 6
      Top = 165
      Width = 62
      Height = 13
      Caption = 'Restore with:'
    end
    object Label3: TLabel
      Left = 6
      Top = 213
      Width = 65
      Height = 13
      Caption = 'Show Output:'
    end
    object But1DFFT: TButton
      Left = 6
      Top = 63
      Width = 91
      Height = 21
      Caption = '1D FFT'
      TabOrder = 0
      OnClick = But1DFFTClick
    end
    object But2DFFT: TButton
      Left = 6
      Top = 90
      Width = 43
      Height = 21
      Caption = '2D FFT'
      TabOrder = 1
      OnClick = But2DFFTClick
    end
    object BLenna: TButton
      Left = 54
      Top = 90
      Width = 43
      Height = 21
      Caption = 'Lenna'
      TabOrder = 2
      OnClick = But2DFFTClick
    end
    object CBReorder: TCheckBox
      Left = 6
      Top = 136
      Width = 59
      Height = 17
      Caption = 'Reorder'
      TabOrder = 3
      OnClick = ExecDemo
    end
    object CBInvFFT: TCheckBox
      Left = 6
      Top = 184
      Width = 59
      Height = 17
      Caption = 'InvFFT'
      TabOrder = 4
      OnClick = ExecDemo
    end
    object RBModule: TRadioButton
      Left = 6
      Top = 232
      Width = 58
      Height = 17
      Caption = 'Module'
      Checked = True
      TabOrder = 5
      TabStop = True
      OnClick = ExecDemo
    end
    object RBPhase: TRadioButton
      Left = 6
      Top = 248
      Width = 58
      Height = 17
      Caption = 'Phase'
      TabOrder = 6
      OnClick = ExecDemo
    end
    object RBReal: TRadioButton
      Left = 6
      Top = 264
      Width = 58
      Height = 17
      Caption = 'Real'
      TabOrder = 7
      OnClick = ExecDemo
    end
    object RBImag: TRadioButton
      Left = 6
      Top = 280
      Width = 58
      Height = 17
      Caption = 'Imag'
      TabOrder = 8
      OnClick = ExecDemo
    end
    object ESize: TLabeledEdit
      Left = 62
      Top = 39
      Width = 35
      Height = 21
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'Pulse size:'
      LabelPosition = lpLeft
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 9
      Text = '8'
    end
    object DeviceBox: TComboBox
      Left = 6
      Top = 6
      Width = 91
      Height = 21
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
    Width = 540
    Height = 306
    Align = alClient
    Constraints.MinHeight = 300
    Constraints.MinWidth = 540
    ParentColor = True
    TabOrder = 1
    object Label1: TLabel
      Left = 112
      Top = 24
      Width = 56
      Height = 13
      Caption = 'Input Signal'
    end
    object LDemoMode: TLabel
      Left = 8
      Top = 9
      Width = 67
      Height = 13
      Alignment = taCenter
      Caption = 'Demo mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 368
      Top = 24
      Width = 64
      Height = 13
      Caption = 'Output Signal'
    end
    object Image2: TImage
      Left = 272
      Top = 40
      Width = 257
      Height = 257
    end
    object Image1: TImage
      Left = 8
      Top = 40
      Width = 257
      Height = 257
    end
  end
  object GLSCUDA1: TGLSCUDA
    Left = 24
    Top = 104
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
  object GLSCUDADevice1: TGLSCUDADevice
    SelectDevice = 'GeForce GT 630M (1)'
    Left = 24
    Top = 56
  end
end
