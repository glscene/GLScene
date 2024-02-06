object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Fast Fourier Transformation'
  ClientHeight = 559
  ClientWidth = 908
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 668
    Top = 56
    Width = 221
    Height = 409
    Caption = 'Panel1'
    TabOrder = 0
    object Label4: TLabel
      Left = 64
      Top = 64
      Width = 34
      Height = 15
      Caption = 'Label4'
    end
    object Label3: TLabel
      Left = 56
      Top = 32
      Width = 34
      Height = 15
      Caption = 'Label3'
    end
    object Label5: TLabel
      Left = 72
      Top = 88
      Width = 34
      Height = 15
      Caption = 'Label5'
    end
  end
  object Panel2: TPanel
    Left = 32
    Top = 65
    Width = 209
    Height = 391
    Caption = 'Panel2'
    TabOrder = 1
  end
  object GLCUDA1: TGLCUDA
    ComputingDevice = GLCUDADevice1
    Left = 306
    Top = 304
  end
  object GLCUDADevice1: TGLCUDADevice
    SelectDevice = 'NVIDIA GeForce GTX 1050 Ti (1)'
    Left = 320
    Top = 232
  end
  object GLCUDACompiler1: TGLCUDACompiler
    Left = 440
    Top = 232
  end
end
