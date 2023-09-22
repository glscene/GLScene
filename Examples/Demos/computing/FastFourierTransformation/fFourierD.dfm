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
    Left = 44
    Top = 35
    Width = 221
    Height = 409
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 640
    Top = 53
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
