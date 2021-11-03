object FormFormula: TFormFormula
  Left = 124
  Top = 110
  BorderWidth = 5
  Caption = 'Formula'
  ClientHeight = 349
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 21
    Width = 371
    Height = 328
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 117.253990173339800000
    PenAsTouch = False
    Align = alLeft
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLSceneViewer2: TGLSceneViewer
    Left = 383
    Top = 21
    Width = 371
    Height = 328
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera2
    FieldOfView = 117.253990173339800000
    PenAsTouch = False
    Align = alRight
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 754
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 371
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      AutoSize = False
      Caption = 'FPS1'
    end
    object Label2: TLabel
      Left = 380
      Top = 0
      Width = 371
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      AutoSize = False
      Caption = 'FPS2'
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 32
    object DummyCube1: TGLDummyCube
      CubeSize = 10.000000000000000000
      object Mesh1: TGLMesh
        Mode = mmTriangles
        VertexMode = vmVNC
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00008C42000048420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000F0410000A0410000803F}
    end
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 192
    Top = 32
  end
  object GLScene2: TGLScene
    Left = 344
    Top = 32
    object DummyCube2: TGLDummyCube
      CubeSize = 10.000000000000000000
      object Mesh2: TGLMesh
        Mode = mmTriangles
        VertexMode = vmVNC
      end
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00008C42000048420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube2
      Position.Coordinates = {000048420000F0410000A0410000803F}
    end
  end
end
