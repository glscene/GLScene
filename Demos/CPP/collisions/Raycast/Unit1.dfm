object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Ray Cast'
  ClientHeight = 270
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 280
    Top = 49
    Width = 225
    Height = 209
  end
  object Bevel1: TBevel
    Left = 0
    Top = 49
    Width = 225
    Height = 209
  end
  object PaintBox1: TPaintBox
    Left = 288
    Top = 56
    Width = 209
    Height = 193
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 57
    Width = 209
    Height = 193
    Camera = GLCamera1
    FieldOfView = 125.219512939453100000
    TabOrder = 0
  end
  object BUCast: TButton
    Left = 232
    Top = 89
    Width = 41
    Height = 25
    Caption = 'Cast!'
    TabOrder = 1
    OnClick = BUCastClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 513
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 514
      Height = 22
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'RayCasting demo/testbed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 505
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 100
      Height = 14
      Caption = 'OpenGL scene view'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 288
      Top = 32
      Width = 136
      Height = 14
      Caption = 'RayCasted/RayTraced view'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 64
    object Sphere1: TGLSphere
      Material.FrontProperties.Diffuse.Color = {8D8C0C3F8D8C0C3F0000803F0000803F}
      Radius = 0.200000002980232200
    end
    object DummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000080330000803F00000000}
      Up.Coordinates = {EE8384BEEA46773FEA4677B300000000}
      CubeSize = 1.000000000000000000
      object Torus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Direction.Coordinates = {5D1C7C3F02004032CED0313E00000000}
        Position.Coordinates = {000000BF00000000000000000000803F}
        Up.Coordinates = {00C02FB20000803F0000403200000000}
        Visible = False
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object Plane1: TGLPlane
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.FaceCulling = fcNoCull
        Height = 0.699999988079071000
        Width = 0.300000011920929000
      end
      object Cylinder1: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F8180003F0000803F}
        Direction.Coordinates = {00000000010000BFD6B35D3F00000000}
        Position.Coordinates = {0000003F00000000000000000000803F}
        Up.Coordinates = {00000000D6B35D3F0100003F00000000}
        BottomRadius = 0.100000001490116100
        Height = 0.600000023841857900
        TopRadius = 0.100000001490116100
        object GLAnnulus1: TGLAnnulus
          Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
          BottomRadius = 0.230000004172325100
          Height = 0.200000002980232200
          BottomInnerRadius = 0.180000007152557400
          TopInnerRadius = 0.180000007152557400
          TopRadius = 0.230000004172325100
        end
      end
      object GLCube1: TGLCube
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Direction.Coordinates = {00000000B28F70BF431DAF3E00000000}
        Position.Coordinates = {000000BF00000000000000000000803F}
        Up.Coordinates = {00000000431DAF3EB28F703F00000000}
        CubeSize = {CDCC4C3ECDCCCC3D9A99993E}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100001041000070410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000000000000040400000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 64
  end
end
