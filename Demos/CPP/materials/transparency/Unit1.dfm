object Form1: TForm1
  Left = 192
  Top = 126
  Caption = 'Transparency'
  ClientHeight = 405
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 408
    Height = 405
    Camera = GLCamera1
    Buffer.BackgroundColor = 13224393
    FieldOfView = 146.991271972656300000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 408
    Top = 0
    Width = 202
    Height = 405
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 7
      Top = 10
      Width = 175
      Height = 117
      Caption = 
        'With Transparency and  Z-Buffering, '#13#10'ordering your objects is i' +
        'mportant.'#13#10#13#10'In this sample, only the spheres are'#13#10'transparent.'#13 +
        #10#13#10'Try the various options and see the'#13#10'differences ordering and' +
        ' blending'#13#10'mode make.'
    end
    object Label2: TLabel
      Left = 45
      Top = 143
      Width = 113
      Height = 16
      Caption = 'Central objects :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 19
      Top = 295
      Width = 123
      Height = 16
      Caption = 'Orbiting spheres :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RBSTC: TRadioButton
      Left = 30
      Top = 199
      Width = 168
      Height = 21
      Caption = 'Sphere, Torus, Cone'
      TabOrder = 0
      OnClick = RBSTCClick
    end
    object RBTSC: TRadioButton
      Left = 30
      Top = 226
      Width = 168
      Height = 21
      Caption = 'Torus, Sphere, Cone'
      TabOrder = 1
      OnClick = RBTSCClick
    end
    object RBTCS: TRadioButton
      Left = 30
      Top = 253
      Width = 168
      Height = 21
      Caption = 'Torus, Cone, Sphere'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = RBTCSClick
    end
    object CBAdditive: TCheckBox
      Left = 37
      Top = 327
      Width = 129
      Height = 21
      Caption = 'Additive blending'
      TabOrder = 3
      OnClick = CBAdditiveClick
    end
    object CBSorting: TCheckBox
      Left = 39
      Top = 354
      Width = 119
      Height = 21
      Caption = 'osFarthestFirst'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBSortingClick
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A041000048420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 60.000000000000000000
      TargetObject = DCCentral
      Position.Coordinates = {0000A04000002040000020400000803F}
      Left = 240
      Top = 144
    end
    object BaseDummyCube: TGLDummyCube
      ObjectsSorting = osRenderFarthestFirst
      CubeSize = 1.000000000000000000
      object OrbitingSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000003F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {0000004000000000000000000000803F}
        Radius = 0.500000000000000000
      end
      object OrbitingSphere2: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000003F}
        Material.BlendingMode = bmTransparency
        Position.Coordinates = {000000C000000000000000000000803F}
        Radius = 0.500000000000000000
      end
      object DCCentral: TGLDummyCube
        ObjectsSorting = osNone
        CubeSize = 1.000000000000000000
        object Torus1: TGLTorus
          Material.FrontProperties.Diffuse.Color = {CDCC4C3EA1A0203EFAF9793F0000803F}
          Material.FrontProperties.Emission.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
          Direction.Coordinates = {000000000000803F2EBD3BB300000000}
          Up.Coordinates = {000000002EBD3BB3000080BF00000000}
          MajorRadius = 0.800000011920929000
          MinorRadius = 0.100000001490116100
          StopAngle = 360.000000000000000000
          Parts = [toSides, toStartDisk, toStopDisk]
        end
        object Cone1: TGLCone
          Material.FrontProperties.Diffuse.Color = {FBFA7A3FA5A4243EF9F8F83D0000803F}
          Material.FrontProperties.Emission.Color = {E5E4E43EC1C0403CE1E0603D0000803F}
          Position.Coordinates = {000000009A99993E000000000000803F}
          BottomRadius = 0.300000011920929000
          Height = 2.000000000000000000
        end
        object CentralSphere: TGLSphere
          Material.FrontProperties.Diffuse.Color = {D3D2523FCCCB4B3FFFFE7E3F9A99193F}
          Material.FrontProperties.Emission.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
          Material.BlendingMode = bmTransparency
          Radius = 0.600000023841857900
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
end
