object FormOdeSimple: TFormOdeSimple
  Left = 192
  Top = 105
  Caption = 'Simple ODE'
  ClientHeight = 528
  ClientWidth = 798
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 191
    Top = 0
    Width = 607
    Height = 528
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clSkyBlue
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 158.551101684570300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 191
    Height = 528
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 80
      Width = 105
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Choose an object'
    end
    object Label2: TLabel
      Left = 10
      Top = 260
      Width = 120
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'HeightField Contact Resolution'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 10
      Top = 10
      Width = 123
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Choose surface type'
    end
    object Spawn: TButton
      Left = 50
      Top = 140
      Width = 94
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Spawn'
      TabOrder = 0
      OnClick = SpawnClick
    end
    object cbObjects: TComboBox
      Left = 10
      Top = 100
      Width = 171
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Sphere'
      Items.Strings = (
        'Sphere'
        'Box'
        'Capsule (CCylinder)'
        'Cylinder'
        'Cone')
    end
    object chbElements: TCheckBox
      Left = 10
      Top = 180
      Width = 161
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show ODE Elements'
      TabOrder = 2
      OnClick = chbElementsClick
    end
    object chbContacts: TCheckBox
      Left = 10
      Top = 210
      Width = 161
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show HeightField Contacts'
      TabOrder = 3
      WordWrap = True
      OnClick = chbContactsClick
    end
    object TrackBar1: TTrackBar
      Left = 13
      Top = 320
      Width = 171
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 4
      ThumbLength = 13
      TickMarks = tmBoth
      OnChange = TrackBar1Change
    end
    object cbSurface: TComboBox
      Left = 10
      Top = 30
      Width = 171
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'Plane'
      OnChange = cbSurfaceChange
      Items.Strings = (
        'Plane'
        'HeightField')
    end
  end
  object GLScene1: TGLScene
    Left = 168
    Top = 8
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F000040400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          LightStyle = lsOmni
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLHeightField1: TGLHeightField
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 0.500000000000000000
      YSamplingScale.Min = -10.000000000000000000
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 0.500000000000000000
      Options = []
      OnGetHeight = GLHeightField1GetHeight
      BehavioursData = {
        0458434F4C02010201061154474C4F44454865696768744669656C6402000618
        4F4445204865696768744669656C6420436F6C6C696465720200020012000000
        0002000500000000006F1283F53F0800000500000000000000FA084005000000
        0000000000000005000000000000000000000500000000000000000000050000
        0000000000000000050000000000000000000005000000000000000000000500
        0000000000000000000500000000000000000000050000000000000000000002
        00050000000000000080FF3F080500000000000000C000400000803F0200}
    end
    object GLPlane1: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 10.000000000000000000
      Width = 10.000000000000000000
      BehavioursData = {
        0458434F4C02010201060C54474C4F44455374617469630200060A4F44452053
        746174696302000200120000000002000500000000006F1283F53F0800000500
        000000000000FA08400500000000000000000000050000000000000000000005
        0000000000000000000005000000000000000000000500000000000000000000
        0500000000000000000000050000000000000000000005000000000000000000
        00050000000000000000000002000458434F4C02010201061254474C4F444545
        6C656D656E74506C616E65020102000605506C616E6502000200080200080200
        08050000000000000080FF3F}
    end
    object ODEObjects: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLRenderPoint1: TGLRenderPoint
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 256
    Top = 8
  end
  object GLODEManager1: TGLODEManager
    Gravity.Coordinates = {00000000C3F51CC1000000000000803F}
    Solver = osmQuickStep
    Iterations = 3
    MaxContacts = 8
    RenderPoint = GLRenderPoint1
    Visible = False
    VisibleAtRunTime = True
    Left = 192
    Top = 88
  end
end
