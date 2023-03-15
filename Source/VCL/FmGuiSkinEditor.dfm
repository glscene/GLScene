object GLSkinEditorForm: TGLSkinEditorForm
  Left = 262
  Top = 321
  Caption = 'Skin Editor'
  ClientHeight = 359
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 340
    Width = 764
    Height = 19
    Panels = <
      item
        Text = 'x:'
        Width = 50
      end
      item
        Text = 'y:'
        Width = 50
      end
      item
        Text = 'dx:'
        Width = 50
      end
      item
        Text = 'dy:'
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object panBottom: TPanel
    Left = 0
    Top = 308
    Width = 764
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      764
      32)
    object Button5: TButton
      Left = 600
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button6: TButton
      Left = 682
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object panZoomImage: TPanel
    Left = 285
    Top = 0
    Width = 330
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object imgFull: TImage
      Left = 6
      Top = 1
      Width = 298
      Height = 279
      OnMouseDown = imgFullMouseDown
      OnMouseMove = imgFullMouseMove
      OnMouseUp = imgFullMouseUp
    end
    object sbarHorizontal: TScrollBar
      Left = 1
      Top = 286
      Width = 308
      Height = 17
      LargeChange = 64
      Max = 256
      Min = 1
      PageSize = 256
      Position = 1
      TabOrder = 0
      OnChange = ScrollbarChange
      OnScroll = ScrollBarScroll
    end
    object sbarVertical: TScrollBar
      Left = 310
      Top = 1
      Width = 17
      Height = 284
      Kind = sbVertical
      LargeChange = 64
      Max = 256
      Min = 1
      PageSize = 256
      Position = 1
      TabOrder = 1
      OnChange = ScrollbarChange
      OnScroll = ScrollBarScroll
    end
  end
  object panImageProperties: TPanel
    Left = 615
    Top = 0
    Width = 149
    Height = 308
    Align = alRight
    TabOrder = 3
    object Label5: TLabel
      Left = 12
      Top = 212
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object Label6: TLabel
      Left = 12
      Top = 236
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object Panel2: TPanel
      Left = 11
      Top = 3
      Width = 130
      Height = 130
      TabOrder = 0
      object imgPreview: TImage
        Left = 1
        Top = 1
        Width = 128
        Height = 128
        Cursor = crHandPoint
        Align = alClient
        OnMouseDown = imgPreviewMouseDown
        OnMouseMove = imgPreviewMouseMove
      end
    end
    object Panel3: TPanel
      Left = 12
      Top = 140
      Width = 129
      Height = 27
      BevelOuter = bvLowered
      TabOrder = 1
      object Label2: TLabel
        Left = 80
        Top = 7
        Width = 15
        Height = 13
        Alignment = taRightJustify
        Caption = '1.0'
      end
      object Label1: TLabel
        Left = 12
        Top = 7
        Width = 30
        Height = 13
        Caption = 'Zoom:'
      end
      object Button3: TButton
        Left = 113
        Top = 1
        Width = 15
        Height = 12
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 113
        Top = 13
        Width = 15
        Height = 12
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = Button4Click
      end
    end
    object CheckBox1: TCheckBox
      Left = 12
      Top = 180
      Width = 129
      Height = 17
      Caption = 'Show Preview'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object WidthEdit: TEdit
      Left = 48
      Top = 208
      Width = 95
      Height = 21
      TabOrder = 3
      Text = '256'
      OnChange = WidthEditChange
    end
    object HeightEdit: TEdit
      Left = 48
      Top = 232
      Width = 95
      Height = 21
      TabOrder = 4
      Text = '256'
      OnChange = HeightEditChange
    end
  end
  object panElements: TPanel
    Left = 0
    Top = 0
    Width = 285
    Height = 308
    Align = alLeft
    TabOrder = 4
    object Bevel2: TBevel
      Left = 140
      Top = 212
      Width = 137
      Height = 57
    end
    object Bevel1: TBevel
      Left = 140
      Top = 72
      Width = 137
      Height = 133
    end
    object Label3: TLabel
      Left = 4
      Top = 12
      Width = 81
      Height = 13
      Caption = 'Skin Elements'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Tag = 1
      Left = 144
      Top = 44
      Width = 43
      Height = 13
      Alignment = taRightJustify
      Caption = 'Skin Part'
    end
    object Label11: TLabel
      Tag = 1
      Left = 144
      Top = 112
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Caption = 'Left'
    end
    object Label12: TLabel
      Tag = 1
      Left = 187
      Top = 80
      Width = 19
      Height = 13
      Alignment = taRightJustify
      Caption = 'Top'
    end
    object Label13: TLabel
      Tag = 1
      Left = 233
      Top = 112
      Width = 25
      Height = 13
      Alignment = taRightJustify
      Caption = 'Right'
    end
    object Label9: TLabel
      Tag = 1
      Left = 169
      Top = 220
      Width = 37
      Height = 13
      Alignment = taRightJustify
      Caption = 'Scale X'
    end
    object Label10: TLabel
      Tag = 1
      Left = 169
      Top = 248
      Width = 37
      Height = 13
      Alignment = taRightJustify
      Caption = 'Scale Y'
    end
    object Label14: TLabel
      Tag = 1
      Left = 189
      Top = 156
      Width = 33
      Height = 13
      Alignment = taRightJustify
      Caption = 'Bottom'
    end
    object lbElements: TListBox
      Left = 4
      Top = 36
      Width = 129
      Height = 257
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbElementsClick
      OnKeyDown = lbElementsKeyDown
    end
    object btnAdd: TButton
      Left = 88
      Top = 12
      Width = 21
      Height = 17
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Tag = 1
      Left = 112
      Top = 12
      Width = 21
      Height = 17
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object ComboBox1: TComboBox
      Tag = 1
      Left = 196
      Top = 40
      Width = 81
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = ComboBox1Change
      Items.Strings = (
        'TopLeft'
        'Top'
        'TopRight'
        'Left'
        'Center'
        'Right'
        'BottomLeft'
        'Bottom'
        'BottomRight')
    end
    object LeftEdit: TEdit
      Tag = 1
      Left = 144
      Top = 128
      Width = 40
      Height = 21
      TabOrder = 4
      Text = '256'
      OnChange = LeftEditChange
      OnKeyPress = EditKeyPress
    end
    object TopEdit: TEdit
      Tag = 1
      Left = 188
      Top = 96
      Width = 40
      Height = 21
      TabOrder = 5
      Text = '256'
      OnChange = TopEditChange
      OnKeyPress = EditKeyPress
    end
    object RightEdit: TEdit
      Tag = 1
      Left = 232
      Top = 128
      Width = 40
      Height = 21
      TabOrder = 6
      Text = '256'
      OnChange = RightEditChange
      OnKeyPress = EditKeyPress
    end
    object BottomEdit: TEdit
      Tag = 1
      Left = 188
      Top = 172
      Width = 40
      Height = 21
      TabOrder = 7
      Text = '256'
      OnChange = BottomEditChange
      OnKeyPress = EditKeyPress
    end
    object ScaleXEdit: TEdit
      Tag = 1
      Left = 216
      Top = 216
      Width = 40
      Height = 21
      TabOrder = 8
      Text = '1'
      OnChange = ScaleXEditChange
      OnKeyPress = EditKeyPress
    end
    object ScaleYEdit: TEdit
      Tag = 1
      Left = 216
      Top = 244
      Width = 40
      Height = 21
      TabOrder = 9
      Text = '1'
      OnChange = ScaleYEditChange
      OnKeyPress = EditKeyPress
    end
  end
  object GLScene1: TGLScene
    Left = 324
    Top = 12
    object HUDSprite1: TGLHUDSprite
      Material.FrontProperties.Ambient.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Emission.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Specular.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Position.Coordinates = {0000804300008043000000000000803F}
      Visible = False
      Width = 512.000000000000000000
      Height = 512.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLPanel1: TGLPanel
      Autosize = False
      RedrawAtOnce = False
      Rotation = 0.000000000000000000
      NoZWrite = False
      DoChangesOnProgress = False
      Visible = False
      Width = 256.000000000000000000
      Height = 256.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000000000000020410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Buffer.ContextOptions = []
    Buffer.DepthTest = False
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    Buffer.ColorDepth = cd8bits
    Left = 324
    Top = 60
  end
  object popElements: TPopupMenu
    Left = 72
    Top = 68
    object mnuTopLeft: TMenuItem
      Caption = 'TopLeft'
      OnClick = MenuItemClick
    end
    object mnuTop: TMenuItem
      Tag = 1
      Caption = 'Top'
      OnClick = MenuItemClick
    end
    object mnuTopRight: TMenuItem
      Tag = 2
      Caption = 'TopRight'
      OnClick = MenuItemClick
    end
    object mnuLeft: TMenuItem
      Tag = 3
      Caption = 'Left'
      OnClick = MenuItemClick
    end
    object mnuCenter: TMenuItem
      Tag = 4
      Caption = 'Center'
      OnClick = MenuItemClick
    end
    object mnuRight: TMenuItem
      Tag = 5
      Caption = 'Right'
      OnClick = MenuItemClick
    end
    object mnuBottomLeft: TMenuItem
      Tag = 6
      Caption = 'BottomLeft'
      OnClick = MenuItemClick
    end
    object mnuBottom: TMenuItem
      Tag = 7
      Caption = 'Bottom'
      OnClick = MenuItemClick
    end
    object mnuBottomRight: TMenuItem
      Tag = 8
      Caption = 'BottomRight'
      OnClick = MenuItemClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuAddAll: TMenuItem
      Caption = 'Add all'
      OnClick = mnuAddAllClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuAllTop: TMenuItem
      Caption = 'All Top'
      OnClick = mnuAllTopClick
    end
    object mnuAllMiddle: TMenuItem
      Caption = 'All Middle'
      OnClick = mnuAllMiddleClick
    end
    object mnuAllBottom: TMenuItem
      Caption = 'All Bottom'
      OnClick = mnuAllBottomClick
    end
  end
end
