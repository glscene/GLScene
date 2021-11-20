object FormShaderLab: TFormShaderLab
  Left = 0
  Top = 0
  Caption = 'Shader Lab'
  ClientHeight = 653
  ClientWidth = 1205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 653
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    TabOrder = 0
    ExplicitLeft = 9
    ExplicitHeight = 638
    object Label54: TLabel
      Left = 185
      Top = 545
      Width = 41
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Object'
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 463
      Height = 536
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = TabSheet3
      Align = alTop
      MultiLine = True
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Fur'
        object lblFurDistance: TLabel
          Left = 4
          Top = 66
          Width = 71
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Fur Length '
        end
        object Label10: TLabel
          Left = 4
          Top = 256
          Width = 65
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Blend Dest'
        end
        object Label11: TLabel
          Left = 4
          Top = 224
          Width = 57
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Blend Src'
        end
        object lblFurPassCount1: TLabel
          Left = 4
          Top = 35
          Width = 73
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Pass Count '
        end
        object lblFurLength: TLabel
          Left = 359
          Top = 66
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.3'
        end
        object Label7: TLabel
          Left = 4
          Top = 94
          Width = 76
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Max Length '
        end
        object lblFurMaxLength: TLabel
          Left = 359
          Top = 95
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '3.0'
        end
        object lblFurPassCount: TLabel
          Left = 359
          Top = 35
          Width = 16
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '16'
        end
        object Label12: TLabel
          Left = 4
          Top = 121
          Width = 46
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Density'
        end
        object lblFurDensity: TLabel
          Left = 359
          Top = 121
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label6: TLabel
          Left = 4
          Top = 291
          Width = 72
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Light Power'
        end
        object lblFurLightPower: TLabel
          Left = 359
          Top = 291
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '2.5'
        end
        object Label8: TLabel
          Left = 4
          Top = 329
          Width = 71
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Color Scale '
        end
        object Label9: TLabel
          Left = 4
          Top = 356
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape1: TShape
          Left = 104
          Top = 328
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnMouseDown = Shape1MouseDown
        end
        object Shape2: TShape
          Left = 104
          Top = 354
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnMouseDown = Shape2MouseDown
        end
        object Label63: TLabel
          Left = 3
          Top = 161
          Width = 76
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Gravity XYZ '
        end
        object chkFurShader: TCheckBox
          Left = 4
          Top = 4
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkFurShaderClick
        end
        object tbFurLength: TTrackBar
          Left = 81
          Top = 61
          Width = 270
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbFurLengthChange
        end
        object cbxFurBlendSrc: TComboBox
          Left = 90
          Top = 220
          Width = 158
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemIndex = 3
          TabOrder = 2
          Text = 'ONE MINUS SRC COLOR'
          OnChange = cbxFurBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxFurBlendDest: TComboBox
          Left = 90
          Top = 254
          Width = 158
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemIndex = 7
          TabOrder = 3
          Text = 'MINUS SRC ALPHA'
          OnChange = cbxFurBlendDestChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object tbFurPassCount: TTrackBar
          Left = 81
          Top = 30
          Width = 270
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 8
          Position = 16
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbFurPassCountChange
        end
        object tbFurMaxLength: TTrackBar
          Left = 81
          Top = 90
          Width = 270
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 300
          TabOrder = 5
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbFurMaxLengthChange
        end
        object chkFurRandomLength: TCheckBox
          Left = 163
          Top = 4
          Width = 181
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Random Fur Length'
          TabOrder = 6
          OnClick = chkFurRandomLengthClick
        end
        object tbFurDensity: TTrackBar
          Left = 81
          Top = 118
          Width = 270
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbFurDensityChange
        end
        object tbFurLightPower: TTrackBar
          Left = 81
          Top = 288
          Width = 270
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 1000
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 250
          TabOrder = 8
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbFurLightPowerChange
        end
        object Button8: TButton
          Left = 4
          Top = 394
          Width = 156
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Main Texture'
          TabOrder = 9
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 223
          Top = 394
          Width = 156
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Noise Texture'
          TabOrder = 10
          OnClick = Button9Click
        end
        object edtFurGravityX: TEdit
          Left = 89
          Top = 155
          Width = 71
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 11
          Text = '0.0'
          OnChange = edtFurGravityXChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityY: TEdit
          Left = 176
          Top = 155
          Width = 72
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 12
          Text = '-2.0'
          OnChange = edtFurGravityYChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityZ: TEdit
          Left = 270
          Top = 155
          Width = 71
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 13
          Text = '0.0'
          OnChange = edtFurGravityZChange
          OnKeyPress = EditFloatKeyPress
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Lattice'
        ImageIndex = 1
        object lblLatticeScaleX: TLabel
          Left = 364
          Top = 51
          Width = 16
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '10'
        end
        object lblLatticeThresholdX: TLabel
          Left = 361
          Top = 116
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.15'
        end
        object lblLatticeScaleY: TLabel
          Left = 363
          Top = 84
          Width = 16
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '40'
        end
        object Label4: TLabel
          Left = 20
          Top = 184
          Width = 94
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Power'
        end
        object Label5: TLabel
          Left = 20
          Top = 215
          Width = 72
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Light Power'
        end
        object lblLatticeThresholdY: TLabel
          Left = 363
          Top = 148
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.30'
        end
        object lblLatticeSpecularPower: TLabel
          Left = 363
          Top = 184
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '8.0'
        end
        object lblLatticeLightPower: TLabel
          Left = 363
          Top = 216
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label23: TLabel
          Left = 13
          Top = 51
          Width = 43
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Scale X'
        end
        object Label29: TLabel
          Left = 13
          Top = 84
          Width = 43
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Scale Y'
        end
        object Label31: TLabel
          Left = 13
          Top = 116
          Width = 72
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Threshold X'
        end
        object Label33: TLabel
          Left = 13
          Top = 153
          Width = 72
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Threshold Y'
        end
        object Label35: TLabel
          Left = 13
          Top = 250
          Width = 77
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse Color'
        end
        object Shape10: TShape
          Left = 113
          Top = 248
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnMouseDown = Shape10MouseDown
        end
        object Label38: TLabel
          Left = 13
          Top = 279
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape11: TShape
          Left = 113
          Top = 278
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 1381653
          OnMouseDown = Shape11MouseDown
        end
        object Label39: TLabel
          Left = 13
          Top = 305
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Color'
        end
        object Shape12: TShape
          Left = 113
          Top = 305
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnMouseDown = Shape12MouseDown
        end
        object tbLatticeScaleX: TTrackBar
          Left = 91
          Top = 46
          Width = 269
          Height = 38
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeScaleXChange
        end
        object tbLatticeThresholdX: TTrackBar
          Left = 91
          Top = 111
          Width = 269
          Height = 34
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 15
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeThresholdXChange
        end
        object chkLatticeShader: TCheckBox
          Left = 20
          Top = 15
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = chkLatticeShaderClick
        end
        object tbLatticeScaleY: TTrackBar
          Left = 91
          Top = 79
          Width = 269
          Height = 40
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 40
          TabOrder = 3
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeScaleYChange
        end
        object tbLatticeThresholdY: TTrackBar
          Left = 91
          Top = 143
          Width = 264
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeThresholdYChange
        end
        object tbLatticeSpecularPower: TTrackBar
          Left = 120
          Top = 179
          Width = 235
          Height = 40
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 800
          TabOrder = 5
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeSpecularPowerChange
        end
        object tbLatticeLightPower: TTrackBar
          Left = 120
          Top = 208
          Width = 235
          Height = 40
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 6
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbLatticeLightPowerChange
        end
        object Button7: TButton
          Left = 20
          Top = 344
          Width = 230
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Main Texture'
          TabOrder = 7
          OnClick = Button3Click
        end
      end
      object TabSheet3: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Erosion'
        ImageIndex = 2
        object Label1: TLabel
          Left = 10
          Top = 58
          Width = 85
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Erosion factor'
        end
        object lblErosionFactor: TLabel
          Left = 369
          Top = 59
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.35'
        end
        object Label3: TLabel
          Left = 10
          Top = 99
          Width = 80
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Erosion Scale'
        end
        object lblErosionScale: TLabel
          Left = 369
          Top = 99
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.03'
        end
        object Label25: TLabel
          Left = 10
          Top = 139
          Width = 105
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Intensity factor 1'
        end
        object lblErosionIFactor1: TLabel
          Left = 369
          Top = 139
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.75'
        end
        object Label28: TLabel
          Left = 10
          Top = 178
          Width = 108
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Intensity Factor 2'
        end
        object lblerosionIFactor2: TLabel
          Left = 369
          Top = 179
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.95'
        end
        object Label2: TLabel
          Left = 10
          Top = 250
          Width = 90
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient factor'
        end
        object lblErosionAmbientF: TLabel
          Left = 369
          Top = 250
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.80'
        end
        object Label27: TLabel
          Left = 10
          Top = 215
          Width = 81
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse factor'
        end
        object lblErosionDiffuseF: TLabel
          Left = 369
          Top = 216
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label30: TLabel
          Left = 10
          Top = 285
          Width = 92
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular factor'
        end
        object lblErosionSpecularF: TLabel
          Left = 369
          Top = 285
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label32: TLabel
          Left = 10
          Top = 325
          Width = 124
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Roughness'
        end
        object lblErosionSpecularR: TLabel
          Left = 371
          Top = 325
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.45'
        end
        object Label34: TLabel
          Left = 10
          Top = 365
          Width = 139
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Anisotropic Roughness'
        end
        object lblErosionAnisoR: TLabel
          Left = 371
          Top = 365
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.35'
        end
        object Label36: TLabel
          Left = 8
          Top = 409
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape8: TShape
          Left = 108
          Top = 406
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 2105376
          OnMouseDown = Shape8MouseDown
        end
        object Shape9: TShape
          Left = 108
          Top = 433
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 14540253
          OnMouseDown = Shape9MouseDown
        end
        object Label37: TLabel
          Left = 8
          Top = 433
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Color'
        end
        object chkErosionShader: TCheckBox
          Left = 30
          Top = 25
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkErosionShaderClick
        end
        object tbErosionFactor: TTrackBar
          Left = 123
          Top = 54
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionFactorChange
        end
        object tberosionScale: TTrackBar
          Left = 123
          Top = 94
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 3
          TabOrder = 2
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tberosionScaleChange
        end
        object tbErosionIFactor1: TTrackBar
          Left = 123
          Top = 134
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 75
          TabOrder = 3
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionIFactor1Change
        end
        object tbErosionIFactor2: TTrackBar
          Left = 125
          Top = 174
          Width = 236
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 195
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionIFactor2Change
        end
        object tbErosionAmbientF: TTrackBar
          Left = 123
          Top = 243
          Width = 238
          Height = 35
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 5
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionAmbientFChange
        end
        object tbErosionDiffuseF: TTrackBar
          Left = 124
          Top = 211
          Width = 237
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 6
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionDiffuseFChange
        end
        object tbErosionSpecularF: TTrackBar
          Left = 123
          Top = 280
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 7
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionSpecularFChange
        end
        object tbErosionSpecularR: TTrackBar
          Left = 139
          Top = 320
          Width = 225
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 45
          TabOrder = 8
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionSpecularRChange
        end
        object tbErosionAnisoR: TTrackBar
          Left = 154
          Top = 360
          Width = 207
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 9
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbErosionAnisoRChange
        end
        object Button5: TButton
          Left = 196
          Top = 433
          Width = 214
          Height = 23
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Main Texture'
          TabOrder = 10
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 4
          Top = 498
          Width = 230
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Erosion Texture'
          TabOrder = 11
          OnClick = Button6Click
        end
      end
      object TabSheet4: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Ivory'
        ImageIndex = 3
        object chkIvoryShader: TCheckBox
          Left = 20
          Top = 20
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkIvoryShaderClick
        end
      end
      object TabSheet5: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Gootch'
        ImageIndex = 4
        object Label13: TLabel
          Left = 20
          Top = 59
          Width = 77
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse Color'
        end
        object Shape3: TShape
          Left = 120
          Top = 58
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clSilver
          OnMouseDown = Shape3MouseDown
        end
        object Label14: TLabel
          Left = 20
          Top = 84
          Width = 74
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Warm Color'
        end
        object Shape4: TShape
          Left = 120
          Top = 84
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clFuchsia
          OnMouseDown = Shape4MouseDown
        end
        object Label15: TLabel
          Left = 20
          Top = 110
          Width = 63
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Cool Color'
        end
        object Shape5: TShape
          Left = 120
          Top = 110
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 1145343
          OnMouseDown = Shape5MouseDown
        end
        object Label16: TLabel
          Left = 20
          Top = 138
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape6: TShape
          Left = 120
          Top = 136
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 3158064
          OnMouseDown = Shape6MouseDown
        end
        object Label17: TLabel
          Left = 20
          Top = 164
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Color'
        end
        object Shape7: TShape
          Left = 120
          Top = 163
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnMouseDown = Shape7MouseDown
        end
        object Label18: TLabel
          Left = 20
          Top = 204
          Width = 81
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse factor'
        end
        object lblGoochDFactor: TLabel
          Left = 379
          Top = 205
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.80'
        end
        object Label20: TLabel
          Left = 20
          Top = 244
          Width = 78
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Warm factor'
        end
        object lblGoochWFactor: TLabel
          Left = 379
          Top = 245
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.55'
        end
        object Label22: TLabel
          Left = 20
          Top = 284
          Width = 67
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Cool factor'
        end
        object lblGoochCFactor: TLabel
          Left = 379
          Top = 285
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.30'
        end
        object Label24: TLabel
          Left = 20
          Top = 324
          Width = 90
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient factor'
        end
        object lblGoochAFactor: TLabel
          Left = 379
          Top = 325
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label26: TLabel
          Left = 20
          Top = 365
          Width = 92
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular factor'
        end
        object lblGoochSFactor: TLabel
          Left = 379
          Top = 365
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label40: TLabel
          Left = 20
          Top = 404
          Width = 70
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Blend Mode'
        end
        object Label41: TLabel
          Left = 205
          Top = 59
          Width = 33
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Alpha'
        end
        object lblGoochAlpha: TLabel
          Left = 386
          Top = 59
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object chkGoochShader: TCheckBox
          Left = 20
          Top = 20
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkGoochShaderClick
        end
        object tbGoochDFactor: TTrackBar
          Left = 120
          Top = 200
          Width = 251
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochDFactorChange
        end
        object tbGoochWFactor: TTrackBar
          Left = 120
          Top = 240
          Width = 251
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 55
          TabOrder = 2
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochWFactorChange
        end
        object tbGoochCFactor: TTrackBar
          Left = 120
          Top = 280
          Width = 251
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 3
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochCFactorChange
        end
        object tbGoochAFactor: TTrackBar
          Left = 120
          Top = 320
          Width = 251
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochAFactorChange
        end
        object tbGoochSFactor: TTrackBar
          Left = 120
          Top = 360
          Width = 251
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 5
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochSFactorChange
        end
        object cbxGootchBlendMode: TComboBox
          Left = 130
          Top = 400
          Width = 241
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 6
          Text = 'Opaque'
          OnChange = cbxGootchBlendModeChange
          Items.Strings = (
            'Opaque'
            'Transparency'
            'Additive'
            'AlphaTest50'
            'AlphaTest100'
            'Modulate'
            'DestColorOne'
            'DestAlphaOne')
        end
        object tbGoochAlpha: TTrackBar
          Left = 245
          Top = 55
          Width = 143
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGoochAlphaChange
        end
      end
      object TabSheet6: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'S.E.M'
        ImageIndex = 5
        object Label19: TLabel
          Left = 20
          Top = 55
          Width = 81
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse factor'
        end
        object lblSemDiffuseF: TLabel
          Left = 379
          Top = 56
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label42: TLabel
          Left = 20
          Top = 90
          Width = 90
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient factor'
        end
        object lblSemAmbientF: TLabel
          Left = 379
          Top = 89
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.80'
        end
        object Label44: TLabel
          Left = 20
          Top = 125
          Width = 92
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular factor'
        end
        object lblSemSpecularF: TLabel
          Left = 379
          Top = 125
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label46: TLabel
          Left = 18
          Top = 163
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape13: TShape
          Left = 118
          Top = 160
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label47: TLabel
          Left = 18
          Top = 186
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Color'
        end
        object Shape14: TShape
          Left = 118
          Top = 186
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object tbSemDiffuseF: TTrackBar
          Left = 134
          Top = 49
          Width = 237
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 0
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbSemDiffuseFChange
        end
        object tbSemAmbientF: TTrackBar
          Left = 133
          Top = 83
          Width = 238
          Height = 35
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbSemAmbientFChange
        end
        object tbSemSpecularF: TTrackBar
          Left = 133
          Top = 120
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 2
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbSemSpecularFChange
        end
        object chkSEMShader: TCheckBox
          Left = 30
          Top = 20
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkSEMShaderClick
        end
        object Button4: TButton
          Left = 20
          Top = 224
          Width = 230
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load MatCap Texture'
          TabOrder = 4
          OnClick = Button4Click
        end
      end
      object Displacement: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Displacement'
        ImageIndex = 6
        object Label21: TLabel
          Left = 30
          Top = 65
          Width = 81
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse factor'
        end
        object Label43: TLabel
          Left = 30
          Top = 100
          Width = 90
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient factor'
        end
        object Label45: TLabel
          Left = 30
          Top = 135
          Width = 92
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular factor'
        end
        object lblVDSpecularF: TLabel
          Left = 389
          Top = 135
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object lblVDAmbientF: TLabel
          Left = 389
          Top = 99
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.80'
        end
        object lblVDDiffuseF: TLabel
          Left = 389
          Top = 66
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.90'
        end
        object Label51: TLabel
          Left = 28
          Top = 173
          Width = 86
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Ambient Color'
        end
        object Shape15: TShape
          Left = 128
          Top = 170
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label52: TLabel
          Left = 28
          Top = 196
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Specular Color'
        end
        object Shape16: TShape
          Left = 128
          Top = 196
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object Label48: TLabel
          Left = 28
          Top = 235
          Width = 32
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Noise'
        end
        object lblVDNoise: TLabel
          Left = 389
          Top = 235
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '10.0'
        end
        object Label49: TLabel
          Left = 28
          Top = 264
          Width = 38
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Period'
        end
        object lblVDPeriod: TLabel
          Left = 389
          Top = 264
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '5.0'
        end
        object Label53: TLabel
          Left = 28
          Top = 295
          Width = 67
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Noise Scale'
        end
        object lblVDNScale: TLabel
          Left = 389
          Top = 296
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.05'
        end
        object Label55: TLabel
          Left = 28
          Top = 325
          Width = 68
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Turbulence'
        end
        object lblVDTurb: TLabel
          Left = 389
          Top = 326
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.5'
        end
        object Label57: TLabel
          Left = 28
          Top = 355
          Width = 116
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Displacement Scale'
        end
        object lblVDDispScale: TLabel
          Left = 389
          Top = 355
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label50: TLabel
          Left = 28
          Top = 384
          Width = 72
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Time Factor'
        end
        object lblVDTimeF: TLabel
          Left = 389
          Top = 384
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.05'
        end
        object chkVDShader: TCheckBox
          Left = 40
          Top = 30
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkVDShaderClick
        end
        object tbVDDiffuseF: TTrackBar
          Left = 144
          Top = 59
          Width = 237
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDDiffuseFChange
        end
        object tbVDAmbientF: TTrackBar
          Left = 143
          Top = 93
          Width = 238
          Height = 35
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 2
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDAmbientFChange
        end
        object tbVDSpecularF: TTrackBar
          Left = 143
          Top = 130
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 3
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDSpecularFChange
        end
        object chkVDAnimate: TCheckBox
          Left = 160
          Top = 30
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Animate'
          TabOrder = 4
        end
        object tbVDNoise: TTrackBar
          Left = 143
          Top = 230
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 5000
          PageSize = 10
          Frequency = 10
          Position = 1000
          TabOrder = 5
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDNoiseChange
        end
        object tbVDPeriod: TTrackBar
          Left = 143
          Top = 259
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 500
          TabOrder = 6
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDPeriodChange
        end
        object tbVDNScale: TTrackBar
          Left = 143
          Top = 288
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 7
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDNScaleChange
        end
        object tbVDTurb: TTrackBar
          Left = 143
          Top = 320
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 8
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDTurbChange
        end
        object tbVDDispScale: TTrackBar
          Left = 143
          Top = 350
          Width = 238
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 9
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDDispScaleChange
        end
        object tbVDTimeF: TTrackBar
          Left = 143
          Top = 379
          Width = 238
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 10
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbVDTimeFChange
        end
        object Button3: TButton
          Left = 20
          Top = 430
          Width = 141
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Reset Time'
          TabOrder = 11
          OnClick = Button3Click
        end
        object Button1: TButton
          Left = 20
          Top = 469
          Width = 230
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Main Texture'
          TabOrder = 12
          OnClick = Button1Click
        end
      end
      object TabSheet7: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Glass'
        ImageIndex = 7
        object Label56: TLabel
          Left = 14
          Top = 45
          Width = 38
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Depth'
        end
        object Label58: TLabel
          Left = 30
          Top = 83
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Mix'
        end
        object Label59: TLabel
          Left = 18
          Top = 183
          Width = 77
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Diffuse Color'
        end
        object Shape17: TShape
          Left = 109
          Top = 181
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 2105376
          OnMouseDown = Shape17MouseDown
        end
        object lblGlassDepth: TLabel
          Left = 356
          Top = 45
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.1'
        end
        object lblGlassMix: TLabel
          Left = 354
          Top = 83
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label60: TLabel
          Left = 13
          Top = 124
          Width = 33
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Alpha'
        end
        object lblGlassAlpha: TLabel
          Left = 353
          Top = 124
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '1.0'
        end
        object Label61: TLabel
          Left = 23
          Top = 224
          Width = 57
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Blend Src'
        end
        object Label62: TLabel
          Left = 23
          Top = 256
          Width = 65
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Blend Dest'
        end
        object tbGlassDepth: TTrackBar
          Left = 58
          Top = 41
          Width = 293
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGlassDepthChange
        end
        object tbGlassMix: TTrackBar
          Left = 56
          Top = 78
          Width = 295
          Height = 35
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGlassMixChange
        end
        object Button10: TButton
          Left = 4
          Top = 318
          Width = 230
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Load Refraction Texture'
          TabOrder = 2
          OnClick = Button10Click
        end
        object chkGlassShader: TCheckBox
          Left = 30
          Top = 10
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkGlassShaderClick
        end
        object tbGlassAlpha: TTrackBar
          Left = 56
          Top = 120
          Width = 294
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbGlassAlphaChange
        end
        object cbxGlassBlendSrc: TComboBox
          Left = 109
          Top = 220
          Width = 181
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemIndex = 6
          TabOrder = 5
          Text = 'SRC ALPHA'
          OnChange = cbxGlassBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxGlassBlendDst: TComboBox
          Left = 109
          Top = 254
          Width = 181
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemIndex = 8
          TabOrder = 6
          Text = 'DST ALPHA'
          OnChange = cbxGlassBlendDstChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
      end
      object TabSheet8: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Toon'
        ImageIndex = 8
        object Label64: TLabel
          Left = 9
          Top = 64
          Width = 84
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'HighLight Size'
        end
        object lblToonHighlightSize: TLabel
          Left = 363
          Top = 64
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.95'
        end
        object Label66: TLabel
          Left = 9
          Top = 91
          Width = 47
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Mid Size'
        end
        object lblToonMidSize: TLabel
          Left = 363
          Top = 91
          Width = 20
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.5'
        end
        object Label68: TLabel
          Left = 9
          Top = 119
          Width = 76
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Shadow Size'
        end
        object lblToonShadowSize: TLabel
          Left = 363
          Top = 119
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.25'
        end
        object Label70: TLabel
          Left = 9
          Top = 146
          Width = 83
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Outline Width'
        end
        object lblToonOutlineWidth: TLabel
          Left = 363
          Top = 146
          Width = 28
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '0.25'
        end
        object Label72: TLabel
          Left = 10
          Top = 183
          Width = 88
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Highlight Color'
        end
        object Shape18: TShape
          Left = 148
          Top = 183
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 15658734
          OnMouseDown = Shape18MouseDown
        end
        object Label73: TLabel
          Left = 10
          Top = 209
          Width = 56
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Mid Color'
        end
        object Shape19: TShape
          Left = 148
          Top = 209
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 13421772
          OnMouseDown = Shape19MouseDown
        end
        object Label74: TLabel
          Left = 10
          Top = 235
          Width = 134
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Lighten Shadow Color'
        end
        object Shape20: TShape
          Left = 148
          Top = 235
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clGray
          OnMouseDown = Shape20MouseDown
        end
        object Label75: TLabel
          Left = 10
          Top = 261
          Width = 133
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Darken Shadow Color'
        end
        object Shape21: TShape
          Left = 148
          Top = 261
          Width = 80
          Height = 19
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = 3158064
          OnMouseDown = Shape21MouseDown
        end
        object Label76: TLabel
          Left = 9
          Top = 288
          Width = 78
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Outline Color'
        end
        object Shape22: TShape
          Left = 148
          Top = 288
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clBlack
          OnMouseDown = Shape22MouseDown
        end
        object chkToonShader: TCheckBox
          Left = 10
          Top = 20
          Width = 121
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkToonShaderClick
        end
        object tbToonHighlightSize: TTrackBar
          Left = 94
          Top = 60
          Width = 266
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 95
          TabOrder = 1
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbToonHighlightSizeChange
        end
        object tbToonMidSize: TTrackBar
          Left = 94
          Top = 88
          Width = 266
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 2
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbToonMidSizeChange
        end
        object tbToonShadowSize: TTrackBar
          Left = 94
          Top = 115
          Width = 266
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 3
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbToonShadowSizeChange
        end
        object tbToonOutlineWidth: TTrackBar
          Left = 94
          Top = 143
          Width = 266
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 4
          ThumbLength = 25
          TickStyle = tsNone
          OnChange = tbToonOutlineWidthChange
        end
      end
    end
    object chkAnimScene: TCheckBox
      Left = 17
      Top = 569
      Width = 122
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Animate Scene'
      TabOrder = 1
    end
    object chkLightmoving: TCheckBox
      Left = 16
      Top = 598
      Width = 122
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Light moving'
      TabOrder = 2
    end
    object cbxObjects: TComboBox
      Left = 234
      Top = 541
      Width = 181
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Suzanne'
      OnChange = cbxObjectsChange
      Items.Strings = (
        'Suzanne'
        'Knot'
        'Spoutnik'
        'Rectangle Spirale'
        'Geode'
        'Syamil'
        'GLTorus'
        'GLSphere')
    end
    object Button2: TButton
      Left = 186
      Top = 574
      Width = 230
      Height = 35
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Choose Background Color'
      TabOrder = 4
      OnClick = Button2Click
    end
    object chkBackgroundImg: TCheckBox
      Left = 160
      Top = 622
      Width = 18
      Height = 21
      Hint = 'Show Background Texture'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 5
      OnClick = chkBackgroundImgClick
    end
    object Button11: TButton
      Left = 186
      Top = 612
      Width = 230
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load Background Texture'
      Enabled = False
      TabOrder = 6
      OnClick = Button11Click
    end
  end
  object Viewer: TGLSceneViewer
    Left = 465
    Top = 0
    Width = 740
    Height = 653
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa2x
    FieldOfView = 162.586807250976600000
    PenAsTouch = False
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 640
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MainTexture'
        Tag = 0
        Material.BackProperties.Shininess = 10
        Material.FrontProperties.Shininess = 10
        Material.FrontProperties.Specular.Color = {9998983E9998983E9998983E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'NoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ShaderMaterial'
        Tag = 0
        Material.FrontProperties.Emission.Color = {77BE9F3D7368913D2506813D0000803F}
        Material.FrontProperties.Specular.Color = {B6F35D3F6DE75B3F6DE75B3F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.Texture.KeepImageAfterTransfer = True
      end
      item
        Name = 'ErosionNoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionMainTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'MatCapTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ExplosionTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'EnvMap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.Disabled = False
      end
      item
        Name = 'RefractMap'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'BackgroundTex'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 768
    Top = 32
  end
  object GLScene1: TGLScene
    Left = 504
    Top = 32
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.001000000047497451
      TargetObject = World
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000A0400000A0410000C8420000803F}
    end
    object LightCube: TGLDummyCube
      Position.Coordinates = {000096C30000A040000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object LightCube2: TGLDummyCube
      Position.Coordinates = {000096430000C842000096430000803F}
      OnProgress = LightCube2Progress
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {EAEA6A3FEAEA6A3FA7AD2D3F0000803F}
        LightStyle = lsParallel
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object ScreenBackGround: TGLHUDSprite
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'BackgroundTex'
        Visible = False
        Width = 256.000000000000000000
        Height = 256.000000000000000000
        Rotation = 0.000000000000000000
      end
      object Objects: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLSphere1: TGLSphere
          Material.MaterialLibrary = MaterialLibrary
          Material.LibMaterialName = 'ShaderMaterial'
          Visible = False
          Radius = 45.000000000000000000
          Slices = 64
          Stacks = 64
        end
        object FreeForm: TGLFreeForm
          AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
          AutoScaling.Coordinates = {0000484200004842000048420000803F}
        end
        object GLTorus1: TGLTorus
          Visible = False
          MajorRadius = 40.000000000000000000
          MinorRadius = 15.000000000000000000
          Rings = 64
          Sides = 64
          StopAngle = 360.000000000000000000
          Parts = [toSides, toStartDisk, toStopDisk]
        end
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 632
    Top = 32
  end
  object ColorDialog: TColorDialog
    Left = 509
    Top = 137
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.100000023841858000
    RotateTargetSpeed = 0.500000000000000000
    FormCaption = 'Shaders Lab - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 896
    Top = 32
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 632
    Top = 136
  end
end
