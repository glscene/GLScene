object FormShaderLab: TFormShaderLab
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Shader Lab'
  ClientHeight = 914
  ClientWidth = 1701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 168
  TextHeight = 23
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 914
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 0
    object Label54: TLabel
      Left = 259
      Top = 763
      Width = 54
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Object'
    end
    object PageControl: TPageControl
      Left = 1
      Top = 1
      Width = 649
      Height = 751
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = TabSheet3
      Align = alTop
      MultiLine = True
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Fur'
        object lblFurDistance: TLabel
          Left = 5
          Top = 93
          Width = 98
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Fur Length '
        end
        object Label10: TLabel
          Left = 5
          Top = 359
          Width = 90
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Blend Dest'
        end
        object Label11: TLabel
          Left = 5
          Top = 313
          Width = 80
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Blend Src'
        end
        object lblFurPassCount1: TLabel
          Left = 5
          Top = 49
          Width = 97
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Pass Count '
        end
        object lblFurLength: TLabel
          Left = 502
          Top = 93
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.3'
        end
        object Label7: TLabel
          Left = 5
          Top = 131
          Width = 104
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Max Length '
        end
        object lblFurMaxLength: TLabel
          Left = 502
          Top = 133
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '3.0'
        end
        object lblFurPassCount: TLabel
          Left = 502
          Top = 49
          Width = 20
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '16'
        end
        object Label12: TLabel
          Left = 5
          Top = 170
          Width = 61
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Density'
        end
        object lblFurDensity: TLabel
          Left = 502
          Top = 170
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label6: TLabel
          Left = 5
          Top = 408
          Width = 98
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Light Power'
        end
        object lblFurLightPower: TLabel
          Left = 502
          Top = 408
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '2.5'
        end
        object Label8: TLabel
          Left = 5
          Top = 460
          Width = 98
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Color Scale '
        end
        object Label9: TLabel
          Left = 5
          Top = 499
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object Shape1: TShape
          Left = 145
          Top = 459
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Pen.Width = 2
          OnMouseDown = Shape1MouseDown
        end
        object Shape2: TShape
          Left = 145
          Top = 495
          Width = 112
          Height = 27
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Pen.Width = 2
          OnMouseDown = Shape2MouseDown
        end
        object Label63: TLabel
          Left = 4
          Top = 226
          Width = 103
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Gravity XYZ '
        end
        object chkFurShader: TCheckBox
          Left = 5
          Top = 5
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkFurShaderClick
        end
        object tbFurLength: TTrackBar
          Left = 114
          Top = 86
          Width = 378
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbFurLengthChange
        end
        object cbxFurBlendSrc: TComboBox
          Left = 126
          Top = 308
          Width = 221
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
          Left = 126
          Top = 355
          Width = 221
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
          Left = 114
          Top = 42
          Width = 378
          Height = 40
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 8
          Position = 16
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbFurPassCountChange
        end
        object tbFurMaxLength: TTrackBar
          Left = 114
          Top = 126
          Width = 378
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 300
          TabOrder = 5
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbFurMaxLengthChange
        end
        object chkFurRandomLength: TCheckBox
          Left = 228
          Top = 5
          Width = 253
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Random Fur Length'
          TabOrder = 6
          OnClick = chkFurRandomLengthClick
        end
        object tbFurDensity: TTrackBar
          Left = 114
          Top = 165
          Width = 378
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbFurDensityChange
        end
        object tbFurLightPower: TTrackBar
          Left = 114
          Top = 403
          Width = 378
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 1000
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 250
          TabOrder = 8
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbFurLightPowerChange
        end
        object Button8: TButton
          Left = 5
          Top = 551
          Width = 219
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Main Texture'
          TabOrder = 9
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 312
          Top = 551
          Width = 218
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Noise Texture'
          TabOrder = 10
          OnClick = Button9Click
        end
        object edtFurGravityX: TEdit
          Left = 124
          Top = 217
          Width = 100
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 11
          Text = '0.0'
          OnChange = edtFurGravityXChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityY: TEdit
          Left = 247
          Top = 217
          Width = 100
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 12
          Text = '-2.0'
          OnChange = edtFurGravityYChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityZ: TEdit
          Left = 378
          Top = 217
          Width = 100
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          TabOrder = 13
          Text = '0.0'
          OnChange = edtFurGravityZChange
          OnKeyPress = EditFloatKeyPress
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Lattice'
        ImageIndex = 1
        object lblLatticeScaleX: TLabel
          Left = 509
          Top = 72
          Width = 20
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '10'
        end
        object lblLatticeThresholdX: TLabel
          Left = 506
          Top = 163
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.15'
        end
        object lblLatticeScaleY: TLabel
          Left = 508
          Top = 117
          Width = 20
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '40'
        end
        object Label4: TLabel
          Left = 28
          Top = 257
          Width = 130
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Power'
        end
        object Label5: TLabel
          Left = 28
          Top = 301
          Width = 98
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Light Power'
        end
        object lblLatticeThresholdY: TLabel
          Left = 508
          Top = 207
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.30'
        end
        object lblLatticeSpecularPower: TLabel
          Left = 508
          Top = 257
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '8.0'
        end
        object lblLatticeLightPower: TLabel
          Left = 508
          Top = 303
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label23: TLabel
          Left = 18
          Top = 72
          Width = 61
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Scale X'
        end
        object Label29: TLabel
          Left = 18
          Top = 117
          Width = 61
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Scale Y'
        end
        object Label31: TLabel
          Left = 18
          Top = 163
          Width = 100
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Threshold X'
        end
        object Label33: TLabel
          Left = 18
          Top = 214
          Width = 100
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Threshold Y'
        end
        object Label35: TLabel
          Left = 18
          Top = 350
          Width = 106
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse Color'
        end
        object Shape10: TShape
          Left = 158
          Top = 347
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Pen.Width = 2
          OnMouseDown = Shape10MouseDown
        end
        object Label38: TLabel
          Left = 18
          Top = 390
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object Shape11: TShape
          Left = 158
          Top = 389
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 1381653
          Pen.Width = 2
          OnMouseDown = Shape11MouseDown
        end
        object Label39: TLabel
          Left = 18
          Top = 427
          Width = 121
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Color'
        end
        object Shape12: TShape
          Left = 158
          Top = 427
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Pen.Width = 2
          OnMouseDown = Shape12MouseDown
        end
        object tbLatticeScaleX: TTrackBar
          Left = 128
          Top = 65
          Width = 376
          Height = 52
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeScaleXChange
        end
        object tbLatticeThresholdX: TTrackBar
          Left = 128
          Top = 156
          Width = 376
          Height = 47
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 15
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeThresholdXChange
        end
        object chkLatticeShader: TCheckBox
          Left = 28
          Top = 21
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = chkLatticeShaderClick
        end
        object tbLatticeScaleY: TTrackBar
          Left = 128
          Top = 110
          Width = 376
          Height = 56
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 40
          TabOrder = 3
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeScaleYChange
        end
        object tbLatticeThresholdY: TTrackBar
          Left = 128
          Top = 200
          Width = 369
          Height = 47
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeThresholdYChange
        end
        object tbLatticeSpecularPower: TTrackBar
          Left = 168
          Top = 250
          Width = 329
          Height = 56
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 800
          TabOrder = 5
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeSpecularPowerChange
        end
        object tbLatticeLightPower: TTrackBar
          Left = 168
          Top = 291
          Width = 329
          Height = 56
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 6
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbLatticeLightPowerChange
        end
        object Button7: TButton
          Left = 28
          Top = 481
          Width = 322
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Main Texture'
          TabOrder = 7
          OnClick = Button3Click
        end
      end
      object TabSheet3: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Erosion'
        ImageIndex = 2
        object Label1: TLabel
          Left = 14
          Top = 81
          Width = 115
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Erosion factor'
        end
        object lblErosionFactor: TLabel
          Left = 516
          Top = 82
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.35'
        end
        object Label3: TLabel
          Left = 14
          Top = 138
          Width = 111
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Erosion Scale'
        end
        object lblErosionScale: TLabel
          Left = 516
          Top = 138
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.03'
        end
        object Label25: TLabel
          Left = 14
          Top = 194
          Width = 142
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Intensity factor 1'
        end
        object lblErosionIFactor1: TLabel
          Left = 516
          Top = 194
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.75'
        end
        object Label28: TLabel
          Left = 14
          Top = 249
          Width = 146
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Intensity Factor 2'
        end
        object lblerosionIFactor2: TLabel
          Left = 516
          Top = 250
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.95'
        end
        object Label2: TLabel
          Left = 14
          Top = 350
          Width = 123
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient factor'
        end
        object lblErosionAmbientF: TLabel
          Left = 516
          Top = 350
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.80'
        end
        object Label27: TLabel
          Left = 14
          Top = 301
          Width = 112
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse factor'
        end
        object lblErosionDiffuseF: TLabel
          Left = 516
          Top = 303
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label30: TLabel
          Left = 14
          Top = 399
          Width = 127
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular factor'
        end
        object lblErosionSpecularF: TLabel
          Left = 516
          Top = 399
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label32: TLabel
          Left = 14
          Top = 455
          Width = 171
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Roughness'
        end
        object lblErosionSpecularR: TLabel
          Left = 520
          Top = 455
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.45'
        end
        object Label34: TLabel
          Left = 14
          Top = 511
          Width = 189
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Anisotropic Roughness'
        end
        object lblErosionAnisoR: TLabel
          Left = 520
          Top = 511
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.35'
        end
        object Label36: TLabel
          Left = 11
          Top = 572
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object shAmbientErosion: TShape
          Left = 151
          Top = 569
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 2105376
          Pen.Width = 2
          OnMouseDown = shAmbientErosionMouseDown
        end
        object shSpecularErosion: TShape
          Left = 151
          Top = 606
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 14540253
          Pen.Width = 2
          OnMouseDown = shSpecularErosionMouseDown
        end
        object Label37: TLabel
          Left = 11
          Top = 606
          Width = 121
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Color'
        end
        object chkErosionShader: TCheckBox
          Left = 42
          Top = 35
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkErosionShaderClick
        end
        object tbErosionFactor: TTrackBar
          Left = 172
          Top = 75
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionFactorChange
        end
        object tberosionScale: TTrackBar
          Left = 172
          Top = 131
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 3
          TabOrder = 2
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tberosionScaleChange
        end
        object tbErosionIFactor1: TTrackBar
          Left = 172
          Top = 187
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 75
          TabOrder = 3
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionIFactor1Change
        end
        object tbErosionIFactor2: TTrackBar
          Left = 175
          Top = 243
          Width = 331
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 195
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionIFactor2Change
        end
        object tbErosionAmbientF: TTrackBar
          Left = 172
          Top = 340
          Width = 334
          Height = 49
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 5
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionAmbientFChange
        end
        object tbErosionDiffuseF: TTrackBar
          Left = 173
          Top = 296
          Width = 333
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 6
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionDiffuseFChange
        end
        object tbErosionSpecularF: TTrackBar
          Left = 172
          Top = 392
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 7
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionSpecularFChange
        end
        object tbErosionSpecularR: TTrackBar
          Left = 194
          Top = 448
          Width = 315
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 45
          TabOrder = 8
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionSpecularRChange
        end
        object tbErosionAnisoR: TTrackBar
          Left = 215
          Top = 504
          Width = 291
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 9
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbErosionAnisoRChange
        end
        object Button5: TButton
          Left = 275
          Top = 606
          Width = 299
          Height = 33
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Main Texture'
          TabOrder = 10
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 5
          Top = 697
          Width = 322
          Height = 43
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Erosion Texture'
          TabOrder = 11
          OnClick = Button6Click
        end
      end
      object TabSheet4: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Ivory'
        ImageIndex = 3
        object chkIvoryShader: TCheckBox
          Left = 28
          Top = 28
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkIvoryShaderClick
        end
      end
      object TabSheet5: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Gootch'
        ImageIndex = 4
        object Label13: TLabel
          Left = 28
          Top = 82
          Width = 106
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse Color'
        end
        object Shape3: TShape
          Left = 168
          Top = 81
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = clSilver
          Pen.Width = 2
          OnMouseDown = Shape3MouseDown
        end
        object Label14: TLabel
          Left = 28
          Top = 117
          Width = 98
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Warm Color'
        end
        object Shape4: TShape
          Left = 168
          Top = 117
          Width = 112
          Height = 27
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = clFuchsia
          Pen.Width = 2
          OnMouseDown = Shape4MouseDown
        end
        object Label15: TLabel
          Left = 28
          Top = 154
          Width = 83
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Cool Color'
        end
        object Shape5: TShape
          Left = 168
          Top = 154
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 1145343
          Pen.Width = 2
          OnMouseDown = Shape5MouseDown
        end
        object Label16: TLabel
          Left = 28
          Top = 193
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object Shape6: TShape
          Left = 168
          Top = 191
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 3158064
          Pen.Width = 2
          OnMouseDown = Shape6MouseDown
        end
        object Label17: TLabel
          Left = 28
          Top = 229
          Width = 121
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Color'
        end
        object Shape7: TShape
          Left = 168
          Top = 228
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Pen.Width = 2
          OnMouseDown = Shape7MouseDown
        end
        object Label18: TLabel
          Left = 28
          Top = 285
          Width = 112
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse factor'
        end
        object lblGoochDFactor: TLabel
          Left = 530
          Top = 287
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.80'
        end
        object Label20: TLabel
          Left = 28
          Top = 341
          Width = 104
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Warm factor'
        end
        object lblGoochWFactor: TLabel
          Left = 530
          Top = 343
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.55'
        end
        object Label22: TLabel
          Left = 28
          Top = 397
          Width = 89
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Cool factor'
        end
        object lblGoochCFactor: TLabel
          Left = 530
          Top = 399
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.30'
        end
        object Label24: TLabel
          Left = 28
          Top = 453
          Width = 123
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient factor'
        end
        object lblGoochAFactor: TLabel
          Left = 530
          Top = 455
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label26: TLabel
          Left = 28
          Top = 511
          Width = 127
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular factor'
        end
        object lblGoochSFactor: TLabel
          Left = 530
          Top = 511
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label40: TLabel
          Left = 28
          Top = 565
          Width = 99
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Blend Mode'
        end
        object Label41: TLabel
          Left = 287
          Top = 82
          Width = 47
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Alpha'
        end
        object lblGoochAlpha: TLabel
          Left = 541
          Top = 82
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object chkGoochShader: TCheckBox
          Left = 28
          Top = 28
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkGoochShaderClick
        end
        object tbGoochDFactor: TTrackBar
          Left = 168
          Top = 280
          Width = 352
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochDFactorChange
        end
        object tbGoochWFactor: TTrackBar
          Left = 168
          Top = 336
          Width = 352
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 55
          TabOrder = 2
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochWFactorChange
        end
        object tbGoochCFactor: TTrackBar
          Left = 168
          Top = 392
          Width = 352
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 3
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochCFactorChange
        end
        object tbGoochAFactor: TTrackBar
          Left = 168
          Top = 448
          Width = 352
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochAFactorChange
        end
        object tbGoochSFactor: TTrackBar
          Left = 168
          Top = 504
          Width = 352
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 5
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochSFactorChange
        end
        object cbxGootchBlendMode: TComboBox
          Left = 182
          Top = 560
          Width = 338
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
          Left = 343
          Top = 77
          Width = 200
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGoochAlphaChange
        end
      end
      object TabSheet6: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'S.E.M'
        ImageIndex = 5
        object Label19: TLabel
          Left = 28
          Top = 77
          Width = 112
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse factor'
        end
        object lblSemDiffuseF: TLabel
          Left = 530
          Top = 79
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label42: TLabel
          Left = 28
          Top = 126
          Width = 123
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient factor'
        end
        object lblSemAmbientF: TLabel
          Left = 530
          Top = 124
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.80'
        end
        object Label44: TLabel
          Left = 28
          Top = 175
          Width = 127
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular factor'
        end
        object lblSemSpecularF: TLabel
          Left = 530
          Top = 175
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label46: TLabel
          Left = 25
          Top = 228
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object Shape13: TShape
          Left = 165
          Top = 224
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 2105376
          Pen.Width = 2
          OnMouseDown = Shape13MouseDown
        end
        object Label47: TLabel
          Left = 25
          Top = 261
          Width = 121
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Color'
        end
        object Shape14: TShape
          Left = 165
          Top = 261
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 14540253
          Pen.Width = 2
          OnMouseDown = Shape14MouseDown
        end
        object tbSemDiffuseF: TTrackBar
          Left = 187
          Top = 68
          Width = 333
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 0
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbSemDiffuseFChange
        end
        object tbSemAmbientF: TTrackBar
          Left = 186
          Top = 116
          Width = 334
          Height = 49
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbSemAmbientFChange
        end
        object tbSemSpecularF: TTrackBar
          Left = 186
          Top = 168
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 2
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbSemSpecularFChange
        end
        object chkSEMShader: TCheckBox
          Left = 42
          Top = 28
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkSEMShaderClick
        end
        object Button4: TButton
          Left = 28
          Top = 313
          Width = 322
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load MatCap Texture'
          TabOrder = 4
          OnClick = Button4Click
        end
      end
      object Displacement: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Displacement'
        ImageIndex = 6
        object Label21: TLabel
          Left = 42
          Top = 91
          Width = 112
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse factor'
        end
        object Label43: TLabel
          Left = 42
          Top = 140
          Width = 123
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient factor'
        end
        object Label45: TLabel
          Left = 42
          Top = 189
          Width = 127
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular factor'
        end
        object lblVDSpecularF: TLabel
          Left = 544
          Top = 189
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object lblVDAmbientF: TLabel
          Left = 544
          Top = 138
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.80'
        end
        object lblVDDiffuseF: TLabel
          Left = 544
          Top = 93
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.90'
        end
        object Label51: TLabel
          Left = 39
          Top = 242
          Width = 117
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Ambient Color'
        end
        object Shape15: TShape
          Left = 179
          Top = 238
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 2105376
          Pen.Width = 2
          OnMouseDown = Shape13MouseDown
        end
        object Label52: TLabel
          Left = 39
          Top = 275
          Width = 121
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Specular Color'
        end
        object Shape16: TShape
          Left = 179
          Top = 275
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 14540253
          Pen.Width = 2
          OnMouseDown = Shape14MouseDown
        end
        object Label48: TLabel
          Left = 39
          Top = 329
          Width = 45
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Noise'
        end
        object lblVDNoise: TLabel
          Left = 544
          Top = 329
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '10.0'
        end
        object Label49: TLabel
          Left = 39
          Top = 369
          Width = 52
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Period'
        end
        object lblVDPeriod: TLabel
          Left = 544
          Top = 369
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '5.0'
        end
        object Label53: TLabel
          Left = 39
          Top = 413
          Width = 95
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Noise Scale'
        end
        object lblVDNScale: TLabel
          Left = 544
          Top = 415
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.05'
        end
        object Label55: TLabel
          Left = 39
          Top = 455
          Width = 95
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Turbulence'
        end
        object lblVDTurb: TLabel
          Left = 544
          Top = 457
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.5'
        end
        object Label57: TLabel
          Left = 39
          Top = 497
          Width = 162
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Displacement Scale'
        end
        object lblVDDispScale: TLabel
          Left = 544
          Top = 497
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label50: TLabel
          Left = 39
          Top = 537
          Width = 99
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Time Factor'
        end
        object lblVDTimeF: TLabel
          Left = 544
          Top = 537
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.05'
        end
        object chkVDShader: TCheckBox
          Left = 56
          Top = 42
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkVDShaderClick
        end
        object tbVDDiffuseF: TTrackBar
          Left = 201
          Top = 82
          Width = 333
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDDiffuseFChange
        end
        object tbVDAmbientF: TTrackBar
          Left = 200
          Top = 130
          Width = 334
          Height = 49
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 2
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDAmbientFChange
        end
        object tbVDSpecularF: TTrackBar
          Left = 200
          Top = 182
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 3
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDSpecularFChange
        end
        object chkVDAnimate: TCheckBox
          Left = 224
          Top = 42
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Animate'
          TabOrder = 4
        end
        object tbVDNoise: TTrackBar
          Left = 200
          Top = 322
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 5000
          PageSize = 10
          Frequency = 10
          Position = 1000
          TabOrder = 5
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDNoiseChange
        end
        object tbVDPeriod: TTrackBar
          Left = 200
          Top = 362
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 500
          TabOrder = 6
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDPeriodChange
        end
        object tbVDNScale: TTrackBar
          Left = 200
          Top = 403
          Width = 334
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 7
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDNScaleChange
        end
        object tbVDTurb: TTrackBar
          Left = 200
          Top = 448
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 8
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDTurbChange
        end
        object tbVDDispScale: TTrackBar
          Left = 200
          Top = 490
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 9
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDDispScaleChange
        end
        object tbVDTimeF: TTrackBar
          Left = 200
          Top = 530
          Width = 334
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 10
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbVDTimeFChange
        end
        object Button3: TButton
          Left = 28
          Top = 602
          Width = 198
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Reset Time'
          TabOrder = 11
          OnClick = Button3Click
        end
        object Button1: TButton
          Left = 28
          Top = 656
          Width = 322
          Height = 44
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Main Texture'
          TabOrder = 12
          OnClick = Button1Click
        end
      end
      object TabSheet7: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Glass'
        ImageIndex = 7
        object Label56: TLabel
          Left = 19
          Top = 63
          Width = 51
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Depth'
        end
        object Label58: TLabel
          Left = 42
          Top = 116
          Width = 28
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Mix'
        end
        object Label59: TLabel
          Left = 25
          Top = 256
          Width = 106
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Diffuse Color'
        end
        object Shape17: TShape
          Left = 152
          Top = 254
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 2105376
          Pen.Width = 2
          OnMouseDown = Shape17MouseDown
        end
        object lblGlassDepth: TLabel
          Left = 499
          Top = 63
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.1'
        end
        object lblGlassMix: TLabel
          Left = 495
          Top = 116
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label60: TLabel
          Left = 18
          Top = 173
          Width = 47
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Alpha'
        end
        object lblGlassAlpha: TLabel
          Left = 494
          Top = 173
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '1.0'
        end
        object Label61: TLabel
          Left = 32
          Top = 313
          Width = 80
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Blend Src'
        end
        object Label62: TLabel
          Left = 32
          Top = 359
          Width = 90
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Blend Dest'
        end
        object tbGlassDepth: TTrackBar
          Left = 81
          Top = 58
          Width = 411
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGlassDepthChange
        end
        object tbGlassMix: TTrackBar
          Left = 79
          Top = 109
          Width = 413
          Height = 49
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGlassMixChange
        end
        object Button10: TButton
          Left = 5
          Top = 445
          Width = 322
          Height = 43
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Load Refraction Texture'
          TabOrder = 2
          OnClick = Button10Click
        end
        object chkGlassShader: TCheckBox
          Left = 42
          Top = 14
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkGlassShaderClick
        end
        object tbGlassAlpha: TTrackBar
          Left = 79
          Top = 168
          Width = 411
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbGlassAlphaChange
        end
        object cbxGlassBlendSrc: TComboBox
          Left = 152
          Top = 308
          Width = 254
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
          Left = 152
          Top = 355
          Width = 254
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
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
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Toon'
        ImageIndex = 8
        object Label64: TLabel
          Left = 12
          Top = 89
          Width = 119
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'HighLight Size'
        end
        object lblToonHighlightSize: TLabel
          Left = 508
          Top = 89
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.95'
        end
        object Label66: TLabel
          Left = 12
          Top = 128
          Width = 69
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Mid Size'
        end
        object lblToonMidSize: TLabel
          Left = 508
          Top = 128
          Width = 26
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.5'
        end
        object Label68: TLabel
          Left = 12
          Top = 166
          Width = 106
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Shadow Size'
        end
        object lblToonShadowSize: TLabel
          Left = 508
          Top = 166
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.25'
        end
        object Label70: TLabel
          Left = 12
          Top = 205
          Width = 114
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Outline Width'
        end
        object lblToonOutlineWidth: TLabel
          Left = 508
          Top = 205
          Width = 36
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = '0.25'
        end
        object Label72: TLabel
          Left = 14
          Top = 256
          Width = 123
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Highlight Color'
        end
        object Shape18: TShape
          Left = 207
          Top = 256
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 15658734
          Pen.Width = 2
          OnMouseDown = Shape18MouseDown
        end
        object Label73: TLabel
          Left = 14
          Top = 292
          Width = 78
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Mid Color'
        end
        object Shape19: TShape
          Left = 207
          Top = 292
          Width = 112
          Height = 27
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 13421772
          Pen.Width = 2
          OnMouseDown = Shape19MouseDown
        end
        object Label74: TLabel
          Left = 14
          Top = 329
          Width = 183
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Lighten Shadow Color'
        end
        object Shape20: TShape
          Left = 207
          Top = 329
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = clGray
          Pen.Width = 2
          OnMouseDown = Shape20MouseDown
        end
        object Label75: TLabel
          Left = 14
          Top = 366
          Width = 181
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Darken Shadow Color'
        end
        object Shape21: TShape
          Left = 207
          Top = 366
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = 3158064
          Pen.Width = 2
          OnMouseDown = Shape21MouseDown
        end
        object Label76: TLabel
          Left = 12
          Top = 403
          Width = 107
          Height = 23
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Outline Color'
        end
        object Shape22: TShape
          Left = 207
          Top = 403
          Width = 112
          Height = 26
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Brush.Color = clBlack
          Pen.Width = 2
          OnMouseDown = Shape22MouseDown
        end
        object chkToonShader: TCheckBox
          Left = 14
          Top = 28
          Width = 170
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkToonShaderClick
        end
        object tbToonHighlightSize: TTrackBar
          Left = 131
          Top = 84
          Width = 373
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 95
          TabOrder = 1
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbToonHighlightSizeChange
        end
        object tbToonMidSize: TTrackBar
          Left = 131
          Top = 123
          Width = 373
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 2
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbToonMidSizeChange
        end
        object tbToonShadowSize: TTrackBar
          Left = 131
          Top = 161
          Width = 373
          Height = 46
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 3
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbToonShadowSizeChange
        end
        object tbToonOutlineWidth: TTrackBar
          Left = 131
          Top = 200
          Width = 373
          Height = 45
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 4
          ThumbLength = 35
          TickStyle = tsNone
          OnChange = tbToonOutlineWidthChange
        end
      end
    end
    object chkAnimScene: TCheckBox
      Left = 25
      Top = 796
      Width = 169
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Animate Scene'
      TabOrder = 1
    end
    object chkLightmoving: TCheckBox
      Left = 23
      Top = 837
      Width = 170
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Light moving'
      TabOrder = 2
    end
    object cbxObjects: TComboBox
      Left = 327
      Top = 758
      Width = 254
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 261
      Top = 803
      Width = 322
      Height = 49
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Choose Background Color'
      TabOrder = 4
      OnClick = Button2Click
    end
    object chkBackgroundImg: TCheckBox
      Left = 224
      Top = 872
      Width = 25
      Height = 28
      Hint = 'Show Background Texture'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 5
      OnClick = chkBackgroundImgClick
    end
    object Button11: TButton
      Left = 261
      Top = 858
      Width = 322
      Height = 42
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load Background Texture'
      Enabled = False
      TabOrder = 6
      OnClick = Button11Click
    end
  end
  object Viewer: TGLSceneViewer
    Left = 651
    Top = 0
    Width = 1050
    Height = 914
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa2x
    FieldOfView = 167.512298583984400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 1
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
    object LightCube1: TGLDummyCube
      Position.Coordinates = {000096C30000A040000096430000803F}
      OnProgress = LightCube1Progress
      CubeSize = 1.000000000000000000
      object LightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object LightCube2: TGLDummyCube
      Position.Coordinates = {000096430000C842000096430000803F}
      OnProgress = LightCube2Progress
      CubeSize = 1.000000000000000000
      object LightSource2: TGLLightSource
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
        object Sphere: TGLSphere
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
        object Torus: TGLTorus
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
