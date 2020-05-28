object DMDXPExpertModule: TDMDXPExpertModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 466
  Top = 110
  Height = 192
  Width = 281
  object PMFreePascal: TPopupMenu
    Left = 32
    Top = 8
    object MIExecute: TMenuItem
      Action = ACFPCExecute
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MICompile: TMenuItem
      Action = ACFPCCompile
    end
    object MIBuild: TMenuItem
      Action = ACFPCBuild
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Options1: TMenuItem
      Action = ACFPCOptions
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 112
    Top = 8
    object ACFPCExecute: TAction
      Category = 'FreePascal'
      Caption = 'Execute'
      ImageIndex = 24
      OnExecute = ACFPCExecuteExecute
    end
    object ACFPCCompile: TAction
      Category = 'FreePascal'
      Caption = 'Compile'
      ImageIndex = 106
      OnExecute = ACFPCCompileExecute
    end
    object ACFPCBuild: TAction
      Category = 'FreePascal'
      Caption = 'Build'
      ImageIndex = 32
      OnExecute = ACFPCBuildExecute
    end
    object ACFPCOptions: TAction
      Category = 'FreePascal'
      Caption = 'Options...'
      ImageIndex = 84
      OnExecute = ACFPCOptionsExecute
    end
    object ACDXPOptions: TAction
      Caption = 'Options...'
      ImageIndex = 36
      OnExecute = ACDXPOptionsExecute
    end
    object ACViewCompilerMessages: TAction
      Caption = 'Compiler Messages'
      OnExecute = ACViewCompilerMessagesExecute
    end
  end
  object PMDXP: TPopupMenu
    Left = 32
    Top = 56
    object View1: TMenuItem
      Caption = 'View'
      object MICompilerMessages: TMenuItem
        Action = ACViewCompilerMessages
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem1: TMenuItem
      Action = ACDXPOptions
    end
  end
end
