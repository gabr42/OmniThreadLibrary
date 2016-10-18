object frmProcessorGroupsNUMA: TfrmProcessorGroupsNUMA
  Left = 0
  Top = 0
  Caption = 'Processor groups & NUMA tester'
  ClientHeight = 279
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblAffinity: TLabel
    Left = 28
    Top = 136
    Width = 39
    Height = 13
    Caption = 'Affinity:'
  end
  object lblProcessorGroups: TLabel
    Left = 28
    Top = 163
    Width = 87
    Height = 13
    Caption = 'Processor groups:'
  end
  object Label1: TLabel
    Left = 28
    Top = 190
    Width = 65
    Height = 13
    Caption = 'NUMA nodes:'
  end
  object lbLog: TListBox
    Left = 248
    Top = 0
    Width = 387
    Height = 279
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnStartProcGroup: TButton
    Left = 8
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Start task in processor group:'
    TabOrder = 1
    OnClick = btnStartProcGroupClick
  end
  object inpProcGroup: TSpinEdit
    Left = 191
    Top = 10
    Width = 49
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object btnStartInNumaNode: TButton
    Left = 8
    Top = 39
    Width = 177
    Height = 25
    Caption = 'Start task in NUMA node:'
    TabOrder = 3
    OnClick = btnStartInNumaNodeClick
  end
  object inpNUMANode: TSpinEdit
    Left = 191
    Top = 41
    Width = 49
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
  object btnScheduleTask: TButton
    Left = 8
    Top = 102
    Width = 234
    Height = 25
    Caption = 'Schedule a task'
    TabOrder = 5
    OnClick = btnScheduleTaskClick
  end
  object inpAffinity: TEdit
    Left = 121
    Top = 133
    Width = 121
    Height = 21
    TabOrder = 6
    TextHint = '000FFF'
  end
  object inpProcessorGroups: TEdit
    Left = 121
    Top = 160
    Width = 121
    Height = 21
    TabOrder = 7
    TextHint = '0,1'
  end
  object inpNUMANodes: TEdit
    Left = 121
    Top = 187
    Width = 121
    Height = 21
    TabOrder = 8
    TextHint = '0,1'
  end
  object btnScheduleAllGroups: TButton
    Left = 8
    Top = 216
    Width = 234
    Height = 25
    Caption = 'Schedule task in all groups'
    TabOrder = 9
    OnClick = btnScheduleAllGroupsClick
  end
  object btnSchedulAllNodes: TButton
    Left = 8
    Top = 247
    Width = 234
    Height = 25
    Caption = 'Schedule task in all NUMA nodes'
    TabOrder = 10
    OnClick = btnSchedulAllNodesClick
  end
end
