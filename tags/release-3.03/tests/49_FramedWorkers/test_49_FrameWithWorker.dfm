object frmFrameWithWorker: TfrmFrameWithWorker
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  DesignSize = (
    320
    240)
  object lbLog: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 314
    Height = 198
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnHello: TButton
    Left = 107
    Top = 207
    Width = 102
    Height = 25
    Anchors = [akBottom]
    Caption = 'Hello, worker!'
    TabOrder = 1
    OnClick = btnHelloClick
  end
end
