unit ForEachOutput1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Generics.Collections,
  OtlCommon,
  OtlCollections,
  OtlParallel;

type
  TTransaction = class
    Transaction: string;
    constructor Create(const transact: string);
  end;

  TfrmForEachOutput = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  end;

var
  frmForEachOutput: TfrmForEachOutput;

implementation

{$R *.dfm}

procedure ProcessTransactions(input: TStringList; output: TList<TTransaction>);
var
  outQueue   : IOmniBlockingCollection;
  transaction: TOmniValue;
begin
  outQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(0, input.Count - 1).NoWait.PreserveOrder.Into(outQueue).Execute(
    procedure(const value: integer; var result: TOmniValue)
    begin
      result := TTransaction.Create(input[value]);
    end
  );
  for transaction in outQueue do
    output.Add(transaction.AsObject as TTransaction);
end;

procedure TfrmForEachOutput.FormCreate(Sender: TObject);
var
  bankStatements: TStringList;
  ch            : char;
  transaction   : TTransaction;
  transactions  : TList<TTransaction>;
begin
  bankStatements := TStringList.Create;
  try
    for ch := '1' to '9' do bankStatements.Add(ch); //for testing
    transactions := TList<TTransaction>.Create;
    try
      ProcessTransactions(bankStatements, transactions);
      for transaction in transactions do begin
        ListBox1.Items.Add(transaction.Transaction);
        transaction.Free;
      end;
    finally FreeAndNil(transactions); end;
  finally FreeAndNil(bankStatements); end;
end;

{ TTransaction }

constructor TTransaction.Create(const transact: string);
begin
  Transaction := transact;
end;

end.

