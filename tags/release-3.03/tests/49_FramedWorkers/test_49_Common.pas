unit test_49_Common;

interface

uses
  Messages;

const

  // form -> thread messages; any number from range 0..65534
  MSG_HELLO = 1;

  // thread -> form messages; must start at WM_USER
  MSG_NOTIFY = WM_USER;

implementation

end.
