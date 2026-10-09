#pragma hdrstop
#include <System.hpp>
#include <stdio.h>
#include "OtlCommon.hpp"
#include "OtlSync.hpp"

int main()
{
	// critical section through the interface: Acquire / Leave (Delphi: Release)
	Otlsync::_di_IOmniCriticalSection cs = Otlsync::CreateOmniCriticalSection();
	cs->Acquire();
	int lock1 = cs->LockCount;
	cs->Leave();
	int lock2 = cs->LockCount;
	printf("lock count while held: %d, after leave: %d\n", lock1, lock2);

	// TOmniValueContainer: indexed access by position and by name
	Otlcommon::TOmniValueContainer* c = new Otlcommon::TOmniValueContainer();
	c->Add(Otlcommon::TOmniValue::_op_Implicit((int)42), "answer");
	printf("item[0] = %d, ItemByName['answer'] = %d\n", (int)c->Item[0], (int)c->ItemByName["answer"]);
	delete c;

	// resource count: Allocate / Leave
	Otlsync::_di_IOmniResourceCount rc = Otlsync::CreateResourceCount(2);
	unsigned a = rc->Allocate();
	unsigned b = rc->Leave();
	printf("resource count: allocate -> %u, leave -> %u\n", a, b);
	return (lock1 == 1 && lock2 == 0) ? 0 : 1;
}
