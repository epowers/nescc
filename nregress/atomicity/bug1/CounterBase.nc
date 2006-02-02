//$Id: CounterBase.nc,v 1.1 2006/02/02 23:24:16 idgay Exp $

//@author Cory Sharp <cssharp@eecs.berkeley.edu>

// The TinyOS Timer interfaces are discussed in TEP 102.

includes Timer;

interface CounterBase<size_type,frequency_tag>
{
  async command size_type get();
  async command bool isOverflowPending();
  async command void clearOverflow();
  async event void overflow();
}

