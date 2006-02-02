// $Id: DebugC.nc,v 1.1 2006/02/02 23:24:16 idgay Exp $

includes Timer;

configuration DebugC 
{
}
implementation
{
  components new DebugM(uint32_t,uint16_t,uint16_t,T32khz);
}

