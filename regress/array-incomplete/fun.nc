module fun { }
implementation {
  uint16_t fun[];

  uint16_t median(uint16_t *tmp, uint8_t len) __attribute__((spontaneous)) {
    uint16_t array[2], i;
    for(i=0;i<len;i++)
	  array[i]=tmp[i] + fun[i];
    
    return len%2 == 0 ? array[len/2-1] : array[(len-1)/2];
  }


}