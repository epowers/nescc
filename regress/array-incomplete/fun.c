typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

  uint16_t median(uint16_t *tmp, uint8_t len) {
    uint16_t array[], i;

    for(i=0;i<len;i++)
	  array[i]=tmp[i];
    
    return len%2 == 0 ? array[len/2-1] : array[(len-1)/2];
  }

int bb[2];
int aa[];

int aa[10];
