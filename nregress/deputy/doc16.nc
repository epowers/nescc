struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc16 { }
implementation {
  /** 
   * @param 'int *@count(j) a' is something
   * @param n is a length
   */
  void f(int *a, int n) @spontaneous() {
  }

  int j;
}
