configuration test { }
implementation {
  components main, new mod(2) as mod1, new mod(int) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
