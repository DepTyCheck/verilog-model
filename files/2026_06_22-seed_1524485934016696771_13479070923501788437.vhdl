-- Seed: 1524485934016696771,13479070923501788437

entity flnw is
  port (xy : buffer time; ropktqgi : linkage integer; pembnht : in integer);
end flnw;

architecture qhnbjtzk of flnw is
  
begin
  -- Single-driven assignments
  xy <= 8#0_0_2_4.66605# ms;
end qhnbjtzk;

entity bvlf is
  port (bkouyz : inout integer);
end bvlf;

architecture uthckyzcj of bvlf is
  signal ubv : integer;
  signal kkkxbyvw : integer;
  signal yokokwx : time;
  signal txvq : time;
  signal xpcwq : integer;
  signal ibhqehn : time;
  signal ke : integer;
  signal wiexxf : time;
begin
  sbdaxjjmaa : entity work.flnw
    port map (xy => wiexxf, ropktqgi => bkouyz, pembnht => ke);
  f : entity work.flnw
    port map (xy => ibhqehn, ropktqgi => ke, pembnht => xpcwq);
  zdqmmcwjrw : entity work.flnw
    port map (xy => txvq, ropktqgi => xpcwq, pembnht => bkouyz);
  otby : entity work.flnw
    port map (xy => yokokwx, ropktqgi => kkkxbyvw, pembnht => ubv);
  
  -- Single-driven assignments
  ubv <= 4_4;
end uthckyzcj;



-- Seed after: 9725107380890196817,13479070923501788437
