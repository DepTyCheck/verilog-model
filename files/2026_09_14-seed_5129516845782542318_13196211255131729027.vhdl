-- Seed: 5129516845782542318,13196211255131729027

entity ib is
  port (cur : buffer real; aznabsenv : inout time; gmct : inout real);
end ib;

architecture n of ib is
  
begin
  
end n;

entity yu is
  port (b : linkage integer);
end yu;

architecture tqluyiho of yu is
  signal mkncf : real;
  signal qlqnsqwuks : time;
  signal rvzy : real;
  signal pqmgudu : real;
  signal rvxkxfqdfu : time;
  signal ta : real;
  signal qj : real;
  signal blvia : time;
  signal pti : real;
begin
  inomywq : entity work.ib
    port map (cur => pti, aznabsenv => blvia, gmct => qj);
  zlpvizcejl : entity work.ib
    port map (cur => ta, aznabsenv => rvxkxfqdfu, gmct => pqmgudu);
  sngwwhx : entity work.ib
    port map (cur => rvzy, aznabsenv => qlqnsqwuks, gmct => mkncf);
end tqluyiho;

entity rjmuxkt is
  port (xxurmy : out boolean; tvionomq : linkage integer; ievq : in real; nszk : out integer);
end rjmuxkt;

architecture ctgobxo of rjmuxkt is
  signal teosc : integer;
  signal tyryik : integer;
begin
  znshnrjc : entity work.yu
    port map (b => nszk);
  hl : entity work.yu
    port map (b => tyryik);
  cghka : entity work.yu
    port map (b => teosc);
  jaj : entity work.yu
    port map (b => tvionomq);
end ctgobxo;



-- Seed after: 10958221609713784151,13196211255131729027
