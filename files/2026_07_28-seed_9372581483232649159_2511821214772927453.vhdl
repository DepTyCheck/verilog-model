-- Seed: 9372581483232649159,2511821214772927453

entity jozt is
  port (mmfjfixpnt : out time; czoizhxfl : buffer boolean_vector(0 to 3));
end jozt;

architecture hncxtgy of jozt is
  
begin
  -- Single-driven assignments
  mmfjfixpnt <= mmfjfixpnt;
  czoizhxfl <= (FALSE, TRUE, TRUE, FALSE);
end hncxtgy;

entity cyo is
  port (kf : out time_vector(3 downto 4));
end cyo;

architecture zygrtrg of cyo is
  signal l : boolean_vector(0 to 3);
  signal lybv : time;
begin
  ugmtzqlh : entity work.jozt
    port map (mmfjfixpnt => lybv, czoizhxfl => l);
  
  -- Single-driven assignments
  kf <= (others => 0 ns);
end zygrtrg;



-- Seed after: 10334102479919656998,2511821214772927453
