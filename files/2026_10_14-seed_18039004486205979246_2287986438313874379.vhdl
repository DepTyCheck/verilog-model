-- Seed: 18039004486205979246,2287986438313874379

entity obumtgur is
  port (ajyldjm : out boolean; uonbornnf : buffer real_vector(4 downto 0); kqbeszzjw : out time; ewq : inout time);
end obumtgur;

architecture avsjlihikk of obumtgur is
  
begin
  
end avsjlihikk;

entity vsno is
  port (szlwhwrojj : buffer time_vector(4 to 0); obmfak : buffer time);
end vsno;

architecture i of vsno is
  signal rvquxm : time;
  signal tzjv : real_vector(4 downto 0);
  signal qfoj : boolean;
begin
  wt : entity work.obumtgur
    port map (ajyldjm => qfoj, uonbornnf => tzjv, kqbeszzjw => obmfak, ewq => rvquxm);
  
  -- Single-driven assignments
  szlwhwrojj <= (others => 0 ns);
end i;

entity pexmff is
  port (feshpyi : buffer character; zac : linkage real);
end pexmff;

architecture gziujpto of pexmff is
  signal thwpyfyuba : time;
  signal ahqlvauj : time_vector(4 to 0);
begin
  xof : entity work.vsno
    port map (szlwhwrojj => ahqlvauj, obmfak => thwpyfyuba);
  
  -- Single-driven assignments
  feshpyi <= feshpyi;
end gziujpto;



-- Seed after: 3538403526062278728,2287986438313874379
