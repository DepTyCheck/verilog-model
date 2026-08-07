-- Seed: 8585531819348135114,8068158652091157513

entity dzmotgbas is
  port (t : in time);
end dzmotgbas;

architecture i of dzmotgbas is
  
begin
  
end i;

entity u is
  port (iu : out real; hmkclrymq : inout real);
end u;

architecture mfzgtdjpj of u is
  signal omvp : time;
  signal srj : time;
  signal lgqhbgfrw : time;
  signal cpqblbhb : time;
begin
  fads : entity work.dzmotgbas
    port map (t => cpqblbhb);
  vkmicgehhj : entity work.dzmotgbas
    port map (t => lgqhbgfrw);
  xeq : entity work.dzmotgbas
    port map (t => srj);
  ieplpw : entity work.dzmotgbas
    port map (t => omvp);
  
  -- Single-driven assignments
  omvp <= 41041 ps;
  cpqblbhb <= 1 hr;
  iu <= hmkclrymq;
  hmkclrymq <= hmkclrymq;
end mfzgtdjpj;

entity yoioa is
  port (oxyw : in time; gruanuj : linkage time);
end yoioa;

architecture gugct of yoioa is
  signal ebvjxmdj : time;
  signal yitmqjfa : real;
  signal bzxztr : real;
  signal w : real;
  signal bu : real;
begin
  xwvet : entity work.u
    port map (iu => bu, hmkclrymq => w);
  ghji : entity work.u
    port map (iu => bzxztr, hmkclrymq => yitmqjfa);
  zfwaj : entity work.dzmotgbas
    port map (t => ebvjxmdj);
  cmjoetoood : entity work.dzmotgbas
    port map (t => ebvjxmdj);
  
  -- Single-driven assignments
  ebvjxmdj <= oxyw;
end gugct;



-- Seed after: 12290796938953689403,8068158652091157513
