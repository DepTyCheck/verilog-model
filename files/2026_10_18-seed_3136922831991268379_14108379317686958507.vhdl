-- Seed: 3136922831991268379,14108379317686958507

entity eihfuavhy is
  port (kyrtcqib : out real; uvvct : buffer time);
end eihfuavhy;

architecture eazdjj of eihfuavhy is
  
begin
  -- Single-driven assignments
  uvvct <= 2#10# fs;
end eazdjj;

entity ser is
  port (j : buffer severity_level; usvhsjl : in severity_level);
end ser;

architecture pg of ser is
  signal kc : time;
  signal yvifrt : real;
  signal i : time;
  signal peauekrdc : real;
  signal g : time;
  signal vyecw : real;
begin
  pmfdr : entity work.eihfuavhy
    port map (kyrtcqib => vyecw, uvvct => g);
  xq : entity work.eihfuavhy
    port map (kyrtcqib => peauekrdc, uvvct => i);
  ltz : entity work.eihfuavhy
    port map (kyrtcqib => yvifrt, uvvct => kc);
  
  -- Single-driven assignments
  j <= usvhsjl;
end pg;



-- Seed after: 461098530968119625,14108379317686958507
