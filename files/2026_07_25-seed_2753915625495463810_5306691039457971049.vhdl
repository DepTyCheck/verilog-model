-- Seed: 2753915625495463810,5306691039457971049

entity ggaveg is
  port (dj : linkage real_vector(4 to 4); rd : linkage integer);
end ggaveg;

architecture xiwuv of ggaveg is
  
begin
  
end xiwuv;

entity xeufhgny is
  port (qd : buffer integer; qf : out time);
end xeufhgny;

architecture vhjjg of xeufhgny is
  signal vbebaq : real_vector(4 to 4);
  signal nrmowu : integer;
  signal usijc : real_vector(4 to 4);
  signal bxpgbv : integer;
  signal sffdg : real_vector(4 to 4);
begin
  mndmmhpt : entity work.ggaveg
    port map (dj => sffdg, rd => bxpgbv);
  nstmrndabd : entity work.ggaveg
    port map (dj => usijc, rd => nrmowu);
  t : entity work.ggaveg
    port map (dj => vbebaq, rd => qd);
end vhjjg;

entity dv is
  port (bavdga : out time; oshbjeiw : buffer time);
end dv;

architecture bswkvcea of dv is
  signal dflct : integer;
  signal uq : real_vector(4 to 4);
begin
  ge : entity work.ggaveg
    port map (dj => uq, rd => dflct);
  
  -- Single-driven assignments
  oshbjeiw <= bavdga;
  bavdga <= bavdga;
end bswkvcea;



-- Seed after: 5491020064172987756,5306691039457971049
