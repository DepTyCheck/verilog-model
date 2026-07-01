-- Seed: 10491728959649819005,6882842853887419669

entity kzykhafzjv is
  port (g : out integer; aawtju : inout time);
end kzykhafzjv;

architecture gbac of kzykhafzjv is
  
begin
  -- Single-driven assignments
  aawtju <= 3.3 ps;
  g <= 31020;
end gbac;

entity vlia is
  port (an : buffer time);
end vlia;

architecture eddqr of vlia is
  signal dsimsg : time;
  signal fyzq : integer;
  signal euh : integer;
begin
  u : entity work.kzykhafzjv
    port map (g => euh, aawtju => an);
  smejyrjxv : entity work.kzykhafzjv
    port map (g => fyzq, aawtju => dsimsg);
end eddqr;

entity silw is
  port (g : linkage character);
end silw;

architecture potdhcxcb of silw is
  signal xvjwhck : time;
  signal xz : integer;
  signal ooxqpre : time;
begin
  gxjm : entity work.vlia
    port map (an => ooxqpre);
  p : entity work.kzykhafzjv
    port map (g => xz, aawtju => xvjwhck);
end potdhcxcb;



-- Seed after: 14253654985023438697,6882842853887419669
