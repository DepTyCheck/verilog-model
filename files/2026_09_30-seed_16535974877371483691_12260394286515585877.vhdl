-- Seed: 16535974877371483691,12260394286515585877

entity h is
  port (sq : buffer real; xqidiv : buffer time; ml : inout time);
end h;

architecture oe of h is
  
begin
  -- Single-driven assignments
  xqidiv <= 0 min;
  sq <= 8#6_2.6_6#;
  ml <= xqidiv;
end oe;

entity nxd is
  port (rjgfle : in integer; mhyyeaaffi : in time; xbviwsi : in time);
end nxd;

architecture qjduh of nxd is
  signal jju : time;
  signal rjgu : time;
  signal odtihhbzr : real;
begin
  cjt : entity work.h
    port map (sq => odtihhbzr, xqidiv => rjgu, ml => jju);
end qjduh;

entity iiqvysc is
  port (tdwqywwtum : linkage boolean; tlja : inout real_vector(4 downto 0); haqm : inout real_vector(2 to 1));
end iiqvysc;

architecture yptlezv of iiqvysc is
  signal il : time;
  signal vg : time;
  signal xeqvkf : integer;
  signal e : time;
  signal ejmo : real;
  signal ip : time;
  signal lpuxd : time;
  signal sehy : integer;
begin
  fgzjnog : entity work.nxd
    port map (rjgfle => sehy, mhyyeaaffi => lpuxd, xbviwsi => ip);
  xjm : entity work.h
    port map (sq => ejmo, xqidiv => e, ml => lpuxd);
  omhfbuavqs : entity work.nxd
    port map (rjgfle => xeqvkf, mhyyeaaffi => vg, xbviwsi => il);
  
  -- Single-driven assignments
  il <= 2 sec;
  haqm <= haqm;
end yptlezv;



-- Seed after: 14304282096167451313,12260394286515585877
