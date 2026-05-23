-- Seed: 10106149000979437498,17341743235868673497



entity vfsaqabopr is
  port (qof : out real; t : in time);
end vfsaqabopr;



architecture nmriib of vfsaqabopr is
  
begin
  
end nmriib;



entity b is
  port (lfbrlnn : in time);
end b;



architecture fkp of b is
  signal vuetefa : real;
  signal m : real;
  signal iiruzt : time;
  signal e : real;
  signal mwfmm : real;
begin
  hapmguxk : entity work.vfsaqabopr
    port map (qof => mwfmm, t => lfbrlnn);
  zbstwqrh : entity work.vfsaqabopr
    port map (qof => e, t => iiruzt);
  j : entity work.vfsaqabopr
    port map (qof => m, t => lfbrlnn);
  gjj : entity work.vfsaqabopr
    port map (qof => vuetefa, t => iiruzt);
end fkp;



entity ajhb is
  port (uv : linkage real; sdhhws : linkage time);
end ajhb;



architecture dnqxskqvg of ajhb is
  signal aays : real;
  signal lydzapih : time;
  signal xtyfni : real;
begin
  vylwds : entity work.vfsaqabopr
    port map (qof => xtyfni, t => lydzapih);
  lgb : entity work.vfsaqabopr
    port map (qof => aays, t => lydzapih);
end dnqxskqvg;



entity kkolfsp is
  port (zrm : inout time; pxgwfci : buffer time; xdwfvwzkp : inout boolean);
end kkolfsp;



architecture iotumtido of kkolfsp is
  signal jhnexu : time;
  signal yia : real;
  signal ijerakndv : time;
begin
  cqlwqkgl : entity work.b
    port map (lfbrlnn => ijerakndv);
  w : entity work.ajhb
    port map (uv => yia, sdhhws => pxgwfci);
  zq : entity work.ajhb
    port map (uv => yia, sdhhws => pxgwfci);
  jstdurhwbx : entity work.ajhb
    port map (uv => yia, sdhhws => jhnexu);
end iotumtido;



-- Seed after: 13103566555108137369,17341743235868673497
