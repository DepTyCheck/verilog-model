-- Seed: 4685533380575496482,6377581586043967497

entity amserdlaci is
  port (vi : in time; edn : buffer real; dagpnshjx : buffer real);
end amserdlaci;

architecture tzjtru of amserdlaci is
  
begin
  -- Single-driven assignments
  dagpnshjx <= edn;
  edn <= edn;
end tzjtru;

entity t is
  port (ykv : out severity_level; f : out integer; dmyklu : out severity_level; fmtntbc : in time);
end t;

architecture qdiv of t is
  signal gvuzybo : real;
  signal xdoiljvjcv : real;
  signal zh : real;
  signal hwzkzkdkow : real;
  signal jnjbigpna : real;
  signal yop : real;
begin
  tnlkaoc : entity work.amserdlaci
    port map (vi => fmtntbc, edn => yop, dagpnshjx => jnjbigpna);
  gkbflhq : entity work.amserdlaci
    port map (vi => fmtntbc, edn => hwzkzkdkow, dagpnshjx => zh);
  qnctfjdi : entity work.amserdlaci
    port map (vi => fmtntbc, edn => xdoiljvjcv, dagpnshjx => gvuzybo);
  
  -- Single-driven assignments
  dmyklu <= dmyklu;
  ykv <= NOTE;
  f <= 16#742#;
end qdiv;

entity qmcowuh is
  port (lirvgxrwx : buffer string(3 to 2); kiuaklc : out severity_level; tzz : linkage real; eszbm : buffer real);
end qmcowuh;

architecture kzwp of qmcowuh is
  signal odipvorocu : real;
  signal fzzlsk : real;
  signal gcyhkp : time;
begin
  vhrrljw : entity work.amserdlaci
    port map (vi => gcyhkp, edn => fzzlsk, dagpnshjx => odipvorocu);
  
  -- Single-driven assignments
  eszbm <= 0.2;
  gcyhkp <= 8#2# ns;
  lirvgxrwx <= lirvgxrwx;
  kiuaklc <= NOTE;
end kzwp;



-- Seed after: 11752796482627112534,6377581586043967497
