-- Seed: 1047262625664704101,14629254427735353553

entity wnslpxd is
  port (eyzz : inout time);
end wnslpxd;

architecture tphkoqzc of wnslpxd is
  
begin
  -- Single-driven assignments
  eyzz <= 1 min;
end tphkoqzc;

entity duwav is
  port (duiqh : out bit);
end duwav;

architecture xpc of duwav is
  signal undmupyup : time;
begin
  u : entity work.wnslpxd
    port map (eyzz => undmupyup);
  
  -- Single-driven assignments
  duiqh <= '1';
end xpc;

entity c is
  port (cdhigvckln : buffer real; pkeq : out integer);
end c;

architecture wztiru of c is
  signal suetth : time;
  signal iogvsrayzf : bit;
  signal gxmje : time;
  signal bt : time;
begin
  imke : entity work.wnslpxd
    port map (eyzz => bt);
  zpzwskmhpu : entity work.wnslpxd
    port map (eyzz => gxmje);
  tfjhjza : entity work.duwav
    port map (duiqh => iogvsrayzf);
  owdktmsop : entity work.wnslpxd
    port map (eyzz => suetth);
  
  -- Single-driven assignments
  pkeq <= 16#0_7#;
  cdhigvckln <= 16#D732.7_6#;
end wztiru;



-- Seed after: 16365034825320481945,14629254427735353553
