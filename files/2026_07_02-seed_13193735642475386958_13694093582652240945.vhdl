-- Seed: 13193735642475386958,13694093582652240945

entity hjjeyxbs is
  port (zucwgs : buffer boolean);
end hjjeyxbs;

architecture owkcwscqcf of hjjeyxbs is
  
begin
  -- Single-driven assignments
  zucwgs <= TRUE;
end owkcwscqcf;

library ieee;
use ieee.std_logic_1164.all;

entity bqvsihri is
  port (ijzd : in integer; mauzm : linkage std_logic; z : inout time);
end bqvsihri;

architecture klb of bqvsihri is
  signal fzhoa : boolean;
  signal s : boolean;
begin
  ohwximwji : entity work.hjjeyxbs
    port map (zucwgs => s);
  dvon : entity work.hjjeyxbs
    port map (zucwgs => fzhoa);
  
  -- Single-driven assignments
  z <= 3_4_1_3_1 ns;
end klb;

library ieee;
use ieee.std_logic_1164.all;

entity altfhw is
  port (txymlra : inout bit; jw : buffer integer; gxynpftjr : in std_logic; vaiy : out integer_vector(4 to 1));
end altfhw;

architecture h of altfhw is
  signal nowzfl : boolean;
  signal qvmqfbdpw : time;
  signal pfplajik : integer;
  signal emecbkux : time;
  signal ysoybpex : integer;
  signal vn : boolean;
begin
  jakdfwckm : entity work.hjjeyxbs
    port map (zucwgs => vn);
  hfrapxmkpf : entity work.bqvsihri
    port map (ijzd => ysoybpex, mauzm => gxynpftjr, z => emecbkux);
  klticqra : entity work.bqvsihri
    port map (ijzd => pfplajik, mauzm => gxynpftjr, z => qvmqfbdpw);
  o : entity work.hjjeyxbs
    port map (zucwgs => nowzfl);
  
  -- Single-driven assignments
  pfplajik <= 2#0#;
  ysoybpex <= 1;
  jw <= 3;
  vaiy <= (others => 0);
end h;



-- Seed after: 1379918947004523395,13694093582652240945
