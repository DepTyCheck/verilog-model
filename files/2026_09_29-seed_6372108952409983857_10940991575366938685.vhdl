-- Seed: 6372108952409983857,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity xhgj is
  port (shgjepfpv : in time; qvxpzgrx : inout std_logic; nja : linkage std_logic_vector(3 to 0); qju : buffer real);
end xhgj;

architecture merengsg of xhgj is
  
begin
  -- Single-driven assignments
  qju <= 2#0.100#;
end merengsg;

entity sm is
  port (xkmmkxom : inout time_vector(0 to 1); dz : in real_vector(1 to 3));
end sm;

library ieee;
use ieee.std_logic_1164.all;

architecture v of sm is
  signal bxvaxhzsv : real;
  signal vmbpgsgdfe : std_logic_vector(3 to 0);
  signal mlq : std_logic;
  signal nvjqsshqr : real;
  signal ikgqlpkj : std_logic_vector(3 to 0);
  signal kfplz : time;
  signal vrea : real;
  signal fy : std_logic_vector(3 to 0);
  signal byqr : std_logic;
  signal ffmdvlxzv : time;
begin
  tqgswew : entity work.xhgj
    port map (shgjepfpv => ffmdvlxzv, qvxpzgrx => byqr, nja => fy, qju => vrea);
  sjlfwhui : entity work.xhgj
    port map (shgjepfpv => kfplz, qvxpzgrx => byqr, nja => ikgqlpkj, qju => nvjqsshqr);
  zhvyesjb : entity work.xhgj
    port map (shgjepfpv => ffmdvlxzv, qvxpzgrx => mlq, nja => vmbpgsgdfe, qju => bxvaxhzsv);
  
  -- Single-driven assignments
  kfplz <= ffmdvlxzv;
  ffmdvlxzv <= 8#6.5# fs;
  xkmmkxom <= xkmmkxom;
  
  -- Multi-driven assignments
  fy <= "";
  byqr <= byqr;
  byqr <= 'X';
end v;

entity romv is
  port (zwlgsgg : buffer real_vector(1 downto 0); rnfsdoo : linkage integer; iynslvsf : in string(1 downto 3));
end romv;

library ieee;
use ieee.std_logic_1164.all;

architecture gwepbw of romv is
  signal ujw : real;
  signal snlxphui : real;
  signal lhhhdrmmo : std_logic_vector(3 to 0);
  signal zrdcyr : std_logic;
  signal zaiqwuzwi : time;
  signal szpsculvr : real_vector(1 to 3);
  signal h : time_vector(0 to 1);
begin
  tpqovbo : entity work.sm
    port map (xkmmkxom => h, dz => szpsculvr);
  wq : entity work.xhgj
    port map (shgjepfpv => zaiqwuzwi, qvxpzgrx => zrdcyr, nja => lhhhdrmmo, qju => snlxphui);
  cb : entity work.xhgj
    port map (shgjepfpv => zaiqwuzwi, qvxpzgrx => zrdcyr, nja => lhhhdrmmo, qju => ujw);
  
  -- Single-driven assignments
  zaiqwuzwi <= 2#0001# us;
  szpsculvr <= szpsculvr;
  zwlgsgg <= (16#8.2#, 0.2_4_1);
  
  -- Multi-driven assignments
  lhhhdrmmo <= lhhhdrmmo;
  zrdcyr <= 'L';
  zrdcyr <= 'Z';
  zrdcyr <= 'Z';
end gwepbw;



-- Seed after: 5917359359792990877,10940991575366938685
