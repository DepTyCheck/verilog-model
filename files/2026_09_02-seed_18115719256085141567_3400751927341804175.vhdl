-- Seed: 18115719256085141567,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity pvspfq is
  port (aczv : in real; lz : out std_logic; wnxgfy : buffer std_logic_vector(4 to 0));
end pvspfq;

architecture y of pvspfq is
  
begin
  
end y;

entity ehvdg is
  port (xoznyeatyv : linkage time; dqund : buffer real; m : out severity_level; znvvtauy : out integer);
end ehvdg;

library ieee;
use ieee.std_logic_1164.all;

architecture wwge of ehvdg is
  signal jzegpozf : std_logic_vector(4 to 0);
  signal edmix : std_logic;
  signal dnz : real;
  signal odeoyjmzr : real;
  signal ziygr : std_logic_vector(4 to 0);
  signal nfp : std_logic;
begin
  ts : entity work.pvspfq
    port map (aczv => dqund, lz => nfp, wnxgfy => ziygr);
  xf : entity work.pvspfq
    port map (aczv => odeoyjmzr, lz => nfp, wnxgfy => ziygr);
  flprtgmiko : entity work.pvspfq
    port map (aczv => dnz, lz => edmix, wnxgfy => jzegpozf);
  
  -- Single-driven assignments
  znvvtauy <= 113;
  dnz <= dqund;
  dqund <= dqund;
  odeoyjmzr <= 2#0_1_1.1#;
  m <= m;
end wwge;

library ieee;
use ieee.std_logic_1164.all;

entity jayuoptcuu is
  port (gjpwtt : linkage std_logic_vector(3 to 3); urbxidn : in boolean_vector(2 to 2); vtxrs : inout real_vector(0 downto 1));
end jayuoptcuu;

library ieee;
use ieee.std_logic_1164.all;

architecture ktwbzqaorr of jayuoptcuu is
  signal hwx : std_logic_vector(4 to 0);
  signal ydykmwvd : std_logic;
  signal bcyfzxag : real;
  signal fsayxgescy : std_logic;
  signal uxcrbwdntb : std_logic_vector(4 to 0);
  signal dfxoqkmm : std_logic;
  signal mmiznzdf : std_logic_vector(4 to 0);
  signal cbdbwjcsu : std_logic;
  signal tcjhngd : real;
begin
  umygn : entity work.pvspfq
    port map (aczv => tcjhngd, lz => cbdbwjcsu, wnxgfy => mmiznzdf);
  eislqulr : entity work.pvspfq
    port map (aczv => tcjhngd, lz => dfxoqkmm, wnxgfy => uxcrbwdntb);
  kmgqhm : entity work.pvspfq
    port map (aczv => tcjhngd, lz => fsayxgescy, wnxgfy => mmiznzdf);
  etyjixb : entity work.pvspfq
    port map (aczv => bcyfzxag, lz => ydykmwvd, wnxgfy => hwx);
  
  -- Multi-driven assignments
  cbdbwjcsu <= dfxoqkmm;
  fsayxgescy <= cbdbwjcsu;
  fsayxgescy <= 'Z';
  ydykmwvd <= cbdbwjcsu;
end ktwbzqaorr;



-- Seed after: 15581128722784441316,3400751927341804175
