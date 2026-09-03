-- Seed: 13191958386296046004,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity fltmxm is
  port (haucm : out std_logic; hfyvhd : buffer bit; gkw : out boolean; gxuolb : in severity_level);
end fltmxm;

architecture vxo of fltmxm is
  
begin
  -- Single-driven assignments
  gkw <= TRUE;
  hfyvhd <= hfyvhd;
end vxo;

entity o is
  port (gwsqydthtv : out real_vector(4 to 4); cwivbvbaar : in time; icwaf : buffer integer; dkxzg : in integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture bruoj of o is
  signal yglkfquk : severity_level;
  signal c : boolean;
  signal zmyfw : bit;
  signal rj : std_logic;
begin
  roqadip : entity work.fltmxm
    port map (haucm => rj, hfyvhd => zmyfw, gkw => c, gxuolb => yglkfquk);
  
  -- Single-driven assignments
  icwaf <= 2#001#;
  yglkfquk <= FAILURE;
  gwsqydthtv <= gwsqydthtv;
  
  -- Multi-driven assignments
  rj <= 'Z';
end bruoj;



-- Seed after: 12456533867591666827,11127274767545411571
