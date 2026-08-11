-- Seed: 13281149639239645,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity bqejsczzn is
  port (aaaoxctqz : in std_logic; ldcug : inout integer_vector(1 downto 4); zsmb : inout std_logic_vector(2 downto 2); vxfaqc : buffer time);
end bqejsczzn;

architecture hvmrmyyt of bqejsczzn is
  
begin
  -- Single-driven assignments
  ldcug <= ldcug;
  vxfaqc <= 00.304 fs;
  
  -- Multi-driven assignments
  zsmb <= (others => 'Z');
  zsmb <= zsmb;
  zsmb <= (others => '-');
end hvmrmyyt;

entity rbnnr is
  port (hun : inout time; wkbewc : buffer real; cpmgpwatld : in real);
end rbnnr;

library ieee;
use ieee.std_logic_1164.all;

architecture xrf of rbnnr is
  signal cfsykw : integer_vector(1 downto 4);
  signal cuddervxfu : time;
  signal cf : std_logic_vector(2 downto 2);
  signal ufgtxoe : integer_vector(1 downto 4);
  signal kuuzyvr : std_logic;
begin
  cehkmclxih : entity work.bqejsczzn
    port map (aaaoxctqz => kuuzyvr, ldcug => ufgtxoe, zsmb => cf, vxfaqc => cuddervxfu);
  szjhyt : entity work.bqejsczzn
    port map (aaaoxctqz => kuuzyvr, ldcug => cfsykw, zsmb => cf, vxfaqc => hun);
  
  -- Single-driven assignments
  wkbewc <= 1_2_0_0.200;
  
  -- Multi-driven assignments
  kuuzyvr <= '0';
  kuuzyvr <= 'U';
  kuuzyvr <= 'H';
  kuuzyvr <= kuuzyvr;
end xrf;



-- Seed after: 15198334635829803212,10594830431004325987
