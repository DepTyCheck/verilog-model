-- Seed: 13004890298358513375,10754487200446211253

entity mgwtmfawbq is
  port (urb : inout time);
end mgwtmfawbq;

architecture nz of mgwtmfawbq is
  
begin
  -- Single-driven assignments
  urb <= 0_0_1_0_0.1434 ps;
end nz;

library ieee;
use ieee.std_logic_1164.all;

entity zsudvmo is
  port (vkvgkxro : buffer time; yr : in std_logic_vector(4 downto 3); uibqy : linkage integer; vs : buffer std_logic_vector(4 downto 3));
end zsudvmo;

architecture wymaxf of zsudvmo is
  signal kekhuxxtao : time;
  signal bfzyrlu : time;
  signal chztccelx : time;
begin
  jyijeyotgw : entity work.mgwtmfawbq
    port map (urb => chztccelx);
  aumf : entity work.mgwtmfawbq
    port map (urb => bfzyrlu);
  qyki : entity work.mgwtmfawbq
    port map (urb => kekhuxxtao);
  jjqiju : entity work.mgwtmfawbq
    port map (urb => vkvgkxro);
  
  -- Multi-driven assignments
  vs <= ('L', '1');
  vs <= "ZL";
end wymaxf;



-- Seed after: 12624763840738958846,10754487200446211253
