-- Seed: 12645520401280026272,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity ckh is
  port (pjzto : out bit_vector(0 to 3); buyfa : buffer std_logic);
end ckh;

architecture crcr of ckh is
  
begin
  -- Multi-driven assignments
  buyfa <= 'L';
  buyfa <= buyfa;
end crcr;

entity bmjuxk is
  port (qflqnazjk : inout time; yelol : inout integer_vector(3 downto 2); yci : buffer time);
end bmjuxk;

library ieee;
use ieee.std_logic_1164.all;

architecture tvlxbts of bmjuxk is
  signal xfa : bit_vector(0 to 3);
  signal nyocxdulc : std_logic;
  signal rgifiqtdaj : bit_vector(0 to 3);
begin
  szm : entity work.ckh
    port map (pjzto => rgifiqtdaj, buyfa => nyocxdulc);
  tiim : entity work.ckh
    port map (pjzto => xfa, buyfa => nyocxdulc);
  
  -- Multi-driven assignments
  nyocxdulc <= nyocxdulc;
  nyocxdulc <= nyocxdulc;
  nyocxdulc <= 'H';
  nyocxdulc <= 'H';
end tvlxbts;



-- Seed after: 265744445665029748,9900838946149614537
