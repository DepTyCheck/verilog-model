-- Seed: 14407965210436675052,12011142928354116943

entity estg is
  port (sr : out time_vector(2 to 4); cmna : out real_vector(1 downto 4));
end estg;

architecture vxntpmkcro of estg is
  
begin
  -- Single-driven assignments
  cmna <= (others => 0.0);
  sr <= (2 min, 16#5_4# ns, 2#0_1_1.100# ms);
end vxntpmkcro;

library ieee;
use ieee.std_logic_1164.all;

entity ooytozn is
  port (bjzdxhh : buffer boolean; yhps : buffer real_vector(0 to 0); dmiaekbhay : out std_logic);
end ooytozn;

architecture fdknstyi of ooytozn is
  signal qr : real_vector(1 downto 4);
  signal sgwsucnwoe : time_vector(2 to 4);
begin
  qtthufwsd : entity work.estg
    port map (sr => sgwsucnwoe, cmna => qr);
  
  -- Single-driven assignments
  bjzdxhh <= TRUE;
  yhps <= (others => 4_1_1.4_0_0_2);
  
  -- Multi-driven assignments
  dmiaekbhay <= 'L';
  dmiaekbhay <= '1';
  dmiaekbhay <= 'U';
end fdknstyi;



-- Seed after: 168866185728307882,12011142928354116943
