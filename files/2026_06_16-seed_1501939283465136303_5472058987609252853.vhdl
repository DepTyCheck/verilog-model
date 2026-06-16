-- Seed: 1501939283465136303,5472058987609252853

entity eawqj is
  port (skty : linkage time_vector(0 to 3); vffqtz : buffer time_vector(1 downto 1));
end eawqj;

architecture pq of eawqj is
  
begin
  -- Single-driven assignments
  vffqtz <= (others => 0 hr);
end pq;

library ieee;
use ieee.std_logic_1164.all;

entity zsucrlxlkb is
  port (xh : linkage std_logic_vector(3 downto 1); zuxvwwilyc : out real; jwme : buffer std_logic_vector(0 to 0));
end zsucrlxlkb;

architecture wlhakmgo of zsucrlxlkb is
  signal vcoua : time_vector(1 downto 1);
  signal opadctek : time_vector(0 to 3);
  signal srx : time_vector(1 downto 1);
  signal j : time_vector(0 to 3);
begin
  oljnrvdc : entity work.eawqj
    port map (skty => j, vffqtz => srx);
  icsgmtgmt : entity work.eawqj
    port map (skty => opadctek, vffqtz => vcoua);
  
  -- Single-driven assignments
  zuxvwwilyc <= 2#1110.1_1_0_0#;
  
  -- Multi-driven assignments
  jwme <= "H";
  jwme <= (others => 'H');
  jwme <= (others => '1');
  jwme <= (others => 'H');
end wlhakmgo;



-- Seed after: 2174161578662080214,5472058987609252853
