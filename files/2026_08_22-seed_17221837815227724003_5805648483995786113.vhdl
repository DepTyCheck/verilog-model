-- Seed: 17221837815227724003,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity vbh is
  port (avhtxvfly : buffer std_logic_vector(2 to 3));
end vbh;

architecture cryssm of vbh is
  
begin
  
end cryssm;

entity gm is
  port (nmbavtoiyf : linkage real);
end gm;

library ieee;
use ieee.std_logic_1164.all;

architecture wlte of gm is
  signal uchsckad : std_logic_vector(2 to 3);
begin
  dut : entity work.vbh
    port map (avhtxvfly => uchsckad);
  jpx : entity work.vbh
    port map (avhtxvfly => uchsckad);
  vghhkauk : entity work.vbh
    port map (avhtxvfly => uchsckad);
  
  -- Multi-driven assignments
  uchsckad <= ('-', 'X');
  uchsckad <= uchsckad;
  uchsckad <= uchsckad;
end wlte;



-- Seed after: 9040005495936891107,5805648483995786113
