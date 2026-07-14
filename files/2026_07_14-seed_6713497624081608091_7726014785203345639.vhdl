-- Seed: 6713497624081608091,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity ihaeta is
  port (us : out std_logic_vector(3 downto 3));
end ihaeta;

architecture zt of ihaeta is
  
begin
  -- Multi-driven assignments
  us <= us;
  us <= us;
  us <= (others => '-');
  us <= us;
end zt;

entity ekd is
  port (zwiooykoct : linkage bit);
end ekd;

library ieee;
use ieee.std_logic_1164.all;

architecture pwzccu of ekd is
  signal jhdpnzg : std_logic_vector(3 downto 3);
  signal nreg : std_logic_vector(3 downto 3);
  signal nukv : std_logic_vector(3 downto 3);
begin
  yzf : entity work.ihaeta
    port map (us => nukv);
  tquogw : entity work.ihaeta
    port map (us => nreg);
  w : entity work.ihaeta
    port map (us => jhdpnzg);
  
  -- Multi-driven assignments
  nukv <= nreg;
  nukv <= (others => 'H');
  nukv <= jhdpnzg;
end pwzccu;



-- Seed after: 3579329793565134537,7726014785203345639
