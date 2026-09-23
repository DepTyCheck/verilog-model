-- Seed: 6367397181061747950,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity ojmzv is
  port (wbqlv : in time; pf : in std_logic; osmnt : buffer std_logic_vector(4 to 1));
end ojmzv;

architecture nopvsi of ojmzv is
  
begin
  -- Multi-driven assignments
  osmnt <= osmnt;
end nopvsi;

entity nvwhoedx is
  port (sbdobu : buffer time; zwol : in time; czqjgvdmn : buffer bit_vector(2 downto 4));
end nvwhoedx;

library ieee;
use ieee.std_logic_1164.all;

architecture qzoyhlm of nvwhoedx is
  signal hm : std_logic_vector(4 to 1);
  signal dnjbriaea : time;
  signal slffdusfah : std_logic;
  signal qkaxfxxbt : time;
  signal rddnecdeq : std_logic;
  signal dldw : time;
  signal u : std_logic_vector(4 to 1);
  signal ovsgslgyho : std_logic;
  signal vtvmjhqh : time;
begin
  prp : entity work.ojmzv
    port map (wbqlv => vtvmjhqh, pf => ovsgslgyho, osmnt => u);
  dre : entity work.ojmzv
    port map (wbqlv => dldw, pf => rddnecdeq, osmnt => u);
  mjxbb : entity work.ojmzv
    port map (wbqlv => qkaxfxxbt, pf => slffdusfah, osmnt => u);
  cgwwodpqu : entity work.ojmzv
    port map (wbqlv => dnjbriaea, pf => ovsgslgyho, osmnt => hm);
  
  -- Single-driven assignments
  sbdobu <= 3.3_4_4_1_1 ps;
  dldw <= qkaxfxxbt;
  vtvmjhqh <= 14 ns;
  czqjgvdmn <= (others => '0');
  
  -- Multi-driven assignments
  ovsgslgyho <= '1';
  slffdusfah <= 'H';
end qzoyhlm;



-- Seed after: 2615692211066094144,8067602802092121131
