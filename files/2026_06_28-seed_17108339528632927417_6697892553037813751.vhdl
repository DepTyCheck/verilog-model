-- Seed: 17108339528632927417,6697892553037813751

entity fgxrrtw is
  port (ricjtre : inout real);
end fgxrrtw;

architecture luoislh of fgxrrtw is
  
begin
  -- Single-driven assignments
  ricjtre <= 1_1.4;
end luoislh;

library ieee;
use ieee.std_logic_1164.all;

entity eohcj is
  port (yxmw : out std_logic; aqahhmbrdm : out real);
end eohcj;

architecture v of eohcj is
  
begin
  
end v;

entity uqjc is
  port (ijrah : inout bit_vector(2 downto 3));
end uqjc;

architecture cayllwrca of uqjc is
  
begin
  
end cayllwrca;

library ieee;
use ieee.std_logic_1164.all;

entity wycx is
  port (w : buffer std_logic_vector(1 downto 0); ao : buffer time; crgocek : buffer std_logic_vector(3 to 1));
end wycx;

library ieee;
use ieee.std_logic_1164.all;

architecture q of wycx is
  signal euzuwel : real;
  signal zhqt : std_logic;
  signal sbup : real;
  signal s : bit_vector(2 downto 3);
begin
  bepsvlrpkr : entity work.uqjc
    port map (ijrah => s);
  oh : entity work.fgxrrtw
    port map (ricjtre => sbup);
  mbjgwis : entity work.eohcj
    port map (yxmw => zhqt, aqahhmbrdm => euzuwel);
  
  -- Single-driven assignments
  ao <= 1 ps;
  
  -- Multi-driven assignments
  zhqt <= '-';
  zhqt <= 'W';
  w <= ('H', 'L');
  crgocek <= (others => '0');
end q;



-- Seed after: 17160028787649347175,6697892553037813751
