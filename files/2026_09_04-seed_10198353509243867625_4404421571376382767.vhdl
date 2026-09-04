-- Seed: 10198353509243867625,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity yya is
  port (m : buffer bit; lboxmi : inout std_logic_vector(1 downto 3); nym : in bit);
end yya;

architecture b of yya is
  
begin
  -- Single-driven assignments
  m <= '0';
  
  -- Multi-driven assignments
  lboxmi <= "";
  lboxmi <= (others => '0');
  lboxmi <= lboxmi;
  lboxmi <= lboxmi;
end b;

library ieee;
use ieee.std_logic_1164.all;

entity vbfgystgf is
  port (adiwh : out integer_vector(1 to 2); zyw : linkage bit_vector(4 downto 3); h : out std_logic_vector(4 to 0));
end vbfgystgf;

library ieee;
use ieee.std_logic_1164.all;

architecture hy of vbfgystgf is
  signal nsoyyp : std_logic_vector(1 downto 3);
  signal ekmjwxmjr : bit;
  signal duycclg : bit;
  signal oxov : bit;
  signal obbkpex : std_logic_vector(1 downto 3);
  signal retjw : bit;
begin
  iwabpjgl : entity work.yya
    port map (m => retjw, lboxmi => obbkpex, nym => oxov);
  ztr : entity work.yya
    port map (m => oxov, lboxmi => h, nym => duycclg);
  rzkb : entity work.yya
    port map (m => ekmjwxmjr, lboxmi => nsoyyp, nym => retjw);
  
  -- Single-driven assignments
  adiwh <= adiwh;
  
  -- Multi-driven assignments
  h <= (others => '0');
end hy;



-- Seed after: 12490551310840495089,4404421571376382767
