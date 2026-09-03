-- Seed: 3424508900098032299,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity gmxbcst is
  port (kigfm : linkage std_logic_vector(3 downto 0); wu : buffer time_vector(3 to 1); vfwxpu : out std_logic; eqbdebqzsp : inout std_logic);
end gmxbcst;

architecture ox of gmxbcst is
  
begin
  
end ox;

entity yxvznw is
  port (dki : linkage time; mc : linkage time);
end yxvznw;

library ieee;
use ieee.std_logic_1164.all;

architecture armzia of yxvznw is
  signal ssucvy : time_vector(3 to 1);
  signal dcyudxkdb : std_logic;
  signal gxgb : time_vector(3 to 1);
  signal dp : std_logic_vector(3 downto 0);
  signal kguupnjjwn : std_logic;
  signal yetb : std_logic;
  signal xvirrk : time_vector(3 to 1);
  signal fbzkpw : std_logic_vector(3 downto 0);
begin
  mtsnsrrst : entity work.gmxbcst
    port map (kigfm => fbzkpw, wu => xvirrk, vfwxpu => yetb, eqbdebqzsp => kguupnjjwn);
  yyg : entity work.gmxbcst
    port map (kigfm => dp, wu => gxgb, vfwxpu => dcyudxkdb, eqbdebqzsp => yetb);
  sgaj : entity work.gmxbcst
    port map (kigfm => dp, wu => ssucvy, vfwxpu => yetb, eqbdebqzsp => yetb);
  
  -- Multi-driven assignments
  fbzkpw <= dp;
  kguupnjjwn <= 'U';
  kguupnjjwn <= yetb;
end armzia;



-- Seed after: 12713971055528757743,11127274767545411571
