-- Seed: 14793150185012876676,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity qhgio is
  port (eiqkjav : inout std_logic_vector(4 to 4); vkqbje : out std_logic; hhjwtpczn : in time);
end qhgio;

architecture kz of qhgio is
  
begin
  -- Multi-driven assignments
  vkqbje <= 'H';
  vkqbje <= 'U';
  vkqbje <= 'U';
end kz;

entity d is
  port (cwpbcl : out severity_level; bx : out integer);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture ykbqqlr of d is
  signal ihaowtkhp : time;
  signal rmfmticu : std_logic_vector(4 to 4);
  signal groscqxyvl : time;
  signal sromtkip : std_logic;
  signal zzz : std_logic_vector(4 to 4);
begin
  ucxbdj : entity work.qhgio
    port map (eiqkjav => zzz, vkqbje => sromtkip, hhjwtpczn => groscqxyvl);
  nyvcksrlxz : entity work.qhgio
    port map (eiqkjav => zzz, vkqbje => sromtkip, hhjwtpczn => groscqxyvl);
  k : entity work.qhgio
    port map (eiqkjav => rmfmticu, vkqbje => sromtkip, hhjwtpczn => ihaowtkhp);
  
  -- Single-driven assignments
  groscqxyvl <= 1 hr;
  ihaowtkhp <= 2#101# ns;
  cwpbcl <= ERROR;
  bx <= 2#01111#;
  
  -- Multi-driven assignments
  zzz <= (others => 'W');
  zzz <= (others => 'H');
  zzz <= (others => 'Z');
  zzz <= "0";
end ykbqqlr;



-- Seed after: 8664989887904875861,3924983747739634027
