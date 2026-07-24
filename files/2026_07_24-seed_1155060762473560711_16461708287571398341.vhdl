-- Seed: 1155060762473560711,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity bgqvm is
  port (hst : inout std_logic; atcu : in severity_level; adng : inout std_logic; r : in character);
end bgqvm;

architecture kvh of bgqvm is
  
begin
  -- Multi-driven assignments
  adng <= '1';
  adng <= 'U';
end kvh;

entity plazi is
  port (eyyklsrk : out time; ybk : out time);
end plazi;

library ieee;
use ieee.std_logic_1164.all;

architecture toqzxckdco of plazi is
  signal j : character;
  signal h : std_logic;
  signal shdskzd : severity_level;
  signal grmvqj : std_logic;
  signal hxujhasyul : character;
  signal guktzlz : std_logic;
  signal pzk : severity_level;
  signal tpmpanaxpq : std_logic;
begin
  nswhbr : entity work.bgqvm
    port map (hst => tpmpanaxpq, atcu => pzk, adng => guktzlz, r => hxujhasyul);
  eewq : entity work.bgqvm
    port map (hst => tpmpanaxpq, atcu => pzk, adng => grmvqj, r => hxujhasyul);
  lyw : entity work.bgqvm
    port map (hst => grmvqj, atcu => shdskzd, adng => tpmpanaxpq, r => hxujhasyul);
  xk : entity work.bgqvm
    port map (hst => tpmpanaxpq, atcu => pzk, adng => h, r => j);
  
  -- Multi-driven assignments
  grmvqj <= tpmpanaxpq;
  grmvqj <= tpmpanaxpq;
end toqzxckdco;



-- Seed after: 3713091301256089528,16461708287571398341
