-- Seed: 13074417405392494446,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity tqhwlbgji is
  port (wl : out real; dgrclihbl : linkage std_logic_vector(2 downto 2));
end tqhwlbgji;

architecture zmmqygagta of tqhwlbgji is
  
begin
  -- Single-driven assignments
  wl <= 1.4;
end zmmqygagta;

entity eyprsyujrr is
  port (cwiq : buffer time);
end eyprsyujrr;

library ieee;
use ieee.std_logic_1164.all;

architecture rze of eyprsyujrr is
  signal laxl : real;
  signal kkeiclgmdy : std_logic_vector(2 downto 2);
  signal fn : real;
  signal iqmcgqa : real;
  signal cbruk : std_logic_vector(2 downto 2);
  signal q : real;
begin
  sfr : entity work.tqhwlbgji
    port map (wl => q, dgrclihbl => cbruk);
  qgtnb : entity work.tqhwlbgji
    port map (wl => iqmcgqa, dgrclihbl => cbruk);
  dwu : entity work.tqhwlbgji
    port map (wl => fn, dgrclihbl => kkeiclgmdy);
  oubaukpli : entity work.tqhwlbgji
    port map (wl => laxl, dgrclihbl => cbruk);
  
  -- Single-driven assignments
  cwiq <= 2#1.00100# us;
  
  -- Multi-driven assignments
  cbruk <= cbruk;
  cbruk <= "H";
  cbruk <= cbruk;
end rze;



-- Seed after: 13831029725776651361,8412319452373742525
