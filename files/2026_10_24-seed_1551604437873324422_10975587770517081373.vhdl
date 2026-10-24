-- Seed: 1551604437873324422,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity ltikexkyh is
  port (ahqlruwfnl : linkage std_logic_vector(2 downto 4));
end ltikexkyh;

architecture maumtygz of ltikexkyh is
  
begin
  
end maumtygz;

entity tpvg is
  port (g : out integer);
end tpvg;

library ieee;
use ieee.std_logic_1164.all;

architecture wrc of tpvg is
  signal ceohlblr : std_logic_vector(2 downto 4);
  signal enholy : std_logic_vector(2 downto 4);
begin
  oiur : entity work.ltikexkyh
    port map (ahqlruwfnl => enholy);
  c : entity work.ltikexkyh
    port map (ahqlruwfnl => ceohlblr);
  
  -- Multi-driven assignments
  enholy <= "";
  enholy <= (others => '0');
  ceohlblr <= enholy;
  enholy <= enholy;
end wrc;

library ieee;
use ieee.std_logic_1164.all;

entity oaqnz is
  port (xgp : inout bit; ws : in std_logic_vector(2 to 0));
end oaqnz;

library ieee;
use ieee.std_logic_1164.all;

architecture a of oaqnz is
  signal hxffakdt : std_logic_vector(2 downto 4);
begin
  tcabim : entity work.ltikexkyh
    port map (ahqlruwfnl => hxffakdt);
  nq : entity work.ltikexkyh
    port map (ahqlruwfnl => ws);
  vioxeunykm : entity work.ltikexkyh
    port map (ahqlruwfnl => ws);
  
  -- Single-driven assignments
  xgp <= xgp;
  
  -- Multi-driven assignments
  hxffakdt <= ws;
end a;



-- Seed after: 15490169538870273502,10975587770517081373
