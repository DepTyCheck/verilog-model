-- Seed: 8775215637648674795,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity jzwu is
  port (mpyfalypm : inout std_logic_vector(4 to 3); wjabkag : out time; lh : inout std_logic_vector(2 downto 0));
end jzwu;

architecture lcsbp of jzwu is
  
begin
  -- Single-driven assignments
  wjabkag <= wjabkag;
  
  -- Multi-driven assignments
  lh <= lh;
  mpyfalypm <= "";
end lcsbp;

entity oejjpchdw is
  port (viix : out integer_vector(2 downto 2));
end oejjpchdw;

library ieee;
use ieee.std_logic_1164.all;

architecture bzyhh of oejjpchdw is
  signal dqsjqmnru : time;
  signal mcygaq : std_logic_vector(2 downto 0);
  signal hpje : time;
  signal vtjvl : std_logic_vector(4 to 3);
  signal ui : std_logic_vector(2 downto 0);
  signal kpcaoyjp : time;
  signal cbedqrv : std_logic_vector(4 to 3);
begin
  hlguvzor : entity work.jzwu
    port map (mpyfalypm => cbedqrv, wjabkag => kpcaoyjp, lh => ui);
  iarg : entity work.jzwu
    port map (mpyfalypm => vtjvl, wjabkag => hpje, lh => mcygaq);
  swzhfoe : entity work.jzwu
    port map (mpyfalypm => vtjvl, wjabkag => dqsjqmnru, lh => ui);
  
  -- Single-driven assignments
  viix <= (others => 0);
  
  -- Multi-driven assignments
  cbedqrv <= cbedqrv;
  mcygaq <= ui;
  vtjvl <= cbedqrv;
end bzyhh;



-- Seed after: 1910922627933438218,4177195558088809003
