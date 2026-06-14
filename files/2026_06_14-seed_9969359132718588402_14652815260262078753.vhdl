-- Seed: 9969359132718588402,14652815260262078753

entity ybcxbf is
  port (eyhvjq : in string(4 downto 2); xzufmyf : linkage integer_vector(4 to 3); cyasx : linkage time; eam : buffer string(2 to 1));
end ybcxbf;

architecture vkxonam of ybcxbf is
  
begin
  
end vkxonam;

library ieee;
use ieee.std_logic_1164.all;

entity hxv is
  port (icuymer : in std_logic_vector(0 downto 1));
end hxv;

architecture zb of hxv is
  
begin
  
end zb;

entity vrig is
  port (tzqz : out real_vector(0 to 4));
end vrig;

architecture bw of vrig is
  signal cydtrrddpr : string(2 to 1);
  signal qzeznr : time;
  signal tllds : integer_vector(4 to 3);
  signal hjadtsuza : string(4 downto 2);
  signal s : string(2 to 1);
  signal napd : time;
  signal frq : integer_vector(4 to 3);
  signal sdrrayl : string(2 to 1);
  signal hrxsz : time;
  signal eehk : integer_vector(4 to 3);
  signal r : string(4 downto 2);
begin
  wqo : entity work.ybcxbf
    port map (eyhvjq => r, xzufmyf => eehk, cyasx => hrxsz, eam => sdrrayl);
  k : entity work.ybcxbf
    port map (eyhvjq => r, xzufmyf => frq, cyasx => napd, eam => s);
  qyjgs : entity work.ybcxbf
    port map (eyhvjq => hjadtsuza, xzufmyf => tllds, cyasx => qzeznr, eam => cydtrrddpr);
  
  -- Single-driven assignments
  r <= ('c', 'o', 'a');
  hjadtsuza <= ('p', 's', 'f');
  tzqz <= (1.201, 2#0000.00100#, 8#2_6.4630#, 8#0_2_2_4_0.55#, 8#7_6_4.32#);
end bw;



-- Seed after: 3578199668773334650,14652815260262078753
