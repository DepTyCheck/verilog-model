-- Seed: 14137782981262456321,4292249356257567981

entity kftq is
  port (vqbenwj : in time_vector(1 downto 4));
end kftq;

architecture qzoihsft of kftq is
  
begin
  
end qzoihsft;

entity v is
  port (zbokdlgm : in real);
end v;

architecture uvtrlqjam of v is
  signal ortbnnrdg : time_vector(1 downto 4);
  signal ur : time_vector(1 downto 4);
begin
  fwuykgaqx : entity work.kftq
    port map (vqbenwj => ur);
  u : entity work.kftq
    port map (vqbenwj => ortbnnrdg);
  
  -- Single-driven assignments
  ortbnnrdg <= (others => 0 ns);
  ur <= (others => 0 ns);
end uvtrlqjam;

library ieee;
use ieee.std_logic_1164.all;

entity udwykuvc is
  port (jdkce : linkage std_logic_vector(0 to 0); bwihhrr : linkage boolean; jxbsvucn : buffer boolean; ugx : buffer real_vector(2 downto 0));
end udwykuvc;

architecture ktjwgkowff of udwykuvc is
  signal jwfn : real;
  signal mnpj : time_vector(1 downto 4);
begin
  e : entity work.kftq
    port map (vqbenwj => mnpj);
  ykhwvaqrl : entity work.v
    port map (zbokdlgm => jwfn);
  b : entity work.kftq
    port map (vqbenwj => mnpj);
  
  -- Single-driven assignments
  jxbsvucn <= jxbsvucn;
end ktjwgkowff;



-- Seed after: 1525510913245853070,4292249356257567981
