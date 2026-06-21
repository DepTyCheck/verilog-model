-- Seed: 2563261835512045233,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity foqhnlwit is
  port (jl : out std_logic_vector(3 to 0));
end foqhnlwit;

architecture yjrik of foqhnlwit is
  
begin
  
end yjrik;

entity zjx is
  port (daxrfsagh : out bit);
end zjx;

library ieee;
use ieee.std_logic_1164.all;

architecture lapn of zjx is
  signal rwdyxjcze : std_logic_vector(3 to 0);
  signal evffxkzzav : std_logic_vector(3 to 0);
begin
  vdfs : entity work.foqhnlwit
    port map (jl => evffxkzzav);
  zbp : entity work.foqhnlwit
    port map (jl => rwdyxjcze);
  
  -- Single-driven assignments
  daxrfsagh <= '1';
  
  -- Multi-driven assignments
  evffxkzzav <= (others => '0');
  evffxkzzav <= (others => '0');
  evffxkzzav <= "";
  evffxkzzav <= (others => '0');
end lapn;



-- Seed after: 12097109476652787915,3687118713772291287
