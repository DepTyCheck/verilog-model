-- Seed: 5748525826813681553,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity oyjaaqqibu is
  port (odiwznc : in std_logic_vector(4 to 3); pnyprckbv : linkage std_logic_vector(4 downto 0));
end oyjaaqqibu;

architecture cqnaxssltt of oyjaaqqibu is
  
begin
  
end cqnaxssltt;

entity bqbedbsa is
  port (adigoq : inout real);
end bqbedbsa;

library ieee;
use ieee.std_logic_1164.all;

architecture s of bqbedbsa is
  signal bryxcle : std_logic_vector(4 downto 0);
  signal ddje : std_logic_vector(4 to 3);
  signal vbepcba : std_logic_vector(4 downto 0);
  signal kv : std_logic_vector(4 to 3);
begin
  thxc : entity work.oyjaaqqibu
    port map (odiwznc => kv, pnyprckbv => vbepcba);
  sgga : entity work.oyjaaqqibu
    port map (odiwznc => ddje, pnyprckbv => bryxcle);
  pxlklfnlb : entity work.oyjaaqqibu
    port map (odiwznc => kv, pnyprckbv => vbepcba);
  
  -- Single-driven assignments
  adigoq <= 1_4_3_0_4.31;
  
  -- Multi-driven assignments
  kv <= (others => '0');
  kv <= (others => '0');
end s;



-- Seed after: 15708146871275825782,1112937151005418631
