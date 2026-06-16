-- Seed: 13236793078794354913,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity tuwnag is
  port (vcrynkv : inout std_logic; vdilkypsw : linkage std_logic);
end tuwnag;

architecture glroi of tuwnag is
  
begin
  -- Multi-driven assignments
  vcrynkv <= '0';
end glroi;

library ieee;
use ieee.std_logic_1164.all;

entity ixazvaew is
  port (vskjdiz : inout real; lgt : linkage std_logic_vector(4 to 4); jmiazkokq : out std_logic_vector(3 to 0); yxwsmlqg : linkage std_logic);
end ixazvaew;

architecture j of ixazvaew is
  
begin
  -- Single-driven assignments
  vskjdiz <= 2#0_0_1_0.01#;
  
  -- Multi-driven assignments
  jmiazkokq <= "";
  jmiazkokq <= "";
end j;

library ieee;
use ieee.std_logic_1164.all;

entity zdvrhduc is
  port (vjisnvvr : linkage std_logic; o : buffer std_logic_vector(2 downto 2));
end zdvrhduc;

library ieee;
use ieee.std_logic_1164.all;

architecture gg of zdvrhduc is
  signal mghuwm : std_logic;
  signal hvmb : std_logic;
  signal n : std_logic;
begin
  t : entity work.tuwnag
    port map (vcrynkv => n, vdilkypsw => hvmb);
  lfamoignx : entity work.tuwnag
    port map (vcrynkv => n, vdilkypsw => n);
  wdonk : entity work.tuwnag
    port map (vcrynkv => n, vdilkypsw => mghuwm);
  
  -- Multi-driven assignments
  o <= "W";
  hvmb <= 'X';
  mghuwm <= 'L';
  o <= (others => '0');
end gg;

entity fzdseksti is
  port (z : out character; wqdcjroydt : buffer integer);
end fzdseksti;

library ieee;
use ieee.std_logic_1164.all;

architecture ito of fzdseksti is
  signal vakmgjymja : std_logic;
  signal lvb : std_logic_vector(3 to 0);
  signal buj : std_logic_vector(2 downto 2);
  signal n : real;
  signal bxucf : std_logic;
  signal gdx : std_logic;
begin
  cuffowfk : entity work.tuwnag
    port map (vcrynkv => gdx, vdilkypsw => bxucf);
  bkf : entity work.tuwnag
    port map (vcrynkv => gdx, vdilkypsw => bxucf);
  p : entity work.ixazvaew
    port map (vskjdiz => n, lgt => buj, jmiazkokq => lvb, yxwsmlqg => gdx);
  lkrcy : entity work.zdvrhduc
    port map (vjisnvvr => vakmgjymja, o => buj);
  
  -- Single-driven assignments
  wqdcjroydt <= 2_2;
  z <= 'k';
  
  -- Multi-driven assignments
  gdx <= 'X';
end ito;



-- Seed after: 4000823282656563678,5472058987609252853
