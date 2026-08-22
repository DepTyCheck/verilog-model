-- Seed: 14181717837969998916,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity njmg is
  port (zlrhejbcwt : buffer boolean_vector(2 downto 0); gkwlgbl : in time; hcxoerfbk : out std_logic_vector(2 downto 1));
end njmg;

architecture ktuwhl of njmg is
  
begin
  -- Single-driven assignments
  zlrhejbcwt <= (FALSE, FALSE, FALSE);
  
  -- Multi-driven assignments
  hcxoerfbk <= ('X', '-');
  hcxoerfbk <= hcxoerfbk;
  hcxoerfbk <= hcxoerfbk;
  hcxoerfbk <= hcxoerfbk;
end ktuwhl;

library ieee;
use ieee.std_logic_1164.all;

entity tiecudm is
  port (s : in std_logic; xuevpkttwn : linkage boolean_vector(1 to 1); znrvfo : out integer);
end tiecudm;

library ieee;
use ieee.std_logic_1164.all;

architecture jmnp of tiecudm is
  signal yvlb : std_logic_vector(2 downto 1);
  signal vwplr : time;
  signal iz : boolean_vector(2 downto 0);
begin
  zhtxxc : entity work.njmg
    port map (zlrhejbcwt => iz, gkwlgbl => vwplr, hcxoerfbk => yvlb);
  
  -- Single-driven assignments
  znrvfo <= 3133;
  vwplr <= 2#1_1_0_0# ps;
  
  -- Multi-driven assignments
  yvlb <= "XU";
  yvlb <= yvlb;
end jmnp;

entity oxjx is
  port (ej : out integer);
end oxjx;

library ieee;
use ieee.std_logic_1164.all;

architecture caept of oxjx is
  signal t : std_logic_vector(2 downto 1);
  signal twt : time;
  signal ysf : boolean_vector(2 downto 0);
  signal ftpeqpo : std_logic_vector(2 downto 1);
  signal ntsnznqqk : time;
  signal ijbqkyxxsl : boolean_vector(2 downto 0);
begin
  elncbp : entity work.njmg
    port map (zlrhejbcwt => ijbqkyxxsl, gkwlgbl => ntsnznqqk, hcxoerfbk => ftpeqpo);
  g : entity work.njmg
    port map (zlrhejbcwt => ysf, gkwlgbl => twt, hcxoerfbk => t);
  
  -- Single-driven assignments
  ej <= 16#E4F#;
  ntsnznqqk <= ntsnznqqk;
  twt <= 8#7# ps;
  
  -- Multi-driven assignments
  ftpeqpo <= "ZW";
end caept;



-- Seed after: 12247934226452760115,5805648483995786113
