-- Seed: 18306846029228821622,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity glht is
  port (vwhunn : out real; ejmsdvhj : out std_logic);
end glht;

architecture rpnlpom of glht is
  
begin
  -- Single-driven assignments
  vwhunn <= vwhunn;
  
  -- Multi-driven assignments
  ejmsdvhj <= '1';
  ejmsdvhj <= '-';
  ejmsdvhj <= ejmsdvhj;
end rpnlpom;

entity xhgjmjoj is
  port (chqmv : in integer; qf : out severity_level; lxst : in severity_level; iv : out time_vector(3 to 0));
end xhgjmjoj;

library ieee;
use ieee.std_logic_1164.all;

architecture pe of xhgjmjoj is
  signal azagusmv : real;
  signal oy : real;
  signal bvwjikabwq : real;
  signal vkylg : std_logic;
  signal zzzgizrqhw : real;
begin
  iheovipz : entity work.glht
    port map (vwhunn => zzzgizrqhw, ejmsdvhj => vkylg);
  ypfkcnv : entity work.glht
    port map (vwhunn => bvwjikabwq, ejmsdvhj => vkylg);
  j : entity work.glht
    port map (vwhunn => oy, ejmsdvhj => vkylg);
  itpaxujhhn : entity work.glht
    port map (vwhunn => azagusmv, ejmsdvhj => vkylg);
  
  -- Single-driven assignments
  qf <= lxst;
  iv <= (others => 0 ns);
  
  -- Multi-driven assignments
  vkylg <= 'W';
  vkylg <= vkylg;
  vkylg <= 'H';
end pe;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (comugair : linkage std_logic_vector(3 downto 2));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture lrzono of r is
  signal cocduvul : time_vector(3 to 0);
  signal vsbuauauuq : severity_level;
  signal orfos : severity_level;
  signal cuzaeqdst : integer;
  signal ks : std_logic;
  signal xh : real;
begin
  nedkxkc : entity work.glht
    port map (vwhunn => xh, ejmsdvhj => ks);
  swzxjvgfu : entity work.xhgjmjoj
    port map (chqmv => cuzaeqdst, qf => orfos, lxst => vsbuauauuq, iv => cocduvul);
  
  -- Single-driven assignments
  vsbuauauuq <= WARNING;
  cuzaeqdst <= cuzaeqdst;
  
  -- Multi-driven assignments
  ks <= '0';
end lrzono;



-- Seed after: 15133841759033044810,5906004015519833893
