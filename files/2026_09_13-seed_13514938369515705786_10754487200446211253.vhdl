-- Seed: 13514938369515705786,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity fmjkyp is
  port (seroz : buffer std_logic; qqiur : buffer bit_vector(3 to 1));
end fmjkyp;

architecture lmlrnykuon of fmjkyp is
  
begin
  -- Multi-driven assignments
  seroz <= 'H';
end lmlrnykuon;

entity gkimppvux is
  port (hwyqvo : in boolean_vector(2 downto 3); nram : out boolean_vector(3 to 1); mfea : out boolean);
end gkimppvux;

library ieee;
use ieee.std_logic_1164.all;

architecture ckq of gkimppvux is
  signal udtpb : bit_vector(3 to 1);
  signal c : std_logic;
begin
  hgbupxw : entity work.fmjkyp
    port map (seroz => c, qqiur => udtpb);
  
  -- Single-driven assignments
  nram <= (others => TRUE);
  mfea <= mfea;
end ckq;

library ieee;
use ieee.std_logic_1164.all;

entity jrt is
  port (bxemajmd : in real; kssi : linkage std_logic_vector(4 downto 3));
end jrt;

library ieee;
use ieee.std_logic_1164.all;

architecture uwtrfpmpsm of jrt is
  signal cqlvwizzxl : bit_vector(3 to 1);
  signal mhdrtptx : bit_vector(3 to 1);
  signal ihdswd : bit_vector(3 to 1);
  signal ajdrjl : std_logic;
begin
  fv : entity work.fmjkyp
    port map (seroz => ajdrjl, qqiur => ihdswd);
  tr : entity work.fmjkyp
    port map (seroz => ajdrjl, qqiur => mhdrtptx);
  viqfyfostr : entity work.fmjkyp
    port map (seroz => ajdrjl, qqiur => cqlvwizzxl);
end uwtrfpmpsm;



-- Seed after: 14279422709050520843,10754487200446211253
