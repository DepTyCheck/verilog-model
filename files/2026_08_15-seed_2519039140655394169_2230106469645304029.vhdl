-- Seed: 2519039140655394169,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (mzhk : inout std_logic_vector(1 to 0));
end x;

architecture wtm of x is
  
begin
  
end wtm;

library ieee;
use ieee.std_logic_1164.all;

entity epmgugunr is
  port (mxsno : inout std_logic_vector(3 to 4); dfbbu : buffer time; ozpubwdll : buffer bit_vector(3 downto 2));
end epmgugunr;

library ieee;
use ieee.std_logic_1164.all;

architecture ywgdz of epmgugunr is
  signal jzhznpbfo : std_logic_vector(1 to 0);
  signal rivghik : std_logic_vector(1 to 0);
begin
  lqpepk : entity work.x
    port map (mzhk => rivghik);
  vlxaij : entity work.x
    port map (mzhk => jzhznpbfo);
  
  -- Single-driven assignments
  dfbbu <= dfbbu;
  
  -- Multi-driven assignments
  rivghik <= "";
  jzhznpbfo <= "";
  jzhznpbfo <= rivghik;
end ywgdz;

entity kcpxk is
  port (kwkc : inout integer);
end kcpxk;

library ieee;
use ieee.std_logic_1164.all;

architecture zpmowtfegy of kcpxk is
  signal usdnncuyaf : bit_vector(3 downto 2);
  signal aoziydx : time;
  signal hfngxytueb : std_logic_vector(1 to 0);
  signal pfehuoscxa : std_logic_vector(1 to 0);
  signal iesrlve : bit_vector(3 downto 2);
  signal synh : time;
  signal fbqx : std_logic_vector(3 to 4);
begin
  peulx : entity work.epmgugunr
    port map (mxsno => fbqx, dfbbu => synh, ozpubwdll => iesrlve);
  arb : entity work.x
    port map (mzhk => pfehuoscxa);
  wfywnkugjv : entity work.x
    port map (mzhk => hfngxytueb);
  ltbkewm : entity work.epmgugunr
    port map (mxsno => fbqx, dfbbu => aoziydx, ozpubwdll => usdnncuyaf);
  
  -- Single-driven assignments
  kwkc <= kwkc;
  
  -- Multi-driven assignments
  fbqx <= "XH";
  hfngxytueb <= "";
end zpmowtfegy;

library ieee;
use ieee.std_logic_1164.all;

entity yawube is
  port (yjfpitma : inout std_logic; rmhnlac : linkage std_logic; a : inout integer);
end yawube;

library ieee;
use ieee.std_logic_1164.all;

architecture mb of yawube is
  signal ihcpsceq : std_logic_vector(1 to 0);
begin
  vnwitku : entity work.x
    port map (mzhk => ihcpsceq);
  piqtf : entity work.x
    port map (mzhk => ihcpsceq);
  
  -- Multi-driven assignments
  yjfpitma <= yjfpitma;
  ihcpsceq <= ihcpsceq;
  yjfpitma <= '0';
end mb;



-- Seed after: 847487614027790316,2230106469645304029
