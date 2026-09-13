-- Seed: 4288927490155887343,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity opr is
  port (mixulqu : inout std_logic; uyucoo : inout boolean; nmtjtlyhyl : out bit; wwcmcvzhhj : buffer std_logic_vector(0 to 4));
end opr;

architecture yjy of opr is
  
begin
  -- Single-driven assignments
  uyucoo <= FALSE;
  nmtjtlyhyl <= '0';
  
  -- Multi-driven assignments
  wwcmcvzhhj <= wwcmcvzhhj;
  mixulqu <= 'L';
end yjy;

library ieee;
use ieee.std_logic_1164.all;

entity gep is
  port (edlyiqzpn : inout std_logic; vw : out std_logic_vector(2 to 4));
end gep;

library ieee;
use ieee.std_logic_1164.all;

architecture tuhdclrm of gep is
  signal gzhupftvhl : std_logic_vector(0 to 4);
  signal ybd : bit;
  signal cbo : boolean;
  signal wnhxumzhp : std_logic;
begin
  nqkaz : entity work.opr
    port map (mixulqu => wnhxumzhp, uyucoo => cbo, nmtjtlyhyl => ybd, wwcmcvzhhj => gzhupftvhl);
  
  -- Multi-driven assignments
  vw <= "L1U";
  vw <= ('0', 'L', '0');
end tuhdclrm;

library ieee;
use ieee.std_logic_1164.all;

entity hmleq is
  port (jitjixt : in std_logic_vector(3 to 4));
end hmleq;

architecture pbhmdel of hmleq is
  
begin
  
end pbhmdel;



-- Seed after: 7245501483633676106,10754487200446211253
