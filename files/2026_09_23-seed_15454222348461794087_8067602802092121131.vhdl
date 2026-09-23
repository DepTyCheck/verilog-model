-- Seed: 15454222348461794087,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity lx is
  port (fmm : inout std_logic_vector(4 downto 4));
end lx;

architecture h of lx is
  
begin
  -- Multi-driven assignments
  fmm <= (others => '0');
end h;

library ieee;
use ieee.std_logic_1164.all;

entity ine is
  port (ytlcbl : inout real; kpmisndbt : inout std_logic_vector(0 to 1); meyzlbxtb : out std_logic_vector(2 to 1));
end ine;

library ieee;
use ieee.std_logic_1164.all;

architecture xetgtd of ine is
  signal fdmssmt : std_logic_vector(4 downto 4);
begin
  jasa : entity work.lx
    port map (fmm => fdmssmt);
  lqtf : entity work.lx
    port map (fmm => fdmssmt);
  
  -- Single-driven assignments
  ytlcbl <= 16#8.E0F3F#;
end xetgtd;



-- Seed after: 3228115101899885638,8067602802092121131
