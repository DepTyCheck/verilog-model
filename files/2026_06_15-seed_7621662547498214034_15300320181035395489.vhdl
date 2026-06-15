-- Seed: 7621662547498214034,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity ql is
  port (ksg : in severity_level; hoe : inout std_logic; sxt : in std_logic_vector(2 downto 4));
end ql;

architecture xuwqo of ql is
  
begin
  -- Multi-driven assignments
  hoe <= '1';
  hoe <= 'H';
  hoe <= '0';
  hoe <= '1';
end xuwqo;

library ieee;
use ieee.std_logic_1164.all;

entity ogjnp is
  port (mc : in std_logic; pzal : out boolean_vector(4 downto 1); atvklwh : out std_logic_vector(0 to 1));
end ogjnp;

library ieee;
use ieee.std_logic_1164.all;

architecture uhmhuyrrk of ogjnp is
  signal udastffryi : std_logic_vector(2 downto 4);
  signal nsxsbpflg : severity_level;
  signal wijp : std_logic_vector(2 downto 4);
  signal tfkrfkf : std_logic;
  signal jvfi : severity_level;
begin
  vusosqvrif : entity work.ql
    port map (ksg => jvfi, hoe => tfkrfkf, sxt => wijp);
  pymankx : entity work.ql
    port map (ksg => nsxsbpflg, hoe => tfkrfkf, sxt => udastffryi);
  
  -- Single-driven assignments
  nsxsbpflg <= WARNING;
  jvfi <= FAILURE;
  pzal <= (FALSE, TRUE, TRUE, TRUE);
  
  -- Multi-driven assignments
  tfkrfkf <= '-';
  wijp <= "";
  atvklwh <= "HW";
end uhmhuyrrk;



-- Seed after: 16467981050122539020,15300320181035395489
