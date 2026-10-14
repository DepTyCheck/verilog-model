-- Seed: 10564558107462630091,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity qfvldxp is
  port (hifkcfg : buffer integer; wf : inout std_logic_vector(0 to 1); e : out bit_vector(4 to 2));
end qfvldxp;

architecture w of qfvldxp is
  
begin
  -- Single-driven assignments
  hifkcfg <= 1_1_3_3_3;
  e <= e;
  
  -- Multi-driven assignments
  wf <= wf;
  wf <= wf;
  wf <= "H0";
end w;

entity su is
  port (vkuawg : buffer real);
end su;

library ieee;
use ieee.std_logic_1164.all;

architecture ulq of su is
  signal u : bit_vector(4 to 2);
  signal ks : std_logic_vector(0 to 1);
  signal qw : integer;
  signal jgir : bit_vector(4 to 2);
  signal g : std_logic_vector(0 to 1);
  signal tyzzow : integer;
begin
  unt : entity work.qfvldxp
    port map (hifkcfg => tyzzow, wf => g, e => jgir);
  moq : entity work.qfvldxp
    port map (hifkcfg => qw, wf => ks, e => u);
  
  -- Single-driven assignments
  vkuawg <= vkuawg;
end ulq;

library ieee;
use ieee.std_logic_1164.all;

entity caxgrq is
  port (dxfqov : in std_logic; tdtgqm : inout bit_vector(3 downto 1); r : linkage std_logic_vector(3 to 0));
end caxgrq;

architecture f of caxgrq is
  
begin
  -- Single-driven assignments
  tdtgqm <= tdtgqm;
end f;



-- Seed after: 11503761220447885820,2287986438313874379
