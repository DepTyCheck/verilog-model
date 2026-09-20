-- Seed: 16933628165395826790,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity iji is
  port (jqrvhvp : buffer std_logic; eqi : out boolean_vector(0 downto 3); okjgqip : buffer std_logic);
end iji;

architecture tua of iji is
  
begin
  -- Single-driven assignments
  eqi <= (others => TRUE);
  
  -- Multi-driven assignments
  jqrvhvp <= '0';
  jqrvhvp <= '1';
  okjgqip <= okjgqip;
  okjgqip <= 'H';
end tua;

entity si is
  port (gmdl : out severity_level; w : inout time);
end si;

library ieee;
use ieee.std_logic_1164.all;

architecture ys of si is
  signal hciek : std_logic;
  signal cstzbs : boolean_vector(0 downto 3);
  signal x : std_logic;
  signal tv : boolean_vector(0 downto 3);
  signal ingft : std_logic;
begin
  vsvxvdf : entity work.iji
    port map (jqrvhvp => ingft, eqi => tv, okjgqip => ingft);
  vdy : entity work.iji
    port map (jqrvhvp => x, eqi => cstzbs, okjgqip => hciek);
  
  -- Single-driven assignments
  w <= w;
  gmdl <= WARNING;
  
  -- Multi-driven assignments
  hciek <= '0';
  x <= ingft;
  ingft <= ingft;
end ys;



-- Seed after: 10119296879931412382,18037650846010261179
