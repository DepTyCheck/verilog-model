-- Seed: 17847355312057198756,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity szcavecmdm is
  port (r : out std_logic_vector(4 downto 0); i : out std_logic_vector(0 downto 2); lrabfgtt : out character);
end szcavecmdm;

architecture k of szcavecmdm is
  
begin
  -- Single-driven assignments
  lrabfgtt <= 'l';
  
  -- Multi-driven assignments
  i <= "";
  i <= i;
  r <= r;
end k;

entity wotefvub is
  port (ub : linkage time);
end wotefvub;

library ieee;
use ieee.std_logic_1164.all;

architecture crgkgydypm of wotefvub is
  signal eox : character;
  signal omyrxhwqdo : std_logic_vector(0 downto 2);
  signal y : character;
  signal unyaqjwdww : std_logic_vector(4 downto 0);
  signal fwef : character;
  signal ydnhj : character;
  signal ovfropftxo : std_logic_vector(0 downto 2);
  signal evj : std_logic_vector(4 downto 0);
begin
  sqmpkcmb : entity work.szcavecmdm
    port map (r => evj, i => ovfropftxo, lrabfgtt => ydnhj);
  oymic : entity work.szcavecmdm
    port map (r => evj, i => ovfropftxo, lrabfgtt => fwef);
  kthqnwi : entity work.szcavecmdm
    port map (r => unyaqjwdww, i => ovfropftxo, lrabfgtt => y);
  jqcwf : entity work.szcavecmdm
    port map (r => unyaqjwdww, i => omyrxhwqdo, lrabfgtt => eox);
  
  -- Multi-driven assignments
  omyrxhwqdo <= omyrxhwqdo;
  omyrxhwqdo <= (others => '0');
  ovfropftxo <= "";
  evj <= ('0', 'H', 'U', 'Z', 'Z');
end crgkgydypm;



-- Seed after: 3863631028370142311,13501862637168280927
