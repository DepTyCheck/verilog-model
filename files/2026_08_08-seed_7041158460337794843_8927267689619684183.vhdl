-- Seed: 7041158460337794843,8927267689619684183

entity rgbeybdb is
  port (bisstsnt : buffer bit_vector(2 downto 1); jzksci : in real_vector(0 downto 3); uxshcl : in real);
end rgbeybdb;

architecture fw of rgbeybdb is
  
begin
  -- Single-driven assignments
  bisstsnt <= bisstsnt;
end fw;

library ieee;
use ieee.std_logic_1164.all;

entity kgx is
  port (rlfxbrk : in std_logic_vector(4 downto 4));
end kgx;

architecture hlhs of kgx is
  signal nqleqjijmw : real_vector(0 downto 3);
  signal jof : bit_vector(2 downto 1);
  signal o : real_vector(0 downto 3);
  signal twhqwys : bit_vector(2 downto 1);
  signal yvcvzto : real_vector(0 downto 3);
  signal echtklfnb : bit_vector(2 downto 1);
  signal q : real;
  signal lsfv : real_vector(0 downto 3);
  signal rwwdicc : bit_vector(2 downto 1);
begin
  smpefzqyq : entity work.rgbeybdb
    port map (bisstsnt => rwwdicc, jzksci => lsfv, uxshcl => q);
  riiz : entity work.rgbeybdb
    port map (bisstsnt => echtklfnb, jzksci => yvcvzto, uxshcl => q);
  mwrlxf : entity work.rgbeybdb
    port map (bisstsnt => twhqwys, jzksci => o, uxshcl => q);
  mgay : entity work.rgbeybdb
    port map (bisstsnt => jof, jzksci => nqleqjijmw, uxshcl => q);
end hlhs;



-- Seed after: 2045147338810417862,8927267689619684183
