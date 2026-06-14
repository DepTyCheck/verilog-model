-- Seed: 13808800863086047616,14652815260262078753

entity hinbwtztas is
  port (jzopinilxo : linkage real; dfqlc : out boolean);
end hinbwtztas;

architecture nbokwtji of hinbwtztas is
  
begin
  -- Single-driven assignments
  dfqlc <= TRUE;
end nbokwtji;

entity sgi is
  port (jwshsdd : buffer bit; fifuc : in integer_vector(4 downto 4));
end sgi;

architecture rfm of sgi is
  signal vsnedn : boolean;
  signal k : real;
  signal erctxvf : boolean;
  signal imasawyzt : real;
  signal gx : boolean;
  signal sm : real;
begin
  vezggmc : entity work.hinbwtztas
    port map (jzopinilxo => sm, dfqlc => gx);
  jxn : entity work.hinbwtztas
    port map (jzopinilxo => imasawyzt, dfqlc => erctxvf);
  rvfbaq : entity work.hinbwtztas
    port map (jzopinilxo => k, dfqlc => vsnedn);
  
  -- Single-driven assignments
  jwshsdd <= '1';
end rfm;

library ieee;
use ieee.std_logic_1164.all;

entity qlqah is
  port (zxufpn : linkage severity_level; yscc : inout std_logic_vector(3 to 3); c : inout real);
end qlqah;

architecture cb of qlqah is
  signal exxtbj : boolean;
  signal u : real;
begin
  aowziwvgcl : entity work.hinbwtztas
    port map (jzopinilxo => u, dfqlc => exxtbj);
  
  -- Single-driven assignments
  c <= 2#1100.010#;
  
  -- Multi-driven assignments
  yscc <= (others => '1');
  yscc <= (others => '0');
  yscc <= (others => 'Z');
  yscc <= "W";
end cb;



-- Seed after: 5169181683310112736,14652815260262078753
