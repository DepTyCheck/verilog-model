-- Seed: 18083827919523303771,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity jho is
  port (olmzordanr : inout std_logic_vector(1 to 2); e : in std_logic_vector(1 to 2); ejsgghxm : inout std_logic_vector(4 downto 0));
end jho;

architecture tjfantckf of jho is
  
begin
  -- Multi-driven assignments
  ejsgghxm <= "1LU0H";
  olmzordanr <= e;
end tjfantckf;

library ieee;
use ieee.std_logic_1164.all;

entity wpbip is
  port (mey : in std_logic; um : inout real; dhfvqbpsgc : out real_vector(2 to 4); dspwpmpu : out real);
end wpbip;

library ieee;
use ieee.std_logic_1164.all;

architecture arkbdeu of wpbip is
  signal wqlzhumquf : std_logic_vector(4 downto 0);
  signal lnkirvvjv : std_logic_vector(1 to 2);
  signal hcwrypgylf : std_logic_vector(4 downto 0);
  signal fjg : std_logic_vector(1 to 2);
begin
  wcjowzafxw : entity work.jho
    port map (olmzordanr => fjg, e => fjg, ejsgghxm => hcwrypgylf);
  iew : entity work.jho
    port map (olmzordanr => fjg, e => fjg, ejsgghxm => hcwrypgylf);
  ardxcoqs : entity work.jho
    port map (olmzordanr => lnkirvvjv, e => fjg, ejsgghxm => wqlzhumquf);
  tfffjr : entity work.jho
    port map (olmzordanr => fjg, e => fjg, ejsgghxm => wqlzhumquf);
  
  -- Single-driven assignments
  um <= 2#1110.0#;
  dspwpmpu <= dspwpmpu;
  
  -- Multi-driven assignments
  fjg <= "0X";
  fjg <= fjg;
  lnkirvvjv <= ('0', 'W');
end arkbdeu;



-- Seed after: 13086710966613742455,6140041381800297705
