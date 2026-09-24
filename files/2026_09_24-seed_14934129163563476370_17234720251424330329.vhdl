-- Seed: 14934129163563476370,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity jnmesnvv is
  port (behrekdc : inout std_logic; bstg : out std_logic_vector(3 downto 1); qyx : buffer severity_level);
end jnmesnvv;

architecture lwa of jnmesnvv is
  
begin
  -- Multi-driven assignments
  bstg <= ('U', 'X', 'U');
  bstg <= "UZX";
  bstg <= "LHH";
end lwa;

entity s is
  port (xwvcr : out time);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture ribyg of s is
  signal iumsfgnw : severity_level;
  signal dekxdsaajl : std_logic_vector(3 downto 1);
  signal cxvnb : severity_level;
  signal cbqoec : std_logic_vector(3 downto 1);
  signal rmthxnuwam : std_logic;
begin
  jpwxvzvqa : entity work.jnmesnvv
    port map (behrekdc => rmthxnuwam, bstg => cbqoec, qyx => cxvnb);
  och : entity work.jnmesnvv
    port map (behrekdc => rmthxnuwam, bstg => dekxdsaajl, qyx => iumsfgnw);
  
  -- Single-driven assignments
  xwvcr <= xwvcr;
  
  -- Multi-driven assignments
  rmthxnuwam <= '0';
  rmthxnuwam <= 'W';
  cbqoec <= "HH-";
end ribyg;



-- Seed after: 16214362566416363497,17234720251424330329
