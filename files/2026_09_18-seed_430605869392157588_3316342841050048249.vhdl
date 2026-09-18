-- Seed: 430605869392157588,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity psv is
  port (aqo : in severity_level; xqfc : inout boolean_vector(0 downto 0); zgjt : buffer std_logic);
end psv;

architecture kzyalc of psv is
  
begin
  -- Single-driven assignments
  xqfc <= xqfc;
  
  -- Multi-driven assignments
  zgjt <= zgjt;
  zgjt <= zgjt;
  zgjt <= zgjt;
end kzyalc;

library ieee;
use ieee.std_logic_1164.all;

entity xteam is
  port (xoodngi : out std_logic_vector(0 downto 3); lvfztbdsdp : buffer real; nqe : buffer std_logic_vector(4 downto 1));
end xteam;

library ieee;
use ieee.std_logic_1164.all;

architecture xuchm of xteam is
  signal vpbvdo : std_logic;
  signal pufrbxk : boolean_vector(0 downto 0);
  signal khr : severity_level;
  signal giq : boolean_vector(0 downto 0);
  signal dkksplebq : severity_level;
  signal g : std_logic;
  signal je : boolean_vector(0 downto 0);
  signal niwq : std_logic;
  signal wjis : boolean_vector(0 downto 0);
  signal dec : severity_level;
begin
  cdkl : entity work.psv
    port map (aqo => dec, xqfc => wjis, zgjt => niwq);
  w : entity work.psv
    port map (aqo => dec, xqfc => je, zgjt => g);
  flup : entity work.psv
    port map (aqo => dkksplebq, xqfc => giq, zgjt => g);
  vzcwlfbye : entity work.psv
    port map (aqo => khr, xqfc => pufrbxk, zgjt => vpbvdo);
  
  -- Single-driven assignments
  lvfztbdsdp <= 2#1100.1#;
  dec <= ERROR;
  dkksplebq <= dkksplebq;
  khr <= WARNING;
  
  -- Multi-driven assignments
  nqe <= nqe;
end xuchm;



-- Seed after: 1844839369898016358,3316342841050048249
