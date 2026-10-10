-- Seed: 1735987579318075044,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity botmdp is
  port (qztdlucdro : linkage real; lssmjumhdw : linkage std_logic; vf : linkage std_logic);
end botmdp;

architecture tfy of botmdp is
  
begin
  
end tfy;

entity hnzf is
  port (cd : buffer boolean_vector(4 downto 3));
end hnzf;

library ieee;
use ieee.std_logic_1164.all;

architecture ynozpqg of hnzf is
  signal kibmijopx : std_logic;
  signal njvm : real;
  signal zmwfjzp : std_logic;
  signal miuuxk : std_logic;
  signal ysb : real;
begin
  hgta : entity work.botmdp
    port map (qztdlucdro => ysb, lssmjumhdw => miuuxk, vf => zmwfjzp);
  iemrkce : entity work.botmdp
    port map (qztdlucdro => njvm, lssmjumhdw => kibmijopx, vf => miuuxk);
  
  -- Single-driven assignments
  cd <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  zmwfjzp <= '1';
  miuuxk <= '0';
end ynozpqg;

entity fkdierpin is
  port (jwgecixo : in bit; apcapxl : in integer);
end fkdierpin;

library ieee;
use ieee.std_logic_1164.all;

architecture kmwmokboq of fkdierpin is
  signal gkr : real;
  signal k : real;
  signal fdyegizr : std_logic;
  signal sgdrlwwf : std_logic;
  signal aopo : real;
begin
  curqphxkna : entity work.botmdp
    port map (qztdlucdro => aopo, lssmjumhdw => sgdrlwwf, vf => fdyegizr);
  jnqnw : entity work.botmdp
    port map (qztdlucdro => k, lssmjumhdw => sgdrlwwf, vf => sgdrlwwf);
  vw : entity work.botmdp
    port map (qztdlucdro => gkr, lssmjumhdw => fdyegizr, vf => fdyegizr);
  
  -- Multi-driven assignments
  sgdrlwwf <= 'H';
  fdyegizr <= 'Z';
  sgdrlwwf <= sgdrlwwf;
end kmwmokboq;

library ieee;
use ieee.std_logic_1164.all;

entity jutxkd is
  port (ogzblawu : out severity_level; vyl : in std_logic; ivz : buffer real);
end jutxkd;

architecture lrytrrasl of jutxkd is
  signal pct : real;
  signal kwm : real;
begin
  qvksdwt : entity work.botmdp
    port map (qztdlucdro => kwm, lssmjumhdw => vyl, vf => vyl);
  sbucazms : entity work.botmdp
    port map (qztdlucdro => pct, lssmjumhdw => vyl, vf => vyl);
  kry : entity work.botmdp
    port map (qztdlucdro => ivz, lssmjumhdw => vyl, vf => vyl);
  
  -- Single-driven assignments
  ogzblawu <= FAILURE;
end lrytrrasl;



-- Seed after: 16409563599240930216,511364357853360275
