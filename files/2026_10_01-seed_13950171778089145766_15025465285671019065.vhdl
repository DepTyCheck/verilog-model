-- Seed: 13950171778089145766,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity dp is
  port (vycuc : linkage std_logic_vector(1 downto 4));
end dp;

architecture bnngmkme of dp is
  
begin
  
end bnngmkme;

library ieee;
use ieee.std_logic_1164.all;

entity duvz is
  port (lanfsbj : inout boolean; xsudrkn : inout std_logic; utqjfej : inout integer; fpljsw : buffer std_logic);
end duvz;

architecture ctwdb of duvz is
  
begin
  -- Single-driven assignments
  utqjfej <= utqjfej;
  lanfsbj <= lanfsbj;
  
  -- Multi-driven assignments
  fpljsw <= 'W';
  xsudrkn <= fpljsw;
  xsudrkn <= fpljsw;
  fpljsw <= '0';
end ctwdb;

entity zf is
  port (vpql : in severity_level; t : in character);
end zf;

library ieee;
use ieee.std_logic_1164.all;

architecture zllbkmlyn of zf is
  signal eqcnjvct : std_logic_vector(1 downto 4);
  signal ijjzg : std_logic;
  signal wm : integer;
  signal ornyirpjq : std_logic;
  signal qqyvwoulk : boolean;
  signal ltj : std_logic_vector(1 downto 4);
begin
  tgwwq : entity work.dp
    port map (vycuc => ltj);
  gpybabuqg : entity work.duvz
    port map (lanfsbj => qqyvwoulk, xsudrkn => ornyirpjq, utqjfej => wm, fpljsw => ijjzg);
  gy : entity work.dp
    port map (vycuc => eqcnjvct);
  bj : entity work.dp
    port map (vycuc => ltj);
  
  -- Multi-driven assignments
  ijjzg <= ornyirpjq;
  ltj <= (others => '0');
end zllbkmlyn;

entity b is
  port (eepz : inout severity_level; phxujauqjx : in integer_vector(0 downto 0); mngownzshc : linkage real);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture xuoogf of b is
  signal ey : character;
  signal prmiev : severity_level;
  signal eutjawto : std_logic;
  signal g : integer;
  signal wogetqeib : std_logic;
  signal nkzxtubdie : boolean;
begin
  ef : entity work.duvz
    port map (lanfsbj => nkzxtubdie, xsudrkn => wogetqeib, utqjfej => g, fpljsw => eutjawto);
  w : entity work.zf
    port map (vpql => prmiev, t => ey);
  
  -- Single-driven assignments
  prmiev <= NOTE;
  eepz <= NOTE;
end xuoogf;



-- Seed after: 13525499230465982423,15025465285671019065
