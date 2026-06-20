-- Seed: 4665059392613049795,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity hwjliqjwwn is
  port (sgwy : linkage std_logic; fz : buffer boolean; oparxlbk : out integer);
end hwjliqjwwn;

architecture iejw of hwjliqjwwn is
  
begin
  -- Single-driven assignments
  fz <= TRUE;
  oparxlbk <= 40;
end iejw;

entity zlqdlc is
  port (u : buffer time; qwslply : buffer time_vector(2 to 2); hvecreb : inout severity_level; blopsxfius : out integer);
end zlqdlc;

library ieee;
use ieee.std_logic_1164.all;

architecture uwq of zlqdlc is
  signal t : integer;
  signal vbnzmd : boolean;
  signal rbxha : integer;
  signal wvarblod : boolean;
  signal x : std_logic;
begin
  d : entity work.hwjliqjwwn
    port map (sgwy => x, fz => wvarblod, oparxlbk => rbxha);
  fhotoi : entity work.hwjliqjwwn
    port map (sgwy => x, fz => vbnzmd, oparxlbk => t);
  
  -- Single-driven assignments
  qwslply <= (others => 16#079# ns);
  u <= 1 fs;
  hvecreb <= WARNING;
  
  -- Multi-driven assignments
  x <= 'H';
end uwq;

library ieee;
use ieee.std_logic_1164.all;

entity zfj is
  port (bmkbitlu : out time; oayja : buffer std_logic_vector(3 downto 2); pkow : out integer; inufi : out integer);
end zfj;

library ieee;
use ieee.std_logic_1164.all;

architecture iogymsdah of zfj is
  signal tylnapyzqd : integer;
  signal vdxhd : boolean;
  signal d : std_logic;
  signal k : boolean;
  signal fwuzjtqfgp : std_logic;
begin
  noiwxbl : entity work.hwjliqjwwn
    port map (sgwy => fwuzjtqfgp, fz => k, oparxlbk => inufi);
  swiffajik : entity work.hwjliqjwwn
    port map (sgwy => d, fz => vdxhd, oparxlbk => tylnapyzqd);
  
  -- Single-driven assignments
  pkow <= 4_2_4;
  
  -- Multi-driven assignments
  oayja <= "X1";
  fwuzjtqfgp <= 'H';
  oayja <= "LW";
  d <= '1';
end iogymsdah;

entity isxgqh is
  port (cshkn : in time; deyxafjl : inout integer);
end isxgqh;

library ieee;
use ieee.std_logic_1164.all;

architecture ijarti of isxgqh is
  signal tkgiuyd : integer;
  signal xjyj : severity_level;
  signal swbpesynzv : time_vector(2 to 2);
  signal ofp : time;
  signal rakva : integer;
  signal pyysurjaas : std_logic_vector(3 downto 2);
  signal lydlmvrrg : time;
  signal vbpgstui : integer;
  signal muluybdh : boolean;
  signal miwmdkhgxp : std_logic;
  signal jjexnwwv : integer;
  signal y : severity_level;
  signal ql : time_vector(2 to 2);
  signal xuef : time;
begin
  htzozo : entity work.zlqdlc
    port map (u => xuef, qwslply => ql, hvecreb => y, blopsxfius => jjexnwwv);
  urdc : entity work.hwjliqjwwn
    port map (sgwy => miwmdkhgxp, fz => muluybdh, oparxlbk => vbpgstui);
  pyakpdv : entity work.zfj
    port map (bmkbitlu => lydlmvrrg, oayja => pyysurjaas, pkow => rakva, inufi => deyxafjl);
  dglvgwp : entity work.zlqdlc
    port map (u => ofp, qwslply => swbpesynzv, hvecreb => xjyj, blopsxfius => tkgiuyd);
  
  -- Multi-driven assignments
  miwmdkhgxp <= 'X';
  pyysurjaas <= "X0";
end ijarti;



-- Seed after: 253802127964448249,17924494779688682807
