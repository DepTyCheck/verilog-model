-- Seed: 17522813604288865733,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (jakfjgzci : buffer std_logic; eodgjvi : in bit_vector(1 to 2); doym : buffer std_logic_vector(4 downto 1); akjodgxm : buffer std_logic);
end f;

architecture pkhp of f is
  
begin
  
end pkhp;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (djdlm : linkage std_logic; da : linkage boolean);
end m;

architecture kvgqsvfl of m is
  
begin
  
end kvgqsvfl;

library ieee;
use ieee.std_logic_1164.all;

entity zrlghxf is
  port (eqcnbkio : buffer std_logic; bpci : buffer severity_level; xiofrzab : in integer; z : inout severity_level);
end zrlghxf;

architecture tqroq of zrlghxf is
  
begin
  -- Multi-driven assignments
  eqcnbkio <= eqcnbkio;
end tqroq;

library ieee;
use ieee.std_logic_1164.all;

entity bqmsr is
  port (boujrsxlbf : buffer std_logic);
end bqmsr;

library ieee;
use ieee.std_logic_1164.all;

architecture d of bqmsr is
  signal qjumvhlld : std_logic;
  signal osdrml : bit_vector(1 to 2);
  signal hcvugtt : std_logic;
  signal lavhllarq : std_logic;
  signal mdp : std_logic_vector(4 downto 1);
  signal k : bit_vector(1 to 2);
begin
  pjitodi : entity work.f
    port map (jakfjgzci => boujrsxlbf, eodgjvi => k, doym => mdp, akjodgxm => boujrsxlbf);
  c : entity work.f
    port map (jakfjgzci => boujrsxlbf, eodgjvi => k, doym => mdp, akjodgxm => lavhllarq);
  vh : entity work.f
    port map (jakfjgzci => hcvugtt, eodgjvi => osdrml, doym => mdp, akjodgxm => qjumvhlld);
  
  -- Single-driven assignments
  k <= k;
  osdrml <= ('1', '1');
  
  -- Multi-driven assignments
  boujrsxlbf <= lavhllarq;
  qjumvhlld <= boujrsxlbf;
  boujrsxlbf <= boujrsxlbf;
  hcvugtt <= 'H';
end d;



-- Seed after: 4202674824071546231,14141408946471626091
