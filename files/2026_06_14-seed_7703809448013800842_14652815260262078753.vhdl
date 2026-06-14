-- Seed: 7703809448013800842,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity vpmdxhkced is
  port (bpuaa : in time; zg : inout std_logic; gqn : buffer boolean);
end vpmdxhkced;

architecture mobfzik of vpmdxhkced is
  
begin
  -- Single-driven assignments
  gqn <= FALSE;
  
  -- Multi-driven assignments
  zg <= 'Z';
end mobfzik;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (rlsxtzsih : out time; wvf : buffer std_logic);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture pzplinbm of g is
  signal zdyvl : boolean;
  signal gfn : time;
  signal f : boolean;
  signal iyhpbsui : std_logic;
  signal rlj : time;
  signal hyzhk : boolean;
  signal ruuqeoy : std_logic;
begin
  zvj : entity work.vpmdxhkced
    port map (bpuaa => rlsxtzsih, zg => ruuqeoy, gqn => hyzhk);
  njwojaox : entity work.vpmdxhkced
    port map (bpuaa => rlj, zg => iyhpbsui, gqn => f);
  hzxgissxyu : entity work.vpmdxhkced
    port map (bpuaa => gfn, zg => ruuqeoy, gqn => zdyvl);
  
  -- Single-driven assignments
  rlsxtzsih <= 2#000# us;
  rlj <= 4 ns;
  gfn <= 2#0_1_1.111# ns;
  
  -- Multi-driven assignments
  iyhpbsui <= 'H';
  ruuqeoy <= 'W';
  wvf <= 'Z';
end pzplinbm;

entity odcxx is
  port (tzajw : linkage time);
end odcxx;

architecture x of odcxx is
  
begin
  
end x;

entity ykbufsh is
  port (dlhques : linkage real_vector(1 downto 0));
end ykbufsh;

library ieee;
use ieee.std_logic_1164.all;

architecture luq of ykbufsh is
  signal e : boolean;
  signal ejkbdaux : std_logic;
  signal p : boolean;
  signal vgxucwqx : boolean;
  signal i : std_logic;
  signal wsbkjt : time;
begin
  ey : entity work.vpmdxhkced
    port map (bpuaa => wsbkjt, zg => i, gqn => vgxucwqx);
  pygribax : entity work.vpmdxhkced
    port map (bpuaa => wsbkjt, zg => i, gqn => p);
  qmnt : entity work.g
    port map (rlsxtzsih => wsbkjt, wvf => i);
  lrppmwrbjc : entity work.vpmdxhkced
    port map (bpuaa => wsbkjt, zg => ejkbdaux, gqn => e);
  
  -- Multi-driven assignments
  i <= 'U';
  ejkbdaux <= 'X';
  i <= 'U';
end luq;



-- Seed after: 8429415568859527935,14652815260262078753
