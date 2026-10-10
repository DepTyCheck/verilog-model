-- Seed: 16811000911794810229,511364357853360275

entity ldks is
  port (zhqwyv : inout real_vector(4 to 4); jtang : out time_vector(4 to 1));
end ldks;

architecture c of ldks is
  
begin
  -- Single-driven assignments
  jtang <= jtang;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity nc is
  port (a : out real; lfwcww : out std_logic_vector(3 downto 1); oamclprg : in boolean; fxkxd : out time);
end nc;

architecture gzovdm of nc is
  signal u : time_vector(4 to 1);
  signal goxuiw : real_vector(4 to 4);
  signal q : time_vector(4 to 1);
  signal rbuzu : real_vector(4 to 4);
  signal upye : time_vector(4 to 1);
  signal whgp : real_vector(4 to 4);
begin
  rm : entity work.ldks
    port map (zhqwyv => whgp, jtang => upye);
  dksquv : entity work.ldks
    port map (zhqwyv => rbuzu, jtang => q);
  oyf : entity work.ldks
    port map (zhqwyv => goxuiw, jtang => u);
  
  -- Single-driven assignments
  a <= a;
  fxkxd <= 1_0 ms;
  
  -- Multi-driven assignments
  lfwcww <= lfwcww;
  lfwcww <= lfwcww;
end gzovdm;

library ieee;
use ieee.std_logic_1164.all;

entity nahwdqu is
  port (nyprq : in real; kkvhpk : buffer std_logic_vector(3 to 4); hpteknkwn : in real; ms : in std_logic_vector(0 downto 2));
end nahwdqu;

library ieee;
use ieee.std_logic_1164.all;

architecture lw of nahwdqu is
  signal gaqbyep : time_vector(4 to 1);
  signal copuckw : real_vector(4 to 4);
  signal dt : time_vector(4 to 1);
  signal mdaytie : real_vector(4 to 4);
  signal cukymohjej : time;
  signal m : boolean;
  signal rcqdsjbx : std_logic_vector(3 downto 1);
  signal brlfhu : real;
begin
  cdobubrzz : entity work.nc
    port map (a => brlfhu, lfwcww => rcqdsjbx, oamclprg => m, fxkxd => cukymohjej);
  fzdmxyw : entity work.ldks
    port map (zhqwyv => mdaytie, jtang => dt);
  vxhmy : entity work.ldks
    port map (zhqwyv => copuckw, jtang => gaqbyep);
  
  -- Single-driven assignments
  m <= FALSE;
  
  -- Multi-driven assignments
  kkvhpk <= kkvhpk;
end lw;



-- Seed after: 9086977804207033605,511364357853360275
