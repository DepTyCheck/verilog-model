-- Seed: 5964411070731752007,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity au is
  port (caxc : buffer std_logic; xzvai : out real; nvcxyphupi : out std_logic; xfigf : out std_logic);
end au;

architecture twlmt of au is
  
begin
  -- Single-driven assignments
  xzvai <= 2#11.1#;
  
  -- Multi-driven assignments
  nvcxyphupi <= 'U';
  nvcxyphupi <= 'H';
  caxc <= 'L';
  nvcxyphupi <= 'X';
end twlmt;

entity yi is
  port (kp : buffer boolean);
end yi;

library ieee;
use ieee.std_logic_1164.all;

architecture o of yi is
  signal h : std_logic;
  signal jeupinj : std_logic;
  signal ptfajkbk : real;
  signal f : std_logic;
  signal lyqzm : std_logic;
  signal qplwh : real;
  signal gna : real;
  signal faeaookjw : std_logic;
begin
  qg : entity work.au
    port map (caxc => faeaookjw, xzvai => gna, nvcxyphupi => faeaookjw, xfigf => faeaookjw);
  fxwiapnagv : entity work.au
    port map (caxc => faeaookjw, xzvai => qplwh, nvcxyphupi => lyqzm, xfigf => f);
  qx : entity work.au
    port map (caxc => f, xzvai => ptfajkbk, nvcxyphupi => jeupinj, xfigf => h);
  
  -- Single-driven assignments
  kp <= TRUE;
  
  -- Multi-driven assignments
  lyqzm <= '-';
  f <= 'W';
end o;

library ieee;
use ieee.std_logic_1164.all;

entity knsmoxi is
  port (ewofrgw : inout character; v : linkage time_vector(1 downto 1); rf : out time; gydfub : buffer std_logic);
end knsmoxi;

library ieee;
use ieee.std_logic_1164.all;

architecture nz of knsmoxi is
  signal xwuee : real;
  signal f : std_logic;
begin
  semboalxx : entity work.au
    port map (caxc => f, xzvai => xwuee, nvcxyphupi => gydfub, xfigf => f);
  
  -- Single-driven assignments
  ewofrgw <= 't';
  rf <= 22.12 us;
  
  -- Multi-driven assignments
  gydfub <= '0';
end nz;

entity yqtzrbc is
  port (cahfljs : in integer);
end yqtzrbc;

library ieee;
use ieee.std_logic_1164.all;

architecture cbcahyizzh of yqtzrbc is
  signal apntlfsf : std_logic;
  signal wehlj : time;
  signal wynxt : time_vector(1 downto 1);
  signal kcbz : character;
  signal d : std_logic;
  signal pl : real;
  signal mclh : real;
  signal lyxd : std_logic;
  signal ssc : std_logic;
  signal voixcvk : real;
  signal ood : std_logic;
begin
  saq : entity work.au
    port map (caxc => ood, xzvai => voixcvk, nvcxyphupi => ood, xfigf => ssc);
  gdu : entity work.au
    port map (caxc => lyxd, xzvai => mclh, nvcxyphupi => ood, xfigf => ssc);
  vz : entity work.au
    port map (caxc => ssc, xzvai => pl, nvcxyphupi => d, xfigf => ood);
  tcxut : entity work.knsmoxi
    port map (ewofrgw => kcbz, v => wynxt, rf => wehlj, gydfub => apntlfsf);
  
  -- Multi-driven assignments
  apntlfsf <= 'U';
  apntlfsf <= 'L';
  lyxd <= 'L';
end cbcahyizzh;



-- Seed after: 7731041931958296384,6697892553037813751
