-- Seed: 6813352848810645594,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity zdmvabdu is
  port (vxzfoz : in time; mynqpwe : out real; tslt : in std_logic; q : buffer integer);
end zdmvabdu;

architecture hniwtmfh of zdmvabdu is
  
begin
  -- Single-driven assignments
  q <= 2#0#;
  mynqpwe <= 0_3_3_4.3_0;
end hniwtmfh;

library ieee;
use ieee.std_logic_1164.all;

entity yokzyzt is
  port (imrltkjk : in real_vector(2 downto 3); ge : buffer std_logic);
end yokzyzt;

architecture l of yokzyzt is
  signal gdwj : integer;
  signal mfqohf : real;
  signal scltjjornq : time;
  signal nvlpslsh : integer;
  signal dmdmzzve : real;
  signal eryzoy : time;
begin
  xfcsjbxssw : entity work.zdmvabdu
    port map (vxzfoz => eryzoy, mynqpwe => dmdmzzve, tslt => ge, q => nvlpslsh);
  rkhqb : entity work.zdmvabdu
    port map (vxzfoz => scltjjornq, mynqpwe => mfqohf, tslt => ge, q => gdwj);
  
  -- Single-driven assignments
  scltjjornq <= scltjjornq;
  eryzoy <= 3 min;
  
  -- Multi-driven assignments
  ge <= 'H';
  ge <= 'L';
end l;

library ieee;
use ieee.std_logic_1164.all;

entity vx is
  port (jsxcuywm : out std_logic);
end vx;

architecture f of vx is
  signal mqborjlgn : integer;
  signal gksne : real;
  signal txg : time;
  signal c : real_vector(2 downto 3);
  signal czvl : integer;
  signal x : real;
  signal tphrfpurtf : time;
begin
  z : entity work.zdmvabdu
    port map (vxzfoz => tphrfpurtf, mynqpwe => x, tslt => jsxcuywm, q => czvl);
  jqgu : entity work.yokzyzt
    port map (imrltkjk => c, ge => jsxcuywm);
  ofdhmua : entity work.zdmvabdu
    port map (vxzfoz => txg, mynqpwe => gksne, tslt => jsxcuywm, q => mqborjlgn);
  
  -- Single-driven assignments
  txg <= txg;
  c <= c;
  tphrfpurtf <= txg;
  
  -- Multi-driven assignments
  jsxcuywm <= 'X';
end f;

entity x is
  port (vmgkdkpuui : in time; qnszedfhuj : inout real; ccsh : out real);
end x;

architecture xqtqp of x is
  
begin
  
end xqtqp;



-- Seed after: 11106916446271452747,13843488114570579517
